program test_complex_reverse_oracle
    !! Independent real-coordinate oracle for the bounded complex VJP slice.
    !!
    !! A real objective obtained from `real(z)`, `dble(z)`, `aimag(z)`, or
    !! nonzero `abs(z)` has a two-coordinate gradient;
    !! FortAD represents it as a complex adjoint.  The hand derivative, a
    !! central difference in an arbitrary complex direction, and the real
    !! adjoint identity all check the generated routines.  Zero, `conjg`, and
    !! complex arithmetic remain explicit refusal boundaries.
    use fortad, only: fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module complex_projection_case"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure real(8) function project(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        y = 2.5d0*real(z) + 0.5d0*dble(z) + 3.25d0*aimag(z) + "// &
        "1.75d0"//nl// &
        "    end function project"//nl// &
        "end module complex_projection_case"//nl
    character(len=*), parameter :: abs_source = &
        "module complex_abs_case"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure real(8) function magnitude(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        y = abs(z)"//nl// &
        "    end function magnitude"//nl// &
        "end module complex_abs_case"//nl
    character(len=*), parameter :: zero_source = &
        "module complex_zero_abs_bad"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure real(8) function zero_magnitude(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        y = abs(z-z)"//nl// &
        "    end function zero_magnitude"//nl// &
        "end module complex_zero_abs_bad"//nl
    character(len=*), parameter :: bad_source = &
        "module complex_projection_bad"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure real(8) function magnitude(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        y = abs(z*z)"//nl// &
        "    end function magnitude"//nl// &
        "end module complex_projection_bad"//nl
    character(len=*), parameter :: conjg_source = &
        "module complex_conjg_projection_bad"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure real(8) function conjugate_projection(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        y = real(conjg(z))"//nl// &
        "    end function conjugate_projection"//nl// &
        "end module complex_conjg_projection_bad"//nl
    type(fad_result_t) :: vjp, abs_vjp, refused_zero, refused_arithmetic
    type(fad_result_t) :: refused_conjg
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    vjp = fad_vjp(source, [character(len=1) :: "z"], dependent="y", &
        from="project", name="project_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL complex projection VJP generation: ", vjp%message
        error stop 1
    end if

    abs_vjp = fad_vjp(abs_source, [character(len=1) :: "z"], dependent="y", &
        from="magnitude", name="magnitude_vjp")
    if (.not. abs_vjp%ok) then
        print *, "FAIL complex abs VJP generation: ", abs_vjp%message
        error stop 2
    end if

    refused_zero = fad_vjp(zero_source, [character(len=1) :: "z"], dependent="y", &
        from="zero_magnitude")
    if (refused_zero%ok .or. .not. allocated(refused_zero%message) .or. &
        index(refused_zero%message, "not differentiable at zero") == 0) then
        print *, "FAIL abs zero boundary was not refused"
        error stop 3
    end if
    refused_arithmetic = fad_vjp(bad_source, [character(len=1) :: "z"], &
        dependent="y", from="magnitude")
    if (refused_arithmetic%ok .or. &
        .not. allocated(refused_arithmetic%message) .or. &
        index(refused_arithmetic%message, "complex arithmetic") == 0) then
        print *, "FAIL complex arithmetic reverse path was accepted"
        error stop 4
    end if
    refused_conjg = fad_vjp(conjg_source, [character(len=1) :: "z"], &
        dependent="y", from="conjugate_projection")
    if (refused_conjg%ok .or. .not. allocated(refused_conjg%message) .or. &
        index(refused_conjg%message, "conjg") == 0) then
        print *, "FAIL complex conjugation projection was accepted"
        error stop 5
    end if

    dir = "build/oracle_complex_reverse"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create complex reverse oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/vjp.f90", status="replace", action="write")
    write (unit, '(a)') "module generated_complex_projection"
    write (unit, '(a)') "contains"
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module generated_complex_projection"
    close (unit)
    open (newunit=unit, file=dir//"/abs_primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') abs_source
    close (unit)
    open (newunit=unit, file=dir//"/abs_vjp.f90", status="replace", action="write")
    write (unit, '(a)') "module generated_complex_abs"
    write (unit, '(a)') "contains"
    write (unit, '(a)') abs_vjp%code
    write (unit, '(a)') "end module generated_complex_abs"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use complex_projection_case, only: project"//nl// &
        "    use generated_complex_projection, only: project_vjp"//nl// &
        "    use complex_abs_case, only: magnitude"//nl// &
        "    use generated_complex_abs, only: magnitude_vjp"//nl// &
        "    implicit none"//nl// &
        "    complex(8) :: z, dz, z_b, abs_z_b, abs_hand"//nl// &
        "    real(8) :: y, y_b, yp, ym, h, fd, want, lhs, rhs, err"//nl// &
        "    real(8) :: abs_y, abs_y_b, abs_yp, abs_ym, abs_fd, abs_want"//nl// &
        "    real(8) :: abs_lhs, abs_rhs, abs_err"//nl// &
        "    z = cmplx(0.7d0,-0.4d0,8)"//nl// &
        "    dz = cmplx(-0.2d0,0.35d0,8)"//nl// &
        "    y_b = -1.3d0"//nl// &
        "    y = project(z)"//nl// &
        "    call project_vjp(z, y, y_b, z_b)"//nl// &
        "    err = abs(z_b-cmplx(3.0d0*y_b,3.25d0*y_b,8))"//nl// &
        "    if (err > 1.0d-13) then"//nl// &
        "        print *, 'complex projection hand-adjoint error', err, z_b"//nl// &
        "        error stop 3"//nl// &
        "    end if"//nl// &
        "    h = 1.0d-6"//nl// &
        "    yp = project(z+h*dz)"//nl// &
        "    ym = project(z-h*dz)"//nl// &
        "    fd = (yp-ym)/(2.0d0*h)"//nl// &
        "    want = 3.0d0*real(dz) + 3.25d0*aimag(dz)"//nl// &
        "    if (abs(fd-want) > 1.0d-8) then"//nl// &
        "        print *, 'complex projection finite-difference error', fd, "// &
        "want"//nl// &
        "        error stop 4"//nl// &
        "    end if"//nl// &
        "    lhs = y_b*want"//nl// &
        "    rhs = real(conjg(z_b)*dz)"//nl// &
        "    if (abs(lhs-rhs) > 1.0d-13) then"//nl// &
        "        print *, 'complex projection adjoint identity error', lhs, rhs"//nl// &
        "        error stop 5"//nl// &
        "    end if"//nl// &
        "    abs_y_b = -0.85d0"//nl// &
        "    abs_y = magnitude(z)"//nl// &
        "    call magnitude_vjp(z, abs_y, abs_y_b, abs_z_b)"//nl// &
        "    abs_hand = abs_y_b*z/abs(z)"//nl// &
        "    abs_err = abs(abs_z_b-abs_hand)"//nl// &
        "    if (abs_err > 1.0d-13) then"//nl// &
        "        print *, 'complex abs hand-adjoint error', abs_err, abs_z_b"//nl// &
        "        error stop 6"//nl// &
        "    end if"//nl// &
        "    abs_yp = magnitude(z+h*dz)"//nl// &
        "    abs_ym = magnitude(z-h*dz)"//nl// &
        "    abs_fd = (abs_yp-abs_ym)/(2.0d0*h)"//nl// &
        "    abs_want = real(conjg(z)*dz)/abs(z)"//nl// &
        "    if (abs(abs_fd-abs_want) > 1.0d-8) then"//nl// &
        "        print *, 'complex abs finite-difference error', abs_fd, "// &
        "abs_want"//nl// &
        "        error stop 7"//nl// &
        "    end if"//nl// &
        "    abs_lhs = abs_y_b*abs_want"//nl// &
        "    abs_rhs = real(conjg(abs_z_b)*dz)"//nl// &
        "    if (abs(abs_lhs-abs_rhs) > 1.0d-13) then"//nl// &
        "        print *, 'complex abs adjoint identity error', abs_lhs, "// &
        "abs_rhs"//nl// &
        "        error stop 8"//nl// &
        "    end if"//nl// &
        "    print *, 'test_complex_reverse_oracle: all cases passed'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/vjp.f90 "// &
        dir//"/abs_primal.f90 "//dir//"/abs_vjp.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL complex generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 6
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL complex independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 7
    end if
    print *, "test_complex_reverse_oracle: all cases passed"

contains

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: ios, file_unit

        open (newunit=file_unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_complex_reverse_oracle
