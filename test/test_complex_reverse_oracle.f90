program test_complex_reverse_oracle
    !! Independent real-coordinate oracle for the bounded complex VJP slice.
    !!
    !! A real objective obtained from `real(z)` has a two-coordinate gradient;
    !! FortAD represents it as a complex adjoint.  The hand derivative, a
    !! central difference in an arbitrary complex direction, and the real
    !! adjoint identity all check the generated routine.  A non-holomorphic
    !! `abs(z)` case remains an explicit refusal boundary.
    use fortad, only: fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module complex_projection_case"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure real(8) function project(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        y = 2.5d0*real(z) + 0.5d0*dble(z) + 1.75d0"//nl// &
        "    end function project"//nl// &
        "end module complex_projection_case"//nl
    character(len=*), parameter :: bad_source = &
        "module complex_projection_bad"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure real(8) function magnitude(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        y = abs(z)"//nl// &
        "    end function magnitude"//nl// &
        "end module complex_projection_bad"//nl
    type(fad_result_t) :: vjp, refused
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    vjp = fad_vjp(source, [character(len=1) :: "z"], dependent="y", &
        from="project", name="project_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL complex projection VJP generation: ", vjp%message
        error stop 1
    end if

    refused = fad_vjp(bad_source, [character(len=1) :: "z"], dependent="y", &
        from="magnitude")
    if (refused%ok .or. .not. allocated(refused%message) .or. &
        index(refused%message, "complex") == 0) then
        print *, "FAIL unsupported complex reverse path was accepted"
        error stop 2
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

    driver = &
        "program driver"//nl// &
        "    use complex_projection_case, only: project"//nl// &
        "    use generated_complex_projection, only: project_vjp"//nl// &
        "    implicit none"//nl// &
        "    complex(8) :: z, dz, z_b"//nl// &
        "    real(8) :: y, y_b, yp, ym, h, fd, want, lhs, rhs, err"//nl// &
        "    z = cmplx(0.7d0,-0.4d0,8)"//nl// &
        "    dz = cmplx(-0.2d0,0.35d0,8)"//nl// &
        "    y_b = -1.3d0"//nl// &
        "    y = project(z)"//nl// &
        "    call project_vjp(z, y, y_b, z_b)"//nl// &
        "    err = abs(z_b-cmplx(3.0d0*y_b,0.0d0,8))"//nl// &
        "    if (err > 1.0d-13) then"//nl// &
        "        print *, 'complex projection hand-adjoint error', err, z_b"//nl// &
        "        error stop 3"//nl// &
        "    end if"//nl// &
        "    h = 1.0d-6"//nl// &
        "    yp = project(z+h*dz)"//nl// &
        "    ym = project(z-h*dz)"//nl// &
        "    fd = (yp-ym)/(2.0d0*h)"//nl// &
        "    want = 3.0d0*real(dz)"//nl// &
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
        "    print *, 'test_complex_reverse_oracle: all cases passed'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/vjp.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL complex projection generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 6
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL complex projection independent oracle failed"
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
