program test_complex_intrinsic_oracle
    !! Independent real-coordinate oracle for complex intrinsic tangents.
    !!
    !! The generated routine must compile for a genuinely complex active
    !! argument.  Central differences in the two real coordinates catch the
    !! common mistake of applying a real-only rule such as sign(1,z) to abs(z),
    !! and also check real/aimag/cmplx/conjg together with complex division.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module complex_kernel"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    function evaluate(z) result(y)"//nl// &
        "        complex(8), intent(in) :: z"//nl// &
        "        complex(8) :: y"//nl// &
        "        y = z/(1.0d0+z) + abs(z) + cmplx(real(z),aimag(z),8) + conjg(z)"//nl// &
        "    end function evaluate"//nl// &
        "end module complex_kernel"//nl
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    jvp = fad_jvp(source, ["z"], from="evaluate", name="evaluate_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL complex intrinsic generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, ["z"], dependent="y", from="evaluate")
    if (vjp%ok .or. index(vjp%message, "complex") == 0) then
        print *, "FAIL complex reverse boundary was not named"
        error stop 2
    end if

    dir = "build/oracle_complex_intrinsics"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create complex oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/jvp.f90", status="replace", action="write")
    write (unit, '(a)') "module generated_complex"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') "end module generated_complex"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use generated_complex, only: evaluate_jvp"//nl// &
        "    implicit none"//nl// &
        "    complex(8) :: z, zd, y, yd, yp, ym, want"//nl// &
        "    real(8) :: h, err"//nl// &
        "    z = cmplx(0.7d0,-0.4d0,8)"//nl// &
        "    zd = cmplx(-0.2d0,0.35d0,8)"//nl// &
        "    h = 1.0d-6"//nl// &
        "    call evaluate_jvp(z, zd, y, yd)"//nl// &
        "    want = (zd*(1.0d0+z)-z*zd)/(1.0d0+z)**2"//nl// &
        "    want = want + real(conjg(z)*zd)/abs(z) + zd + conjg(zd)"//nl// &
        "    err = abs(yd-want)/max(1.0d0,abs(want))"//nl// &
        "    if (err > 1.0d-12) then"//nl// &
        "        print *, 'analytic complex tangent error', err, yd, want"//nl// &
        "        error stop 3"//nl// &
        "    end if"//nl// &
        "    call evaluate_jvp(z+h*zd, (0.0d0,0.0d0), yp, yd)"//nl// &
        "    call evaluate_jvp(z-h*zd, (0.0d0,0.0d0), ym, yd)"//nl// &
        "    err = abs((yp-ym)/(2.0d0*h)-want)/max(1.0d0,abs(want))"//nl// &
        "    if (err > 1.0d-7) then"//nl// &
        "        print *, 'finite-difference complex tangent error', err"//nl// &
        "        error stop 4"//nl// &
        "    end if"//nl// &
        "    print *, 'test_complex_intrinsic_oracle: all cases passed'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/jvp.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL complex generated source did not compile"
        error stop 5
    end if
    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "complex intrinsic oracle failed"
end program test_complex_intrinsic_oracle
