program test_optional_oracle
    !! Optional-dummy regression with an independent compiled oracle.
    !!
    !! `present(y)` is a source-level interface contract, not an arithmetic
    !! operation.  The generated JVP and VJP must preserve it when `y` is
    !! supplied and when it is omitted.  An active optional is checked too:
    !! both its primal and tangent actual may be omitted together.  The
    !! driver checks both paths against central differences and the closed-form
    !! derivative; reverse active-optionals remain an explicit refusal.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "function f(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(in), optional :: y"//nl// &
        "    real(8) :: z"//nl// &
        "    z = x"//nl// &
        "    if (present(y)) z = z + x*y"//nl// &
        "end function f"//nl
    type(fad_result_t) :: jvp, vjp, active_jvp, active_vjp
    integer :: failures, unit, stat
    character(len=:), allocatable :: dir, driver

    failures = 0
    dir = "build/oracle_optional"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL optional: could not create oracle directory"
        error stop 1
    end if

    jvp = fad_jvp(source, ["x"], name="f_jvp")
    vjp = fad_vjp(source, ["x"], name="f_vjp")
    if (.not. jvp%ok) then
        print *, "FAIL optional JVP generation: ", jvp%message
        failures = failures + 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL optional VJP generation: ", vjp%message
        failures = failures + 1
    end if
    if (failures > 0) error stop 1

    active_jvp = fad_jvp(source, ["y"], name="f_y_jvp")
    active_vjp = fad_vjp( &
        "function active_optional(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(in), optional :: y"//nl// &
        "    real(8) :: z"//nl// &
        "    z = x"//nl// &
        "    if (present(y)) z = z + y*y"//nl// &
        "end function active_optional"//nl, ["y"], name="active_optional_vjp")
    if (.not. active_jvp%ok .or. active_vjp%ok) then
        print *, "FAIL optional: active optional JVP/VJP boundary changed"
        error stop 1
    end if
    if (index(active_vjp%message, "active optional") == 0) then
        print *, "FAIL optional: active optional refusal was not named"
        error stop 1
    end if

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module primal_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module primal_mod"
    close (unit)

    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module derivative_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') active_jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module derivative_mod"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use primal_mod, only: f"//nl// &
        "    use derivative_mod, only: f_jvp, f_y_jvp, f_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, y, z, zd, xb, h, fp, fm"//nl// &
        "    x = 2.0d0"//nl// &
        "    y = 4.0d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    call f_jvp(x=x, x_d=1.0d0, y=y, z=z, z_d=zd)"//nl// &
        "    if (abs(z - 10.0d0) > 1.0d-13 .or. abs(zd - 5.0d0) > 1.0d-13) error stop 1"//nl// &
        "    fp = f(x+h, y); fm = f(x-h, y)"//nl// &
        "    if (abs(zd - (fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 2"//nl// &
        "    call f_y_jvp(x=x, y=y, y_d=1.0d0, z=z, z_d=zd)"//nl// &
        "    if (abs(z - 10.0d0) > 1.0d-13 .or. abs(zd - 2.0d0) > 1.0d-13) error stop 7"//nl// &
        "    fp = f(x, y+h); fm = f(x, y-h)"//nl// &
        "    if (abs(zd - (fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 8"//nl// &
        "    call f_vjp(x=x, y=y, z=z, z_b=1.0d0, x_b=xb)"//nl// &
        "    if (abs(z - 10.0d0) > 1.0d-13 .or. abs(xb - 5.0d0) > 1.0d-13) error stop 3"//nl// &
        "    call f_y_jvp(x=x, z=z, z_d=zd)"//nl// &
        "    if (abs(z - 2.0d0) > 1.0d-13 .or. abs(zd) > 1.0d-13) error stop 9"//nl// &
        "    call f_jvp(x=x, x_d=1.0d0, z=z, z_d=zd)"//nl// &
        "    if (abs(z - 2.0d0) > 1.0d-13 .or. abs(zd - 1.0d0) > 1.0d-13) error stop 4"//nl// &
        "    fp = f(x+h); fm = f(x-h)"//nl// &
        "    if (abs(zd - (fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 5"//nl// &
        "    call f_vjp(x=x, z=z, z_b=1.0d0, x_b=xb)"//nl// &
        "    if (abs(z - 2.0d0) > 1.0d-13 .or. abs(xb - 1.0d0) > 1.0d-13) error stop 6"//nl// &
        "    print *, 'optional oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL optional: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL optional: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_optional_oracle: all cases passed"

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

end program test_optional_oracle
