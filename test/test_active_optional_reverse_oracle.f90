program test_active_optional_reverse_oracle
    !! Independent oracle for an active optional reverse argument.
    !! The primal optional may be present or omitted; its VJP cotangent is a
    !! required output and is zero on the omitted path.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir, driver
    type(fad_result_t) :: jvp, vjp
    integer :: stat

    source = &
        "subroutine active_optional(x, y, z)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(in), optional :: y"//nl// &
        "    real(8), intent(out) :: z"//nl// &
        "    z = x"//nl// &
        "    if (present(y)) z = z + y*y"//nl// &
        "end subroutine active_optional"//nl

    jvp = fad_jvp(source, ["y"], from="active_optional", &
        name="active_optional_jvp")
    vjp = fad_vjp(source, ["y"], dependent="z", from="active_optional", &
        name="active_optional_vjp")
    if (.not. jvp%ok) then
        print *, "FAIL active optional reverse JVP: ", jvp%message
        error stop 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL active optional reverse VJP: ", vjp%message
        error stop 2
    end if

    dir = "build/oracle_active_optional_reverse"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create active-optional oracle directory"

    call write_file(dir//"/primal.f90", "module primal_mod"//nl// &
        "contains"//nl//source//"end module primal_mod"//nl)
    call write_file(dir//"/derivatives.f90", "module derivative_mod"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module derivative_mod"//nl)
    driver = driver_source()
    call write_file(dir//"/driver.f90", driver)

    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
        "-Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "active-optional generated source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "active-optional numerical oracle failed"
    end if
    print *, "test_active_optional_reverse_oracle: all cases passed"

contains

    function driver_source() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use primal_mod, only: active_optional"//nl// &
            "    use derivative_mod, only: active_optional_jvp, "// &
            "active_optional_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, y, z, yd, zd, yb, h, fp, fm, dy"//nl// &
            "    x = 1.5d0"//nl// &
            "    y = 2.0d0"//nl// &
            "    dy = -0.7d0"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call active_optional_jvp(x=x, y=y, y_d=dy, z=z, z_d=zd)"//nl// &
            "    if (abs(z-5.5d0) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(zd-4.0d0*dy) > 1.0d-12) error stop 2"//nl// &
            "    fp = active_value(x, y+h); fm = active_value(x, y-h)"//nl// &
            "    if (abs(zd-dy*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
            "    call active_optional_vjp(x=x, y=y, z=z, z_b=1.0d0, y_b=yb)"//nl// &
            "    if (abs(yb-4.0d0) > 1.0d-12) error stop 4"//nl// &
            "    if (abs(zd-yb*dy) > 1.0d-12) error stop 5"//nl// &
            "    call active_optional_jvp(x=x, z=z, z_d=zd)"//nl// &
            "    if (abs(z-x) > 1.0d-12 .or. abs(zd) > 1.0d-12) error stop 6"//nl// &
            "    call active_optional_vjp(x=x, z=z, z_b=1.0d0, y_b=yb)"//nl// &
            "    if (abs(yb) > 1.0d-12) error stop 7"//nl// &
            "    print *, 'active optional reverse oracle pass'"//nl// &
            "contains"//nl// &
            "    real(8) function active_value(x, y) result(z)"//nl// &
            "        real(8), intent(in) :: x, y"//nl// &
            "        z = x + y*y"//nl// &
            "    end function active_value"//nl// &
            "end program driver"//nl
    end function driver_source

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: file_unit

        open (newunit=file_unit, file=path, status="replace", action="write")
        write (file_unit, '(a)') text
        close (file_unit)
    end subroutine write_file

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

end program test_active_optional_reverse_oracle
