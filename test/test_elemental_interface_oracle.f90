program test_elemental_interface_oracle
    !! Elemental procedure regression with an independent compiled oracle.
    !!
    !! The generated JVP and VJP must retain ELEMENTAL semantics.  The driver
    !! calls each derivative with both scalar and rank-one actuals, compares the
    !! scalar tangent with a central finite difference, and checks the reverse
    !! product componentwise against the hand derivative.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module elemental_source_mod"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    elemental real(8) function scale(x) result(z)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        z = 3d0*x + 1d0"//nl// &
        "    end function scale"//nl// &
        "end module elemental_source_mod"//nl
    type(fad_result_t) :: jvp, vjp
    integer :: failures, unit, stat
    character(len=:), allocatable :: dir, driver

    failures = 0
    dir = "build/oracle_elemental_interface"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL elemental: could not create oracle directory"
        error stop 1
    end if

    jvp = fad_jvp(source, [character(len=1) :: "x"], from="scale", &
        module_name="elemental_jvp_mod", name="scale_jvp")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="z", &
        from="scale", module_name="elemental_vjp_mod", name="scale_vjp")
    if (.not. jvp%ok) then
        print *, "FAIL elemental JVP generation: ", jvp%message
        failures = failures + 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL elemental VJP generation: ", vjp%message
        failures = failures + 1
    end if
    if (jvp%ok) then
        if (index(jvp%code, "elemental pure subroutine scale_jvp") == 0) then
            print *, "FAIL elemental JVP lost ELEMENTAL prefix"
            failures = failures + 1
        end if
    end if
    if (vjp%ok) then
        if (index(vjp%code, "elemental pure subroutine scale_vjp") == 0) then
            print *, "FAIL elemental VJP lost ELEMENTAL prefix"
            failures = failures + 1
        end if
    end if
    if (failures > 0) error stop 1

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') source
    close (unit)

    open (newunit=unit, file=dir//"/jvp.f90", status="replace", &
        action="write")
    write (unit, '(a)') jvp%code
    close (unit)

    open (newunit=unit, file=dir//"/vjp.f90", status="replace", &
        action="write")
    write (unit, '(a)') vjp%code
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use elemental_source_mod, only: scale"//nl// &
        "    use elemental_jvp_mod, only: scale_jvp"//nl// &
        "    use elemental_vjp_mod, only: scale_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, x_d, z, z_d, x_b, h, fp, fm"//nl// &
        "    real(8) :: xa(3), x_da(3), za(3), z_da(3)"//nl// &
        "    real(8) :: x_ba(3), z_ba(3)"//nl// &
        "    x = 2.0d0"//nl// &
        "    x_d = 1.0d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    call scale_jvp(x, x_d, z, z_d)"//nl// &
        "    if (abs(z - 7.0d0) > 1.0d-12 .or. "// &
        "abs(z_d - 3.0d0) > 1.0d-12) error stop 1"//nl// &
        "    fp = scale(x + h)"//nl// &
        "    fm = scale(x - h)"//nl// &
        "    if (abs(z_d - (fp - fm)/(2.0d0*h)) > 1.0d-7) error stop 2"//nl// &
        "    call scale_vjp(x, z, 1.0d0, x_b)"//nl// &
        "    if (abs(x_b - 3.0d0) > 1.0d-12) error stop 3"//nl// &
        "    xa = [1.0d0, 2.0d0, 4.0d0]"//nl// &
        "    x_da = [2.0d0, -1.0d0, 0.5d0]"//nl// &
        "    z_ba = [1.0d0, 2.0d0, -3.0d0]"//nl// &
        "    call scale_jvp(xa, x_da, za, z_da)"//nl// &
        "    if (maxval(abs(za - [4.0d0, 7.0d0, 13.0d0])) > 1.0d-12) "// &
        "error stop 4"//nl// &
        "    if (maxval(abs(z_da - [6.0d0, -3.0d0, 1.5d0])) > 1.0d-12) "// &
        "error stop 5"//nl// &
        "    call scale_vjp(xa, za, z_ba, x_ba)"//nl// &
        "    if (maxval(abs(x_ba - [3.0d0, 6.0d0, -9.0d0])) > 1.0d-12) "// &
        "error stop 6"//nl// &
        "    print *, 'elemental oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/jvp.f90 "//dir//"/vjp.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL elemental: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL elemental: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_elemental_interface_oracle: all cases passed"

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

end program test_elemental_interface_oracle
