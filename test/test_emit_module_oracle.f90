program test_emit_module_oracle
    !! Independent numerical oracle for the module-wrapped emitter path.
    !!
    !! The generated module is compiled with GFortran and its JVP is checked
    !! against a hand-derived directional derivative.  This exercises the
    !! complete emitted module, including the indentation path, rather than
    !! checking only the generated text.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, directory
    type(fad_result_t) :: result
    integer :: stat, unit

    source = &
        "subroutine emitter_module_kernel(n, z, y)"//nl// &
        "    integer, intent(in) :: n"//nl// &
        "    real(8), intent(in) :: z(2*n)"//nl// &
        "    real(8), intent(out) :: y"//nl// &
        "    integer :: i, base"//nl// &
        "    real(8) :: value"//nl// &
        "    y = 0.0d0"//nl// &
        "    do i = 1, n"//nl// &
        "        base = 2*(i - 1)"//nl// &
        "        value = sin(z(base + 1))*exp(0.1d0*z(base + 2)) + &"//nl// &
        "            z(base + 1)**2"//nl// &
        "        y = y + value"//nl// &
        "    end do"//nl// &
        "end subroutine emitter_module_kernel"//nl

    result = fad_jvp(source, ["z"], name="emitter_module_kernel_jvp", &
        module_name="emitter_generated")
    if (.not. result%ok) error stop "module emitter generation failed"

    directory = "build/oracle_emit_module"
    call execute_command_line("mkdir -p "//directory, exitstat=stat)
    if (stat /= 0) error stop "could not create module emitter oracle directory"

    open (newunit=unit, file=directory//"/generated.f90", status="replace", &
        action="write")
    write (unit, '(a)') result%code
    close (unit)

    open (newunit=unit, file=directory//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line("cd "//directory//" && gfortran -std=f2018 -O2 "// &
        "-o run generated.f90 driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) error stop "module emitter generated source did not compile"
    call execute_command_line("cd "//directory//" && ./run", exitstat=stat)
    if (stat /= 0) error stop "module emitter independent oracle failed"
    print *, "test_emit_module_oracle: passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use emitter_generated, only: emitter_module_kernel_jvp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 23"//nl// &
            "    real(8) :: z(2*n), dz(2*n), y, y_d, want, want_d"//nl// &
            "    real(8) :: a, b, da, db, value"//nl// &
            "    integer :: i"//nl// &
            "    do i = 1, 2*n"//nl// &
            "        z(i) = 0.1d0 + 0.013d0*i"//nl// &
            "        dz(i) = sin(0.17d0*i)"//nl// &
            "    end do"//nl// &
            "    call emitter_module_kernel_jvp(n, z, dz, y, y_d)"//nl// &
            "    want = 0.0d0"//nl// &
            "    want_d = 0.0d0"//nl// &
            "    do i = 1, n"//nl// &
            "        a = z(2*i - 1)"//nl// &
            "        b = z(2*i)"//nl// &
            "        da = dz(2*i - 1)"//nl// &
            "        db = dz(2*i)"//nl// &
            "        value = sin(a)*exp(0.1d0*b) + a**2"//nl// &
            "        want = want + value"//nl// &
            "        want_d = want_d + (cos(a)*exp(0.1d0*b) + 2.0d0*a)*da + &"//nl// &
            "            0.1d0*sin(a)*exp(0.1d0*b)*db"//nl// &
            "    end do"//nl// &
            "    if (abs(y - want) > 2.0d-13) error stop 1"//nl// &
            "    if (abs(y_d - want_d) > 2.0d-13) error stop 2"//nl// &
            "end program driver"//nl
    end function driver_text

end program test_emit_module_oracle
