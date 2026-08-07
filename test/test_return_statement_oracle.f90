program test_return_statement_oracle
    !! A terminal plain RETURN does not alter a derivative path.
    !! Alternate and non-terminal RETURN remain explicit refusal boundaries.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: terminal_source, early_source, dir
    type(fad_result_t) :: jvp, vjp, early
    integer :: stat, unit

    terminal_source = &
        "subroutine terminal_return(x, y)"//nl// &
        "    real, intent(in) :: x"//nl// &
        "    real, intent(out) :: y"//nl// &
        "    y = 3.0*x + 2.0"//nl// &
        "    return"//nl// &
        "end subroutine terminal_return"//nl

    jvp = fad_jvp(terminal_source, [character(len=1) :: "x"], &
        name="terminal_return_jvp")
    if (.not. jvp%ok) error stop "terminal RETURN JVP generation failed: "//jvp%message
    vjp = fad_vjp(terminal_source, [character(len=1) :: "x"], &
        dependent="y", name="terminal_return_vjp")
    if (.not. vjp%ok) error stop "terminal RETURN VJP generation failed: "//vjp%message
    if (index(jvp%code, nl//"    return") > 0) then
        error stop "terminal RETURN leaked into JVP"
    end if
    if (index(vjp%code, nl//"    return") > 0) then
        error stop "terminal RETURN leaked into VJP"
    end if

    early_source = &
        "subroutine early_return(x, y)"//nl// &
        "    real, intent(in) :: x"//nl// &
        "    real, intent(out) :: y"//nl// &
        "    if (x > 0.0) return"//nl// &
        "    y = x"//nl// &
        "end subroutine early_return"//nl
    early = fad_jvp(early_source, [character(len=1) :: "x"], &
        name="early_return_jvp")
    if (early%ok) error stop "non-terminal RETURN was accepted"
    if (index(early%message, "RETURN") == 0) then
        error stop "non-terminal RETURN refusal was not named"
    end if

    dir = "build/oracle_return_statement"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create return oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') "module primal_mod"
    write (unit, '(a)') "contains"
    write (unit, '(a)') terminal_source
    write (unit, '(a)') "end module primal_mod"
    close (unit)

    open (newunit=unit, file=dir//"/derivs.f90", status="replace", action="write")
    write (unit, '(a)') "module derivs_mod"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module derivs_mod"
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') "program driver"
    write (unit, '(a)') "    use primal_mod, only: terminal_return"
    write (unit, '(a)') "    use derivs_mod, only: terminal_return_jvp, terminal_return_vjp"
    write (unit, '(a)') "    real :: x, xd, y, yd, yb, xb"
    write (unit, '(a)') "    x = 2.0; xd = 0.5"
    write (unit, '(a)') "    call terminal_return(x, y)"
    write (unit, '(a)') "    if (abs(y - 8.0) > 1.0e-6) error stop 1"
    write (unit, '(a)') "    call terminal_return_jvp(x, xd, y, yd)"
    write (unit, '(a)') "    if (abs(yd - 1.5) > 1.0e-6) error stop 2"
    write (unit, '(a)') "    yb = 0.75"
    write (unit, '(a)') "    call terminal_return_vjp(x, y, yb, xb)"
    write (unit, '(a)') "    if (abs(xb - 2.25) > 1.0e-6) error stop 3"
    write (unit, '(a)') "end program driver"
    close (unit)

    call execute_command_line( &
        "cd "//dir//" && gfortran -std=f2018 -pedantic-errors -O2 -o run "// &
        "primal.f90 derivs.f90 driver.f90", exitstat=stat)
    if (stat /= 0) error stop "terminal RETURN generated source did not compile"
    call execute_command_line("cd "//dir//" && ./run", exitstat=stat)
    if (stat /= 0) error stop "terminal RETURN independent numerical oracle failed"

    print '(a)', "test_return_statement_oracle: all cases passed"
end program test_return_statement_oracle
