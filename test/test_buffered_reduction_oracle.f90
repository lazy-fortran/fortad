program test_buffered_reduction_oracle
    !! Independent closed-form oracle for the buffered reduction emitter path.
    !!
    !! For f(z) = sum_i exp(a_i*b_i), the directional derivative is
    !! sum_i exp(a_i*b_i)*(da_i*b_i + a_i*db_i).  The check compiles and runs
    !! the generated routine, so it tests the emitted code rather than merely
    !! inspecting the transformation text.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir
    type(fad_result_t) :: result
    integer :: stat, unit

    source = &
        "subroutine adaptive_trace_integrand(n, z, y)"//nl// &
        "    integer, intent(in) :: n"//nl// &
        "    real(8), intent(in) :: z(2*n)"//nl// &
        "    real(8), intent(out) :: y"//nl// &
        "    integer :: i, base"//nl// &
        "    real(8) :: value"//nl// &
        "    y = 0.0d0"//nl// &
        "    do i = 1, n"//nl// &
        "        base = 2*(i - 1)"//nl// &
        "        value = exp(z(base + 2)*z(base + 1))"//nl// &
        "        y = y + value"//nl// &
        "    end do"//nl// &
        "end subroutine adaptive_trace_integrand"//nl

    result = fad_jvp(source, ["z"])
    if (.not. result%ok) error stop "buffered reduction generation failed"
    if (index(result%code, "fad_buffer_") == 0) then
        error stop "independent reduction was not buffered"
    end if

    dir = "build/oracle_buffered_reduction"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create buffered reduction oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') "module primal_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module primal_mod"
    close (unit)

    open (newunit=unit, file=dir//"/generated.f90", status="replace", action="write")
    write (unit, '(a)') "module generated_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') result%code
    write (unit, '(a)') "end module generated_mod"
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line("cd "//dir//" && gfortran -std=f2018 -O2 -o run "// &
        "primal.f90 generated.f90 driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "buffered reduction generated source did not compile"
        error stop 1
    end if
    call execute_command_line("cd "//dir//" && ./run", exitstat=stat)
    if (stat /= 0) error stop "buffered reduction closed-form oracle failed"
    print *, "test_buffered_reduction_oracle: passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use generated_mod, only: adaptive_trace_integrand_jvp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 17"//nl// &
            "    real(8) :: z(2*n), dz(2*n), y, y_d, want, want_d, term"//nl// &
            "    real(8) :: a, b, da, db"//nl// &
            "    integer :: i"//nl// &
            "    do i = 1, 2*n"//nl// &
            "        z(i) = 0.2d0 + 0.013d0*i"//nl// &
            "        dz(i) = sin(0.17d0*i)"//nl// &
            "    end do"//nl// &
            "    call adaptive_trace_integrand_jvp(n, z, dz, y, y_d)"//nl// &
            "    want = 0.0d0"//nl// &
            "    want_d = 0.0d0"//nl// &
            "    do i = 1, n"//nl// &
            "        a = z(2*i - 1)"//nl// &
            "        b = z(2*i)"//nl// &
            "        da = dz(2*i - 1)"//nl// &
            "        db = dz(2*i)"//nl// &
            "        term = exp(a*b)"//nl// &
            "        want = want + term"//nl// &
            "        want_d = want_d + term*(da*b + a*db)"//nl// &
            "    end do"//nl// &
            "    if (abs(y - want) > 1.0d-13) error stop 1"//nl// &
            "    if (abs(y_d - want_d) > 1.0d-13) error stop 2"//nl// &
            "end program driver"//nl
    end function driver_text

end program test_buffered_reduction_oracle
