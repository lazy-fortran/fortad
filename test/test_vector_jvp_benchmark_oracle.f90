program test_vector_jvp_benchmark_oracle
    !! Independent analytic oracle for the batched vector-JVP benchmark.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, directory
    type(fad_result_t) :: scalar_result, batch_result
    integer :: stat, unit

    source = &
        "subroutine vector_jvp_kernel(n, x, y)"//nl// &
        "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
        "    integer, intent(in) :: n"//nl// &
        "    real(dp), intent(in) :: x(n)"//nl// &
        "    real(dp), intent(out) :: y(n)"//nl// &
        "    integer :: i"//nl// &
        "    do i = 1, n"//nl// &
        "        y(i) = sin(x(i)) + 0.25_dp*x(i)*x(i) + exp(-0.1_dp*x(i))"//nl// &
        "    end do"//nl// &
        "end subroutine vector_jvp_kernel"//nl

    scalar_result = fad_jvp(source, ["x"], name="vector_jvp_scalar")
    batch_result = fad_jvp(source, ["x"], name="vector_jvp_batch", &
                           n_directions="nd")
    if (.not. scalar_result%ok) error stop "scalar vector-JVP generation failed"
    if (.not. batch_result%ok) error stop "batch vector-JVP generation failed"

    directory = "build/oracle_vector_jvp_benchmark"
    call execute_command_line("mkdir -p "//directory, exitstat=stat)
    if (stat /= 0) error stop "could not create vector-JVP oracle directory"

    open (newunit=unit, file=directory//"/generated.f90", status="replace", &
          action="write")
    write (unit, '(a)') "module generated_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') scalar_result%code
    write (unit, '(a)') batch_result%code
    write (unit, '(a)') "end module generated_mod"
    close (unit)

    open (newunit=unit, file=directory//"/driver.f90", status="replace", &
          action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line("cd "//directory//" && gfortran -std=f2018 -O2 "// &
        "-o run generated.f90 driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) error stop "vector-JVP generated source did not compile"
    call execute_command_line("cd "//directory//" && ./run", exitstat=stat)
    if (stat /= 0) error stop "vector-JVP analytic oracle failed"
    print *, "test_vector_jvp_benchmark_oracle: passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use generated_mod, only: vector_jvp_scalar, vector_jvp_batch"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 19, nd = 5"//nl// &
            "    real(8) :: x(n), xd(nd,n), y(n), yd(nd,n), ys(n), yds(n)"//nl// &
            "    real(8) :: want(n), want_d(nd,n), slope"//nl// &
            "    integer :: i, j"//nl// &
            "    do i = 1, n"//nl// &
            "        x(i) = 0.2d0*sin(0.03d0*i)"//nl// &
            "        want(i) = sin(x(i)) + 0.25d0*x(i)*x(i) + exp(-0.1d0*x(i))"//nl// &
            "        slope = cos(x(i)) + 0.5d0*x(i) - 0.1d0*exp(-0.1d0*x(i))"//nl// &
            "        do j = 1, nd"//nl// &
            "            xd(j,i) = cos(0.11d0*i + 0.2d0*j)"//nl// &
            "            want_d(j,i) = slope*xd(j,i)"//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    call vector_jvp_batch(nd, n, x, xd, y, yd)"//nl// &
            "    if (maxval(abs(y-want)) > 2.0d-13) error stop 1"//nl// &
            "    if (maxval(abs(yd-want_d)) > 2.0d-13) error stop 2"//nl// &
            "    do j = 1, nd"//nl// &
            "        call vector_jvp_scalar(n, x, xd(j,:), ys, yds)"//nl// &
            "        if (maxval(abs(ys-want)) > 2.0d-13) error stop 3"//nl// &
            "        if (maxval(abs(yds-want_d(j,:))) > 2.0d-13) error stop 4"//nl// &
            "    end do"//nl// &
            "end program driver"//nl
    end function driver_text

end program test_vector_jvp_benchmark_oracle
