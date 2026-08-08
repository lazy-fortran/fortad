program vector_jvp_benchmark
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use vector_scalar, only: vector_jvp_scalar
    use vector_batch, only: vector_jvp_batch
    implicit none

    integer, parameter :: n = 131072
    integer, parameter :: nd = 8
    integer, parameter :: repetitions = 24
    real(dp) :: x(n), x_d(nd, n), y(n), y_d(nd, n)
    real(dp) :: scalar_y(n), scalar_y_d(n), expected(n), expected_d(nd, n)
    real(dp) :: slope, scalar_seconds, batch_seconds, t0, t1, sink
    integer :: i, j, r

    do i = 1, n
        x(i) = 0.2_dp*sin(0.003_dp*i) + 0.01_dp*cos(0.011_dp*i)
        expected(i) = sin(x(i)) + 0.25_dp*x(i)*x(i) + exp(-0.1_dp*x(i))
        slope = cos(x(i)) + 0.5_dp*x(i) - 0.1_dp*exp(-0.1_dp*x(i))
        do j = 1, nd
            x_d(j, i) = sin(0.017_dp*i + 0.13_dp*j)
            expected_d(j, i) = slope*x_d(j, i)
        end do
    end do

    call vector_jvp_batch(nd, n, x, x_d, y, y_d)
    if (maxval(abs(y - expected)) > 2.0e-13_dp) error stop 1
    if (maxval(abs(y_d - expected_d)) > 2.0e-13_dp) error stop 2

    do j = 1, nd
        call vector_jvp_scalar(n, x, x_d(j, :), scalar_y, scalar_y_d)
        if (maxval(abs(scalar_y - expected)) > 2.0e-13_dp) error stop 3
        if (maxval(abs(scalar_y_d - expected_d(j, :))) > 2.0e-13_dp) error stop 4
    end do

    sink = 0.0_dp
    call cpu_time(t0)
    do r = 1, repetitions
        do j = 1, nd
            call vector_jvp_scalar(n, x, x_d(j, :), scalar_y, scalar_y_d)
            sink = sink + scalar_y(n/2) + scalar_y_d(n/2)
        end do
    end do
    call cpu_time(t1)
    scalar_seconds = t1 - t0

    call cpu_time(t0)
    do r = 1, repetitions
        call vector_jvp_batch(nd, n, x, x_d, y, y_d)
        sink = sink + y(n/2) + y_d(1, n/2)
    end do
    call cpu_time(t1)
    batch_seconds = t1 - t0

    if (sink == 0.0_dp) error stop 5
    if (scalar_seconds <= 0.0_dp .or. batch_seconds <= 0.0_dp) error stop 6
    write (*, '(a,1x,i0)') 'directions', nd
    write (*, '(a,1x,i0)') 'elements', n
    write (*, '(a,1x,es16.8)') 'scalar_seconds', scalar_seconds
    write (*, '(a,1x,es16.8)') 'batch_seconds', batch_seconds
    write (*, '(a,1x,es16.8)') 'batch_over_scalar', batch_seconds/scalar_seconds
end program vector_jvp_benchmark
