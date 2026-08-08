program buffered_reduction_benchmark
    use, intrinsic :: iso_fortran_env, only: real64
    use buffered_generated, only: buffered_reduction_kernel_jvp
    use buffered_reduction_baseline, only: buffered_reduction_kernel_jvp_baseline
    implicit none

    integer, parameter :: n = 4096
    integer, parameter :: repetitions = 1200
    real(real64) :: z(2*n), z_d(2*n)
    real(real64) :: y, y_d, expected, expected_d
    real(real64) :: t0, t1, baseline_seconds, buffered_seconds, sink
    integer :: i, r

    do i = 1, 2*n
        z(i) = 0.2_real64 + 0.00013_real64*i
        z_d(i) = sin(0.017_real64*i)
    end do
    expected = 0.0_real64
    expected_d = 0.0_real64
    do i = 1, n
        expected = expected + exp(z(2*i - 1)*z(2*i))
        expected_d = expected_d + exp(z(2*i - 1)*z(2*i)) * &
            (z_d(2*i - 1)*z(2*i) + z(2*i - 1)*z_d(2*i))
    end do

    call buffered_reduction_kernel_jvp_baseline(n, z, z_d, y, y_d)
    if (abs(y - expected) > 1.0e-12_real64) error stop 1
    if (abs(y_d - expected_d) > 1.0e-12_real64) error stop 2
    call buffered_reduction_kernel_jvp(n, z, z_d, y, y_d)
    if (abs(y - expected) > 1.0e-12_real64) error stop 3
    if (abs(y_d - expected_d) > 1.0e-12_real64) error stop 4

    call cpu_time(t0)
    sink = 0.0_real64
    do r = 1, repetitions
        call buffered_reduction_kernel_jvp_baseline(n, z, z_d, y, y_d)
        sink = sink + y + y_d
    end do
    call cpu_time(t1)
    baseline_seconds = t1 - t0

    call cpu_time(t0)
    do r = 1, repetitions
        call buffered_reduction_kernel_jvp(n, z, z_d, y, y_d)
        sink = sink + y + y_d
    end do
    call cpu_time(t1)
    buffered_seconds = t1 - t0

    if (sink == 0.0_real64) error stop 5
    write (*, '(a,1x,es16.8)') 'baseline_seconds', baseline_seconds
    write (*, '(a,1x,es16.8)') 'buffered_seconds', buffered_seconds
    write (*, '(a,1x,es16.8)') 'buffered_over_baseline', buffered_seconds / baseline_seconds
end program buffered_reduction_benchmark
