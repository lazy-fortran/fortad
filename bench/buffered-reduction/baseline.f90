module buffered_reduction_baseline
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
contains

    subroutine buffered_reduction_kernel_jvp_baseline(n, z, z_d, y, y_d)
        integer, intent(in) :: n
        real(dp), intent(in) :: z(2*n)
        real(dp), intent(in) :: z_d(2*n)
        real(dp), intent(out) :: y
        real(dp), intent(out) :: y_d
        integer :: i, base
        real(dp) :: value, value_d

        y_d = 0.0_dp
        y = 0.0_dp
        do i = 1, n
            base = 2*(i - 1)
            value = exp(z(base + 2)*z(base + 1))
            value_d = value * (z_d(base + 2)*z(base + 1) + &
                z(base + 2)*z_d(base + 1))
            y_d = y_d + value_d
            y = y + value
        end do
    end subroutine buffered_reduction_kernel_jvp_baseline

end module buffered_reduction_baseline
