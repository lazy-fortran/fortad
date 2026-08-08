subroutine vector_jvp_kernel(n, x, y)
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer, intent(in) :: n
    real(dp), intent(in) :: x(n)
    real(dp), intent(out) :: y(n)
    integer :: i

    do i = 1, n
        y(i) = sin(x(i)) + 0.25_dp*x(i)*x(i) + exp(-0.1_dp*x(i))
    end do
end subroutine vector_jvp_kernel
