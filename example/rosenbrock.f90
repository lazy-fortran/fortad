subroutine rosenbrock(x, f)
    implicit none

    real(8), intent(in) :: x(2)
    real(8), intent(out) :: f

    f = (1.0d0 - x(1))**2 + 100.0d0*(x(2) - x(1)**2)**2
end subroutine rosenbrock
