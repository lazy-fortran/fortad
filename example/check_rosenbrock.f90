program check_rosenbrock
    use rosenbrock_ad, only: rosenbrock_vjp
    implicit none

    real(8), parameter :: tolerance = 1.0d-12
    real(8) :: x(2), f, f_b, x_b(2)

    x = [-1.2d0, 1.0d0]
    f_b = 1.0d0
    call rosenbrock_vjp(x, f, f_b, x_b)

    if (abs(f - 24.2d0) > tolerance) error stop "wrong primal value"
    if (maxval(abs(x_b - [-215.6d0, -88.0d0])) > tolerance) then
        error stop "wrong gradient"
    end if

    print '(a,f5.1,a,2(f7.1,1x))', "f = ", f, ", gradient = ", x_b
end program check_rosenbrock
