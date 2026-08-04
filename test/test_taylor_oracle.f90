program test_taylor_oracle
    !! Independent behavioural oracle for Taylor arithmetic.
    !!
    !! The oracle is closed-form: for each function there is a point where every
    !! Taylor coefficient is known exactly in advance, so the test compares
    !! against mathematics rather than against another derivative computation.
    !!
    !!   exp(t)      coefficients 1/k!
    !!   1/(1-t)     coefficients 1
    !!   log(1+t)    coefficients (-1)^(k+1)/k
    !!   sqrt(1+t)   coefficients binom(1/2, k)
    !!   sin, cos    the alternating factorial series
    !!
    !! A composed case is then checked against a high-order finite difference of
    !! the composed function, which knows nothing about the recurrences.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortad_taylor, only: tay_const, tay_var, tay_add, tay_sub, tay_mul, &
                             tay_div, tay_exp, tay_log, tay_sqrt, tay_sin_cos, &
                             tay_pow_int, tay_derivative
    implicit none

    integer, parameter :: D = 8
    integer :: failures

    failures = 0

    call test_exp_series(failures)
    call test_geometric_series(failures)
    call test_log_series(failures)
    call test_sqrt_series(failures)
    call test_sin_cos_series(failures)
    call test_product_rule(failures)
    call test_integer_powers(failures)
    call test_composition_against_differences(failures)
    call test_derivative_conversion(failures)

    if (failures == 0) then
        print *, "test_taylor_oracle: all cases passed"
    else
        print *, "test_taylor_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine test_exp_series(failures)
        !! exp(t) has coefficients 1/k!.
        integer, intent(inout) :: failures
        real(dp) :: t(0:D), z(0:D), want(0:D)
        integer :: k

        call tay_var(0.0_dp, 1.0_dp, t)
        call tay_exp(t, z)
        want(0) = 1.0_dp
        do k = 1, D
            want(k) = want(k - 1)/real(k, dp)
        end do
        call expect("exp_series", z, want, failures)
    end subroutine test_exp_series

    subroutine test_geometric_series(failures)
        !! 1/(1 - t) has every coefficient equal to one.
        integer, intent(inout) :: failures
        real(dp) :: t(0:D), one(0:D), den(0:D), z(0:D), want(0:D)

        call tay_var(0.0_dp, 1.0_dp, t)
        call tay_const(1.0_dp, one)
        call tay_sub(one, t, den)
        call tay_div(one, den, z)
        want = 1.0_dp
        call expect("geometric_series", z, want, failures)
    end subroutine test_geometric_series

    subroutine test_log_series(failures)
        !! log(1 + t) has coefficients (-1)^(k+1)/k, and zero constant term.
        integer, intent(inout) :: failures
        real(dp) :: t(0:D), one(0:D), arg(0:D), z(0:D), want(0:D)
        integer :: k

        call tay_var(0.0_dp, 1.0_dp, t)
        call tay_const(1.0_dp, one)
        call tay_add(one, t, arg)
        call tay_log(arg, z)
        want(0) = 0.0_dp
        do k = 1, D
            want(k) = merge(1.0_dp, -1.0_dp, mod(k, 2) == 1)/real(k, dp)
        end do
        call expect("log_series", z, want, failures)
    end subroutine test_log_series

    subroutine test_sqrt_series(failures)
        !! sqrt(1 + t) has coefficients binom(1/2, k), built by the same
        !! recurrence the closed form satisfies but computed independently here.
        integer, intent(inout) :: failures
        real(dp) :: t(0:D), one(0:D), arg(0:D), z(0:D), want(0:D)
        integer :: k

        call tay_var(0.0_dp, 1.0_dp, t)
        call tay_const(1.0_dp, one)
        call tay_add(one, t, arg)
        call tay_sqrt(arg, z)
        want(0) = 1.0_dp
        do k = 1, D
            want(k) = want(k - 1)*(0.5_dp - real(k - 1, dp))/real(k, dp)
        end do
        call expect("sqrt_series", z, want, failures)
    end subroutine test_sqrt_series

    subroutine test_sin_cos_series(failures)
        !! sin and cos at zero: the alternating factorial series.
        integer, intent(inout) :: failures
        real(dp) :: t(0:D), s(0:D), c(0:D), ws(0:D), wc(0:D)
        real(dp) :: fact
        integer :: k

        call tay_var(0.0_dp, 1.0_dp, t)
        call tay_sin_cos(t, s, c)
        ws = 0.0_dp
        wc = 0.0_dp
        fact = 1.0_dp
        do k = 0, D
            if (k > 0) fact = fact*real(k, dp)
            select case (mod(k, 4))
            case (0)
                wc(k) = 1.0_dp/fact
            case (1)
                ws(k) = 1.0_dp/fact
            case (2)
                wc(k) = -1.0_dp/fact
            case (3)
                ws(k) = -1.0_dp/fact
            end select
        end do
        call expect("sin_series", s, ws, failures)
        call expect("cos_series", c, wc, failures)
    end subroutine test_sin_cos_series

    subroutine test_product_rule(failures)
        !! exp(t)*exp(t) must equal exp(2t), coefficient by coefficient. This
        !! catches an error in the Cauchy product that a single function's
        !! recurrence would not.
        integer, intent(inout) :: failures
        real(dp) :: t(0:D), two_t(0:D), e(0:D), prod(0:D), want(0:D)
        integer :: k

        call tay_var(0.0_dp, 1.0_dp, t)
        call tay_exp(t, e)
        call tay_mul(e, e, prod)

        two_t = 2.0_dp*t
        call tay_exp(two_t, want)
        call expect("product_rule", prod, want, failures)
    end subroutine test_product_rule

    subroutine test_integer_powers(failures)
        !! a**3 must equal a*a*a, and a**(-2) its reciprocal squared, at a point
        !! where the base is negative - which is exactly where an exp/log
        !! implementation of integer powers would fail.
        integer, intent(inout) :: failures
        real(dp) :: a(0:D), cube(0:D), tmp(0:D), want(0:D)
        real(dp) :: inv(0:D), one(0:D), sq(0:D), want_inv(0:D)

        call tay_var(-1.3_dp, 0.7_dp, a)
        call tay_pow_int(a, 3, cube)
        call tay_mul(a, a, tmp)
        call tay_mul(tmp, a, want)
        call expect("integer_cube_negative_base", cube, want, failures)

        call tay_pow_int(a, -2, inv)
        call tay_const(1.0_dp, one)
        call tay_mul(a, a, sq)
        call tay_div(one, sq, want_inv)
        call expect("integer_negative_power", inv, want_inv, failures)
    end subroutine test_integer_powers

    subroutine test_composition_against_differences(failures)
        !! A composed function checked against high-order central differences,
        !! which know nothing about the recurrences. Only low orders are
        !! compared: a differenced k-th derivative loses roughly k digits, so
        !! demanding agreement at order 8 would be testing roundoff.
        integer, intent(inout) :: failures
        real(dp) :: x(0:D), z(0:D)
        real(dp) :: h, d1, d2, got
        logical :: bad

        bad = .false.
        call tay_var(0.6_dp, 1.0_dp, x)
        call composed(x, z)

        h = 1.0e-4_dp
        d1 = (f(0.6_dp + h) - f(0.6_dp - h))/(2.0_dp*h)
        got = tay_derivative(z, 1)
        if (abs(got - d1) > 1.0e-6_dp*max(1.0_dp, abs(d1))) then
            print *, "  first derivative:", got, " fd:", d1
            bad = .true.
        end if

        d2 = (f(0.6_dp + h) - 2.0_dp*f(0.6_dp) + f(0.6_dp - h))/(h*h)
        got = tay_derivative(z, 2)
        if (abs(got - d2) > 1.0e-4_dp*max(1.0_dp, abs(d2))) then
            print *, "  second derivative:", got, " fd:", d2
            bad = .true.
        end if

        if (bad) then
            print *, "FAIL composition_against_differences"
            failures = failures + 1
        else
            print *, "pass composition_against_differences"
        end if
    end subroutine test_composition_against_differences

    subroutine composed(x, z)
        !! z = exp(x)*sin(x) / sqrt(1 + x*x), in Taylor arithmetic.
        real(dp), intent(in) :: x(0:)
        real(dp), intent(out) :: z(0:)
        real(dp) :: e(0:D), s(0:D), c(0:D), num(0:D)
        real(dp) :: sq(0:D), one(0:D), arg(0:D), den(0:D)

        call tay_exp(x, e)
        call tay_sin_cos(x, s, c)
        call tay_mul(e, s, num)
        call tay_mul(x, x, sq)
        call tay_const(1.0_dp, one)
        call tay_add(one, sq, arg)
        call tay_sqrt(arg, den)
        call tay_div(num, den, z)
    end subroutine composed

    real(dp) function f(x) result(y)
        !! The same function in ordinary arithmetic.
        real(dp), intent(in) :: x

        y = exp(x)*sin(x)/sqrt(1.0_dp + x*x)
    end function f

    subroutine test_derivative_conversion(failures)
        !! The k-th derivative is k! times the k-th coefficient. For exp at
        !! zero every derivative is one, which pins the factorial exactly.
        integer, intent(inout) :: failures
        real(dp) :: t(0:D), z(0:D)
        integer :: k
        logical :: bad

        bad = .false.
        call tay_var(0.0_dp, 1.0_dp, t)
        call tay_exp(t, z)
        do k = 0, D
            if (abs(tay_derivative(z, k) - 1.0_dp) > 1.0e-12_dp) then
                print *, "  derivative", k, "is", tay_derivative(z, k), "not 1"
                bad = .true.
            end if
        end do
        if (bad) then
            print *, "FAIL derivative_conversion"
            failures = failures + 1
        else
            print *, "pass derivative_conversion"
        end if
    end subroutine test_derivative_conversion

    subroutine expect(label, got, want, failures)
        !! Compare coefficient arrays with a tolerance that grows with order,
        !! because a k-th coefficient accumulates k rounds of arithmetic.
        character(len=*), intent(in) :: label
        real(dp), intent(in) :: got(0:), want(0:)
        integer, intent(inout) :: failures
        integer :: k
        logical :: bad

        bad = .false.
        do k = 0, ubound(got, 1)
            if (abs(got(k) - want(k)) > 1.0e-12_dp*real(k + 1, dp)* &
                max(1.0_dp, abs(want(k)))) then
                print *, "  coefficient", k, "is", got(k), "not", want(k)
                bad = .true.
            end if
        end do
        if (bad) then
            print *, "FAIL ", label
            failures = failures + 1
        else
            print *, "pass ", label
        end if
    end subroutine expect

end program test_taylor_oracle
