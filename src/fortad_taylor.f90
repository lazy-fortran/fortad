module fortad_taylor
    !! Univariate Taylor arithmetic to arbitrary order.
    !!
    !! A Taylor object carries the coefficients of `f(x + t v)` in `t`:
    !! `a(k) = (1/k!) dᵏ/dtᵏ f(x + t v)` at `t = 0`. Propagating those through a
    !! computation gives every derivative up to order `d` in one sweep, at
    !! `O(d²)` per operation rather than the `O(2ᵈ)` that nesting a first-order
    !! tool `d` times would cost.
    !!
    !! Each rule below is a recurrence derived from differentiating the defining
    !! identity of the function, which is why they are cheap: `z = exp(a)`
    !! satisfies `z' = z a'`, and reading off coefficients gives
    !! `k z_k = Σ i a_i z_{k-i}` directly. Nothing here differentiates a series
    !! expansion of the intrinsic; these are exact relations.
    !!
    !! Every routine is `pure`: coefficients in, coefficients out, no state.
    !! Generated Taylor-mode code is therefore pure too, which is what lets the
    !! consumer's compiler treat a call to it as it would any other expression.
    !!
    !! The transformation that rewrites a Fortran kernel into calls to these
    !! routines is `fortad_taylor_gen`.
    use fortad_kinds, only: dp
    implicit none
    private

    public :: tay_const, tay_var, tay_add, tay_sub, tay_scale
    public :: tay_mul, tay_div, tay_exp, tay_log, tay_sqrt, tay_sin_cos
    public :: tay_pow_int, tay_derivative

contains

    pure subroutine tay_const(value, z)
        !! A constant: no dependence on `t`.
        real(dp), intent(in) :: value
        real(dp), intent(out) :: z(0:)

        z = 0.0_dp
        z(0) = value
    end subroutine tay_const

    pure subroutine tay_var(value, direction, z)
        !! An independent variable seeded along `direction`.
        real(dp), intent(in) :: value, direction
        real(dp), intent(out) :: z(0:)

        z = 0.0_dp
        z(0) = value
        if (ubound(z, 1) >= 1) z(1) = direction
    end subroutine tay_var

    pure subroutine tay_add(a, b, z)
        !! `z = a + b`.
        real(dp), intent(in) :: a(0:), b(0:)
        real(dp), intent(out) :: z(0:)

        z = a + b
    end subroutine tay_add

    pure subroutine tay_sub(a, b, z)
        !! `z = a - b`.
        real(dp), intent(in) :: a(0:), b(0:)
        real(dp), intent(out) :: z(0:)

        z = a - b
    end subroutine tay_sub

    pure subroutine tay_scale(c, a, z)
        !! `z = c*a` for a scalar `c`.
        real(dp), intent(in) :: c, a(0:)
        real(dp), intent(out) :: z(0:)

        z = c*a
    end subroutine tay_scale

    pure subroutine tay_mul(a, b, z)
        !! `z = a*b`: the Cauchy product, `z_k = Σ a_i b_{k-i}`.
        real(dp), intent(in) :: a(0:), b(0:)
        real(dp), intent(out) :: z(0:)
        integer :: k, i, d

        d = ubound(z, 1)
        do k = 0, d
            z(k) = 0.0_dp
            do i = 0, k
                z(k) = z(k) + a(i)*b(k - i)
            end do
        end do
    end subroutine tay_mul

    pure subroutine tay_div(a, b, z)
        !! `z = a/b`, from `a = z b`: `z_k = (a_k - Σ_{i<k} z_i b_{k-i}) / b_0`.
        !!
        !! Requires `b_0 /= 0`. A zero constant term means the quotient has a
        !! pole and no Taylor series exists there; the caller gets a NaN rather
        !! than a quietly wrong number, which is the honest outcome.
        real(dp), intent(in) :: a(0:), b(0:)
        real(dp), intent(out) :: z(0:)
        integer :: k, i, d
        real(dp) :: acc

        d = ubound(z, 1)
        do k = 0, d
            acc = a(k)
            do i = 0, k - 1
                acc = acc - z(i)*b(k - i)
            end do
            z(k) = acc/b(0)
        end do
    end subroutine tay_div

    pure subroutine tay_exp(a, z)
        !! `z = exp(a)`, from `z' = z a'`: `k z_k = Σ_{i=1..k} i a_i z_{k-i}`.
        real(dp), intent(in) :: a(0:)
        real(dp), intent(out) :: z(0:)
        integer :: k, i, d
        real(dp) :: acc

        d = ubound(z, 1)
        z(0) = exp(a(0))
        do k = 1, d
            acc = 0.0_dp
            do i = 1, k
                acc = acc + real(i, dp)*a(i)*z(k - i)
            end do
            z(k) = acc/real(k, dp)
        end do
    end subroutine tay_exp

    pure subroutine tay_log(a, z)
        !! `z = log(a)`, from `a z' = a'`:
        !! `z_k = (a_k - (1/k) Σ_{i=1..k-1} i z_i a_{k-i}) / a_0`.
        real(dp), intent(in) :: a(0:)
        real(dp), intent(out) :: z(0:)
        integer :: k, i, d
        real(dp) :: acc

        d = ubound(z, 1)
        z(0) = log(a(0))
        do k = 1, d
            acc = a(k)
            do i = 1, k - 1
                acc = acc - real(i, dp)/real(k, dp)*z(i)*a(k - i)
            end do
            z(k) = acc/a(0)
        end do
    end subroutine tay_log

    pure subroutine tay_sqrt(a, z)
        !! `z = sqrt(a)`, from `z² = a`:
        !! `z_k = (a_k - Σ_{0<i<k} z_i z_{k-i}) / (2 z_0)`.
        real(dp), intent(in) :: a(0:)
        real(dp), intent(out) :: z(0:)
        integer :: k, i, d
        real(dp) :: acc

        d = ubound(z, 1)
        z(0) = sqrt(a(0))
        do k = 1, d
            acc = a(k)
            do i = 1, k - 1
                acc = acc - z(i)*z(k - i)
            end do
            z(k) = acc/(2.0_dp*z(0))
        end do
    end subroutine tay_sqrt

    pure subroutine tay_sin_cos(a, s, c)
        !! `s = sin(a)` and `c = cos(a)` together.
        !!
        !! Their recurrences are coupled - `s' = c a'`, `c' = -s a'` - so
        !! computing one alone would compute the other anyway. Returning both
        !! is free and saves the caller from asking twice.
        real(dp), intent(in) :: a(0:)
        real(dp), intent(out) :: s(0:), c(0:)
        integer :: k, i, d
        real(dp) :: acc_s, acc_c

        d = ubound(s, 1)
        s(0) = sin(a(0))
        c(0) = cos(a(0))
        do k = 1, d
            acc_s = 0.0_dp
            acc_c = 0.0_dp
            do i = 1, k
                acc_s = acc_s + real(i, dp)*a(i)*c(k - i)
                acc_c = acc_c - real(i, dp)*a(i)*s(k - i)
            end do
            s(k) = acc_s/real(k, dp)
            c(k) = acc_c/real(k, dp)
        end do
    end subroutine tay_sin_cos

    pure subroutine tay_pow_int(a, p, z)
        !! `z = a**p` for an integer `p`, by repeated multiplication.
        !!
        !! Not via `exp(p log a)`: that would fail for negative `a` where the
        !! integer power is perfectly well defined.
        real(dp), intent(in) :: a(0:)
        integer, intent(in) :: p
        real(dp), intent(out) :: z(0:)
        real(dp), allocatable :: acc(:), tmp(:)
        integer :: d, i

        d = ubound(z, 1)
        allocate (acc(0:d), tmp(0:d))

        if (p == 0) then
            call tay_const(1.0_dp, z)
            return
        end if

        acc = a
        do i = 2, abs(p)
            call tay_mul(acc, a, tmp)
            acc = tmp
        end do

        if (p > 0) then
            z = acc
        else
            call tay_const(1.0_dp, tmp)
            call tay_div(tmp, acc, z)
        end if
    end subroutine tay_pow_int

    pure real(dp) function tay_derivative(z, k) result(value)
        !! The `k`-th derivative, recovered as `k! * z_k`.
        !!
        !! Taylor coefficients are the natural storage - the recurrences are
        !! clean in them and the factorials cancel - but a caller usually wants
        !! the derivative, so the conversion belongs here rather than in every
        !! call site.
        real(dp), intent(in) :: z(0:)
        integer, intent(in) :: k
        integer :: i

        value = 0.0_dp
        if (k < 0 .or. k > ubound(z, 1)) return
        value = z(k)
        do i = 2, k
            value = value*real(i, dp)
        end do
    end function tay_derivative

end module fortad_taylor
