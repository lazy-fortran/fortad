module fortad_rules
    !! The derivative rule table.
    !!
    !! One table, declarative, separate from the transformation that walks it.
    !! Each rule states the derivative of an operation with respect to each
    !! argument; forward and reverse mode both read this table, so the two modes
    !! cannot disagree about what a derivative is.
    !!
    !! A tangent index of 0 means a structural zero. Propagating that through
    !! the builders below is expression-level activity analysis: it is why the
    !! emitted code contains no `+ 0.0_dp` terms and no dead tangent statements.
    !!
    !! Style rule enforced throughout: **every builder call that mutates `p`
    !! gets its own statement and its own temporary.** Nesting one such call
    !! inside another call that also takes `p` is an aliasing hazard - Fortran
    !! does not order argument evaluation, and the arena may reallocate under
    !! the outer call. Verbose, but the alternative is silently wrong output.
    use fortad_ir, only: fad_proc_t, expr_const, expr_var, expr_binop, &
                        expr_unop, expr_call, FAD_CONST
    implicit none
    private

    public :: jvp_binop, jvp_unop, jvp_call, has_rule
    public :: fad_add, fad_sub, fad_mul, fad_div, fad_neg, fad_lit, fad_real
    public :: fad_raw, fad_pow, fad_pow_int, fad_fn1, fad_fn2, fad_fn3

    !! 2/sqrt(pi), the coefficient in d/dx erf(x).
    character(len=*), parameter :: ERF_COEFF = "1.1283791670955126"

contains

    logical function has_rule(name) result(yes)
        !! True when fortad knows the derivative of intrinsic `name`.
        character(len=*), intent(in) :: name

        select case (lower(name))
        case ("sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", &
              "tanh", "exp", "log", "log10", "sqrt", "abs", "erf", "erfc", &
              "max", "min", "sign", "atan2", "hypot", "real", "dble", "sum")
            yes = .true.
        case default
            yes = .false.
        end select
    end function has_rule

    integer function jvp_binop(p, op, a, b, da, db) result(out)
        !! Tangent of `a op b` given tangents `da`, `db`.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: op
        integer, intent(in) :: a, b, da, db
        integer :: t1, t2, num, den

        select case (trim(op))
        case ("+")
            out = fad_add(p, da, db)
        case ("-")
            out = fad_sub(p, da, db)
        case ("*")
            t1 = fad_mul(p, da, b)
            t2 = fad_mul(p, a, db)
            out = fad_add(p, t1, t2)
        case ("/")
            if (db == 0) then
                out = fad_div(p, da, b)
            else
                ! (da*b - a*db) / b**2
                t1 = fad_mul(p, da, b)
                t2 = fad_mul(p, a, db)
                num = fad_sub(p, t1, t2)
                den = fad_pow_int(p, b, "2")
                out = fad_div(p, num, den)
            end if
        case ("**")
            out = jvp_power(p, a, b, da, db)
        case default
            ! Comparison and logical operators carry no tangent.
            out = 0
        end select
    end function jvp_binop

    integer function jvp_power(p, a, b, da, db) result(out)
        !! Tangent of `a**b`. A constant exponent is the common case and gets
        !! the cheap form; the general case needs `log(a)`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a, b, da, db
        integer :: one, bm1, pw, coeff, t1, t2, full, inner, la, bda

        out = 0
        if (da == 0 .and. db == 0) return

        if (db == 0) then
            ! b*a**(b-1)*da
            one = fad_lit(p, "1")
            bm1 = fad_raw(p, "-", b, one)
            pw = fad_raw(p, "**", a, bm1)
            coeff = fad_mul(p, b, pw)
            out = fad_mul(p, coeff, da)
            return
        end if

        ! a**b * (db*log(a) + b*da/a)
        full = fad_raw(p, "**", a, b)
        t1 = 0
        if (db /= 0) then
            la = fad_fn1(p, "log", a)
            t1 = fad_mul(p, db, la)
        end if
        t2 = 0
        if (da /= 0) then
            bda = fad_mul(p, b, da)
            t2 = fad_div(p, bda, a)
        end if
        inner = fad_add(p, t1, t2)
        out = fad_mul(p, full, inner)
    end function jvp_power

    integer function jvp_unop(p, op, a, da) result(out)
        !! Tangent of a unary operation.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: op
        integer, intent(in) :: a, da

        select case (trim(op))
        case ("-")
            out = fad_neg(p, da)
        case ("+")
            out = da
        case default
            out = 0
        end select
    end function jvp_unop

    integer function jvp_call(p, name, args, dargs) result(out)
        !! Tangent of an intrinsic call. Rules are closed forms, never the
        !! derivative of some series expansion of the intrinsic.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: name
        integer, intent(in) :: args(:), dargs(:)
        integer :: a, da, b, db
        integer :: u, v, w, one, two, den, num, coeff

        out = 0
        if (size(args) == 0) return
        a = args(1)
        da = dargs(1)

        select case (lower(name))
        case ("sin")
            u = fad_fn1(p, "cos", a)
            out = fad_mul(p, u, da)

        case ("cos")
            u = fad_fn1(p, "sin", a)
            v = fad_mul(p, u, da)
            out = fad_neg(p, v)

        case ("tan")
            u = fad_fn1(p, "cos", a)
            den = fad_pow_int(p, u, "2")
            out = fad_div(p, da, den)

        case ("exp")
            u = fad_fn1(p, "exp", a)
            out = fad_mul(p, u, da)

        case ("log")
            out = fad_div(p, da, a)

        case ("log10")
            u = fad_real(p, "10.0")
            v = fad_fn1(p, "log", u)
            den = fad_mul(p, a, v)
            out = fad_div(p, da, den)

        case ("sqrt")
            u = fad_fn1(p, "sqrt", a)
            two = fad_real(p, "2.0")
            den = fad_mul(p, two, u)
            out = fad_div(p, da, den)

        case ("sinh")
            u = fad_fn1(p, "cosh", a)
            out = fad_mul(p, u, da)

        case ("cosh")
            u = fad_fn1(p, "sinh", a)
            out = fad_mul(p, u, da)

        case ("tanh")
            ! da * (1 - tanh(a)**2)
            u = fad_fn1(p, "tanh", a)
            v = fad_pow_int(p, u, "2")
            one = fad_real(p, "1.0")
            w = fad_sub(p, one, v)
            out = fad_mul(p, da, w)

        case ("asin")
            ! da / sqrt(1 - a**2)
            v = fad_pow_int(p, a, "2")
            one = fad_real(p, "1.0")
            w = fad_sub(p, one, v)
            den = fad_fn1(p, "sqrt", w)
            out = fad_div(p, da, den)

        case ("acos")
            v = fad_pow_int(p, a, "2")
            one = fad_real(p, "1.0")
            w = fad_sub(p, one, v)
            den = fad_fn1(p, "sqrt", w)
            u = fad_div(p, da, den)
            out = fad_neg(p, u)

        case ("atan")
            ! da / (1 + a**2)
            v = fad_pow_int(p, a, "2")
            one = fad_real(p, "1.0")
            den = fad_add(p, one, v)
            out = fad_div(p, da, den)

        case ("erf")
            ! 2/sqrt(pi) * exp(-a**2) * da
            v = fad_pow_int(p, a, "2")
            w = fad_neg(p, v)
            u = fad_fn1(p, "exp", w)
            coeff = fad_real(p, ERF_COEFF)
            v = fad_mul(p, coeff, u)
            out = fad_mul(p, v, da)

        case ("erfc")
            v = fad_pow_int(p, a, "2")
            w = fad_neg(p, v)
            u = fad_fn1(p, "exp", w)
            coeff = fad_real(p, ERF_COEFF)
            v = fad_mul(p, coeff, u)
            w = fad_mul(p, v, da)
            out = fad_neg(p, w)

        case ("abs")
            ! sign(1, a)*da. Not differentiable at 0; matching Fortran's own
            ! sign convention is the least surprising choice there.
            one = fad_real(p, "1.0")
            u = fad_fn2(p, "sign", one, a)
            out = fad_mul(p, u, da)

        case ("sum")
            if (da /= 0) out = fad_fn1(p, "sum", da)

        case ("real", "dble")
            out = da

        case ("max", "min")
            if (size(args) < 2) return
            out = jvp_minmax(p, lower(name), a, args(2), da, dargs(2))

        case ("sign")
            ! sign(a, b): magnitude from a, sign from b, so the tangent
            ! follows a with b's sign.
            if (size(args) < 2) return
            one = fad_real(p, "1.0")
            u = fad_fn2(p, "sign", one, args(2))
            out = fad_mul(p, u, da)

        case ("atan2")
            ! (b*da - a*db) / (a**2 + b**2)
            if (size(args) < 2) return
            b = args(2)
            db = dargs(2)
            u = fad_mul(p, b, da)
            v = fad_mul(p, a, db)
            num = fad_sub(p, u, v)
            u = fad_pow_int(p, a, "2")
            v = fad_pow_int(p, b, "2")
            den = fad_add(p, u, v)
            out = fad_div(p, num, den)

        case ("hypot")
            ! (a*da + b*db) / hypot(a, b)
            if (size(args) < 2) return
            b = args(2)
            db = dargs(2)
            u = fad_mul(p, a, da)
            v = fad_mul(p, b, db)
            num = fad_add(p, u, v)
            den = fad_fn2(p, "hypot", a, b)
            out = fad_div(p, num, den)
        end select
    end function jvp_call

    integer function jvp_minmax(p, name, a, b, da, db) result(out)
        !! Tangent of max/min: the tangent of whichever argument is selected,
        !! emitted as a `merge` so the branch stays a single expression.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: name
        integer, intent(in) :: a, b, da, db
        integer :: cond, ta, tb

        ta = da
        tb = db
        if (ta == 0) ta = fad_real(p, "0.0")
        if (tb == 0) tb = fad_real(p, "0.0")
        if (name == "max") then
            cond = fad_raw(p, ">=", a, b)
        else
            cond = fad_raw(p, "<=", a, b)
        end if
        out = fad_fn3(p, "merge", ta, tb, cond)
    end function jvp_minmax

    ! ---------------------------------------------------------------- builders
    !
    ! Zero-aware constructors. Every rule goes through these, so no rule has to
    ! reason about structural zeros, and none of them nests a mutating call.

    integer function fad_lit(p, text) result(out)
        !! An integer literal, or any text needing no kind suffix.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: text

        out = p%add_expr(expr_const(text))
    end function fad_lit

    integer function fad_real(p, mantissa) result(out)
        !! A real literal in the primal's own kind.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: mantissa
        character(len=:), allocatable :: suffix

        suffix = "d0"
        if (allocated(p%real_suffix)) suffix = p%real_suffix
        out = p%add_expr(expr_const(mantissa//suffix))
    end function fad_real

    integer function fad_raw(p, op, a, b) result(out)
        !! A binary node, folding integer-literal arithmetic and the identity
        !! powers. Folding here rather than in a later pass matters: `d(y**2)`
        !! would otherwise emit `2 * y ** (2 - 1)`, and no Fortran compiler is
        !! obliged to turn that back into `2 * y`.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: op
        integer, intent(in) :: a, b
        integer :: ia, ib
        logical :: got_a, got_b
        character(len=32) :: buf

        call int_literal(p, a, ia, got_a)
        call int_literal(p, b, ib, got_b)

        if (got_a .and. got_b) then
            select case (trim(op))
            case ("+")
                write (buf, '(i0)') ia + ib
                out = fad_lit(p, trim(buf))
                return
            case ("-")
                write (buf, '(i0)') ia - ib
                out = fad_lit(p, trim(buf))
                return
            case ("*")
                write (buf, '(i0)') ia*ib
                out = fad_lit(p, trim(buf))
                return
            end select
        end if

        ! x**1 is x, and x**0 is 1 for the exponents a derivative rule builds.
        if (trim(op) == "**" .and. got_b) then
            if (ib == 1) then
                out = a
                return
            else if (ib == 0) then
                out = fad_real(p, "1.0")
                return
            end if
        end if

        out = p%add_expr(expr_binop(op, a, b))
    end function fad_raw

    subroutine int_literal(p, idx, value, found)
        !! Read an integer literal constant, if that is what `idx` is.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer, intent(out) :: value
        logical, intent(out) :: found
        integer :: ios

        value = 0
        found = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind /= FAD_CONST) return
        if (.not. allocated(p%exprs(idx)%text)) return
        if (verify(trim(p%exprs(idx)%text), "0123456789") /= 0) return
        read (p%exprs(idx)%text, *, iostat=ios) value
        found = ios == 0
    end subroutine int_literal

    integer function fad_pow_int(p, a, exponent_text) result(out)
        !! `a ** <integer literal>`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a
        character(len=*), intent(in) :: exponent_text
        integer :: e

        out = 0
        if (a == 0) return
        e = fad_lit(p, exponent_text)
        out = fad_raw(p, "**", a, e)
    end function fad_pow_int

    integer function fad_pow(p, a, b) result(out)
        !! `a ** b`, absorbing a zero base or exponent.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a, b

        out = 0
        if (a == 0 .or. b == 0) return
        out = fad_raw(p, "**", a, b)
    end function fad_pow

    integer function fad_fn1(p, name, a) result(out)
        !! A one-argument call.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: name
        integer, intent(in) :: a
        integer :: args(1)

        out = 0
        if (a == 0) return
        args(1) = a
        out = p%add_expr(expr_call(name, args))
    end function fad_fn1

    integer function fad_fn2(p, name, a, b) result(out)
        !! A two-argument call.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: name
        integer, intent(in) :: a, b
        integer :: args(2)

        args(1) = a
        args(2) = b
        out = p%add_expr(expr_call(name, args))
    end function fad_fn2

    integer function fad_fn3(p, name, a, b, c) result(out)
        !! A three-argument call.
        type(fad_proc_t), intent(inout) :: p
        character(len=*), intent(in) :: name
        integer, intent(in) :: a, b, c
        integer :: args(3)

        args(1) = a
        args(2) = b
        args(3) = c
        out = p%add_expr(expr_call(name, args))
    end function fad_fn3

    integer function fad_add(p, a, b) result(out)
        !! `a + b`, dropping structural zeros.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a, b

        if (a == 0) then
            out = b
        else if (b == 0) then
            out = a
        else
            out = fad_raw(p, "+", a, b)
        end if
    end function fad_add

    integer function fad_sub(p, a, b) result(out)
        !! `a - b`, dropping structural zeros.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a, b

        if (b == 0) then
            out = a
        else if (a == 0) then
            out = fad_neg(p, b)
        else
            out = fad_raw(p, "-", a, b)
        end if
    end function fad_sub

    integer function fad_mul(p, a, b) result(out)
        !! `a * b`, absorbing structural zeros and dropping unit factors.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a, b

        out = 0
        if (a == 0 .or. b == 0) return
        if (is_one(p, a)) then
            out = b
        else if (is_one(p, b)) then
            out = a
        else
            out = fad_raw(p, "*", a, b)
        end if
    end function fad_mul

    integer function fad_div(p, a, b) result(out)
        !! `a / b`, absorbing a zero numerator.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a, b

        out = 0
        if (a == 0) return
        if (is_one(p, b)) then
            out = a
        else
            out = fad_raw(p, "/", a, b)
        end if
    end function fad_div

    integer function fad_neg(p, a) result(out)
        !! `-a`, absorbing a structural zero.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: a

        out = 0
        if (a == 0) return
        out = p%add_expr(expr_unop("-", a))
    end function fad_neg

    logical function is_one(p, idx) result(yes)
        !! True for a literal one in any spelling fortad itself emits.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind /= FAD_CONST) return
        if (.not. allocated(p%exprs(idx)%text)) return
        select case (trim(p%exprs(idx)%text))
        case ("1", "1.0", "1.0_dp", "1.d0", "1.0d0", "1.0e0", "1.0_wp")
            yes = .true.
        end select
    end function is_one

    pure function lower(s) result(out)
        !! ASCII lowercase, for case-insensitive intrinsic lookup.
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, c

        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
                out(i:i) = achar(c + 32)
            else
                out(i:i) = s(i:i)
            end if
        end do
    end function lower

end module fortad_rules
