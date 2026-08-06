module fortad_taylor_gen
    !! Taylor-mode transformation: rewrite a kernel into Taylor arithmetic.
    !!
    !! Forward and reverse mode build derivative *expressions*, because a
    !! tangent is one number and fits in one. A Taylor object is `d+1` numbers
    !! and each operation is a convolution, so it does not fit in an expression;
    !! the transformation emits a sequence of calls to `fortad_taylor` with a
    !! temporary per intermediate instead.
    !!
    !! That shape is deliberate rather than a limitation. Each emitted call is a
    !! fixed-trip loop over the coefficient array, which is what a compiler
    !! vectorises and what makes the cost `O(d²)` per operation rather than the
    !! `O(2ᵈ)` of nesting a first-order tool `d` times.
    !!
    !! Straight-line scalar code only. A Taylor object is a coefficient array,
    !! so an array-valued kernel would need a rank more than fortad's IR carries,
    !! and loops would need the temporaries to be loop-local. Both are refused
    !! by name.
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
        expr_const, expr_var, FAD_CONST, FAD_VAR, FAD_BINOP, &
        FAD_UNOP, FAD_CALL, FAD_INDEX, FAD_ASSIGN, &
        FAD_CALL_STMT, FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, &
        FAD_END_IF, FAD_INTENT_IN, FAD_INTENT_OUT, &
        FAD_INTENT_NONE
    implicit none
    private

    public :: differentiate_taylor, taylor_spec_t, taylor_status_t

    type :: taylor_spec_t
        !! What to differentiate, and to what order.
        character(len=:), allocatable :: independents(:)
        !! Name of the order dummy argument. The coefficient arrays are
        !! declared `(0:order)`, so the caller chooses the order at the call
        !! site rather than at generation time.
        character(len=:), allocatable :: order_name
        !! Suffix for Taylor variables. `x` becomes `x_t` by default.
        character(len=:), allocatable :: suffix
        character(len=:), allocatable :: name
    end type taylor_spec_t

    type :: taylor_status_t
        logical :: ok = .false.
        character(len=:), allocatable :: message
    end type taylor_status_t

contains

    subroutine differentiate_taylor(primal, spec, taylor, status)
        !! Build the Taylor-mode procedure.
        type(fad_proc_t), intent(in) :: primal
        type(taylor_spec_t), intent(in) :: spec
        type(fad_proc_t), intent(out) :: taylor
        type(taylor_status_t), intent(out) :: status
        character(len=:), allocatable :: suffix, order
        integer :: n_tmp

        status%ok = .true.
        suffix = "_t"
        if (allocated(spec%suffix)) suffix = spec%suffix
        order = "order"
        if (allocated(spec%order_name)) order = spec%order_name

        if (.not. allocated(spec%independents)) then
            status%ok = .false.
            status%message = "no independent variables given"
            return
        end if

        call check_supported(primal, status)
        if (.not. status%ok) return

        taylor%name = primal%name//"_taylor"
        if (allocated(spec%name)) taylor%name = spec%name
        taylor%is_function = .false.
        taylor%is_pure = .true.
        taylor%real_suffix = "d0"
        if (allocated(primal%real_suffix)) taylor%real_suffix = primal%real_suffix

        call build_signature(primal, taylor, suffix, order)
        n_tmp = 0
        call build_body(primal, taylor, suffix, order, n_tmp, status)
    end subroutine differentiate_taylor

    subroutine check_supported(primal, status)
        !! Refuse what a coefficient array cannot represent here.
        type(fad_proc_t), intent(in) :: primal
        type(taylor_status_t), intent(inout) :: status
        integer :: i

        do i = 1, primal%n_decls
            if (primal%decls(i)%is_array) then
                status%ok = .false.
                status%message = "Taylor mode: '"//primal%decls(i)%name// &
                    "' is an array, and a Taylor object is already an array of "// &
                    "coefficients; use forward or reverse mode"
                return
            end if
        end do

        do i = 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                continue
            case default
                status%ok = .false.
                status%message = "Taylor mode: only straight-line assignments "// &
                    "are supported; loops and branches would need loop-local "// &
                    "coefficient temporaries"
                return
            end select
        end do
    end subroutine check_supported

    subroutine build_signature(primal, taylor, suffix, order)
        !! Every real entity becomes a coefficient array; the order leads.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: taylor
        character(len=*), intent(in) :: suffix, order
        character(len=64), allocatable :: names(:)
        type(fad_decl_t) :: d
        integer :: i, n, di, ignored

        allocate (names(size(primal%params) + 4))
        n = 1
        names(1) = order
        d%name = order
        d%type_name = "integer"
        d%intent = FAD_INTENT_IN
        ignored = taylor%add_decl(d)

        do i = 1, size(primal%params)
            di = primal%decl_index(trim(primal%params(i)))
            if (di == 0) cycle
            n = n + 1
            names(n) = trim(primal%params(i))//suffix
            call add_taylor_decl(taylor, primal%decls(di), suffix, order, &
                primal%decls(di)%intent)
        end do

        if (primal%is_function) then
            di = primal%decl_index(primal%result_name)
            if (di > 0) then
                n = n + 1
                names(n) = primal%result_name//suffix
                call add_taylor_decl(taylor, primal%decls(di), suffix, order, &
                    FAD_INTENT_OUT)
            end if
        end if

        allocate (character(len=64) :: taylor%params(n))
        do i = 1, n
            taylor%params(i) = names(i)
        end do
    end subroutine build_signature

    subroutine add_taylor_decl(taylor, primal_decl, suffix, order, intent_code)
        !! Declare one coefficient array.
        type(fad_proc_t), intent(inout) :: taylor
        type(fad_decl_t), intent(in) :: primal_decl
        character(len=*), intent(in) :: suffix, order
        integer, intent(in) :: intent_code
        type(fad_decl_t) :: d
        integer :: ignored

        d = primal_decl
        d%name = primal_decl%name//suffix
        d%intent = intent_code
        d%is_result = .false.
        d%is_array = .true.
        d%dims = "0:"//order
        ignored = taylor%add_decl(d)
    end subroutine add_taylor_decl

    subroutine build_body(primal, taylor, suffix, order, n_tmp, status)
        !! Emit one call sequence per statement.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: taylor
        character(len=*), intent(in) :: suffix, order
        integer, intent(inout) :: n_tmp
        type(taylor_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=:), allocatable :: result_name
        integer :: i, di, ignored

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            di = primal%decl_index(primal%stmts(i)%target)
            if (di == 0) then
                status%ok = .false.
                status%message = "assignment to undeclared '"// &
                    primal%stmts(i)%target//"'"
                return
            end if
            ! A local target needs its own coefficient array.
            if (taylor%decl_index(primal%stmts(i)%target//suffix) == 0) then
                call add_taylor_decl(taylor, primal%decls(di), suffix, order, &
                    FAD_INTENT_NONE)
            end if

            call emit_expr_calls(primal, taylor, primal%stmts(i)%value, suffix, &
                order, n_tmp, result_name, status)
            if (.not. status%ok) return

            ! Copy the result into the target's array. A whole-array assignment
            ! is one loop the compiler will fuse with the producing call.
            s%kind = FAD_ASSIGN
            s%target = primal%stmts(i)%target//suffix
            s%value = taylor%add_expr(expr_var(result_name))
            ignored = taylor%add_stmt(s)
        end do
    end subroutine build_body

    recursive subroutine emit_expr_calls(primal, taylor, idx, suffix, order, &
            n_tmp, result_name, status)
        !! Emit the calls computing one expression, returning the name holding
        !! its coefficients.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: taylor
        integer, intent(in) :: idx
        character(len=*), intent(in) :: suffix, order
        integer, intent(inout) :: n_tmp
        character(len=:), allocatable, intent(out) :: result_name
        type(taylor_status_t), intent(inout) :: status
        character(len=:), allocatable :: a_name, b_name, spare
        integer :: kind

        result_name = ""
        if (idx <= 0 .or. idx > primal%n_exprs) then
            status%ok = .false.
            status%message = "empty expression"
            return
        end if

        kind = primal%exprs(idx)%kind
        select case (kind)
        case (FAD_VAR)
            result_name = primal%exprs(idx)%text//suffix
            return

        case (FAD_CONST)
            call fresh_temp(taylor, order, n_tmp, result_name)
            call emit_call(taylor, "tay_const", primal%exprs(idx)%text, &
                result_name)
            return

        case (FAD_BINOP)
            call emit_expr_calls(primal, taylor, primal%exprs(idx)%args(1), &
                suffix, order, n_tmp, a_name, status)
            if (.not. status%ok) return
            call emit_expr_calls(primal, taylor, primal%exprs(idx)%args(2), &
                suffix, order, n_tmp, b_name, status)
            if (.not. status%ok) return
            call fresh_temp(taylor, order, n_tmp, result_name)
            select case (trim(primal%exprs(idx)%text))
            case ("+")
                call emit_call(taylor, "tay_add", a_name, b_name, result_name)
            case ("-")
                call emit_call(taylor, "tay_sub", a_name, b_name, result_name)
            case ("*")
                call emit_call(taylor, "tay_mul", a_name, b_name, result_name)
            case ("/")
                call emit_call(taylor, "tay_div", a_name, b_name, result_name)
            case default
                status%ok = .false.
                status%message = "Taylor mode: no rule for operator '"// &
                    trim(primal%exprs(idx)%text)//"'"
            end select
            return

        case (FAD_UNOP)
            call emit_expr_calls(primal, taylor, primal%exprs(idx)%args(1), &
                suffix, order, n_tmp, a_name, status)
            if (.not. status%ok) return
            if (trim(primal%exprs(idx)%text) /= "-") then
                status%ok = .false.
                status%message = "Taylor mode: no rule for unary '"// &
                    trim(primal%exprs(idx)%text)//"'"
                return
            end if
            call fresh_temp(taylor, order, n_tmp, result_name)
            call emit_call(taylor, "tay_scale", &
                "-1.0"//taylor%real_suffix, a_name, result_name)
            return

        case (FAD_CALL)
            call emit_call_rule(primal, taylor, idx, suffix, order, n_tmp, &
                result_name, status)
            return

        case default
            status%ok = .false.
            status%message = "Taylor mode: unsupported expression"
        end select
    end subroutine emit_expr_calls

    recursive subroutine emit_call_rule(primal, taylor, idx, suffix, order, &
            n_tmp, result_name, status)
        !! Emit the call for an intrinsic.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: taylor
        integer, intent(in) :: idx
        character(len=*), intent(in) :: suffix, order
        integer, intent(inout) :: n_tmp
        character(len=:), allocatable, intent(out) :: result_name
        type(taylor_status_t), intent(inout) :: status
        character(len=:), allocatable :: a_name, spare

        call emit_expr_calls(primal, taylor, primal%exprs(idx)%args(1), suffix, &
            order, n_tmp, a_name, status)
        if (.not. status%ok) return
        call fresh_temp(taylor, order, n_tmp, result_name)

        select case (lower(primal%exprs(idx)%text))
        case ("exp")
            call emit_call(taylor, "tay_exp", a_name, result_name)
        case ("log")
            call emit_call(taylor, "tay_log", a_name, result_name)
        case ("sqrt")
            call emit_call(taylor, "tay_sqrt", a_name, result_name)
        case ("sin")
            ! sin and cos share a recurrence, so the unused one is a temporary.
            call fresh_temp(taylor, order, n_tmp, spare)
            call emit_call(taylor, "tay_sin_cos", a_name, result_name, spare)
        case ("cos")
            call fresh_temp(taylor, order, n_tmp, spare)
            call emit_call(taylor, "tay_sin_cos", a_name, spare, result_name)
        case default
            status%ok = .false.
            status%message = "Taylor mode: no rule for '"// &
                primal%exprs(idx)%text//"'; the arithmetic in fortad_taylor "// &
                "covers exp, log, sqrt, sin and cos"
        end select
    end subroutine emit_call_rule

    subroutine fresh_temp(taylor, order, n_tmp, name)
        !! Allocate a coefficient temporary.
        type(fad_proc_t), intent(inout) :: taylor
        character(len=*), intent(in) :: order
        integer, intent(inout) :: n_tmp
        character(len=:), allocatable, intent(out) :: name
        type(fad_decl_t) :: d
        character(len=32) :: buf
        integer :: ignored

        do
            n_tmp = n_tmp + 1
            write (buf, '(i0)') n_tmp
            name = "fad_ty"//trim(buf)
            if (taylor%decl_index(name) == 0) exit
        end do

        d%name = name
        d%type_name = "real(8)"
        d%intent = FAD_INTENT_NONE
        d%is_array = .true.
        d%dims = "0:"//order
        ignored = taylor%add_decl(d)
    end subroutine fresh_temp

    subroutine emit_call(taylor, name, a1, a2, a3)
        !! Emit `call name(a1, a2 [, a3])`, each argument a bare name.
        !!
        !! Separate arguments rather than an array: building the list with
        !! `[character(len=64) :: x, y]` over deferred-length strings made
        !! gfortran write past the constructor temporary, and the corruption
        !! only surfaced as a double free on subroutine exit. Explicit
        !! arguments have no such ambiguity.
        type(fad_proc_t), intent(inout) :: taylor
        character(len=*), intent(in) :: name, a1, a2
        character(len=*), intent(in), optional :: a3
        type(fad_stmt_t) :: s
        integer, allocatable :: cargs(:)
        integer :: ignored

        if (present(a3)) then
            allocate (cargs(3))
        else
            allocate (cargs(2))
        end if
        cargs(1) = taylor%add_expr(expr_var(trim(a1)))
        cargs(2) = taylor%add_expr(expr_var(trim(a2)))
        if (present(a3)) cargs(3) = taylor%add_expr(expr_var(trim(a3)))

        s%kind = FAD_CALL_STMT
        s%target = name
        s%call_args = cargs
        ignored = taylor%add_stmt(s)
    end subroutine emit_call

    pure function lower(s) result(out)
        !! ASCII lowercase.
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

end module fortad_taylor_gen
