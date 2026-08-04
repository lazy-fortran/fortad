module fortad_opt
    !! Machine-independent optimisation of the emitted derivative code.
    !!
    !! The transformations here are the ones a Fortran compiler is not allowed
    !! to make. Reassociating floating-point arithmetic changes rounding, so
    !! `gfortran` and `flang` will not turn `b + b*c` into `b*(1 + c)` without
    !! `-ffast-math`, and the caller's build almost certainly does not use it.
    !! An AD tool may: the two forms are equal in exact arithmetic, and the
    !! derivative is an approximation of a limit either way. Tapenade does this,
    !! and on a linear recurrence it is the difference between a three-operation
    !! dependence chain per iteration and a single multiply.
    !!
    !! The passes run in sequence and each is a fixed point:
    !!
    !! 1. `propagate_copies`   - `a = b` makes later reads of `a` read `b`.
    !! 2. `substitute_temps`   - inline a definition into its use.
    !! 3. `factor_self_update` - `x = x*c1 + x*c2` becomes `x = x*(c1 + c2)`.
    !! 4. `hoist_invariants`   - move loop-invariant work out of the loop.
    !!
    !! Every pass reasons only within a straight-line run of assignments: any
    !! `do`, `if`, or call between a definition and a use ends the run. That is
    !! conservative - it gives up on code it could handle - but it needs no
    !! dataflow lattice, and a wrong answer here is a wrong derivative rather
    !! than a crash.
    use fortad_ir, only: fad_proc_t, fad_stmt_t, fad_expr_t, expr_var, &
                         expr_const, expr_binop, FAD_CONST, FAD_VAR, &
                         FAD_BINOP, FAD_UNOP, FAD_CALL, FAD_INDEX, &
                         FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, &
                         FAD_END_IF, FAD_CALL_STMT, fad_decl_t, &
                         FAD_INTENT_NONE
    implicit none
    private

    public :: optimise

    !! Substituting a definition into its use duplicates the definition when the
    !! use count is above one. That is worth it for a single arithmetic
    !! operation, whose result the factoring pass can then collapse, and not
    !! worth it for a large tree. This caps the expression a substitution may
    !! produce.
    integer, parameter :: MAX_EXPR_NODES = 32

contains

    subroutine optimise(p)
        !! Run the passes to a fixed point.
        type(fad_proc_t), intent(inout) :: p
        integer :: pass

        do pass = 1, 4
            call propagate_copies(p)
            call substitute_temps(p)
            call factor_self_update(p)
        end do
        call regroup_products(p)
        call hoist_invariants(p)
        call hoist_subexpressions(p)
    end subroutine optimise

    ! ------------------------------------------------------------------
    ! Straight-line reasoning
    ! ------------------------------------------------------------------

    logical function straight_line(p, a, b) result(yes)
        !! Whether statements strictly between `a` and `b` are all assignments.
        !!
        !! Control flow between a definition and a use means the definition may
        !! not reach it, or may reach it from another iteration.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: a, b
        integer :: i

        yes = .false.
        if (b < a) return
        do i = a + 1, b - 1
            if (p%stmts(i)%kind /= FAD_ASSIGN) return
        end do
        yes = .true.
    end function straight_line

    logical function assigns_to(p, idx, name) result(yes)
        !! Whether statement `idx` writes `name`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name

        yes = .false.
        if (p%stmts(idx)%kind == FAD_DO) then
            yes = allocated(p%stmts(idx)%target)
            if (yes) yes = trim(p%stmts(idx)%target) == name
            return
        end if
        if (p%stmts(idx)%kind /= FAD_ASSIGN) return
        if (.not. allocated(p%stmts(idx)%target)) return
        yes = base_of(p%stmts(idx)%target) == name
    end function assigns_to

    logical function assigned_between(p, a, b, name) result(yes)
        !! Whether `name` is written by any statement in `(a, b)`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: a, b
        character(len=*), intent(in) :: name
        integer :: i

        yes = .true.
        do i = a + 1, b - 1
            if (assigns_to(p, i, name)) return
        end do
        yes = .false.
    end function assigned_between

    function base_of(target) result(base)
        !! The name in an assignment target, without any subscript.
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: base
        integer :: pos

        pos = index(target, "(")
        if (pos > 0) then
            base = trim(target(1:pos - 1))
        else
            base = trim(target)
        end if
    end function base_of

    ! ------------------------------------------------------------------
    ! Expression helpers
    ! ------------------------------------------------------------------

    recursive integer function expr_size(p, idx) result(n)
        !! Node count of an expression tree.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: i

        n = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        n = 1
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            n = n + expr_size(p, p%exprs(idx)%args(i))
        end do
    end function expr_size

    recursive integer function count_reads(p, idx, name) result(n)
        !! How many times `name` is read as a plain variable in an expression.
        !!
        !! Only `FAD_VAR` nodes whose text is exactly the name count. A name
        !! appearing inside a subscript or inside text fortad did not build is
        !! deliberately not matched here; `mentions` is the conservative test
        !! used to decide legality, and this one only decides profitability.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        n = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_VAR) then
            if (allocated(p%exprs(idx)%text)) then
                if (trim(p%exprs(idx)%text) == name) n = 1
            end if
            return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            n = n + count_reads(p, p%exprs(idx)%args(i), name)
        end do
    end function count_reads

    recursive logical function mentions(p, idx, name) result(yes)
        !! Whether `name` occurs anywhere in an expression, including inside
        !! the text of a node fortad cannot see into.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (allocated(p%exprs(idx)%text)) then
            if (name_in_text(p%exprs(idx)%text, name)) then
                yes = .true.
                return
            end if
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            if (mentions(p, p%exprs(idx)%args(i), name)) then
                yes = .true.
                return
            end if
        end do
    end function mentions

    logical function name_in_text(text, name) result(yes)
        !! Whether `name` appears in `text` as a whole identifier.
        character(len=*), intent(in) :: text, name
        integer :: pos, from, l

        yes = .false.
        l = len_trim(name)
        if (l == 0) return
        from = 1
        do
            pos = index(text(from:), trim(name))
            if (pos == 0) return
            pos = pos + from - 1
            if (boundary(text, pos - 1) .and. boundary(text, pos + l)) then
                yes = .true.
                return
            end if
            from = pos + 1
            if (from > len(text)) return
        end do
    end function name_in_text

    logical function boundary(text, pos) result(yes)
        !! Whether position `pos` is outside an identifier.
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos
        character :: c

        yes = .true.
        if (pos < 1 .or. pos > len(text)) return
        c = text(pos:pos)
        yes = .not. ((c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") &
                     .or. (c >= "0" .and. c <= "9") .or. c == "_")
    end function boundary

    recursive integer function replace_var(p, idx, name, repl) result(out)
        !! Rebuild an expression with plain reads of `name` replaced by `repl`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx, repl
        character(len=*), intent(in) :: name
        integer, allocatable :: new_args(:)
        type(fad_expr_t) :: e
        integer :: i, kind_here
        character(len=:), allocatable :: text_here
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_VAR) then
            if (allocated(p%exprs(idx)%text)) then
                if (trim(p%exprs(idx)%text) == name) out = repl
            end if
            return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return

        ! Snapshot before rebuilding: `add_expr` may grow the arena and
        ! invalidate anything read through `p%exprs(idx)` afterwards.
        kind_here = p%exprs(idx)%kind
        text_here = p%exprs(idx)%text
        new_args = p%exprs(idx)%args
        changed = .false.
        do i = 1, size(new_args)
            block
                integer :: sub
                sub = replace_var(p, new_args(i), name, repl)
                if (sub /= new_args(i)) changed = .true.
                new_args(i) = sub
            end block
        end do
        if (.not. changed) return

        e%kind = kind_here
        e%text = text_here
        e%args = new_args
        out = p%add_expr(e)
    end function replace_var

    ! ------------------------------------------------------------------
    ! Pass 1: copy propagation
    ! ------------------------------------------------------------------

    subroutine propagate_copies(p)
        !! Make a read of `a` read `b` instead, given an `a = b` that reaches it.
        type(fad_proc_t), intent(inout) :: p
        character(len=:), allocatable :: lhs, rhs
        integer :: i, j
        logical :: changed

        do while (.true.)
            changed = .false.
            do i = 1, p%n_stmts
                if (p%stmts(i)%kind /= FAD_ASSIGN) cycle
                if (.not. allocated(p%stmts(i)%target)) cycle
                if (index(p%stmts(i)%target, "(") > 0) cycle
                if (p%stmts(i)%value <= 0) cycle
                if (p%exprs(p%stmts(i)%value)%kind /= FAD_VAR) cycle
                lhs = trim(p%stmts(i)%target)
                rhs = trim(p%exprs(p%stmts(i)%value)%text)
                if (lhs == rhs) cycle
                do j = i + 1, p%n_stmts
                    if (.not. straight_line(p, i, j)) exit
                    if (p%stmts(j)%kind /= FAD_ASSIGN) exit
                    ! The copy stops being valid once either name is rewritten.
                    if (assigned_between(p, i, j, lhs)) exit
                    if (assigned_between(p, i, j, rhs)) exit
                    if (p%stmts(j)%value > 0) then
                        block
                            integer :: new
                            new = replace_var(p, p%stmts(j)%value, lhs, &
                                              p%stmts(i)%value)
                            if (new /= p%stmts(j)%value) then
                                p%stmts(j)%value = new
                                changed = .true.
                            end if
                        end block
                    end if
                    if (assigns_to(p, j, lhs)) exit
                    if (assigns_to(p, j, rhs)) exit
                end do
            end do
            if (.not. changed) exit
        end do
        call drop_self_assignments(p)
    end subroutine propagate_copies

    subroutine drop_self_assignments(p)
        !! Remove `x = x`, which copy propagation produces and which blocks it.
        type(fad_proc_t), intent(inout) :: p
        type(fad_stmt_t), allocatable :: out(:)
        integer :: i, n

        if (p%n_stmts == 0) return
        allocate (out(p%n_stmts))
        n = 0
        do i = 1, p%n_stmts
            if (p%stmts(i)%kind == FAD_ASSIGN) then
                if (allocated(p%stmts(i)%target) .and. p%stmts(i)%value > 0) then
                    if (index(p%stmts(i)%target, "(") == 0 .and. &
                        p%exprs(p%stmts(i)%value)%kind == FAD_VAR) then
                        if (trim(p%stmts(i)%target) == &
                            trim(p%exprs(p%stmts(i)%value)%text)) cycle
                    end if
                end if
            end if
            n = n + 1
            out(n) = p%stmts(i)
        end do
        p%stmts(1:n) = out(1:n)
        p%n_stmts = n
    end subroutine drop_self_assignments

    ! ------------------------------------------------------------------
    ! Pass 2: substitution
    ! ------------------------------------------------------------------

    subroutine substitute_temps(p)
        !! Inline a definition into the statements that read it.
        !!
        !! A definition is inlined when it is read exactly once, or when it is
        !! one arithmetic operation on names and literals. The second case
        !! duplicates work, and is worth it because it is what exposes the
        !! coefficient of a self-update to the factoring pass; the result is
        !! then usually hoisted out of the loop entirely.
        type(fad_proc_t), intent(inout) :: p
        character(len=:), allocatable :: lhs
        integer :: i, j, uses, last_use, stop_at
        logical :: changed, blocked

        do while (.true.)
            changed = .false.
            do i = 1, p%n_stmts
                if (.not. simple_definition(p, i)) cycle
                lhs = trim(p%stmts(i)%target)
                if (is_dummy(p, lhs)) cycle

                ! Count reads reachable in a straight line, and stop at the
                ! first thing that ends the run or redefines the name.
                uses = 0
                last_use = 0
                blocked = .false.
                stop_at = p%n_stmts + 1
                do j = i + 1, p%n_stmts
                    ! The run ends at the first thing that is not a plain
                    ! assignment. That is not a failure - it just bounds the
                    ! window - but everything outside the window must then be
                    ! free of the name, which `escapes` checks below.
                    if (.not. straight_line(p, i, j) .or. &
                        p%stmts(j)%kind /= FAD_ASSIGN) then
                        stop_at = j
                        exit
                    end if
                    if (p%stmts(j)%value > 0) then
                        block
                            integer :: c
                            c = count_reads(p, p%stmts(j)%value, lhs)
                            if (c > 0) then
                                uses = uses + c
                                last_use = j
                            end if
                            ! A mention the read counter cannot see - in a
                            ! subscript, or in text - is not safe to remove.
                            if (mentions(p, p%stmts(j)%value, lhs) .and. c == 0) then
                                blocked = .true.
                                exit
                            end if
                        end block
                    end if
                    if (allocated(p%stmts(j)%target)) then
                        if (name_in_text(p%stmts(j)%target, lhs) .and. &
                            base_of(p%stmts(j)%target) /= lhs) then
                            blocked = .true.
                            exit
                        end if
                    end if
                    if (assigns_to(p, j, lhs)) then
                        stop_at = j + 1
                        exit
                    end if
                end do
                if (blocked .or. uses == 0 .or. last_use == 0) cycle
                if (escapes(p, lhs, i, stop_at)) cycle
                if (uses > 1 .and. .not. cheap(p, p%stmts(i)%value)) cycle

                do j = i + 1, last_use
                    if (p%stmts(j)%value <= 0) cycle
                    if (count_reads(p, p%stmts(j)%value, lhs) == 0) cycle
                    block
                        integer :: new
                        new = replace_var(p, p%stmts(j)%value, lhs, &
                                          p%stmts(i)%value)
                        if (expr_size(p, new) > MAX_EXPR_NODES) cycle
                        p%stmts(j)%value = new
                        changed = .true.
                    end block
                end do
            end do
            if (.not. changed) exit
        end do
    end subroutine substitute_temps

    logical function escapes(p, name, first, stop_at) result(yes)
        !! Whether `name` is mentioned outside the window it was tracked in.
        !!
        !! Substitution is only sound if every read of the definition was seen.
        !! Reads before the definition belong to an earlier value, and reads
        !! after the window may see this one through a path this pass does not
        !! model, so either rules it out.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: name
        integer, intent(in) :: first, stop_at
        integer :: j

        yes = .true.
        do j = 1, p%n_stmts
            if (j >= first .and. j < stop_at) cycle
            if (p%stmts(j)%value > 0) then
                if (mentions(p, p%stmts(j)%value, name)) return
            end if
            if (p%stmts(j)%lo > 0) then
                if (mentions(p, p%stmts(j)%lo, name)) return
            end if
            if (p%stmts(j)%hi > 0) then
                if (mentions(p, p%stmts(j)%hi, name)) return
            end if
            if (allocated(p%stmts(j)%target)) then
                if (name_in_text(p%stmts(j)%target, name)) return
            end if
        end do
        yes = .false.
    end function escapes

    logical function simple_definition(p, idx) result(yes)
        !! Whether a statement is `name = expression` with a plain target.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx

        yes = .false.
        if (p%stmts(idx)%kind /= FAD_ASSIGN) return
        if (.not. allocated(p%stmts(idx)%target)) return
        if (index(p%stmts(idx)%target, "(") > 0) return
        if (p%stmts(idx)%value <= 0) return
        yes = .true.
    end function simple_definition

    logical function cheap(p, idx) result(yes)
        !! One arithmetic operation whose operands are names or literals.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_CONST, FAD_VAR)
            yes = .true.
            return
        case (FAD_BINOP, FAD_UNOP)
        case default
            return
        end select
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            select case (p%exprs(p%exprs(idx)%args(i))%kind)
            case (FAD_CONST, FAD_VAR)
            case default
                return
            end select
        end do
        yes = .true.
    end function cheap

    logical function is_dummy(p, name) result(yes)
        !! Whether a name is a dummy argument.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        if (.not. allocated(p%params)) return
        do i = 1, size(p%params)
            if (trim(p%params(i)) == name) then
                yes = .true.
                return
            end if
        end do
    end function is_dummy

    ! ------------------------------------------------------------------
    ! Pass 3: factoring a self-update
    ! ------------------------------------------------------------------

    subroutine factor_self_update(p)
        !! Rewrite `x = x*c1 + x*c2 + r` as `x = x*(c1 + c2) + r`.
        !!
        !! This is the adjoint of a linear recurrence. Left alone it is a chain
        !! of a multiply and an add per term, all dependent on the previous
        !! iteration's `x`; factored, it is one multiply, and the coefficient no
        !! longer depends on `x` so the next pass can lift it out of the loop.
        !!
        !! Reassociation changes rounding. It does not change the value in exact
        !! arithmetic, and it is what every source-transformation tool does
        !! here.
        type(fad_proc_t), intent(inout) :: p
        integer :: i

        do i = 1, p%n_stmts
            if (.not. simple_definition(p, i)) cycle
            call factor_one(p, i)
        end do
    end subroutine factor_self_update

    subroutine factor_one(p, idx)
        !! Factor the right-hand side of statement `idx` around its own target.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        integer, parameter :: MAX_TERMS = 64
        integer :: terms(MAX_TERMS), signs(MAX_TERMS), n_terms
        integer :: coeffs(MAX_TERMS), coeff_signs(MAX_TERMS), n_coeffs
        integer :: rest(MAX_TERMS), rest_signs(MAX_TERMS), n_rest
        character(len=:), allocatable :: name
        integer :: i, coeff, acc, value

        name = trim(p%stmts(idx)%target)
        n_terms = 0
        call flatten_sum(p, p%stmts(idx)%value, 1, terms, signs, n_terms, MAX_TERMS)
        if (n_terms < 2 .or. n_terms > MAX_TERMS) return

        n_coeffs = 0
        n_rest = 0
        do i = 1, n_terms
            coeff = coefficient_of(p, terms(i), name)
            if (coeff == -1) return          ! `x` appears in a shape we cannot factor
            if (coeff == 0) then
                n_rest = n_rest + 1
                rest(n_rest) = terms(i)
                rest_signs(n_rest) = signs(i)
            else
                n_coeffs = n_coeffs + 1
                coeffs(n_coeffs) = coeff
                coeff_signs(n_coeffs) = signs(i)
            end if
        end do
        if (n_coeffs < 2) return

        acc = 0
        do i = 1, n_coeffs
            if (acc == 0) then
                if (coeff_signs(i) > 0) then
                    acc = coeffs(i)
                else
                    block
                        type(fad_expr_t) :: e
                        e%kind = FAD_UNOP
                        e%text = "-"
                        e%args = [coeffs(i)]
                        acc = p%add_expr(e)
                    end block
                end if
            else
                block
                    integer :: lhs_idx
                    lhs_idx = acc
                    if (coeff_signs(i) > 0) then
                        acc = p%add_expr(expr_binop("+", lhs_idx, coeffs(i)))
                    else
                        acc = p%add_expr(expr_binop("-", lhs_idx, coeffs(i)))
                    end if
                end block
            end if
        end do

        block
            integer :: var_idx
            var_idx = p%add_expr(expr_var(name))
            value = p%add_expr(expr_binop("*", var_idx, acc))
        end block
        do i = 1, n_rest
            block
                integer :: lhs_idx
                lhs_idx = value
                if (rest_signs(i) > 0) then
                    value = p%add_expr(expr_binop("+", lhs_idx, rest(i)))
                else
                    value = p%add_expr(expr_binop("-", lhs_idx, rest(i)))
                end if
            end block
        end do
        p%stmts(idx)%value = value
    end subroutine factor_one

    recursive subroutine flatten_sum(p, idx, sign, terms, signs, n, cap)
        !! Split an expression into signed additive terms.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, sign, cap
        integer, intent(inout) :: terms(:), signs(:), n

        if (idx <= 0 .or. idx > p%n_exprs .or. n > cap) return
        if (p%exprs(idx)%kind == FAD_BINOP) then
            if (trim(p%exprs(idx)%text) == "+") then
                call flatten_sum(p, p%exprs(idx)%args(1), sign, terms, signs, n, cap)
                call flatten_sum(p, p%exprs(idx)%args(2), sign, terms, signs, n, cap)
                return
            else if (trim(p%exprs(idx)%text) == "-") then
                call flatten_sum(p, p%exprs(idx)%args(1), sign, terms, signs, n, cap)
                call flatten_sum(p, p%exprs(idx)%args(2), -sign, terms, signs, n, cap)
                return
            end if
        end if
        if (n >= cap) return
        n = n + 1
        terms(n) = idx
        signs(n) = sign
    end subroutine flatten_sum

    integer function coefficient_of(p, idx, name) result(coeff)
        !! The multiplier of `name` in a single product term.
        !!
        !! Returns 0 when the term does not involve `name` at all, -1 when it
        !! involves it in a shape this pass will not rewrite, and otherwise the
        !! expression index of the coefficient. A bare `x` has coefficient one.
        !!
        !! The term is flattened into factors first, because substitution
        !! produces left-leaning chains like `(x*a)*b` where neither operand of
        !! the top multiply is `x`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer, parameter :: MAX_FACTORS = 32
        integer :: factors(MAX_FACTORS), n_factors
        integer :: i, n_plain

        coeff = -1
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (.not. mentions(p, idx, name)) then
            coeff = 0
            return
        end if

        n_factors = 0
        call flatten_product(p, idx, factors, n_factors, MAX_FACTORS)
        if (n_factors == 0 .or. n_factors > MAX_FACTORS) return

        ! Exactly one factor must be the variable itself. Any other mention
        ! makes the term nonlinear in it, and this pass leaves it alone.
        n_plain = 0
        do i = 1, n_factors
            if (is_plain(p, factors(i), name)) then
                n_plain = n_plain + 1
            else if (mentions(p, factors(i), name)) then
                return
            end if
        end do
        if (n_plain /= 1) return

        coeff = 0
        do i = 1, n_factors
            if (is_plain(p, factors(i), name)) cycle
            if (coeff == 0) then
                coeff = factors(i)
            else
                block
                    integer :: lhs_idx
                    lhs_idx = coeff
                    coeff = p%add_expr(expr_binop("*", lhs_idx, factors(i)))
                end block
            end if
        end do
        if (coeff == 0) coeff = p%add_expr(expr_const("1.0d0"))
    end function coefficient_of

    recursive subroutine flatten_product(p, idx, factors, n, cap)
        !! Split an expression into multiplied factors.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, cap
        integer, intent(inout) :: factors(:), n

        if (idx <= 0 .or. idx > p%n_exprs .or. n > cap) return
        if (p%exprs(idx)%kind == FAD_BINOP) then
            if (trim(p%exprs(idx)%text) == "*") then
                call flatten_product(p, p%exprs(idx)%args(1), factors, n, cap)
                call flatten_product(p, p%exprs(idx)%args(2), factors, n, cap)
                return
            end if
        end if
        if (n >= cap) return
        n = n + 1
        factors(n) = idx
    end subroutine flatten_product

    logical function is_plain(p, idx, name) result(yes)
        !! Whether an expression is exactly the variable `name`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind /= FAD_VAR) return
        if (.not. allocated(p%exprs(idx)%text)) return
        yes = trim(p%exprs(idx)%text) == name
    end function is_plain

    ! ------------------------------------------------------------------
    ! Pass 4: loop-invariant code motion
    ! ------------------------------------------------------------------

    logical function loop_invariant(p, first, last, idx) result(yes)
        !! Whether an expression has the same value in every iteration.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last, idx
        integer :: j

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (allocated(p%stmts(first)%target)) then
            if (mentions(p, idx, trim(p%stmts(first)%target))) return
        end if
        do j = first + 1, last - 1
            if (p%stmts(j)%kind /= FAD_ASSIGN) return
            if (.not. allocated(p%stmts(j)%target)) return
            if (mentions(p, idx, base_of(p%stmts(j)%target))) return
        end do
        yes = .true.
    end function loop_invariant

    logical function plain_body(p, first, last) result(yes)
        !! Whether a loop body is a run of plain assignments.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        integer :: j

        yes = .false.
        if (last <= first) return
        do j = first + 1, last - 1
            if (p%stmts(j)%kind /= FAD_ASSIGN) return
        end do
        yes = .true.
    end function plain_body

    subroutine regroup_products(p)
        !! Reassociate each product so its loop-invariant factors sit together.
        !!
        !! Substitution builds left-leaning chains: `(b*dt)*0.05` has no subtree
        !! that is invariant, even though `dt*0.05` is. Grouping the invariant
        !! factors into one subtree is what lets them be hoisted, and it costs
        !! nothing when there are none.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last, j

        i = 1
        do while (i <= p%n_stmts)
            if (p%stmts(i)%kind /= FAD_DO) then
                i = i + 1
                cycle
            end if
            call loop_extent(p, i, first, last)
            if (last == 0) then
                i = i + 1
                cycle
            end if
            if (plain_body(p, first, last)) then
                do j = first + 1, last - 1
                    p%stmts(j)%value = regroup(p, first, last, p%stmts(j)%value)
                end do
            end if
            i = last + 1
        end do
    end subroutine regroup_products

    recursive integer function regroup(p, first, last, idx) result(out)
        !! Rebuild an expression with each product's invariant factors grouped.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: first, last, idx
        integer, parameter :: MAX_FACTORS = 32
        integer :: factors(MAX_FACTORS), n_factors
        integer :: i, var_side, inv_side, n_inv
        integer, allocatable :: new_args(:)
        type(fad_expr_t) :: e
        character(len=:), allocatable :: text_here
        integer :: kind_here
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return

        if (p%exprs(idx)%kind == FAD_BINOP) then
            if (trim(p%exprs(idx)%text) == "*") then
                n_factors = 0
                call flatten_product(p, idx, factors, n_factors, MAX_FACTORS)
                if (n_factors > 2 .and. n_factors <= MAX_FACTORS) then
                    n_inv = 0
                    do i = 1, n_factors
                        if (loop_invariant(p, first, last, factors(i))) &
                            n_inv = n_inv + 1
                    end do
                    if (n_inv > 1 .and. n_inv < n_factors) then
                        var_side = 0
                        inv_side = 0
                        do i = 1, n_factors
                            block
                                integer :: side, f
                                f = regroup(p, first, last, factors(i))
                                if (loop_invariant(p, first, last, factors(i))) then
                                    side = inv_side
                                    if (side == 0) then
                                        inv_side = f
                                    else
                                        inv_side = p%add_expr( &
                                            expr_binop("*", side, f))
                                    end if
                                else
                                    side = var_side
                                    if (side == 0) then
                                        var_side = f
                                    else
                                        var_side = p%add_expr( &
                                            expr_binop("*", side, f))
                                    end if
                                end if
                            end block
                        end do
                        out = p%add_expr(expr_binop("*", var_side, inv_side))
                        return
                    end if
                end if
            end if
        end if

        if (.not. allocated(p%exprs(idx)%args)) return
        kind_here = p%exprs(idx)%kind
        text_here = p%exprs(idx)%text
        new_args = p%exprs(idx)%args
        changed = .false.
        do i = 1, size(new_args)
            block
                integer :: sub
                sub = regroup(p, first, last, new_args(i))
                if (sub /= new_args(i)) changed = .true.
                new_args(i) = sub
            end block
        end do
        if (.not. changed) return
        e%kind = kind_here
        e%text = text_here
        e%args = new_args
        out = p%add_expr(e)
    end function regroup

    subroutine hoist_subexpressions(p)
        !! Lift each maximal loop-invariant subexpression into a temporary
        !! computed before the loop.
        !!
        !! Whole-statement hoisting is not enough: after factoring, the
        !! invariant is a coefficient buried inside a statement that is itself
        !! not invariant. Naming it and computing it once is the whole point of
        !! the factoring.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last, j, n_hoisted
        character(len=32) :: label

        n_hoisted = 0
        i = 1
        do while (i <= p%n_stmts)
            if (p%stmts(i)%kind /= FAD_DO) then
                i = i + 1
                cycle
            end if
            call loop_extent(p, i, first, last)
            if (last == 0 .or. .not. plain_body(p, first, last)) then
                i = i + 1
                cycle
            end if
            j = first + 1
            do while (j <= last - 1)
                block
                    integer :: node, decl_i, new_stmt, before
                    type(fad_stmt_t) :: s
                    type(fad_decl_t) :: d
                    node = maximal_invariant(p, first, last, p%stmts(j)%value)
                    if (node <= 0) then
                        j = j + 1
                        cycle
                    end if
                    decl_i = 0
                    if (allocated(p%stmts(j)%target)) &
                        decl_i = p%decl_index(base_of(p%stmts(j)%target))
                    if (decl_i == 0) then
                        j = j + 1
                        cycle
                    end if
                    n_hoisted = n_hoisted + 1
                    write (label, '(a,i0)') "fad_c", n_hoisted
                    d = p%decls(decl_i)
                    d%name = trim(label)
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    d%is_array = .false.
                    if (allocated(d%dims)) deallocate (d%dims)
                    before = p%add_decl(d)

                    call replace_node(p, first, last, node, trim(label))

                    s%kind = FAD_ASSIGN
                    s%target = trim(label)
                    s%value = node
                    call insert_before(p, first, s)
                    first = first + 1
                    last = last + 1
                    j = j + 1
                end block
            end do
            i = last + 1
        end do
    end subroutine hoist_subexpressions

    recursive integer function maximal_invariant(p, first, last, idx) result(node)
        !! The outermost invariant subexpression worth naming, or zero.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last, idx
        integer :: i

        node = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_BINOP, FAD_UNOP, FAD_CALL)
            ! A leaf is already as cheap as a temporary would be.
            if (loop_invariant(p, first, last, idx)) then
                node = idx
                return
            end if
        end select
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            node = maximal_invariant(p, first, last, p%exprs(idx)%args(i))
            if (node > 0) return
        end do
    end function maximal_invariant

    subroutine replace_node(p, first, last, node, name)
        !! Replace every occurrence of an expression by a variable reference.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: first, last, node
        character(len=*), intent(in) :: name
        integer :: j, repl

        repl = p%add_expr(expr_var(name))
        do j = first + 1, last - 1
            if (p%stmts(j)%value <= 0) cycle
            p%stmts(j)%value = swap_node(p, p%stmts(j)%value, node, repl)
        end do
    end subroutine replace_node

    recursive integer function swap_node(p, idx, node, repl) result(out)
        !! Rebuild an expression with one arena node replaced.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx, node, repl
        integer, allocatable :: new_args(:)
        type(fad_expr_t) :: e
        character(len=:), allocatable :: text_here
        integer :: i, kind_here
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (idx == node) then
            out = repl
            return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        kind_here = p%exprs(idx)%kind
        text_here = p%exprs(idx)%text
        new_args = p%exprs(idx)%args
        changed = .false.
        do i = 1, size(new_args)
            block
                integer :: sub
                sub = swap_node(p, new_args(i), node, repl)
                if (sub /= new_args(i)) changed = .true.
                new_args(i) = sub
            end block
        end do
        if (.not. changed) return
        e%kind = kind_here
        e%text = text_here
        e%args = new_args
        out = p%add_expr(e)
    end function swap_node

    subroutine insert_before(p, at, s)
        !! Insert a statement immediately before index `at`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: at
        type(fad_stmt_t), intent(in) :: s
        type(fad_stmt_t), allocatable :: out(:)
        integer :: i, n

        allocate (out(p%n_stmts + 1))
        n = 0
        do i = 1, p%n_stmts
            if (i == at) then
                n = n + 1
                out(n) = s
            end if
            n = n + 1
            out(n) = p%stmts(i)
        end do
        if (size(p%stmts) < n) then
            block
                type(fad_stmt_t), allocatable :: grown(:)
                allocate (grown(2*n))
                grown(1:p%n_stmts) = p%stmts(1:p%n_stmts)
                call move_alloc(grown, p%stmts)
            end block
        end if
        p%stmts(1:n) = out(1:n)
        p%n_stmts = n
    end subroutine insert_before

    subroutine hoist_invariants(p)
        !! Move assignments out of a loop when nothing they read changes in it.
        type(fad_proc_t), intent(inout) :: p
        type(fad_stmt_t), allocatable :: out(:)
        integer :: first, last, i, j, n, pass
        logical :: moved

        do pass = 1, 4
            moved = .false.
            i = 1
            do while (i <= p%n_stmts)
                if (p%stmts(i)%kind /= FAD_DO) then
                    i = i + 1
                    cycle
                end if
                call loop_extent(p, i, first, last)
                if (last == 0) then
                    i = i + 1
                    cycle
                end if
                ! Only a body of plain assignments: with control flow inside,
                ! an assignment may not run every iteration and hoisting it
                ! would make it run once when it should not run at all.
                do j = first + 1, last - 1
                    if (p%stmts(j)%kind /= FAD_ASSIGN) then
                        last = 0
                        exit
                    end if
                end do
                if (last == 0) then
                    i = i + 1
                    cycle
                end if
                do j = first + 1, last - 1
                    if (.not. invariant_here(p, first, last, j)) cycle
                    allocate (out(p%n_stmts))
                    n = 0
                    block
                        integer :: k
                        do k = 1, p%n_stmts
                            if (k == first) then
                                n = n + 1
                                out(n) = p%stmts(j)
                            end if
                            if (k == j) cycle
                            n = n + 1
                            out(n) = p%stmts(k)
                        end do
                    end block
                    p%stmts(1:n) = out(1:n)
                    p%n_stmts = n
                    deallocate (out)
                    moved = .true.
                    exit
                end do
                if (moved) exit
                i = last + 1
            end do
            if (.not. moved) exit
        end do
    end subroutine hoist_invariants

    subroutine loop_extent(p, first, lo, hi)
        !! The statement range of the loop opening at `first`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first
        integer, intent(out) :: lo, hi
        integer :: j, depth

        lo = first
        hi = 0
        depth = 0
        do j = first, p%n_stmts
            select case (p%stmts(j)%kind)
            case (FAD_DO)
                depth = depth + 1
            case (FAD_END_DO)
                depth = depth - 1
                if (depth == 0) then
                    hi = j
                    return
                end if
            end select
        end do
    end subroutine loop_extent

    logical function invariant_here(p, first, last, idx) result(yes)
        !! Whether statement `idx` can be lifted out of the loop `first..last`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last, idx
        character(len=:), allocatable :: name
        integer :: j

        yes = .false.
        if (.not. simple_definition(p, idx)) return
        name = trim(p%stmts(idx)%target)
        if (is_dummy(p, name)) return
        ! Written once, so lifting it cannot lose a later value.
        do j = first + 1, last - 1
            if (j == idx) cycle
            if (assigns_to(p, j, name)) return
        end do
        ! Nothing it reads may change inside the loop, the loop variable
        ! included.
        if (allocated(p%stmts(first)%target)) then
            if (mentions(p, p%stmts(idx)%value, trim(p%stmts(first)%target))) return
        end if
        do j = first + 1, last - 1
            if (j == idx) cycle
            if (p%stmts(j)%kind /= FAD_ASSIGN) return
            if (.not. allocated(p%stmts(j)%target)) return
            if (mentions(p, p%stmts(idx)%value, base_of(p%stmts(j)%target))) return
        end do
        ! It must not be read before its definition, which would mean the first
        ! iteration reads a value from outside the loop.
        do j = first + 1, idx - 1
            if (p%stmts(j)%value > 0) then
                if (mentions(p, p%stmts(j)%value, name)) return
            end if
        end do
        yes = .true.
    end function invariant_here

end module fortad_opt
