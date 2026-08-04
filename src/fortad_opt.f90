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
    integer, parameter :: MAX_EXPR_NODES = 96
    !! The bold attempt needs a far larger budget: collapsing a multi-stage
    !! integrator means substituting every stage into the carried update before
    !! factoring can fold the result back into one coefficient. The intermediate
    !! expression is large even though the final one is tiny.
    integer, parameter :: MAX_EXPR_NODES_BOLD = 2048

    !! Substitution normally refuses a definition read more than once unless it
    !! is a single cheap operation. Sometimes duplicating a larger definition is
    !! what lets factoring fold the whole chain into constants, after which the
    !! duplicates disappear - and sometimes it just duplicates work. Which one
    !! it is cannot be decided in advance, so `optimise` tries both and keeps
    !! the cheaper body. This flag selects the aggressive attempt.
    logical :: aggressive = .false.

contains

    subroutine optimise(p)
        !! Run the passes, twice, and keep the cheaper result.
        type(fad_proc_t), intent(inout) :: p
        type(fad_proc_t) :: bold
        integer :: base_cost, bold_cost

        bold = p
        call run_passes(p, .false.)
        base_cost = loop_cost(p)

        call run_passes(bold, .true.)
        bold_cost = loop_cost(bold)

        ! Ties go to the conservative result: it has fewer duplicated
        ! subexpressions and so fewer chances of a register spill the cost
        ! model cannot see.
        if (bold_cost < base_cost) p = bold
    end subroutine optimise

    subroutine run_passes(p, bold) 
        !! One pass sequence at the given substitution aggressiveness.
        type(fad_proc_t), intent(inout) :: p
        logical, intent(in) :: bold
        integer :: pass

        aggressive = bold
        call propagate_loop_zeros(p)
        ! Renaming the body into single assignment first is what lets
        ! substitution reach an accumulated variable. Without it a stage adjoint
        ! that is written twice is opaque, and a loop body that is wholly linear
        ! in its carried variable never collapses.
        call rename_bodies(p)
        do pass = 1, 4
            call propagate_copies(p)
            call substitute_temps(p)
            call factor_self_update(p)
        end do
        call coalesce_element_updates(p)
        call regroup_products(p)
        call hoist_invariants(p)
        call hoist_subexpressions(p)
        call rotate_carried(p)
        aggressive = .false.
    end subroutine run_passes

    integer function loop_cost(p) result(cost)
        !! Total expression size inside loops, as a stand-in for work done.
        !!
        !! Only loop bodies are counted: a statement outside a loop runs once
        !! and is noise beside one that runs per element. Expression size counts
        !! a duplicated subexpression twice, which is the whole point - it is
        !! what tells the two attempts apart.
        type(fad_proc_t), intent(in) :: p
        integer :: i, depth

        cost = 0
        depth = 0
        do i = 1, p%n_stmts
            select case (p%stmts(i)%kind)
            case (FAD_DO)
                depth = depth + 1
            case (FAD_END_DO)
                depth = depth - 1
            case (FAD_ASSIGN)
                if (depth > 0 .and. p%stmts(i)%value > 0) &
                    cost = cost + expr_size(p, p%stmts(i)%value)
            end select
        end do
    end function loop_cost

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
                if (uses > 1 .and. .not. cheap(p, p%stmts(i)%value) &
                    .and. .not. aggressive) cycle

                do j = i + 1, last_use
                    if (p%stmts(j)%value <= 0) cycle
                    if (count_reads(p, p%stmts(j)%value, lhs) == 0) cycle
                    ! The definition may only be moved to a point where it
                    ! would still compute the same thing. `t = u` followed by a
                    ! write to `u` and then a read of `t` is the case that
                    ! matters: inlining there silently reads the new `u`.
                    if (.not. rhs_stable(p, i, j)) cycle
                    block
                        integer :: new
                        new = replace_var(p, p%stmts(j)%value, lhs, &
                                          p%stmts(i)%value)
                        if (aggressive) then
                            if (expr_size(p, new) > MAX_EXPR_NODES_BOLD) cycle
                        else
                            if (expr_size(p, new) > MAX_EXPR_NODES) cycle
                        end if
                        p%stmts(j)%value = new
                        changed = .true.
                    end block
                end do
            end do
            if (.not. changed) exit
        end do
    end subroutine substitute_temps

    logical function rhs_stable(p, def_idx, use_idx) result(yes)
        !! Whether the definition's operands are unchanged between the two.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: def_idx, use_idx
        integer :: k

        yes = .false.
        do k = def_idx + 1, use_idx - 1
            if (p%stmts(k)%kind /= FAD_ASSIGN) return
            if (.not. allocated(p%stmts(k)%target)) return
            if (mentions(p, p%stmts(def_idx)%value, &
                         base_of(p%stmts(k)%target))) return
        end do
        yes = .true.
    end function rhs_stable

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
            ! The target first, because a self-update is the case that shortens
            ! the loop-carried chain. Then any other variable common to the
            ! terms: after a multi-stage integrator is substituted flat, every
            ! term of the scatter carries the same carried adjoint, and pulling
            ! it out turns a polynomial into one multiply.
            if (factor_around(p, i, trim(p%stmts(i)%target))) cycle
            call factor_by_common(p, i)
        end do
    end subroutine factor_self_update

    subroutine factor_by_common(p, idx)
        !! Factor around whichever variable the terms share.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        integer, parameter :: MAX_TERMS = 64
        integer :: terms(MAX_TERMS), signs(MAX_TERMS), n_terms
        character(len=64) :: cands(32)
        integer :: n_cands, i, k
        logical :: ignored

        n_terms = 0
        call flatten_sum(p, p%stmts(idx)%value, 1, terms, signs, n_terms, MAX_TERMS)
        if (n_terms < 2 .or. n_terms > MAX_TERMS) return

        n_cands = 0
        do i = 1, n_terms
            call collect_factor_names(p, terms(i), cands, n_cands)
        end do
        do k = 1, n_cands
            if (trim(cands(k)) == trim(p%stmts(idx)%target)) cycle
            ignored = factor_around(p, idx, trim(cands(k)))
            if (ignored) return
        end do
    end subroutine factor_by_common

    subroutine collect_factor_names(p, idx, names, n)
        !! Record the plain variables multiplied together in one term.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=64), intent(inout) :: names(:)
        integer, intent(inout) :: n
        integer, parameter :: MAX_FACTORS = 32
        integer :: factors(MAX_FACTORS), n_factors, i, k
        logical :: seen

        n_factors = 0
        call flatten_product(p, idx, factors, n_factors, MAX_FACTORS)
        do i = 1, min(n_factors, MAX_FACTORS)
            if (p%exprs(factors(i))%kind /= FAD_VAR) cycle
            seen = .false.
            do k = 1, n
                if (trim(names(k)) == trim(p%exprs(factors(i))%text)) seen = .true.
            end do
            if (seen .or. n >= size(names)) cycle
            n = n + 1
            names(n) = trim(p%exprs(factors(i))%text)
        end do
    end subroutine collect_factor_names

    logical function factor_around(p, idx, name) result(changed)
        !! Factor the right-hand side of statement `idx` around `name`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer, parameter :: MAX_TERMS = 64
        integer :: terms(MAX_TERMS), signs(MAX_TERMS), n_terms
        integer :: coeffs(MAX_TERMS), coeff_signs(MAX_TERMS), n_coeffs
        integer :: rest(MAX_TERMS), rest_signs(MAX_TERMS), n_rest
        integer :: i, coeff, acc, value

        changed = .false.
        n_terms = 0
        call flatten_sum(p, p%stmts(idx)%value, 1, terms, signs, n_terms, MAX_TERMS)
        if (n_terms < 2 .or. n_terms > MAX_TERMS) return

        n_coeffs = 0
        n_rest = 0
        do i = 1, n_terms
            coeff = coefficient_of(p, terms(i), name)
            if (coeff == -1) return          ! it appears in a shape we cannot factor
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
        changed = .true.
    end function factor_around

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

    subroutine rename_bodies(p)
        !! Rewrite each loop body so every scalar is assigned at most once.
        !!
        !! A reverse sweep accumulates onto a stage adjoint several times in one
        !! iteration. Substitution refuses to look through that, because the
        !! name means different things at different points. Giving each write
        !! its own name removes the ambiguity without changing what the body
        !! computes, and the value carried to the next iteration is copied back
        !! under the original name at the end.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last

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
            call rename_one_body(p, first, last)
            call loop_extent(p, first, first, last)
            i = last + 1
        end do
    end subroutine rename_bodies

    subroutine rename_one_body(p, first, last)
        !! Rename multiply-assigned scalars in one loop body.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(inout) :: first, last
        character(len=64) :: names(64)
        integer :: counts(64), n_names
        integer :: j, k

        n_names = 0
        do j = first + 1, last - 1
            if (p%stmts(j)%kind /= FAD_ASSIGN) cycle
            if (.not. allocated(p%stmts(j)%target)) cycle
            if (index(p%stmts(j)%target, "(") > 0) cycle
            if (is_dummy(p, trim(p%stmts(j)%target))) cycle
            call bump(names, counts, n_names, trim(p%stmts(j)%target))
        end do

        do k = 1, n_names
            if (counts(k) < 2) cycle
            call rename_name(p, first, last, trim(names(k)))
            call loop_extent(p, first, first, last)
        end do
    end subroutine rename_one_body

    subroutine bump(names, counts, n, name)
        !! Count one occurrence of a name.
        character(len=64), intent(inout) :: names(:)
        integer, intent(inout) :: counts(:), n
        character(len=*), intent(in) :: name
        integer :: i

        do i = 1, n
            if (trim(names(i)) == name) then
                counts(i) = counts(i) + 1
                return
            end if
        end do
        if (n >= size(names)) return
        n = n + 1
        names(n) = name
        counts(n) = 1
    end subroutine bump

    subroutine rename_name(p, first, last, name)
        !! Give each write of `name` in the body its own name.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: first
        integer, intent(inout) :: last
        character(len=*), intent(in) :: name
        character(len=64) :: current
        character(len=32) :: suffix
        type(fad_decl_t) :: d
        type(fad_stmt_t) :: s
        integer :: j, version, di, ignored, repl

        di = p%decl_index(name)
        if (di == 0) return
        current = name
        version = 0
        do j = first + 1, last - 1
            if (p%stmts(j)%kind /= FAD_ASSIGN) cycle
            ! Reads see the version in force at this point.
            if (p%stmts(j)%value > 0 .and. trim(current) /= name) then
                repl = p%add_expr(expr_var(trim(current)))
                p%stmts(j)%value = replace_var(p, p%stmts(j)%value, name, repl)
            end if
            if (.not. allocated(p%stmts(j)%target)) cycle
            if (trim(p%stmts(j)%target) /= name) cycle
            version = version + 1
            write (suffix, '(a,i0)') "__", version
            current = name//trim(suffix)
            d = p%decls(di)
            d%name = trim(current)
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            ignored = p%add_decl(d)
            p%stmts(j)%target = trim(current)
        end do
        if (version == 0 .or. trim(current) == name) return

        ! Carry the last version back under the original name, for the next
        ! iteration and for anything after the loop.
        s%kind = FAD_ASSIGN
        s%target = name
        s%value = p%add_expr(expr_var(trim(current)))
        call insert_before(p, last, s)
        last = last + 1
    end subroutine rename_name

    subroutine propagate_loop_zeros(p)
        !! Turn the first accumulation onto a per-iteration adjoint into a
        !! plain assignment.
        !!
        !! A reverse sweep gives each intermediate its own adjoint, clears it at
        !! the end of the iteration, and accumulates onto it. The clear makes
        !! the variable provably zero at the top of every iteration, so the
        !! first `v = v + e` is just `v = e` - and once nothing reads the old
        !! value, the clear itself is dead and dead-store elimination removes
        !! it. On a four-stage Runge-Kutta that is four adds and four stores an
        !! iteration.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last, j

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
                    integer :: drop
                    call zero_start(p, first, last, j, drop)
                    if (drop > 0) then
                        call remove_stmt(p, drop)
                        call loop_extent(p, first, first, last)
                    end if
                end block
                j = j + 1
            end do
            i = last + 1
        end do
    end subroutine propagate_loop_zeros

    subroutine zero_start(p, first, last, idx, drop)
        !! Rewrite `v = v + e` at `idx` when `v` is zero at every iteration.
        !!
        !! `drop` returns the index of a clear that the rewrite made dead, or
        !! zero. Dead-store elimination will not find it on its own: the read it
        !! is protecting is the accumulation earlier in the body, which belongs
        !! to the *next* iteration, and that pass does not model iterations.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: first, last, idx
        integer, intent(out) :: drop
        character(len=:), allocatable :: name
        integer :: k, v, last_write

        drop = 0

        if (p%stmts(idx)%kind /= FAD_ASSIGN) return
        if (.not. allocated(p%stmts(idx)%target)) return
        if (index(p%stmts(idx)%target, "(") > 0) return
        name = trim(p%stmts(idx)%target)
        if (is_dummy(p, name)) return
        if (.not. accumulates_onto(p, idx, name)) return

        ! It must be the first thing in the body to touch the name at all.
        do k = first + 1, idx - 1
            if (assigns_to(p, k, name)) return
            if (p%stmts(k)%value > 0) then
                if (mentions(p, p%stmts(k)%value, name)) return
            end if
        end do

        ! The body must end by clearing it, which is what makes it zero on the
        ! next iteration, and it must be zero before the first iteration too.
        last_write = 0
        do k = idx + 1, last - 1
            if (assigns_to(p, k, name)) last_write = k
        end do
        if (last_write == 0) return
        if (.not. assigns_zero(p, last_write)) return
        if (.not. zero_before_loop(p, first, name)) return

        v = p%stmts(idx)%value
        p%stmts(idx)%value = p%exprs(v)%args(2)

        ! The clear is now dead if nothing between it and the end of the body
        ! reads the name, and nothing after the loop does either.
        do k = last_write + 1, last - 1
            if (p%stmts(k)%value > 0) then
                if (mentions(p, p%stmts(k)%value, name)) return
            end if
            if (allocated(p%stmts(k)%target)) then
                if (name_in_text(p%stmts(k)%target, name) .and. &
                    base_of(p%stmts(k)%target) /= name) return
            end if
        end do
        do k = last + 1, p%n_stmts
            if (p%stmts(k)%value > 0) then
                if (mentions(p, p%stmts(k)%value, name)) return
            end if
            if (allocated(p%stmts(k)%target)) then
                if (name_in_text(p%stmts(k)%target, name)) return
            end if
        end do
        drop = last_write
    end subroutine zero_start

    subroutine remove_stmt(p, at)
        !! Delete one statement.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: at
        integer :: i

        do i = at, p%n_stmts - 1
            p%stmts(i) = p%stmts(i + 1)
        end do
        p%n_stmts = p%n_stmts - 1
    end subroutine remove_stmt

    logical function assigns_zero(p, idx) result(yes)
        !! Whether a statement assigns a literal zero.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: v

        yes = .false.
        v = p%stmts(idx)%value
        if (v <= 0 .or. v > p%n_exprs) return
        if (p%exprs(v)%kind /= FAD_CONST) return
        if (.not. allocated(p%exprs(v)%text)) return
        yes = is_zero_text(p%exprs(v)%text)
    end function assigns_zero

    logical function is_zero_text(text) result(yes)
        !! Whether a literal is zero, in any of the spellings fortad emits.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s
        integer :: e

        s = trim(adjustl(text))
        e = scan(s, "dDeE")
        if (e > 1) s = s(1:e - 1)
        yes = .false.
        if (len(s) == 0) return
        yes = verify(s, "0.+-") == 0 .and. verify(s, "0") /= 0 .or. s == "0"
        if (.not. yes) yes = verify(s, "0.") == 0
    end function is_zero_text

    logical function zero_before_loop(p, first, name) result(yes)
        !! Whether the last assignment to `name` before the loop is zero.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first
        character(len=*), intent(in) :: name
        integer :: k, last_write

        last_write = 0
        do k = 1, first - 1
            if (assigns_to(p, k, name)) last_write = k
        end do
        yes = .false.
        if (last_write == 0) return
        yes = assigns_zero(p, last_write)
    end function zero_before_loop

    subroutine coalesce_element_updates(p)
        !! Accumulate repeated updates of one array element in a scalar.
        !!
        !! A reverse sweep scatters into `z_b(i)` once per use of `z(i)` in the
        !! primal, and each of those is a load and a store of the same address.
        !! The compiler will not merge them: `z_b` is a dummy array and every
        !! store may alias. Naming the element and writing it back once turns
        !! `k` load-store pairs into one.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last
        integer :: n_named

        n_named = 0
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
            call coalesce_in_body(p, first, last, n_named)
            call loop_extent(p, first, first, last)
            i = last + 1
        end do
    end subroutine coalesce_element_updates

    subroutine coalesce_in_body(p, first, last, n_named)
        !! Coalesce one element target per call, repeating until none is left.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(inout) :: first, last, n_named
        character(len=:), allocatable :: target_text, base
        integer :: j, k, n_hits, hit_first, hit_last
        character(len=32) :: label
        logical :: again

        again = .true.
        do while (again)
            again = .false.
            do j = first + 1, last - 1
                if (p%stmts(j)%kind /= FAD_ASSIGN) cycle
                if (.not. allocated(p%stmts(j)%target)) cycle
                if (index(p%stmts(j)%target, "(") == 0) cycle
                target_text = trim(p%stmts(j)%target)
                base = base_of(target_text)
                if (index(target_text, "fad_e") == 1) cycle

                ! Count identical targets, and require every one of them to be
                ! an accumulation onto itself: anything else and the element's
                ! value at that point is not simply the running sum.
                n_hits = 0
                hit_first = 0
                hit_last = 0
                do k = first + 1, last - 1
                    if (.not. allocated(p%stmts(k)%target)) cycle
                    if (trim(p%stmts(k)%target) /= target_text) cycle
                    if (.not. accumulates_onto(p, k, target_text)) then
                        n_hits = 0
                        exit
                    end if
                    n_hits = n_hits + 1
                    if (hit_first == 0) hit_first = k
                    hit_last = k
                end do
                if (n_hits < 2) cycle

                ! No other element of the same array may be touched, and the
                ! element must not be read outside its own accumulations.
                if (other_use(p, first, last, base, target_text)) cycle

                n_named = n_named + 1
                write (label, '(a,i0)') "fad_e", n_named
                call rewrite_element(p, first, last, target_text, trim(label), &
                                     hit_first, hit_last, base)
                again = .true.
                exit
            end do
            if (again) call loop_extent(p, first, first, last)
        end do
    end subroutine coalesce_in_body

    logical function accumulates_onto(p, idx, target_text) result(yes)
        !! Whether a statement is `t = t + something`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: target_text
        integer :: v

        yes = .false.
        v = p%stmts(idx)%value
        if (v <= 0 .or. v > p%n_exprs) return
        if (p%exprs(v)%kind /= FAD_BINOP) return
        if (trim(p%exprs(v)%text) /= "+") return
        if (.not. allocated(p%exprs(v)%args)) return
        yes = is_named(p, p%exprs(v)%args(1), target_text) .and. &
              .not. mentions(p, p%exprs(v)%args(2), target_text)
    end function accumulates_onto

    logical function is_named(p, idx, text) result(yes)
        !! Whether an expression is exactly the given variable or element.
        !!
        !! An array element is an `FAD_INDEX` node whose text is only the array
        !! name, so it is compared by what it renders to. That is the same
        !! string the assignment target carries, which is what makes the two
        !! sides of `z_b(i) = z_b(i) + ...` recognisable as the same place.
        use fortad_emit, only: emit_expr
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: text

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (.not. allocated(p%exprs(idx)%text)) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR)
            yes = trim(p%exprs(idx)%text) == text
        case (FAD_INDEX)
            yes = emit_expr(p, idx) == text
        end select
    end function is_named

    logical function other_use(p, first, last, base, target_text) result(yes)
        !! Whether the array is touched other than through this one element.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: base, target_text
        integer :: k

        yes = .true.
        do k = first + 1, last - 1
            if (allocated(p%stmts(k)%target)) then
                if (name_in_text(p%stmts(k)%target, base) .and. &
                    trim(p%stmts(k)%target) /= target_text) return
            end if
            if (p%stmts(k)%value <= 0) cycle
            if (.not. mentions(p, p%stmts(k)%value, base)) cycle
            ! The only permitted read is the left operand of its own
            ! accumulation, which `accumulates_onto` already vouched for.
            if (.not. allocated(p%stmts(k)%target)) return
            if (trim(p%stmts(k)%target) /= target_text) return
            if (mentions(p, p%exprs(p%stmts(k)%value)%args(2), base)) return
        end do
        yes = .false.
    end function other_use

    subroutine rewrite_element(p, first, last, target_text, label, &
                               hit_first, hit_last, base)
        !! Replace the element by a scalar, loaded once and stored once.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: first, last, hit_first, hit_last
        character(len=*), intent(in) :: target_text, label, base
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        integer :: k, di, ignored, repl

        di = p%decl_index(base)
        if (di == 0) return
        d = p%decls(di)
        d%name = label
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_array = .false.
        d%is_contiguous = .false.
        if (allocated(d%dims)) deallocate (d%dims)
        ignored = p%add_decl(d)

        repl = p%add_expr(expr_var(label))
        do k = hit_first, hit_last
            if (.not. allocated(p%stmts(k)%target)) cycle
            if (trim(p%stmts(k)%target) /= target_text) cycle
            p%stmts(k)%target = label
            p%stmts(k)%value = swap_named(p, p%stmts(k)%value, target_text, repl)
        end do

        ! Store after the last accumulation, then load before the first, so the
        ! second insertion does not shift the index of the first.
        s%kind = FAD_ASSIGN
        s%target = target_text
        s%value = repl
        call insert_before(p, hit_last + 1, s)

        s%kind = FAD_ASSIGN
        s%target = label
        s%value = p%add_expr(expr_var(target_text))
        call insert_before(p, hit_first, s)
    end subroutine rewrite_element

    recursive integer function swap_named(p, idx, text, repl) result(out)
        !! Rebuild an expression with reads of a named entity replaced.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx, repl
        character(len=*), intent(in) :: text
        integer, allocatable :: new_args(:)
        type(fad_expr_t) :: e
        character(len=:), allocatable :: text_here
        integer :: i, kind_here
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (is_named(p, idx, text)) then
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
                sub = swap_named(p, new_args(i), text, repl)
                if (sub /= new_args(i)) changed = .true.
                new_args(i) = sub
            end block
        end do
        if (.not. changed) return
        e%kind = kind_here
        e%text = text_here
        e%args = new_args
        out = p%add_expr(e)
    end function swap_named

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

    subroutine rotate_carried(p)
        !! Issue a loop-carried self-update first, behind a snapshot.
        !!
        !! The loop-carried dependence is the critical path: every iteration
        !! waits for the previous one's `state_b`. Anything scheduled before the
        !! update lengthens that wait, and a scatter into an array is the usual
        !! offender. Snapshotting the incoming value and updating immediately
        !! costs one register move and lets the scatter overlap with the next
        !! iteration.
        !!
        !! This runs last, after the passes that shorten the update itself.
        !! Running it earlier would only give them a snapshot to see through.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last, n_snap

        n_snap = 0
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
            call rotate_one(p, first, last, n_snap)
            call loop_extent(p, first, first, last)
            i = last + 1
        end do
    end subroutine rotate_carried

    subroutine rotate_one(p, first, last, n_snap)
        !! Rotate one self-update to the top of a loop body.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(inout) :: first, last, n_snap
        character(len=:), allocatable :: name
        character(len=32) :: label
        type(fad_stmt_t) :: s, moved
        type(fad_decl_t) :: d
        integer :: j, k, di, ignored, repl, target_stmt

        target_stmt = 0
        do j = first + 2, last - 1
            if (p%stmts(j)%kind /= FAD_ASSIGN) cycle
            if (.not. allocated(p%stmts(j)%target)) cycle
            if (index(p%stmts(j)%target, "(") > 0) cycle
            name = trim(p%stmts(j)%target)
            if (is_dummy(p, name)) cycle
            if (p%stmts(j)%value <= 0) cycle
            if (count_reads(p, p%stmts(j)%value, name) == 0) cycle
            ! Written once, so moving it cannot reorder two updates.
            if (count_writes(p, first, last, name) /= 1) cycle
            ! Everything else it reads must be invariant, or it cannot run
            ! before the statements it is being moved ahead of.
            if (.not. ready_at_top(p, first, last, j, name)) cycle
            target_stmt = j
            exit
        end do
        if (target_stmt == 0) return

        di = p%decl_index(name)
        if (di == 0) return
        n_snap = n_snap + 1
        write (label, '(a,i0)') "fad_r", n_snap
        d = p%decls(di)
        d%name = trim(label)
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_array = .false.
        if (allocated(d%dims)) deallocate (d%dims)
        ignored = p%add_decl(d)

        ! Reads of the incoming value, in the update itself and in everything
        ! it is moving ahead of, become reads of the snapshot.
        repl = p%add_expr(expr_var(trim(label)))
        do k = first + 1, target_stmt
            if (p%stmts(k)%value <= 0) cycle
            p%stmts(k)%value = replace_var(p, p%stmts(k)%value, name, repl)
        end do

        moved = p%stmts(target_stmt)
        do k = target_stmt, first + 2, -1
            p%stmts(k) = p%stmts(k - 1)
        end do
        p%stmts(first + 1) = moved

        s%kind = FAD_ASSIGN
        s%target = trim(label)
        s%value = p%add_expr(expr_var(name))
        call insert_before(p, first + 1, s)
        last = last + 1
    end subroutine rotate_one

    integer function count_writes(p, first, last, name) result(n)
        !! How many statements in the body write `name`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: name
        integer :: j

        n = 0
        do j = first + 1, last - 1
            if (assigns_to(p, j, name)) n = n + 1
        end do
    end function count_writes

    logical function ready_at_top(p, first, last, idx, name) result(yes)
        !! Whether the update reads nothing the loop body produces but `name`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last, idx
        character(len=*), intent(in) :: name
        integer :: j

        yes = .false.
        if (allocated(p%stmts(first)%target)) then
            if (mentions(p, p%stmts(idx)%value, trim(p%stmts(first)%target))) return
        end if
        do j = first + 1, last - 1
            if (j == idx) cycle
            if (p%stmts(j)%kind /= FAD_ASSIGN) return
            if (.not. allocated(p%stmts(j)%target)) return
            if (base_of(p%stmts(j)%target) == name) cycle
            if (mentions(p, p%stmts(idx)%value, base_of(p%stmts(j)%target))) return
        end do
        yes = .true.
    end function ready_at_top

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
