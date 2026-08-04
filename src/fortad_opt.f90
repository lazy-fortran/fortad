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
    use fortad_affine, only: collapse_affine_loops
    use fortad_affine, only: collapse_affine_loops
    use fortad_dce, only: eliminate_dead_stores
    use fortad_kinds, only: dp
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

    !! Rough relative costs, in multiplies. A divide is around four times a
    !! multiply on current hardware and a transcendental an order more; the
    !! numbers only have to order the candidates, not predict a cycle count.
    integer, parameter :: DIVIDE_COST = 4
    integer, parameter :: CALL_COST = 10

    !! Names the span table holds. A procedure with more distinct names than
    !! this loses the shortcut and substitution declines rather than guessing.
    integer, parameter :: MAX_SPAN = 4096

    type :: span_t
        !! The first and last statement mentioning each name.
        character(len=64) :: names(MAX_SPAN) = ""
        integer :: first_at(MAX_SPAN) = 0
        integer :: last_at(MAX_SPAN) = 0
        integer :: n = 0
    end type span_t

contains

    subroutine optimise(p)
        !! Run the passes once, in order.
        !!
        !! One sequence, not a search. An earlier version rewrote each loop body
        !! two ways - one conservative, one substituting and distributing
        !! everything - and kept whichever measured smaller. That is a search
        !! over an expression graph, so its cost is exponential in the body: 27
        !! seconds on a quartic Bezier edge area, and non-terminating on a
        !! quintic Lagrange weight. What it was searching for is decided
        !! directly by `collapse_affine_loops`, in one pass, and the three
        !! bounds that had been put on the search went with it.
        type(fad_proc_t), intent(inout) :: p
        integer :: pass

        call propagate_loop_zeros(p)
        ! Coalescing before renaming, so that a scatter built as a chain of
        ! accumulations onto an array element becomes a chain onto a scalar.
        ! Renaming then puts that chain into single assignment, substitution
        ! flattens it into one expression, and factoring can pull the carried
        ! adjoint out of it. Coalescing afterwards leaves the chain opaque.
        call coalesce_element_updates(p)
        ! Renaming the body into single assignment is what lets substitution
        ! reach an accumulated variable. Without it a stage adjoint that is
        ! written twice is opaque.
        call rename_bodies(p)
        do pass = 1, 4
            call propagate_copies(p)
            call substitute_temps(p)
            call factor_self_update(p)
        end do
        ! With the body in single assignment and its copies propagated, the
        ! affine analysis has the clearest view of it.
        call collapse_affine_loops(p)
        call reciprocate_divisions(p)
        call regroup_products(p)
        call hoist_invariants(p)
        call hoist_subexpressions(p)
        call share_subexpressions(p)
        call rotate_carried(p)
        call fold_identities(p)
        call fold_negations(p)
        ! Substitution and factoring leave their inputs behind.
        call eliminate_dead_stores(p)
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

    recursive integer function op_count(p, idx) result(n)
        !! What an expression tree costs to evaluate.
        !!
        !! Not a count of operations: a divide is several times a multiply and
        !! a transcendental is more again, and treating them alike made
        !! subexpression sharing skip the one case worth it most. A reciprocal
        !! `1/d` is a single node, so an operation count said it was not worth
        !! naming, and every use of it emitted its own divide - fortfem's
        !! quintic Lagrange adjoint went to eighty of them.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: i

        n = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_BINOP)
            if (trim(p%exprs(idx)%text) == "/") then
                n = DIVIDE_COST
            else if (trim(p%exprs(idx)%text) == "**") then
                n = CALL_COST
            else
                n = 1
            end if
        case (FAD_UNOP)
            n = 1
        case (FAD_CALL)
            n = CALL_COST
        end select
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            n = n + op_count(p, p%exprs(idx)%args(i))
        end do
    end function op_count

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
        type(span_t) :: span
        integer :: i, j, uses, last_use, stop_at
        logical :: changed, blocked

        do while (.true.)
            changed = .false.
            ! Rebuilt each round, because the previous round rewrote statements.
            call build_span(p, span)
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
                if (escapes(p, lhs, i, stop_at, span)) cycle
                if (uses > 1 .and. .not. cheap(p, p%stmts(i)%value)) cycle

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
                        if (expr_size(p, new) > MAX_EXPR_NODES) cycle
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

    logical function escapes(p, name, first, stop_at, span) result(yes)
        !! Whether `name` is mentioned outside the window it was tracked in.
        !!
        !! Substitution is only sound if every read of the definition was seen.
        !! Reads before the definition belong to an earlier value, and reads
        !! after the window may see this one through a path this pass does not
        !! model, so either rules it out.
        !!
        !! Answered from a precomputed span - the first and last statement that
        !! mentions each name - rather than by scanning the procedure. Scanning
        !! made substitution quadratic in the number of statements, which is
        !! unnoticeable on a thirty-statement kernel and is most of the time
        !! spent on a four-thousand-statement one.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: name
        integer, intent(in) :: first, stop_at
        type(span_t), intent(in) :: span
        integer :: k

        yes = .true.
        k = span_index(span, name)
        if (k == 0) then
            ! Mentioned nowhere at all, so nothing outside the window either.
            yes = .false.
            return
        end if
        if (span%first_at(k) < first) return
        if (span%last_at(k) >= stop_at) return
        yes = .false.
    end function escapes

    subroutine build_span(p, span)
        !! The first and last statement mentioning each name in the procedure.
        type(fad_proc_t), intent(in) :: p
        type(span_t), intent(out) :: span
        character(len=64) :: names(MAX_SPAN)
        integer :: j, i, n

        n = 0
        do j = 1, p%n_stmts
            if (p%stmts(j)%value > 0) call span_names(p, p%stmts(j)%value, &
                                                     names, n, span, j)
            if (p%stmts(j)%lo > 0) call span_names(p, p%stmts(j)%lo, names, n, &
                                                   span, j)
            if (p%stmts(j)%hi > 0) call span_names(p, p%stmts(j)%hi, names, n, &
                                                   span, j)
            if (allocated(p%stmts(j)%target)) &
                call note_span(span, names, n, trim(p%stmts(j)%target), j)
        end do
        span%n = n
        do i = 1, n
            span%names(i) = names(i)
        end do
    end subroutine build_span

    recursive subroutine span_names(p, idx, names, n, span, at)
        !! Record every name an expression mentions as seen at statement `at`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, at
        character(len=64), intent(inout) :: names(:)
        integer, intent(inout) :: n
        type(span_t), intent(inout) :: span
        integer :: i

        if (idx <= 0 .or. idx > p%n_exprs) return
        if (allocated(p%exprs(idx)%text)) then
            select case (p%exprs(idx)%kind)
            case (FAD_VAR, FAD_INDEX)
                call note_span(span, names, n, trim(p%exprs(idx)%text), at)
            end select
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            call span_names(p, p%exprs(idx)%args(i), names, n, span, at)
        end do
    end subroutine span_names

    subroutine note_span(span, names, n, text, at)
        !! Widen a name's span, and every name whose text this one contains.
        !!
        !! `z_b(i)` mentions `i` and `z_b`, and a target carries its subscript
        !! in its text, so the containment test is what the scanning version
        !! did with `name_in_text`.
        type(span_t), intent(inout) :: span
        character(len=64), intent(inout) :: names(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: text
        integer, intent(in) :: at
        character(len=:), allocatable :: base
        integer :: i, par

        base = trim(text)
        par = index(base, "(")
        if (par > 1) base = base(1:par - 1)
        do i = 1, n
            if (trim(names(i)) /= base) cycle
            span%first_at(i) = min(span%first_at(i), at)
            span%last_at(i) = max(span%last_at(i), at)
            return
        end do
        if (n >= MAX_SPAN) return
        n = n + 1
        names(n) = base
        span%first_at(n) = at
        span%last_at(n) = at
    end subroutine note_span

    integer function span_index(span, name) result(k)
        !! Where a name sits in the span table, or zero.
        type(span_t), intent(in) :: span
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: base
        integer :: i, par

        base = trim(name)
        par = index(base, "(")
        if (par > 1) base = base(1:par - 1)
        k = 0
        do i = 1, span%n
            if (trim(span%names(i)) == base) then
                k = i
                return
            end if
        end do
    end function span_index

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
        !!
        !! Asked statement by statement. Inverting it - collecting the names the
        !! body writes and asking the expression whether it reads any of them -
        !! looks like the better shape and is not: the body writes as many names
        !! as it has statements, so the inner loop moves rather than
        !! disappears, and it measured three and a half times slower on
        !! fortfem's degree-eleven Bezier.
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
            ! A subscript in the assignment *target* is a read too. It lives in
            ! the target's text rather than in the expression arena, so
            ! `replace_var` never sees it. Missing this renamed every load and
            ! left every store indexed by the original name, which by then held
            ! nothing: `z_b(base + 1) = z_b(base__2 + 1) + ...` with `base`
            ! never assigned.
            if (allocated(p%stmts(j)%target) .and. trim(current) /= name) then
                if (index(p%stmts(j)%target, "(") > 0) then
                    if (name_in_text(subscript_of(p%stmts(j)%target), name)) then
                        p%stmts(j)%target = rename_in_text(p%stmts(j)%target, &
                                                           name, trim(current))
                    end if
                end if
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

    function subscript_of(target) result(text)
        !! The subscript part of an assignment target, or an empty string.
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: text
        integer :: pos

        pos = index(target, "(")
        if (pos > 0) then
            text = target(pos:)
        else
            text = ""
        end if
    end function subscript_of

    function rename_in_text(target, name, replacement) result(text)
        !! Replace whole-identifier occurrences of `name` in a target's
        !! subscript, leaving the array name itself alone.
        character(len=*), intent(in) :: target, name, replacement
        character(len=:), allocatable :: text, head
        integer :: pos, from, l

        pos = index(target, "(")
        if (pos == 0) then
            text = target
            return
        end if
        head = target(1:pos - 1)
        text = target(pos:)
        l = len_trim(name)
        from = 1
        do
            pos = index(text(from:), trim(name))
            if (pos == 0) exit
            pos = pos + from - 1
            if (boundary(text, pos - 1) .and. boundary(text, pos + l)) then
                text = text(1:pos - 1)//trim(replacement)//text(pos + l:)
                from = pos + len_trim(replacement)
            else
                from = pos + 1
            end if
            if (from > len(text)) exit
        end do
        text = head//text
    end function rename_in_text

    subroutine fold_negations(p)
        !! Move a negated term's sign into the sum around it.
        !!
        !! The rule table produces negations honestly - the derivative of a
        !! subtraction is a subtraction, the derivative of `cos` is `-sin` - and
        !! they arrive as `(-a)*b + c`, which is a negate, a multiply and an
        !! add. Written as `c - a*b` it is a multiply and a subtract. The
        !! polygon edge area's tangent carries two of them.
        !!
        !! A Fortran compiler will not do this either: for a signed zero
        !! `(-a)*b + c` and `c - a*b` differ, and it has no licence to decide
        !! that does not matter. For a derivative it does not.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, pass
        logical :: changed

        do pass = 1, 3
            changed = .false.
            do i = 1, p%n_stmts
                if (p%stmts(i)%value <= 0) cycle
                block
                    integer :: new
                    new = fold_negations_in(p, p%stmts(i)%value)
                    if (new /= p%stmts(i)%value) then
                        p%stmts(i)%value = new
                        changed = .true.
                    end if
                end block
            end do
            if (.not. changed) exit
        end do
    end subroutine fold_negations

    recursive integer function fold_negations_in(p, idx) result(out)
        !! Fold negations in every sum in an expression, not only the outermost.
        !!
        !! The sum that carries them is usually nested: the polygon edge area's
        !! tangent is a sum of four products scaled by a half, so the whole
        !! statement is a product and only its left operand is a sum.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        integer, allocatable :: new_args(:)
        type(fad_expr_t) :: e
        character(len=:), allocatable :: text_here
        integer :: i, kind_here, folded
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return

        if (allocated(p%exprs(idx)%args)) then
            kind_here = p%exprs(idx)%kind
            text_here = p%exprs(idx)%text
            new_args = p%exprs(idx)%args
            changed = .false.
            do i = 1, size(new_args)
                block
                    integer :: sub
                    sub = fold_negations_in(p, new_args(i))
                    if (sub /= new_args(i)) changed = .true.
                    new_args(i) = sub
                end block
            end do
            if (changed) then
                e%kind = kind_here
                e%text = text_here
                e%args = new_args
                out = p%add_expr(e)
            end if
        end if

        folded = signed_sum(p, out)
        if (folded > 0) out = folded
    end function fold_negations_in

    integer function signed_sum(p, idx) result(out)
        !! Rebuild a sum with each term's negations pulled into its sign.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        integer, parameter :: MAX_TERMS = 64
        integer :: terms(MAX_TERMS), signs(MAX_TERMS), n_terms
        integer :: i, sign_here, bare, acc
        logical :: any_change

        out = 0
        n_terms = 0
        call flatten_sum(p, idx, 1, terms, signs, n_terms, MAX_TERMS)
        if (n_terms < 2 .or. n_terms > MAX_TERMS) return

        any_change = .false.
        do i = 1, n_terms
            call strip_negation(p, terms(i), bare, sign_here)
            if (sign_here < 0) then
                terms(i) = bare
                signs(i) = -signs(i)
                any_change = .true.
            end if
        end do
        if (.not. any_change) return
        ! The sum has to start with a term that is added. Terms commute, so a
        ! positive one is brought to the front; if there is none the sum is
        ! negative throughout and folding would only move the minus sign.
        if (signs(1) < 0) then
            sign_here = 0
            do i = 2, n_terms
                if (signs(i) > 0) then
                    sign_here = i
                    exit
                end if
            end do
            if (sign_here == 0) return
            bare = terms(1)
            terms(1) = terms(sign_here)
            terms(sign_here) = bare
            signs(sign_here) = signs(1)
            signs(1) = 1
        end if

        acc = terms(1)
        do i = 2, n_terms
            block
                integer :: lhs_idx
                lhs_idx = acc
                if (signs(i) > 0) then
                    acc = p%add_expr(expr_binop("+", lhs_idx, terms(i)))
                else
                    acc = p%add_expr(expr_binop("-", lhs_idx, terms(i)))
                end if
            end block
        end do
        out = acc
    end function signed_sum

    subroutine strip_negation(p, idx, bare, sign_out)
        !! A product term without its negations, and their combined sign.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        integer, intent(out) :: bare, sign_out
        integer, parameter :: MAX_FACTORS = 32
        integer :: factors(MAX_FACTORS), n_factors, i, inner

        bare = idx
        sign_out = 1
        n_factors = 0
        call flatten_product(p, idx, factors, n_factors, MAX_FACTORS)
        if (n_factors == 0 .or. n_factors > MAX_FACTORS) return

        do i = 1, n_factors
            if (p%exprs(factors(i))%kind /= FAD_UNOP) cycle
            if (trim(p%exprs(factors(i))%text) /= "-") cycle
            factors(i) = p%exprs(factors(i))%args(1)
            sign_out = -sign_out
        end do
        if (sign_out > 0) return

        bare = factors(1)
        do i = 2, n_factors
            inner = bare
            bare = p%add_expr(expr_binop("*", inner, factors(i)))
        end do
    end subroutine strip_negation

    subroutine fold_identities(p)
        !! Remove arithmetic that does nothing: `x + 0`, `x*1`, `x**1`, `0 - x`.
        !!
        !! The rule table produces these honestly - the derivative of a node
        !! against a constant is a zero or a one, and the builder writes what
        !! the rule says. They survive to the emitted source because a Fortran
        !! compiler will not fold `x - 0.0` either: for signed zero and for a
        !! NaN it is not the identity. For a derivative it is, and the
        !! difference is not observable in the answer.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, pass
        logical :: changed

        do pass = 1, 4
            changed = .false.
            do i = 1, p%n_stmts
                if (p%stmts(i)%value > 0) then
                    block
                        integer :: new
                        new = fold(p, p%stmts(i)%value)
                        if (new /= p%stmts(i)%value) then
                            p%stmts(i)%value = new
                            changed = .true.
                        end if
                    end block
                end if
            end do
            if (.not. changed) exit
        end do
    end subroutine fold_identities

    recursive integer function fold(p, idx) result(out)
        !! Rebuild an expression with the identities applied.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        integer, allocatable :: new_args(:)
        type(fad_expr_t) :: e
        character(len=:), allocatable :: text_here, op
        integer :: i, kind_here, a, b
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (.not. allocated(p%exprs(idx)%args)) return

        kind_here = p%exprs(idx)%kind
        text_here = p%exprs(idx)%text
        new_args = p%exprs(idx)%args
        changed = .false.
        do i = 1, size(new_args)
            block
                integer :: sub
                sub = fold(p, new_args(i))
                if (sub /= new_args(i)) changed = .true.
                new_args(i) = sub
            end block
        end do

        if (kind_here == FAD_BINOP) then
            op = trim(text_here)
            a = new_args(1)
            b = new_args(2)
            select case (op)
            case ("+")
                if (is_literal(p, b, 0.0_dp)) then
                    out = a
                    return
                else if (is_literal(p, a, 0.0_dp)) then
                    out = b
                    return
                end if
            case ("-")
                if (is_literal(p, b, 0.0_dp)) then
                    out = a
                    return
                else if (is_literal(p, a, 0.0_dp)) then
                    e%kind = FAD_UNOP
                    e%text = "-"
                    e%args = [b]
                    out = p%add_expr(e)
                    return
                end if
            case ("*")
                if (is_literal(p, b, 1.0_dp)) then
                    out = a
                    return
                else if (is_literal(p, a, 1.0_dp)) then
                    out = b
                    return
                end if
            case ("/")
                if (is_literal(p, b, 1.0_dp)) then
                    out = a
                    return
                end if
            case ("**")
                if (is_literal(p, b, 1.0_dp)) then
                    out = a
                    return
                end if
            end select
        end if

        if (.not. changed) return
        e%kind = kind_here
        e%text = text_here
        e%args = new_args
        out = p%add_expr(e)
    end function fold

    logical function is_literal(p, idx, value) result(yes)
        !! Whether an expression is exactly this literal.
        !!
        !! The text is parsed rather than pattern-matched, because the same
        !! number reaches here as `0`, `0.0`, `0.0d0` and `0.0_dp` depending on
        !! which rule produced it.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        real(dp), intent(in) :: value
        character(len=:), allocatable :: text
        real(dp) :: number
        integer :: ios, pos

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind /= FAD_CONST) return
        if (.not. allocated(p%exprs(idx)%text)) return
        text = trim(adjustl(p%exprs(idx)%text))
        ! A kind suffix is not something `read` understands.
        pos = index(text, "_")
        if (pos > 1) text = text(1:pos - 1)
        read (text, *, iostat=ios) number
        if (ios /= 0) return
        yes = number == value
    end function is_literal

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

    subroutine reciprocate_divisions(p)
        !! Turn division by a loop-invariant into multiplication by its
        !! reciprocal, so the divide happens once instead of per iteration.
        !!
        !! A divide costs several times a multiply and the reciprocal is the
        !! same value every iteration, but no Fortran compiler will make this
        !! substitution without `-ffast-math`: `x/d` and `x*(1/d)` differ in the
        !! last bit. An AD tool may, on the same grounds as the reassociations
        !! around it - the derivative is an approximation of a limit either way.
        !!
        !! A divisor that is not invariant still qualifies if it divides more
        !! than one thing: `k` divisions by the same value become one reciprocal
        !! and `k` multiplies, and subexpression sharing then computes that
        !! reciprocal once. fortfem's quintic Lagrange weights divide by the
        !! same node differences throughout - seventy scalar divides in the
        !! adjoint, where Enzyme has none.
        !!
        !! A divisor used once is left alone: reciprocating it would trade one
        !! divide for a divide and a multiply.
        type(fad_proc_t), intent(inout) :: p
        integer, allocatable :: divisors(:)
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
            call count_divisors(p, first, last, divisors)
            do j = first + 1, last - 1
                if (p%stmts(j)%value <= 0) cycle
                p%stmts(j)%value = reciprocate(p, first, last, &
                                               p%stmts(j)%value, divisors)
            end do
            i = last + 1
        end do
    end subroutine reciprocate_divisions

    subroutine count_divisors(p, first, last, divisors)
        !! How often each arena node appears as a denominator in the body.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        integer, allocatable, intent(out) :: divisors(:)
        integer :: j

        if (allocated(divisors)) deallocate (divisors)
        allocate (divisors(max(1, p%n_exprs)))
        divisors = 0
        do j = first + 1, last - 1
            if (p%stmts(j)%value <= 0) cycle
            call tally_divisors(p, p%stmts(j)%value, divisors)
        end do
    end subroutine count_divisors

    recursive subroutine tally_divisors(p, idx, divisors)
        !! Count denominators in an expression tree.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer, intent(inout) :: divisors(:)
        integer :: i, d

        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_BINOP) then
            if (trim(p%exprs(idx)%text) == "/") then
                d = p%exprs(idx)%args(2)
                if (d >= 1 .and. d <= size(divisors)) &
                    divisors(d) = divisors(d) + 1
            end if
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            call tally_divisors(p, p%exprs(idx)%args(i), divisors)
        end do
    end subroutine tally_divisors

    recursive integer function reciprocate(p, first, last, idx, divisors) &
        result(out)
        !! Rebuild an expression with repeated or invariant divisions turned
        !! into products.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: first, last, idx
        integer, intent(in) :: divisors(:)
        integer, allocatable :: new_args(:)
        type(fad_expr_t) :: e
        character(len=:), allocatable :: text_here
        integer :: i, kind_here, one, recip
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (.not. allocated(p%exprs(idx)%args)) return

        kind_here = p%exprs(idx)%kind
        text_here = p%exprs(idx)%text
        new_args = p%exprs(idx)%args
        changed = .false.
        do i = 1, size(new_args)
            block
                integer :: sub
                sub = reciprocate(p, first, last, new_args(i), divisors)
                if (sub /= new_args(i)) changed = .true.
                new_args(i) = sub
            end block
        end do

        if (kind_here == FAD_BINOP) then
            if (trim(text_here) == "/") then
                ! Counted against the original divisor, not the rebuilt one:
                ! rewriting happens bottom-up, so by the time a division is
                ! reached its divisor may be a node created moments ago, which
                ! the tally taken before the pass has never seen.
                if (worth_reciprocating(p, first, last, p%exprs(idx)%args(2), &
                                        new_args(2), divisors)) then
                    one = p%add_expr(expr_const("1.0"//suffix_of(p)))
                    recip = p%add_expr(expr_binop("/", one, new_args(2)))
                    out = p%add_expr(expr_binop("*", new_args(1), recip))
                    return
                end if
            end if
        end if

        if (.not. changed) return
        e%kind = kind_here
        e%text = text_here
        e%args = new_args
        out = p%add_expr(e)
    end function reciprocate

    logical function worth_reciprocating(p, first, last, original, current, &
                                         divisors) result(yes)
        !! Whether turning division by this divisor into multiplication pays.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last, original, current
        integer, intent(in) :: divisors(:)

        yes = .false.
        ! Invariant: the reciprocal leaves the loop entirely.
        if (loop_invariant(p, first, last, current)) then
            yes = .true.
            return
        end if
        ! Repeated: one reciprocal serves every division by it.
        if (original < 1 .or. original > size(divisors)) return
        yes = divisors(original) > 1
    end function worth_reciprocating

    function suffix_of(p) result(text)
        !! The kind suffix for literals fortad emits into this procedure.
        type(fad_proc_t), intent(in) :: p
        character(len=:), allocatable :: text

        text = "d0"
        if (allocated(p%real_suffix)) text = p%real_suffix
    end function suffix_of

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
                    ! Deliberately not advancing: a statement can contain more
                    ! than one invariant subexpression, and after factoring it
                    ! usually does - the coefficient and the scatter's scale.
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
        !!
        !! Shifts in place. Building the new list in a temporary and copying it
        !! back meant an allocation and two full copies per insertion, and
        !! hoisting inserts once per invariant it finds - on fortfem's
        !! degree-eleven Bezier that is hundreds of insertions into a
        !! four-thousand-statement list.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: at
        type(fad_stmt_t), intent(in) :: s
        integer :: i

        if (p%n_stmts + 1 > size(p%stmts)) then
            block
                type(fad_stmt_t), allocatable :: grown(:)
                allocate (grown(2*(p%n_stmts + 1)))
                grown(1:p%n_stmts) = p%stmts(1:p%n_stmts)
                call move_alloc(grown, p%stmts)
            end block
        end if
        do i = p%n_stmts, at, -1
            p%stmts(i + 1) = p%stmts(i)
        end do
        p%stmts(at) = s
        p%n_stmts = p%n_stmts + 1
    end subroutine insert_before

    subroutine share_subexpressions(p)
        !! Name a subexpression a loop body computes more than once.
        !!
        !! The expression arena is hash-consed, so two identical subtrees are
        !! one node. Finding what is recomputed is therefore counting uses of a
        !! node, not comparing trees.
        !!
        !! This is deliberately local to one loop body and runs after every
        !! other pass. An earlier whole-procedure common-subexpression pass
        !! produced silently non-symmetric Hessians through the forward-over-
        !! reverse composition and was withdrawn; confining the rewrite to a
        !! straight-line run of assignments inside one loop is what makes it
        !! defensible, and the Hessian symmetry oracle is what checks it.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last, n_named

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
            call share_in_body(p, first, last, n_named)
            call loop_extent(p, first, first, last)
            i = last + 1
        end do
    end subroutine share_subexpressions

    subroutine share_in_body(p, first, last, n_named)
        !! Hoist one repeated subexpression per round, until none is left.
        !!
        !! Uses are counted in a single traversal of the body rather than by
        !! asking each candidate how often it appears. The per-candidate form
        !! was O(nodes x statements x tree) a round, which is fine on a
        !! thirty-statement kernel and takes minutes on a degree-eleven Bezier
        !! with four thousand.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(inout) :: first, last, n_named
        integer, parameter :: MIN_OPS = 2
        integer :: node, j, best, best_gain, gain, decl_i
        integer, allocatable :: uses(:)
        character(len=32) :: label
        logical :: again

        again = .true.
        do while (again)
            again = .false.
            allocate (uses(max(1, p%n_exprs)))
            uses = 0
            do j = first + 1, last - 1
                if (p%stmts(j)%value <= 0) cycle
                call tally(p, p%stmts(j)%value, uses)
            end do

            best = 0
            best_gain = 0
            do node = 1, min(size(uses), p%n_exprs)
                if (uses(node) < 2) cycle
                if (op_count(p, node) < MIN_OPS) cycle
                ! What is saved is one evaluation of the node per extra use.
                gain = (uses(node) - 1)*op_count(p, node)
                if (gain > best_gain) then
                    best_gain = gain
                    best = node
                end if
            end do
            deallocate (uses)
            if (best == 0) exit

            ! The temporary takes its type from the subexpression, not from the
            ! statement it sits in. `base + 1` inside `z(base + 1)` is an
            ! integer in a real-valued statement, and declaring it real makes
            ! the subscript illegal.
            decl_i = node_type_decl(p, best)
            if (decl_i == 0) exit

            n_named = n_named + 1
            write (label, '(a,i0)') "fad_s", n_named
            call name_subexpression(p, first, last, best, trim(label), decl_i)
            call loop_extent(p, first, first, last)
            again = .true.
        end do
    end subroutine share_in_body

    recursive subroutine tally(p, idx, uses)
        !! Count every node an expression tree mentions.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer, intent(inout) :: uses(:)
        integer :: i

        if (idx <= 0 .or. idx > p%n_exprs .or. idx > size(uses)) return
        uses(idx) = uses(idx) + 1
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            call tally(p, p%exprs(idx)%args(i), uses)
        end do
    end subroutine tally

    integer function node_type_decl(p, node) result(decl_i)
        !! A declaration whose type the subexpression's value would have.
        !!
        !! An expression mentioning any real is real, so a real declaration
        !! wins over an integer one wherever both appear. With no declared name
        !! in it at all there is nothing to copy a type from, and the
        !! subexpression is left alone.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: node
        integer :: first_seen

        decl_i = 0
        first_seen = 0
        call scan_types(p, node, first_seen, decl_i)
        if (decl_i == 0) decl_i = first_seen
    end function node_type_decl

    recursive subroutine scan_types(p, idx, first_seen, real_seen)
        !! Record the first declared name in a subexpression, and the first
        !! real one.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer, intent(inout) :: first_seen, real_seen
        character(len=:), allocatable :: name
        integer :: i, di, par

        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            if (allocated(p%exprs(idx)%text)) then
                name = trim(p%exprs(idx)%text)
                par = index(name, "(")
                if (par > 1) name = name(1:par - 1)
                di = p%decl_index(name)
                if (di > 0) then
                    if (first_seen == 0) first_seen = di
                    if (real_seen == 0 .and. allocated(p%decls(di)%type_name)) then
                        if (index(p%decls(di)%type_name, "real") > 0) real_seen = di
                    end if
                end if
            end if
        end select
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            call scan_types(p, p%exprs(idx)%args(i), first_seen, real_seen)
        end do
    end subroutine scan_types

    recursive integer function occurrences(p, idx, node) result(n)
        !! How many times a node appears in an expression tree.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, node
        integer :: i

        n = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (idx == node) then
            n = 1
            return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            n = n + occurrences(p, p%exprs(idx)%args(i), node)
        end do
    end function occurrences

    subroutine name_subexpression(p, first, last, node, label, decl_i)
        !! Assign a repeated subexpression to a local, before its first use.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(inout) :: first, last
        integer, intent(in) :: node, decl_i
        character(len=*), intent(in) :: label
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        integer :: j, ignored, repl, at

        d = p%decls(decl_i)
        d%name = label
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_array = .false.
        if (allocated(d%dims)) deallocate (d%dims)
        ignored = p%add_decl(d)

        at = 0
        do j = first + 1, last - 1
            if (p%stmts(j)%value <= 0) cycle
            if (occurrences(p, p%stmts(j)%value, node) > 0) then
                at = j
                exit
            end if
        end do
        if (at == 0) return

        repl = p%add_expr(expr_var(label))
        do j = at, last - 1
            if (p%stmts(j)%value <= 0) cycle
            p%stmts(j)%value = swap_node(p, p%stmts(j)%value, node, repl)
        end do

        s%kind = FAD_ASSIGN
        s%target = label
        s%value = node
        call insert_before(p, at, s)
        last = last + 1
    end subroutine name_subexpression

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
