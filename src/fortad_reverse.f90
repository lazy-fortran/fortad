module fortad_reverse
    !! Reverse (adjoint) mode: build the VJP procedure from the primal IR.
    !!
    !! Two design decisions carry this module.
    !!
    !! **Partials come from the forward rules.** The adjoint of an operation is
    !! its transpose, so instead of a second rule table fortad asks the JVP rule
    !! for the partial with respect to one argument by seeding that argument's
    !! tangent with one and the rest with zero. Forward and reverse therefore
    !! cannot disagree about what a derivative is, and a new intrinsic needs one
    !! rule, not two. This is the JAX `jvp + transpose` decomposition applied to
    !! a mutation-first language.
    !!
    !! **No tape.** The forward sweep is renamed into static single assignment,
    !! so every value the reverse sweep needs is still live in its own scalar
    !! local. The compiler allocates those in registers. A tape would be simpler
    !! to write and strictly slower to run, and for straight-line code it buys
    !! nothing at all.
    !!
    !! SSA does not extend to loops, where the number of versions is a runtime
    !! quantity. Loops are therefore refused by name here rather than silently
    !! mishandled; they need the typed per-loop storage described in the
    !! roadmap, which is the next milestone.
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                        expr_const, expr_var, expr_binop, expr_unop, expr_call, &
                        FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, FAD_CALL, &
                        FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, &
                        FAD_ELSE, FAD_END_IF, FAD_INTENT_IN, FAD_INTENT_OUT, &
                        FAD_INTENT_INOUT, FAD_INTENT_NONE
    use fortad_rules, only: jvp_binop, jvp_unop, jvp_call, has_rule, &
                            fad_add, fad_mul, fad_neg, fad_real
    use fortad_reverse_loop, only: loop_shape_t, analyse_loop, LOOP_OK, &
                                   split_accumulation, target_base
    implicit none
    private

    public :: differentiate_reverse, reverse_spec_t, reverse_status_t

    type :: reverse_spec_t
        !! What to differentiate, and with respect to what.
        character(len=:), allocatable :: independents(:)
        !! The single dependent whose adjoint seeds the sweep. Defaults to the
        !! procedure result, or the sole `intent(out)` argument.
        character(len=:), allocatable :: dependent
        !! Suffix for adjoint names. `x` becomes `x_b` by default.
        character(len=:), allocatable :: suffix
        !! Name of the generated procedure. Defaults to `<primal>_vjp`.
        character(len=:), allocatable :: name
    end type reverse_spec_t

    type :: reverse_status_t
        logical :: ok = .false.
        character(len=:), allocatable :: message
    end type reverse_status_t

    type :: loop_record_t
        !! What the reverse sweep needs to invert one reduction loop.
        type(loop_shape_t) :: shape
        character(len=:), allocatable :: var
        integer :: lo = 0, hi = 0, step = 0
        character(len=64), allocatable :: accum_names(:)
        !! Statement indices in the generated procedure of this loop's `do`
        !! and `end do`, so the adjoint body can be spliced into the same loop.
        integer :: do_stmt = 0
        integer :: end_do_stmt = 0
        character(len=64), allocatable :: body_lhs(:)
        integer, allocatable :: body_rhs(:)
        logical, allocatable :: body_is_accum(:)
        !! +1 for `s = s + e`, -1 for `s = s - e`, 0 for a temporary,
        !! ELEMENT_TARGET for `c(i) = e`, CARRIED_TARGET for a taped recurrence.
        integer, allocatable :: body_sign(:)
        integer :: n_body = 0
        !! Text of the tape index expression, e.g. "i - (1) + 1".
        character(len=:), allocatable :: tape_index
        logical :: taped = .false.
        !! Carried variables and the SSA name each holds at the end of the body.
        character(len=64), allocatable :: carried_name(:)
        character(len=64), allocatable :: carried_last(:)
        !! SSA name the carried variable held on entry to the loop.
        character(len=64), allocatable :: carried_in(:)
        integer :: n_carried = 0
    end type loop_record_t

    !! Marker in `body_sign` for an array-element target, distinguishing it
    !! from a scalar temporary (0) and an accumulation term (+-1).
    integer, parameter :: ELEMENT_TARGET = 2

    !! Marker for a loop-carried variable whose per-iteration value is taped.
    integer, parameter :: CARRIED_TARGET = 3

    integer, parameter :: ORDER_STMT = 1
    integer, parameter :: ORDER_LOOP = 2
    integer, parameter :: ORDER_BRANCH = 3

    type :: branch_record_t
        !! What the reverse sweep needs to invert one if/else construct.
        !!
        !! The condition is re-evaluated rather than recorded. Its operands are
        !! SSA values from before the branch, so they are still live and still
        !! hold exactly what they held when the forward sweep tested them; a
        !! stored flag would cost memory to learn the same thing.
        integer :: cond = 0
        character(len=64), allocatable :: then_lhs(:), else_lhs(:)
        integer, allocatable :: then_rhs(:), else_rhs(:)
        integer :: n_then = 0, n_else = 0
        !! Merge variables and the arm-local values feeding them.
        character(len=64), allocatable :: merge_name(:)
        character(len=64), allocatable :: merge_from_then(:)
        character(len=64), allocatable :: merge_from_else(:)
        integer :: n_merge = 0
    end type branch_record_t

    type :: ssa_map_t
        !! Current SSA name of each declared variable, and the version counter.
        character(len=64), allocatable :: base(:)
        character(len=64), allocatable :: current(:)
        integer, allocatable :: version(:)
        integer :: n = 0
    end type ssa_map_t

contains

    subroutine differentiate_reverse(primal, spec, adjoint, status)
        !! Build the adjoint procedure.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_spec_t), intent(in) :: spec
        type(fad_proc_t), intent(out) :: adjoint
        type(reverse_status_t), intent(out) :: status
        character(len=:), allocatable :: suffix, dependent
        logical, allocatable :: active(:)
        type(ssa_map_t) :: ssa
        character(len=64), allocatable :: lhs_names(:)
        integer, allocatable :: rhs_exprs(:)
        type(loop_record_t), allocatable :: loops(:)
        type(branch_record_t), allocatable :: branches(:)
        integer, allocatable :: order_kind(:), order_index(:)
        integer :: n_rec, n_loops, n_branches, n_order

        status%ok = .true.
        suffix = "_b"
        if (allocated(spec%suffix)) suffix = spec%suffix

        if (.not. allocated(spec%independents)) then
            status%ok = .false.
            status%message = "no independent variables given"
            return
        end if

        call choose_dependent(primal, spec, dependent, status)
        if (.not. status%ok) return

        call check_supported(primal, status)
        if (.not. status%ok) return

        call seed_activity(primal, spec, dependent, active, status)
        if (.not. status%ok) return

        adjoint%name = primal%name//"_vjp"
        if (allocated(spec%name)) adjoint%name = spec%name
        adjoint%is_function = .false.
        adjoint%real_suffix = "d0"
        if (allocated(primal%real_suffix)) adjoint%real_suffix = primal%real_suffix

        call build_signature(primal, adjoint, spec, dependent, suffix, active)
        call build_forward_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
                                 n_rec, loops, n_loops, branches, n_branches, &
                                 order_kind, order_index, n_order, status)
        if (.not. status%ok) return
        call build_reverse_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
                                 n_rec, loops, n_loops, branches, n_branches, &
                                 order_kind, order_index, n_order, &
                                 spec, dependent, suffix, active, status)
    end subroutine differentiate_reverse

    subroutine choose_dependent(primal, spec, dependent, status)
        !! The dependent whose adjoint seeds the sweep.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_spec_t), intent(in) :: spec
        character(len=:), allocatable, intent(out) :: dependent
        type(reverse_status_t), intent(inout) :: status
        integer :: i, n_out, last_out

        if (allocated(spec%dependent)) then
            dependent = trim(spec%dependent)
            if (primal%decl_index(dependent) == 0) then
                status%ok = .false.
                status%message = "dependent '"//dependent// &
                                 "' is not declared in "//primal%name
            end if
            return
        end if

        if (primal%is_function) then
            dependent = primal%result_name
            return
        end if

        n_out = 0
        last_out = 0
        do i = 1, primal%n_decls
            if (primal%decls(i)%intent == FAD_INTENT_OUT) then
                n_out = n_out + 1
                last_out = i
            end if
        end do
        if (n_out == 1) then
            dependent = primal%decls(last_out)%name
            return
        end if

        status%ok = .false.
        if (n_out == 0) then
            status%message = "no dependent found: "//primal%name// &
                             " has no intent(out) argument; name one explicitly"
        else
            status%message = "several intent(out) arguments in "//primal%name// &
                             "; name the dependent explicitly"
        end if
    end subroutine choose_dependent

    subroutine check_supported(primal, status)
        !! Refuse what this milestone cannot do correctly, by name.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_status_t), intent(inout) :: status
        type(loop_shape_t) :: shape
        integer :: i, depth

        depth = 0
        do i = 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                ! Inside a loop, an element write is a scatter that analyse_loop
                ! validates. Outside one there is no index to scatter over, so
                ! the adjoint would have nowhere to go.
                if (depth == 0 .and. index(primal%stmts(i)%target, "(") > 0) then
                    status%ok = .false.
                    status%message = "reverse mode: assignment to an array "// &
                        "element outside a loop is not supported yet; forward "// &
                        "mode handles it"
                    return
                end if
            case (FAD_DO)
                depth = depth + 1
                call analyse_loop(primal, i, shape)
                if (shape%status /= LOOP_OK) then
                    status%ok = .false.
                    status%message = shape%message
                    return
                end if
            case (FAD_END_DO)
                depth = max(0, depth - 1)
            case (FAD_IF, FAD_ELSE, FAD_END_IF)
                ! Branches are handled by re-evaluating the condition in the
                ! reverse sweep; see emit_branch_forward.
                continue
            end select
        end do
    end subroutine check_supported

    subroutine seed_activity(primal, spec, dependent, active, status)
        !! Activity is varied **and** useful: reachable forward from an
        !! independent, and reachable backward from the dependent. Either test
        !! alone leaves dead derivative statements in the output.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_spec_t), intent(in) :: spec
        character(len=*), intent(in) :: dependent
        logical, allocatable, intent(out) :: active(:)
        type(reverse_status_t), intent(inout) :: status
        logical, allocatable :: varied(:), useful(:)
        integer :: i, j, di
        logical :: changed

        allocate (varied(max(1, primal%n_decls)))
        allocate (useful(max(1, primal%n_decls)))
        varied = .false.
        useful = .false.

        do i = 1, size(spec%independents)
            di = primal%decl_index(trim(spec%independents(i)))
            if (di == 0) then
                status%ok = .false.
                status%message = "independent '"//trim(spec%independents(i))// &
                                 "' is not declared in "//primal%name
                return
            end if
            varied(di) = .true.
        end do

        changed = .true.
        do while (changed)
            changed = .false.
            do j = 1, primal%n_stmts
                if (primal%stmts(j)%kind /= FAD_ASSIGN) cycle
                if (.not. reads_any(primal, primal%stmts(j)%value, varied)) cycle
                ! An array-element target must resolve to its array, or the
                ! array never becomes active and its adjoint is silently
                ! dropped - a wrong gradient that looks plausible.
                di = primal%decl_index(target_base(primal%stmts(j)%target))
                if (di > 0) then
                    if (.not. varied(di)) then
                        varied(di) = .true.
                        changed = .true.
                    end if
                end if
            end do
        end do

        di = primal%decl_index(dependent)
        if (di > 0) useful(di) = .true.
        changed = .true.
        do while (changed)
            changed = .false.
            do j = primal%n_stmts, 1, -1
                if (primal%stmts(j)%kind /= FAD_ASSIGN) cycle
                di = primal%decl_index(target_base(primal%stmts(j)%target))
                if (di == 0) cycle
                if (.not. useful(di)) cycle
                if (mark_reads(primal, primal%stmts(j)%value, useful)) changed = .true.
            end do
        end do

        allocate (active(max(1, primal%n_decls)))
        active = varied .and. useful
    end subroutine seed_activity

    recursive logical function reads_any(p, idx, flags) result(yes)
        !! True when the expression reads any flagged variable.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        logical, intent(in) :: flags(:)
        integer :: i, di

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            di = p%decl_index(p%exprs(idx)%text)
            if (di > 0) then
                if (flags(di)) then
                    yes = .true.
                    return
                end if
            end if
        end select
        do i = 1, size(p%exprs(idx)%args)
            if (reads_any(p, p%exprs(idx)%args(i), flags)) then
                yes = .true.
                return
            end if
        end do
    end function reads_any

    recursive logical function mark_reads(p, idx, flags) result(changed)
        !! Flag every variable the expression reads. Returns whether anything
        !! became newly flagged, for the fixed-point loop.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        logical, intent(inout) :: flags(:)
        integer :: i, di

        changed = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            di = p%decl_index(p%exprs(idx)%text)
            if (di > 0) then
                if (.not. flags(di)) then
                    flags(di) = .true.
                    changed = .true.
                end if
            end if
        end select
        do i = 1, size(p%exprs(idx)%args)
            if (mark_reads(p, p%exprs(idx)%args(i), flags)) changed = .true.
        end do
    end function mark_reads

    subroutine build_signature(primal, adjoint, spec, dependent, suffix, active)
        !! Dummy arguments: the primal ones, the dependent's adjoint as the
        !! incoming seed, and one outgoing adjoint per independent.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(reverse_spec_t), intent(in) :: spec
        character(len=*), intent(in) :: dependent, suffix
        logical, intent(in) :: active(:)
        character(len=64), allocatable :: names(:)
        type(fad_decl_t) :: d
        integer :: i, n, di, ignored

        allocate (names(2*(size(primal%params) + size(spec%independents) + 4)))
        n = 0

        do i = 1, size(primal%params)
            n = n + 1
            names(n) = trim(primal%params(i))
            di = primal%decl_index(trim(primal%params(i)))
            if (di == 0) cycle
            d = primal%decls(di)
            ! The adjoint routine recomputes the primal, so an argument the
            ! primal only wrote is still only written here.
            ignored = adjoint%add_decl(d)
        end do

        ! The dependent: value out, adjoint seed in.
        di = primal%decl_index(dependent)
        if (di > 0) then
            if (.not. is_dummy(primal, dependent)) then
                n = n + 1
                names(n) = dependent
                d = primal%decls(di)
                d%intent = FAD_INTENT_OUT
                d%is_result = .false.
                ignored = adjoint%add_decl(d)
            end if
            n = n + 1
            names(n) = dependent//suffix
            d = primal%decls(di)
            d%name = dependent//suffix
            d%intent = FAD_INTENT_IN
            d%is_result = .false.
            ignored = adjoint%add_decl(d)
        end if

        ! One outgoing adjoint per independent.
        do i = 1, size(spec%independents)
            di = primal%decl_index(trim(spec%independents(i)))
            if (di == 0) cycle
            n = n + 1
            names(n) = trim(spec%independents(i))//suffix
            d = primal%decls(di)
            d%name = trim(spec%independents(i))//suffix
            d%intent = FAD_INTENT_OUT
            d%is_result = .false.
            ignored = adjoint%add_decl(d)
        end do

        adjoint%params = names(1:n)
    end subroutine build_signature

    subroutine build_forward_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
                                   n_rec, loops, n_loops, branches, n_branches, &
                                   order_kind, order_index, n_order, status)
        !! Emit the primal, renaming straight-line assignments into static
        !! single assignment and emitting reduction loops verbatim.
        !!
        !! Inside a loop, SSA is suspended: a per-iteration temporary is a
        !! single scalar local, and an accumulator is one local mutated across
        !! iterations. Neither needs a version history, because the reverse
        !! sweep recomputes the first and never reads the second.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(inout) :: ssa
        character(len=64), allocatable, intent(out) :: lhs_names(:)
        integer, allocatable, intent(out) :: rhs_exprs(:)
        integer, intent(out) :: n_rec
        type(loop_record_t), allocatable, intent(out) :: loops(:)
        integer, intent(out) :: n_loops
        type(branch_record_t), allocatable, intent(out) :: branches(:)
        integer, intent(out) :: n_branches
        !! Blocks in forward program order, so the reverse sweep can walk them
        !! backwards. Reversing all straight-line statements and only then the
        !! loops and branches is wrong whenever a construct sits between two
        !! assignments, which it usually does.
        integer, allocatable, intent(out) :: order_kind(:), order_index(:)
        integer, intent(out) :: n_order
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=:), allocatable :: fresh, current
        type(loop_shape_t) :: shape
        integer :: i, k, di, ignored, after

        call ssa_init(primal, ssa)
        allocate (lhs_names(max(1, primal%n_stmts)))
        allocate (rhs_exprs(max(1, primal%n_stmts)))
        allocate (loops(max(1, primal%n_stmts)))
        allocate (branches(max(1, primal%n_stmts)))
        allocate (order_kind(max(1, primal%n_stmts)))
        allocate (order_index(max(1, primal%n_stmts)))
        n_rec = 0
        n_loops = 0
        n_branches = 0
        n_order = 0

        i = 1
        do while (i <= primal%n_stmts)
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                di = primal%decl_index(primal%stmts(i)%target)
                if (di == 0) then
                    status%ok = .false.
                    status%message = "assignment to undeclared '"// &
                                     primal%stmts(i)%target//"'"
                    return
                end if
                s%kind = FAD_ASSIGN
                s%value = copy_renamed(primal, adjoint, primal%stmts(i)%value, ssa)
                call ssa_fresh(ssa, primal%stmts(i)%target, fresh)
                s%target = fresh
                d = primal%decls(di)
                d%name = fresh
                d%intent = FAD_INTENT_NONE
                d%is_result = .false.
                ignored = adjoint%add_decl(d)
                ignored = adjoint%add_stmt(s)
                n_rec = n_rec + 1
                lhs_names(n_rec) = fresh
                rhs_exprs(n_rec) = s%value
                n_order = n_order + 1
                order_kind(n_order) = ORDER_STMT
                order_index(n_order) = n_rec
                i = i + 1

            case (FAD_DO)
                call analyse_loop(primal, i, shape)
                if (shape%status /= LOOP_OK) then
                    status%ok = .false.
                    status%message = shape%message
                    return
                end if
                n_loops = n_loops + 1
                call emit_loop_forward(primal, adjoint, ssa, shape, &
                                       loops(n_loops), status)
                if (.not. status%ok) return
                n_order = n_order + 1
                order_kind(n_order) = ORDER_LOOP
                order_index(n_order) = n_loops
                i = shape%last + 1

            case (FAD_IF)
                n_branches = n_branches + 1
                call emit_branch_forward(primal, adjoint, ssa, i, &
                                         branches(n_branches), after, status)
                if (.not. status%ok) return
                n_order = n_order + 1
                order_kind(n_order) = ORDER_BRANCH
                order_index(n_order) = n_branches
                i = after

            case default
                i = i + 1
            end select
        end do
    end subroutine build_forward_sweep

    subroutine emit_branch_forward(primal, adjoint, ssa, first, rec, after, status)
        !! Emit one if/else, renaming each arm independently and merging.
        !!
        !! Each arm is straight-line, so SSA works inside it. The arms disagree
        !! about the current version of anything they both assign, so the join
        !! needs an explicit merge variable, written at the end of whichever arm
        !! ran. That is a phi node made concrete, which is the only honest way
        !! to do it in a language with no phi.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(inout) :: ssa
        integer, intent(in) :: first
        type(branch_record_t), intent(out) :: rec
        integer, intent(out) :: after
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        type(ssa_map_t) :: before, after_then
        character(len=:), allocatable :: fresh, merged, from_then, from_else
        integer :: i, di, ignored, else_at, end_at, depth

        after = first + 1

        ! Find the matching else and end if.
        depth = 0
        else_at = 0
        end_at = 0
        do i = first, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_IF)
                depth = depth + 1
            case (FAD_ELSE)
                if (depth == 1) else_at = i
            case (FAD_END_IF)
                depth = depth - 1
                if (depth == 0) then
                    end_at = i
                    exit
                end if
            case (FAD_DO)
                if (depth >= 1) then
                    status%ok = .false.
                    status%message = "reverse mode: a loop inside a branch is "// &
                        "not supported yet"
                    return
                end if
            end select
        end do
        if (end_at == 0) then
            status%ok = .false.
            status%message = "unterminated if construct"
            return
        end if
        after = end_at + 1

        ! The condition is evaluated with the SSA state before the branch.
        rec%cond = copy_renamed(primal, adjoint, primal%stmts(first)%value, ssa)
        before = ssa

        allocate (rec%then_lhs(end_at - first), rec%then_rhs(end_at - first))
        allocate (rec%else_lhs(end_at - first), rec%else_rhs(end_at - first))
        allocate (rec%merge_name(end_at - first))
        allocate (rec%merge_from_then(end_at - first))
        allocate (rec%merge_from_else(end_at - first))
        rec%n_then = 0
        rec%n_else = 0
        rec%n_merge = 0

        s%kind = FAD_IF
        s%value = rec%cond
        ignored = adjoint%add_stmt(s)

        call emit_arm(primal, adjoint, ssa, first + 1, &
                      merge(else_at, end_at, else_at > 0) - 1, &
                      rec%then_lhs, rec%then_rhs, rec%n_then, status)
        if (.not. status%ok) return
        after_then = ssa

        s%kind = FAD_ELSE
        s%value = 0
        ignored = adjoint%add_stmt(s)

        ssa = before
        if (else_at > 0) then
            call emit_arm(primal, adjoint, ssa, else_at + 1, end_at - 1, &
                          rec%else_lhs, rec%else_rhs, rec%n_else, status)
            if (.not. status%ok) return
        end if

        ! Merge: every variable either arm assigned gets one post-branch name.
        do i = 1, primal%n_decls
            call ssa_lookup(after_then, primal%decls(i)%name, from_then)
            call ssa_lookup(ssa, primal%decls(i)%name, from_else)
            if (from_then == from_else) cycle
            rec%n_merge = rec%n_merge + 1
            ! The merge name must not collide with a version either arm used.
            ! The else arm's counter alone is not enough: the then arm may have
            ! gone further, and its names are already in the emitted code.
            call ssa_set(ssa, primal%decls(i)%name, from_else)
            call ssa_advance_to(ssa, primal%decls(i)%name, &
                                ssa_version(after_then, primal%decls(i)%name))
            call ssa_fresh(ssa, primal%decls(i)%name, merged)
            d = primal%decls(i)
            d%name = merged
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            ignored = adjoint%add_decl(d)
            rec%merge_name(rec%n_merge) = merged
            rec%merge_from_then(rec%n_merge) = from_then
            rec%merge_from_else(rec%n_merge) = from_else
            ! Written in the else arm here; the then arm's copy is spliced in
            ! below, before the `else`.
            s%kind = FAD_ASSIGN
            s%target = merged
            s%value = adjoint%add_expr(expr_var(from_else))
            ignored = adjoint%add_stmt(s)
        end do

        s%kind = FAD_END_IF
        s%value = 0
        ignored = adjoint%add_stmt(s)

        call splice_then_merges(adjoint, rec)
    end subroutine emit_branch_forward

    subroutine emit_arm(primal, adjoint, ssa, lo, hi, arm_lhs, arm_rhs, n, status)
        !! Emit one arm's statements in SSA form, recording them.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(inout) :: ssa
        integer, intent(in) :: lo, hi
        character(len=64), intent(inout) :: arm_lhs(:)
        integer, intent(inout) :: arm_rhs(:)
        integer, intent(inout) :: n
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=:), allocatable :: fresh
        integer :: i, di, ignored

        do i = lo, hi
            if (primal%stmts(i)%kind /= FAD_ASSIGN) then
                status%ok = .false.
                status%message = "reverse mode: only assignments are supported "// &
                    "inside a branch arm"
                return
            end if
            di = primal%decl_index(primal%stmts(i)%target)
            if (di == 0) then
                status%ok = .false.
                status%message = "assignment to undeclared '"// &
                                 primal%stmts(i)%target//"'"
                return
            end if
            s%kind = FAD_ASSIGN
            s%value = copy_renamed(primal, adjoint, primal%stmts(i)%value, ssa)
            call ssa_fresh(ssa, primal%stmts(i)%target, fresh)
            s%target = fresh
            d = primal%decls(di)
            d%name = fresh
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            ignored = adjoint%add_decl(d)
            ignored = adjoint%add_stmt(s)
            n = n + 1
            arm_lhs(n) = fresh
            arm_rhs(n) = s%value
        end do
    end subroutine emit_arm

    subroutine splice_then_merges(adjoint, rec)
        !! Put the then-arm's merge writes at the end of the then arm.
        !!
        !! They are built after the else arm, because the merge names are only
        !! known once both arms have been renamed, so they have to be moved back
        !! into place rather than emitted where they belong.
        type(fad_proc_t), intent(inout) :: adjoint
        type(branch_record_t), intent(in) :: rec
        type(fad_stmt_t), allocatable :: rebuilt(:)
        type(fad_stmt_t) :: s
        integer :: i, k, else_at, n_new

        if (rec%n_merge == 0) return

        else_at = 0
        do i = adjoint%n_stmts, 1, -1
            if (adjoint%stmts(i)%kind == FAD_ELSE) then
                else_at = i
                exit
            end if
        end do
        if (else_at == 0) return

        allocate (rebuilt(adjoint%n_stmts + rec%n_merge))
        n_new = 0
        do i = 1, else_at - 1
            n_new = n_new + 1
            rebuilt(n_new) = adjoint%stmts(i)
        end do
        do k = 1, rec%n_merge
            s%kind = FAD_ASSIGN
            s%target = trim(rec%merge_name(k))
            s%value = adjoint%add_expr(expr_var(trim(rec%merge_from_then(k))))
            n_new = n_new + 1
            rebuilt(n_new) = s
        end do
        do i = else_at, adjoint%n_stmts
            n_new = n_new + 1
            rebuilt(n_new) = adjoint%stmts(i)
        end do

        if (.not. allocated(adjoint%stmts)) return
        if (size(adjoint%stmts) < n_new) then
            deallocate (adjoint%stmts)
            allocate (adjoint%stmts(n_new + 32))
        end if
        adjoint%stmts(1:n_new) = rebuilt(1:n_new)
        adjoint%n_stmts = n_new
    end subroutine splice_then_merges

    subroutine emit_loop_forward(primal, adjoint, ssa, shape, rec, status)
        !! Emit one reduction loop, and record what the reverse sweep needs.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(inout) :: ssa
        type(loop_shape_t), intent(in) :: shape
        type(loop_record_t), intent(out) :: rec
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=:), allocatable :: fresh, incoming, current
        character(len=64), allocatable :: carried_entry(:)
        integer :: i, k, di, ignored

        rec%shape = shape
        rec%var = primal%stmts(shape%first)%target

        ! Loop bounds are evaluated outside the loop, so they use the SSA names
        ! current before it.
        rec%lo = copy_renamed(primal, adjoint, primal%stmts(shape%first)%lo, ssa)
        rec%hi = copy_renamed(primal, adjoint, primal%stmts(shape%first)%hi, ssa)
        rec%step = 0
        if (primal%stmts(shape%first)%step /= 0) then
            rec%step = copy_renamed(primal, adjoint, &
                                    primal%stmts(shape%first)%step, ssa)
        end if

        ! The loop index needs a declaration in the generated procedure.
        di = primal%decl_index(rec%var)
        if (di > 0) then
            d = primal%decls(di)
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            ignored = adjoint%add_decl(d)
        end if

        ! Each accumulator gets one local for the whole loop, seeded from its
        ! incoming SSA value. After the loop that local is the current version.
        allocate (rec%accum_names(max(1, shape%n_accumulators)))
        do k = 1, shape%n_accumulators
            call ssa_lookup(ssa, trim(shape%accumulators(k)), incoming)
            call ssa_fresh(ssa, trim(shape%accumulators(k)), fresh)
            di = primal%decl_index(trim(shape%accumulators(k)))
            d = primal%decls(di)
            d%name = fresh
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            ignored = adjoint%add_decl(d)
            s%kind = FAD_ASSIGN
            s%target = fresh
            s%value = adjoint%add_expr(expr_var(incoming))
            ignored = adjoint%add_stmt(s)
            rec%accum_names(k) = fresh
        end do

        ! A per-iteration temporary keeps its own name; it is one local.
        do k = 1, shape%n_temporaries
            call ssa_set(ssa, trim(shape%temporaries(k)), &
                         trim(shape%temporaries(k)))
            di = primal%decl_index(trim(shape%temporaries(k)))
            d = primal%decls(di)
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            ignored = adjoint%add_decl(d)
        end do

        ! A taped loop needs one array per carried variable, sized from the
        ! loop bounds. The tape is the price of a recurrence that cannot be
        ! recomputed; everything else in this module exists to avoid paying it.
        rec%taped = shape%n_carried > 0
        if (rec%taped) allocate (carried_entry(max(1, shape%n_carried)))
        if (rec%taped) then
            block
                use fortad_emit, only: emit_expr
                character(len=:), allocatable :: lo_text, hi_text
                lo_text = emit_expr(adjoint, rec%lo)
                hi_text = emit_expr(adjoint, rec%hi)
                rec%tape_index = rec%var//" - ("//lo_text//") + 1"
                do k = 1, shape%n_carried
                    di = primal%decl_index(trim(shape%carried(k)))
                    if (di == 0) cycle
                    d = primal%decls(di)
                    d%name = trim(shape%carried(k))//"_tape"
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    d%is_array = .true.
                    d%dims = "("//hi_text//") - ("//lo_text//") + 1"
                    ignored = adjoint%add_decl(d)
                    ! Seed the carrier from whatever the variable held before
                    ! the loop, or the first iteration reads an undefined value.
                    call ssa_lookup(ssa, trim(shape%carried(k)), incoming)
                    carried_entry(k) = incoming
                    if (incoming /= trim(shape%carried(k))) then
                        s%kind = FAD_ASSIGN
                        s%target = trim(shape%carried(k))
                        s%value = adjoint%add_expr(expr_var(incoming))
                        ignored = adjoint%add_stmt(s)
                    end if
                    ! The loop-entry value lives under the plain name; each
                    ! assignment inside the body gets its own SSA version, so
                    ! the reverse sweep can tell the pre-update value from the
                    ! post-update one. Conflating them was the first bug here.
                    call ssa_set(ssa, trim(shape%carried(k)), &
                                 trim(shape%carried(k)))
                    d = primal%decls(di)
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    ignored = adjoint%add_decl(d)
                end do
            end block
        end if

        s%kind = FAD_DO
        s%target = rec%var
        s%lo = rec%lo
        s%hi = rec%hi
        s%step = rec%step
        rec%do_stmt = adjoint%add_stmt(s)

        ! Store each carried value as it enters the iteration.
        if (rec%taped) then
            do k = 1, shape%n_carried
                s%kind = FAD_ASSIGN
                s%target = trim(shape%carried(k))//"_tape("//rec%tape_index//")"
                s%value = adjoint%add_expr(expr_var(trim(shape%carried(k))))
                ignored = adjoint%add_stmt(s)
            end do
        end if

        ! One accumulation can expand into several additive terms, so the body
        ! record list is sized generously rather than one entry per statement.
        allocate (rec%body_lhs(64*(shape%last - shape%first) + 64))
        allocate (rec%body_rhs(64*(shape%last - shape%first) + 64))
        allocate (rec%body_is_accum(64*(shape%last - shape%first) + 64))
        allocate (rec%body_sign(64*(shape%last - shape%first) + 64))
        rec%n_body = 0

        do i = shape%first + 1, shape%last - 1
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            s%kind = FAD_ASSIGN
            s%value = copy_renamed(primal, adjoint, primal%stmts(i)%value, ssa)
            if (is_known_name(shape%carried, shape%n_carried, &
                              primal%stmts(i)%target)) then
                call ssa_fresh(ssa, primal%stmts(i)%target, fresh)
                block
                    integer :: cdi
                    type(fad_decl_t) :: cd
                    cdi = primal%decl_index(primal%stmts(i)%target)
                    cd = primal%decls(cdi)
                    cd%name = fresh
                    cd%intent = FAD_INTENT_NONE
                    cd%is_result = .false.
                    ignored = adjoint%add_decl(cd)
                end block
            else if (index(primal%stmts(i)%target, "(") > 0) then
                fresh = primal%stmts(i)%target
                ! An element target is not renamed, so its array still needs a
                ! declaration in the generated procedure when it was a local.
                block
                    integer :: base_di
                    type(fad_decl_t) :: bd
                    base_di = primal%decl_index(target_base(fresh))
                    if (base_di > 0) then
                        bd = primal%decls(base_di)
                        bd%is_result = .false.
                        ignored = adjoint%add_decl(bd)
                    end if
                end block
            else
                call ssa_lookup(ssa, primal%stmts(i)%target, fresh)
            end if
            s%target = fresh
            ignored = adjoint%add_stmt(s)

            rec%n_body = rec%n_body + 1
            rec%body_lhs(rec%n_body) = fresh
            rec%body_is_accum(rec%n_body) = &
                is_known_name(shape%accumulators, shape%n_accumulators, &
                              primal%stmts(i)%target)
            if (index(primal%stmts(i)%target, "(") > 0) then
                ! An array-element write: its adjoint is a scatter, and the
                ! written value survives the loop so nothing is saved.
                rec%body_lhs(rec%n_body) = fresh
                rec%body_is_accum(rec%n_body) = .false.
                rec%body_rhs(rec%n_body) = s%value
                rec%body_sign(rec%n_body) = ELEMENT_TARGET
                cycle
            end if
            if (is_known_name(shape%carried, shape%n_carried, &
                              primal%stmts(i)%target)) then
                ! Renamed like any other in-body assignment; what makes it
                ! special is only that its final version feeds the next
                ! iteration, which is handled at the end of the loop.
                rec%body_lhs(rec%n_body) = fresh
                rec%body_is_accum(rec%n_body) = .false.
                rec%body_rhs(rec%n_body) = s%value
                rec%body_sign(rec%n_body) = 0
                cycle
            end if
            if (rec%body_is_accum(rec%n_body)) then
                ! `s = s + e1 - e2 + ...`: the reverse sweep needs each term
                ! and its sign, so the accumulation expands into one body
                ! record per term, all sharing the accumulator's name.
                block
                    integer :: terms(64), signs(64), n_terms, k2
                    logical :: split_ok
                    call split_accumulation(adjoint, s%value, fresh, terms, &
                                            signs, n_terms, split_ok)
                    if (.not. split_ok) then
                        status%ok = .false.
                        status%message = "reverse mode: could not split the "// &
                            "accumulation into additive terms"
                        return
                    end if
                    rec%n_body = rec%n_body - 1
                    do k2 = 1, n_terms
                        rec%n_body = rec%n_body + 1
                        rec%body_lhs(rec%n_body) = fresh
                        rec%body_is_accum(rec%n_body) = .true.
                        rec%body_rhs(rec%n_body) = terms(k2)
                        rec%body_sign(rec%n_body) = signs(k2)
                    end do
                end block
            else
                rec%body_rhs(rec%n_body) = s%value
                rec%body_sign(rec%n_body) = 0
            end if
        end do

        ! Carry each recurrence's final in-body value into the next iteration.
        allocate (rec%carried_name(max(1, shape%n_carried)))
        allocate (rec%carried_last(max(1, shape%n_carried)))
        allocate (rec%carried_in(max(1, shape%n_carried)))
        rec%n_carried = shape%n_carried
        do k = 1, shape%n_carried
            call ssa_lookup(ssa, trim(shape%carried(k)), current)
            rec%carried_name(k) = trim(shape%carried(k))
            rec%carried_last(k) = current
            rec%carried_in(k) = carried_entry(k)
            s%kind = FAD_ASSIGN
            s%target = trim(shape%carried(k))
            s%value = adjoint%add_expr(expr_var(current))
            ignored = adjoint%add_stmt(s)
            ! After the loop the plain name holds the final value.
            call ssa_set(ssa, trim(shape%carried(k)), trim(shape%carried(k)))
        end do

        s%kind = FAD_END_DO
        s%value = 0
        rec%end_do_stmt = adjoint%add_stmt(s)
    end subroutine emit_loop_forward

    subroutine build_reverse_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
                                   n_rec, loops, n_loops, branches, n_branches, &
                                   order_kind, order_index, n_order, &
                                   spec, dependent, suffix, active, status)
        !! Walk backwards, accumulating adjoints.
        !!
        !! Straight-line statements are inverted directly against their SSA
        !! values. A reduction loop becomes a second loop whose body recomputes
        !! the per-iteration temporaries and scatters the accumulator's adjoint
        !! into the array adjoints.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: lhs_names(:)
        integer, intent(in) :: rhs_exprs(:), n_rec
        type(loop_record_t), intent(in) :: loops(:)
        integer, intent(in) :: n_loops
        type(branch_record_t), intent(in) :: branches(:)
        integer, intent(in) :: n_branches
        integer, intent(in) :: order_kind(:), order_index(:), n_order
        type(reverse_spec_t), intent(in) :: spec
        character(len=*), intent(in) :: dependent, suffix
        logical, intent(in) :: active(:)
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: final_name
        integer :: i, k, di, ignored, zero, n_tmp, seed_expr

        call ssa_lookup(ssa, dependent, final_name)
        if (final_name /= dependent) then
            s%kind = FAD_ASSIGN
            s%target = dependent
            s%value = adjoint%add_expr(expr_var(final_name))
            ignored = adjoint%add_stmt(s)
        end if

        ! Zero every adjoint before anything accumulates into it.
        zero = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
        do i = 1, n_rec
            if (.not. adjoint_is_live(primal, ssa, lhs_names(i), active)) cycle
            call declare_adjoint(primal, adjoint, ssa, trim(lhs_names(i)), suffix)
            s%kind = FAD_ASSIGN
            s%target = trim(lhs_names(i))//suffix
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do
        do k = 1, n_loops
            do i = 1, loops(k)%shape%n_accumulators
                call declare_adjoint(primal, adjoint, ssa, &
                                     trim(loops(k)%accum_names(i)), suffix)
                s%kind = FAD_ASSIGN
                s%target = trim(loops(k)%accum_names(i))//suffix
                s%value = zero
                ignored = adjoint%add_stmt(s)
            end do
            do i = 1, loops(k)%n_carried
                call declare_adjoint(primal, adjoint, ssa, &
                                     trim(loops(k)%carried_name(i)), suffix)
                s%kind = FAD_ASSIGN
                s%target = trim(loops(k)%carried_name(i))//suffix
                s%value = zero
                ignored = adjoint%add_stmt(s)
            end do
            do i = 1, loops(k)%n_body
                if (loops(k)%body_is_accum(i)) cycle
                if (loops(k)%body_sign(i) == ELEMENT_TARGET) then
                    ! The written array needs an adjoint array of its own,
                    ! declared as a local unless it is already an argument.
                    call declare_array_adjoint(primal, adjoint, &
                        target_base(trim(loops(k)%body_lhs(i))), suffix, zero)
                    cycle
                end if
                call declare_adjoint(primal, adjoint, ssa, &
                                     trim(loops(k)%body_lhs(i)), suffix)
                s%kind = FAD_ASSIGN
                s%target = trim(loops(k)%body_lhs(i))//suffix
                s%value = zero
                ignored = adjoint%add_stmt(s)
            end do
        end do
        do k = 1, n_branches
            call zero_branch_adjoints(primal, adjoint, ssa, branches(k), suffix, &
                                      active, zero)
        end do
        do i = 1, size(spec%independents)
            di = primal%decl_index(trim(spec%independents(i)))
            if (di == 0) cycle
            s%kind = FAD_ASSIGN
            if (primal%decls(di)%is_array) then
                s%target = trim(spec%independents(i))//suffix//"(:)"
            else
                s%target = trim(spec%independents(i))//suffix
            end if
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do

        call ssa_lookup(ssa, dependent, final_name)
        s%kind = FAD_ASSIGN
        s%target = final_name//suffix
        s%value = adjoint%add_expr(expr_var(dependent//suffix))
        ignored = adjoint%add_stmt(s)

        n_tmp = 0

        ! Exact reverse of forward program order.
        do k = n_order, 1, -1
            select case (order_kind(k))
            case (ORDER_STMT)
                i = order_index(k)
                if (.not. adjoint_is_live(primal, ssa, lhs_names(i), active)) cycle
                seed_expr = adjoint%add_expr(expr_var(trim(lhs_names(i))//suffix))
                call accumulate(primal, adjoint, rhs_exprs(i), seed_expr, ssa, &
                                suffix, active, n_tmp, status)
            case (ORDER_BRANCH)
                call emit_branch_reverse(primal, adjoint, ssa, &
                                         branches(order_index(k)), suffix, &
                                         active, n_tmp, status)
            case (ORDER_LOOP)
                call emit_loop_reverse(primal, adjoint, ssa, &
                                       loops(order_index(k)), suffix, &
                                       active, n_tmp, status)
            end select
            if (.not. status%ok) return
        end do

        ! Fusing the adjoint of a reduction back into its own primal loop turns
        ! two passes over the input arrays into one. The seed is loop-invariant
        ! and the adjoint work is per-element, so the fused loop computes the
        ! same values in the same order and stays parallelisable. Memory
        ! bandwidth, not arithmetic, is what bounds these kernels, so halving
        ! the traffic is the single largest win available here.
        if (n_loops == 1 .and. .not. loops(1)%taped .and. &
            can_fuse(primal, loops(1), dependent)) then
            call fuse_loop(adjoint, loops(1), suffix)
        end if
    end subroutine build_reverse_sweep

    subroutine zero_branch_adjoints(primal, adjoint, ssa, rec, suffix, active, &
                                    zero)
        !! Declare and zero every adjoint a branch introduces.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(branch_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: active(:)
        integer, intent(in) :: zero
        type(fad_stmt_t) :: s
        integer :: i, ignored

        do i = 1, rec%n_then
            if (.not. adjoint_is_live(primal, ssa, rec%then_lhs(i), active)) cycle
            call declare_adjoint(primal, adjoint, ssa, trim(rec%then_lhs(i)), suffix)
            s%kind = FAD_ASSIGN
            s%target = trim(rec%then_lhs(i))//suffix
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do
        do i = 1, rec%n_else
            if (.not. adjoint_is_live(primal, ssa, rec%else_lhs(i), active)) cycle
            call declare_adjoint(primal, adjoint, ssa, trim(rec%else_lhs(i)), suffix)
            s%kind = FAD_ASSIGN
            s%target = trim(rec%else_lhs(i))//suffix
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do
        do i = 1, rec%n_merge
            if (.not. adjoint_is_live(primal, ssa, rec%merge_name(i), active)) cycle
            call declare_adjoint(primal, adjoint, ssa, trim(rec%merge_name(i)), &
                                 suffix)
            s%kind = FAD_ASSIGN
            s%target = trim(rec%merge_name(i))//suffix
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do
    end subroutine zero_branch_adjoints

    subroutine emit_branch_reverse(primal, adjoint, ssa, rec, suffix, active, &
                                   n_tmp, status)
        !! The adjoint of one if/else.
        !!
        !! The condition is re-evaluated, not recorded: its operands are SSA
        !! values from before the branch, so they still hold what the forward
        !! sweep tested. The reverse sweep then takes the same arm, pushes the
        !! merge adjoint into that arm's final value, and unwinds the arm.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(branch_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: active(:)
        integer, intent(inout) :: n_tmp
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        integer :: i, ignored, seed_expr, lhs

        s%kind = FAD_IF
        s%value = rec%cond
        ignored = adjoint%add_stmt(s)

        do i = 1, rec%n_merge
            if (.not. adjoint_is_live(primal, ssa, rec%merge_from_then(i), &
                                      active)) cycle
            s%kind = FAD_ASSIGN
            s%target = trim(rec%merge_from_then(i))//suffix
            lhs = adjoint%add_expr(expr_var(trim(rec%merge_from_then(i))//suffix))
            seed_expr = adjoint%add_expr(expr_var(trim(rec%merge_name(i))//suffix))
            s%value = fad_add(adjoint, lhs, seed_expr)
            ignored = adjoint%add_stmt(s)
        end do
        do i = rec%n_then, 1, -1
            if (.not. adjoint_is_live(primal, ssa, rec%then_lhs(i), active)) cycle
            seed_expr = adjoint%add_expr(expr_var(trim(rec%then_lhs(i))//suffix))
            call accumulate(primal, adjoint, rec%then_rhs(i), seed_expr, ssa, &
                            suffix, active, n_tmp, status)
            if (.not. status%ok) return
        end do

        s%kind = FAD_ELSE
        s%value = 0
        ignored = adjoint%add_stmt(s)

        do i = 1, rec%n_merge
            if (.not. adjoint_is_live(primal, ssa, rec%merge_from_else(i), &
                                      active)) cycle
            s%kind = FAD_ASSIGN
            s%target = trim(rec%merge_from_else(i))//suffix
            lhs = adjoint%add_expr(expr_var(trim(rec%merge_from_else(i))//suffix))
            seed_expr = adjoint%add_expr(expr_var(trim(rec%merge_name(i))//suffix))
            s%value = fad_add(adjoint, lhs, seed_expr)
            ignored = adjoint%add_stmt(s)
        end do
        do i = rec%n_else, 1, -1
            if (.not. adjoint_is_live(primal, ssa, rec%else_lhs(i), active)) cycle
            seed_expr = adjoint%add_expr(expr_var(trim(rec%else_lhs(i))//suffix))
            call accumulate(primal, adjoint, rec%else_rhs(i), seed_expr, ssa, &
                            suffix, active, n_tmp, status)
            if (.not. status%ok) return
        end do

        s%kind = FAD_END_IF
        s%value = 0
        ignored = adjoint%add_stmt(s)
    end subroutine emit_branch_reverse

    subroutine declare_array_adjoint(primal, adjoint, base, suffix, zero)
        !! Declare and zero the adjoint array of an array written in a loop.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        character(len=*), intent(in) :: base, suffix
        integer, intent(in) :: zero
        type(fad_decl_t) :: d
        type(fad_stmt_t) :: s
        integer :: di, ignored

        di = primal%decl_index(base)
        if (di == 0) return
        if (adjoint%decl_index(base//suffix) > 0) return
        d = primal%decls(di)
        d%name = base//suffix
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        ignored = adjoint%add_decl(d)
        s%kind = FAD_ASSIGN
        s%target = base//suffix//"(:)"
        s%value = zero
        ignored = adjoint%add_stmt(s)
    end subroutine declare_array_adjoint

    function adjoint_element(target, suffix) result(name)
        !! `c(i)` becomes `c_b(i)`: the suffix goes on the array name, not on
        !! the whole reference.
        character(len=*), intent(in) :: target, suffix
        character(len=:), allocatable :: name
        integer :: pos

        pos = index(target, "(")
        if (pos > 0) then
            name = target(1:pos - 1)//suffix//target(pos:)
        else
            name = target//suffix
        end if
    end function adjoint_element

    logical function can_fuse(primal, rec, dependent) result(yes)
        !! Fusion is safe when the loop's accumulator adjoint is already final
        !! when the forward loop runs.
        !!
        !! That holds when nothing after the loop feeds the accumulator except
        !! plain copies establishing the dependent: the accumulator's adjoint is
        !! then exactly the incoming seed, known before the loop starts.
        type(fad_proc_t), intent(in) :: primal
        type(loop_record_t), intent(in) :: rec
        character(len=*), intent(in) :: dependent
        integer :: i
        logical :: seen_loop

        yes = .false.
        seen_loop = .false.
        do i = 1, primal%n_stmts
            if (i == rec%shape%last) then
                seen_loop = .true.
                cycle
            end if
            if (.not. seen_loop) cycle
            ! After the loop, only a bare copy of an accumulator is allowed.
            if (primal%stmts(i)%kind /= FAD_ASSIGN) return
            if (primal%stmts(i)%target /= dependent) return
            if (primal%stmts(i)%value <= 0) return
            if (primal%exprs(primal%stmts(i)%value)%kind /= FAD_VAR) return
        end do
        yes = seen_loop
    end function can_fuse

    subroutine fuse_loop(adjoint, rec, suffix)
        !! Move the adjoint loop's body into the primal loop and drop the
        !! now-empty second loop.
        !!
        !! The adjoint setup - zeroing every adjoint and seeding the dependent's
        !! - was emitted after the primal loop, because the reverse sweep is
        !! built after the forward one. Fusing moves the loop bodies together,
        !! so that setup has to move **before** the loop: the fused body reads
        !! the seed on its first iteration. Anything else in the tail, such as
        !! publishing the accumulator's final value, still belongs after.
        type(fad_proc_t), intent(inout) :: adjoint
        type(loop_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        type(fad_stmt_t), allocatable :: fused(:)
        integer :: i, rev_do, rev_end, n_new

        rev_do = 0
        do i = rec%end_do_stmt + 1, adjoint%n_stmts
            if (adjoint%stmts(i)%kind == FAD_DO) then
                rev_do = i
                exit
            end if
        end do
        if (rev_do == 0) return

        rev_end = 0
        do i = rev_do + 1, adjoint%n_stmts
            if (adjoint%stmts(i)%kind == FAD_END_DO) then
                rev_end = i
                exit
            end if
        end do
        if (rev_end == 0) return
        if (rec%do_stmt <= 0 .or. rec%end_do_stmt <= rec%do_stmt) return

        allocate (fused(adjoint%n_stmts))
        n_new = 0

        ! Everything before the primal loop, unchanged.
        do i = 1, rec%do_stmt - 1
            n_new = n_new + 1
            fused(n_new) = adjoint%stmts(i)
        end do

        ! Adjoint setup, hoisted above the loop.
        do i = rec%end_do_stmt + 1, rev_do - 1
            if (.not. is_adjoint_setup(adjoint%stmts(i), suffix)) cycle
            n_new = n_new + 1
            fused(n_new) = adjoint%stmts(i)
        end do

        ! The loop header, both bodies, and one `end do`.
        n_new = n_new + 1
        fused(n_new) = adjoint%stmts(rec%do_stmt)
        do i = rec%do_stmt + 1, rec%end_do_stmt - 1
            n_new = n_new + 1
            fused(n_new) = adjoint%stmts(i)
        end do
        do i = rev_do + 1, rev_end - 1
            n_new = n_new + 1
            fused(n_new) = adjoint%stmts(i)
        end do
        n_new = n_new + 1
        fused(n_new) = adjoint%stmts(rec%end_do_stmt)

        ! The rest of the tail, in order, minus what was hoisted or dropped.
        do i = rec%end_do_stmt + 1, adjoint%n_stmts
            if (i >= rev_do .and. i <= rev_end) cycle
            if (is_adjoint_setup(adjoint%stmts(i), suffix)) cycle
            n_new = n_new + 1
            fused(n_new) = adjoint%stmts(i)
        end do

        adjoint%stmts(1:n_new) = fused(1:n_new)
        adjoint%n_stmts = n_new
    end subroutine fuse_loop

    logical function is_adjoint_setup(s, suffix) result(yes)
        !! True for an assignment whose target is an adjoint variable, which is
        !! how the zeroing and seeding statements are recognised.
        type(fad_stmt_t), intent(in) :: s
        character(len=*), intent(in) :: suffix
        integer :: cut

        yes = .false.
        if (s%kind /= FAD_ASSIGN) return
        if (.not. allocated(s%target)) return
        cut = index(s%target, "(")
        if (cut > 0) then
            yes = ends_with(s%target(1:cut - 1), suffix)
        else
            yes = ends_with(s%target, suffix)
        end if
    end function is_adjoint_setup

    logical function ends_with(text, tail) result(yes)
        !! Suffix test.
        character(len=*), intent(in) :: text, tail

        yes = .false.
        if (len(text) < len(tail)) return
        yes = text(len(text) - len(tail) + 1:) == tail
    end function ends_with

    subroutine emit_loop_reverse(primal, adjoint, ssa, rec, suffix, active, &
                                 n_tmp, status)
        !! The adjoint of one reduction loop.
        !!
        !! Emitted in **ascending** index order on purpose. The accumulator's
        !! adjoint is loop-invariant and each iteration touches its own array
        !! elements, so there is no loop-carried dependence to reverse; running
        !! forwards keeps the memory access pattern identical to the primal's
        !! and leaves the loop vectorisable and parallelisable. A taped adjoint
        !! of the same loop has neither property.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(loop_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: active(:)
        integer, intent(inout) :: n_tmp
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        integer :: i, ignored, seed_expr, carried_seed

        ! A taped loop must run backwards: the carried variable's adjoint
        ! flows from later iterations to earlier ones, which is exactly the
        ! dependence the untaped cases do not have.
        s%kind = FAD_DO
        s%target = rec%var
        if (rec%taped) then
            s%lo = rec%hi
            s%hi = rec%lo
            s%step = adjoint%add_expr(expr_const("-1"))
        else
            s%lo = rec%lo
            s%hi = rec%hi
            s%step = rec%step
        end if
        ignored = adjoint%add_stmt(s)

        ! Restore each carried value as it was on entry to this iteration, so
        ! the body can be recomputed exactly as the forward sweep ran it.
        if (rec%taped) then
            do i = 1, rec%n_carried
                s%kind = FAD_ASSIGN
                s%target = trim(rec%carried_name(i))
                s%value = adjoint%add_expr(expr_var( &
                    trim(rec%carried_name(i))//"_tape("//rec%tape_index//")"))
                ignored = adjoint%add_stmt(s)
            end do
        end if

        ! Recompute the per-iteration temporaries rather than storing them.
        ! An array-element write is not recomputed: its value is still in the
        ! array, and rewriting it would be pure waste.
        do i = 1, rec%n_body
            if (rec%body_is_accum(i)) cycle
            if (rec%body_sign(i) == ELEMENT_TARGET) cycle
            if (rec%body_sign(i) == CARRIED_TARGET) cycle
            s%kind = FAD_ASSIGN
            s%target = trim(rec%body_lhs(i))
            s%value = rec%body_rhs(i)
            ignored = adjoint%add_stmt(s)
        end do

        ! The adjoint arriving from the next iteration belongs to this
        ! iteration's final version, so hand it over and clear the carrier.
        if (rec%taped) then
            do i = 1, rec%n_carried
                s%kind = FAD_ASSIGN
                s%target = trim(rec%carried_last(i))//suffix
                s%value = adjoint%add_expr( &
                    expr_var(trim(rec%carried_name(i))//suffix))
                ignored = adjoint%add_stmt(s)
                s%kind = FAD_ASSIGN
                s%target = trim(rec%carried_name(i))//suffix
                s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
                ignored = adjoint%add_stmt(s)
            end do
        end if

        ! Then unwind the body in reverse order, whatever each statement is.
        ! Kind-by-kind passes were wrong: an element write's adjoint is fed by
        ! the accumulation that reads it, so the accumulation has to be
        ! processed first, which reverse program order gives for free.
        do i = rec%n_body, 1, -1
            if (rec%body_is_accum(i)) then
                seed_expr = adjoint%add_expr( &
                    expr_var(trim(rec%body_lhs(i))//suffix))
                if (rec%body_sign(i) < 0) seed_expr = fad_neg(adjoint, seed_expr)
                call accumulate(primal, adjoint, rec%body_rhs(i), seed_expr, &
                                ssa, suffix, active, n_tmp, status)
                if (.not. status%ok) return

            else if (rec%body_sign(i) == ELEMENT_TARGET) then
                seed_expr = adjoint%add_expr(expr_var(adjoint_element( &
                    trim(rec%body_lhs(i)), suffix)))
                call accumulate(primal, adjoint, rec%body_rhs(i), seed_expr, &
                                ssa, suffix, active, n_tmp, status)
                if (.not. status%ok) return
                ! That element's adjoint belongs to this iteration alone.
                s%kind = FAD_ASSIGN
                s%target = adjoint_element(trim(rec%body_lhs(i)), suffix)
                s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
                ignored = adjoint%add_stmt(s)

            else
                if (.not. adjoint_is_live(primal, ssa, rec%body_lhs(i), &
                                          active)) cycle
                seed_expr = adjoint%add_expr( &
                    expr_var(trim(rec%body_lhs(i))//suffix))
                call accumulate(primal, adjoint, rec%body_rhs(i), seed_expr, &
                                ssa, suffix, active, n_tmp, status)
                if (.not. status%ok) return
                s%kind = FAD_ASSIGN
                s%target = trim(rec%body_lhs(i))//suffix
                s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
                ignored = adjoint%add_stmt(s)
            end if
        end do

        s%kind = FAD_END_DO
        s%value = 0
        ignored = adjoint%add_stmt(s)

        ! What the carrier holds after the reverse loop is the adjoint of the
        ! value the loop was entered with, so pass it to that value's own name.
        do i = 1, rec%n_carried
            if (trim(rec%carried_in(i)) == trim(rec%carried_name(i))) cycle
            s%kind = FAD_ASSIGN
            s%target = trim(rec%carried_in(i))//suffix
            block
                integer :: lhs_e, rhs_e
                lhs_e = adjoint%add_expr( &
                    expr_var(trim(rec%carried_in(i))//suffix))
                rhs_e = adjoint%add_expr( &
                    expr_var(trim(rec%carried_name(i))//suffix))
                s%value = fad_add(adjoint, lhs_e, rhs_e)
            end block
            ignored = adjoint%add_stmt(s)
        end do
    end subroutine emit_loop_reverse

    logical function adjoint_is_live(primal, ssa, ssa_name, active) result(yes)
        !! Whether an SSA value's adjoint is worth computing.
        type(fad_proc_t), intent(in) :: primal
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: ssa_name
        logical, intent(in) :: active(:)
        character(len=:), allocatable :: base
        integer :: di

        yes = .false.
        call ssa_base_of(ssa, trim(ssa_name), base)
        di = primal%decl_index(base)
        if (di > 0) yes = active(di)
    end function adjoint_is_live

    subroutine declare_adjoint(primal, adjoint, ssa, ssa_name, suffix)
        !! Declare the adjoint local for an SSA value.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: ssa_name, suffix
        type(fad_decl_t) :: d
        character(len=:), allocatable :: base
        integer :: di, ignored

        call ssa_base_of(ssa, ssa_name, base)
        di = primal%decl_index(base)
        if (di == 0) return
        d = primal%decls(di)
        d%name = ssa_name//suffix
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        ignored = adjoint%add_decl(d)
    end subroutine declare_adjoint

    recursive subroutine accumulate(primal, adjoint, idx, seed, ssa, suffix, &
                                    active, n_tmp, status)
        !! Push the adjoint `seed` through expression `idx`.
        !!
        !! Partials are obtained by seeding the forward rule with one for the
        !! argument of interest and zero for the rest. That is the transpose,
        !! and it means this module owns no derivative knowledge of its own.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        integer, intent(in) :: idx, seed
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: active(:)
        integer, intent(inout) :: n_tmp
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        integer, allocatable :: dargs(:), node_args(:)
        integer :: i, j, one, partial, contrib, child_seed, ignored, di
        integer :: node_kind, lhs
        character(len=:), allocatable :: base, node_text

        if (idx <= 0 .or. idx > adjoint%n_exprs) return
        if (seed == 0) return

        ! Snapshot the node before anything below grows the arena. Passing
        ! `adjoint%exprs(idx)%args(j)` straight into a call that also takes
        ! `adjoint` is the same aliasing hazard the rule builders avoid: the
        ! arena reallocates and the actual argument is left dangling.
        node_kind = adjoint%exprs(idx)%kind
        node_text = adjoint%exprs(idx)%text
        node_args = adjoint%exprs(idx)%args

        select case (node_kind)
        case (FAD_CONST)
            return

        case (FAD_VAR)
            call ssa_base_of(ssa, node_text, base)
            di = primal%decl_index(base)
            if (di > 0) then
                if (.not. active(di)) return
            end if
            s%kind = FAD_ASSIGN
            s%target = node_text//suffix
            lhs = adjoint%add_expr(expr_var(node_text//suffix))
            s%value = fad_add(adjoint, lhs, seed)
            ignored = adjoint%add_stmt(s)

        case (FAD_BINOP)
            one = fad_real(adjoint, "1.0")
            do j = 1, 2
                ! Nothing downstream of a constant subtree has an adjoint, so
                ! computing its partial would only emit dead temporaries.
                if (.not. carries_adjoint(primal, adjoint, node_args(j), ssa, &
                                          active)) cycle
                if (j == 1) then
                    partial = jvp_binop(adjoint, node_text, node_args(1), &
                                        node_args(2), one, 0)
                else
                    partial = jvp_binop(adjoint, node_text, node_args(1), &
                                        node_args(2), 0, one)
                end if
                if (partial == 0) cycle
                contrib = fad_mul(adjoint, seed, partial)
                call materialise(primal, adjoint, contrib, ssa, n_tmp, child_seed)
                call accumulate(primal, adjoint, node_args(j), child_seed, ssa, &
                                suffix, active, n_tmp, status)
                if (.not. status%ok) return
            end do

        case (FAD_UNOP)
            one = fad_real(adjoint, "1.0")
            partial = jvp_unop(adjoint, node_text, node_args(1), one)
            if (partial == 0) return
            contrib = fad_mul(adjoint, seed, partial)
            call materialise(primal, adjoint, contrib, ssa, n_tmp, child_seed)
            call accumulate(primal, adjoint, node_args(1), child_seed, ssa, &
                            suffix, active, n_tmp, status)

        case (FAD_CALL)
            if (.not. has_rule(node_text)) then
                status%ok = .false.
                status%message = "no derivative rule for '"//node_text// &
                    "'; register one with fad_add_rule, or keep it out of "// &
                    "the active path"
                return
            end if
            one = fad_real(adjoint, "1.0")
            allocate (dargs(size(node_args)))
            do j = 1, size(node_args)
                if (.not. carries_adjoint(primal, adjoint, node_args(j), ssa, &
                                          active)) cycle
                dargs = 0
                dargs(j) = one
                partial = jvp_call(adjoint, node_text, node_args, dargs)
                if (partial == 0) cycle
                contrib = fad_mul(adjoint, seed, partial)
                call materialise(primal, adjoint, contrib, ssa, n_tmp, child_seed)
                call accumulate(primal, adjoint, node_args(j), child_seed, ssa, &
                                suffix, active, n_tmp, status)
                if (.not. status%ok) return
            end do

        case (FAD_INDEX)
            ! `a(i)` contributes to `a_b(i)`. Inside a reduction loop each
            ! iteration touches a different element, so these scatters carry no
            ! loop-carried dependence.
            call ssa_base_of(ssa, node_text, base)
            di = primal%decl_index(base)
            if (di > 0) then
                if (.not. active(di)) return
            end if
            block
                type(fad_expr_t) :: target_expr
                integer :: read_idx
                target_expr%kind = FAD_INDEX
                target_expr%text = node_text//suffix
                target_expr%args = node_args
                read_idx = adjoint%add_expr(target_expr)
                s%kind = FAD_ASSIGN
                s%target = index_text(adjoint, read_idx)
                s%value = fad_add(adjoint, read_idx, seed)
                ignored = adjoint%add_stmt(s)
            end block
        end select
    end subroutine accumulate

    function index_text(p, idx) result(text)
        !! An array element reference as text, for use as an assignment target.
        use fortad_emit, only: emit_expr
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=:), allocatable :: text

        text = emit_expr(p, idx)
    end function index_text

    recursive logical function carries_adjoint(primal, adjoint, idx, ssa, active) &
        result(yes)
        !! True when the subtree reads at least one active variable, and so has
        !! an adjoint worth accumulating.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(in) :: adjoint
        integer, intent(in) :: idx
        type(ssa_map_t), intent(in) :: ssa
        logical, intent(in) :: active(:)
        character(len=:), allocatable :: base
        integer :: i, di

        yes = .false.
        if (idx <= 0 .or. idx > adjoint%n_exprs) return
        select case (adjoint%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            call ssa_base_of(ssa, adjoint%exprs(idx)%text, base)
            di = primal%decl_index(base)
            if (di > 0) then
                if (active(di)) then
                    yes = .true.
                    return
                end if
            end if
        end select
        do i = 1, size(adjoint%exprs(idx)%args)
            if (carries_adjoint(primal, adjoint, adjoint%exprs(idx)%args(i), &
                                ssa, active)) then
                yes = .true.
                return
            end if
        end do
    end function carries_adjoint

    subroutine materialise(primal, adjoint, expr, ssa, n_tmp, out, force)
        !! Bind a compound seed to a scalar local.
        !!
        !! This is statement-level preaccumulation: the partial chain for one
        !! statement is computed once into a register-sized temporary instead of
        !! being rebuilt inside every leaf's accumulation.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        integer, intent(in) :: expr
        type(ssa_map_t), intent(in) :: ssa
        integer, intent(inout) :: n_tmp
        integer, intent(out) :: out
        !! Bind even a bare variable reference. Needed when the caller is about
        !! to overwrite that variable and still needs its old value.
        logical, intent(in), optional :: force
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=32) :: buf
        character(len=:), allocatable :: name
        integer :: ignored
        logical :: force_bind

        force_bind = .false.
        if (present(force)) force_bind = force

        if (expr <= 0) then
            out = 0
            return
        end if
        if (adjoint%exprs(expr)%kind == FAD_CONST) then
            out = expr
            return
        end if
        if (adjoint%exprs(expr)%kind == FAD_VAR .and. .not. force_bind) then
            out = expr
            return
        end if

        n_tmp = n_tmp + 1
        write (buf, '(i0)') n_tmp
        name = "fad_t"//trim(buf)

        d%name = name
        d%type_name = real_type_of(primal)
        d%intent = FAD_INTENT_NONE
        ignored = adjoint%add_decl(d)

        s%kind = FAD_ASSIGN
        s%target = name
        s%value = expr
        ignored = adjoint%add_stmt(s)
        out = adjoint%add_expr(expr_var(name))
    end subroutine materialise

    function real_type_of(primal) result(type_name)
        !! The real type the primal works in.
        type(fad_proc_t), intent(in) :: primal
        character(len=:), allocatable :: type_name
        integer :: i

        type_name = "real(8)"
        do i = 1, primal%n_decls
            if (.not. allocated(primal%decls(i)%type_name)) cycle
            if (index(primal%decls(i)%type_name, "real") == 1) then
                type_name = primal%decls(i)%type_name
                return
            end if
        end do
    end function real_type_of

    recursive integer function copy_renamed(src, dst, idx, ssa) result(out)
        !! Copy a primal expression, replacing each variable read with its
        !! current SSA name.
        type(fad_proc_t), intent(in) :: src
        type(fad_proc_t), intent(inout) :: dst
        integer, intent(in) :: idx
        type(ssa_map_t), intent(in) :: ssa
        type(fad_expr_t) :: e
        integer, allocatable :: args(:)
        character(len=:), allocatable :: name
        integer :: i

        out = 0
        if (idx <= 0 .or. idx > src%n_exprs) return
        e%kind = src%exprs(idx)%kind
        if (e%kind == FAD_VAR) then
            call ssa_lookup(ssa, src%exprs(idx)%text, name)
            e%text = name
        else
            e%text = src%exprs(idx)%text
        end if
        allocate (args(size(src%exprs(idx)%args)))
        do i = 1, size(args)
            args(i) = copy_renamed(src, dst, src%exprs(idx)%args(i), ssa)
        end do
        e%args = args
        out = dst%add_expr(e)
    end function copy_renamed

    ! ------------------------------------------------------------ the SSA map

    subroutine ssa_init(primal, ssa)
        !! Every declared name starts as its own current version.
        type(fad_proc_t), intent(in) :: primal
        type(ssa_map_t), intent(out) :: ssa
        integer :: i

        ssa%n = primal%n_decls
        allocate (ssa%base(max(1, ssa%n)))
        allocate (ssa%current(max(1, ssa%n)))
        allocate (ssa%version(max(1, ssa%n)))
        ssa%base = ""
        ssa%current = ""
        ssa%version = 0
        do i = 1, primal%n_decls
            ssa%base(i) = primal%decls(i)%name
            ssa%current(i) = primal%decls(i)%name
        end do
    end subroutine ssa_init

    subroutine ssa_fresh(ssa, name, fresh)
        !! Allocate the next version of `name` and make it current.
        type(ssa_map_t), intent(inout) :: ssa
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: fresh
        character(len=32) :: buf
        integer :: i

        fresh = name
        do i = 1, ssa%n
            if (trim(ssa%base(i)) /= name) cycle
            ssa%version(i) = ssa%version(i) + 1
            write (buf, '(i0)') ssa%version(i)
            fresh = name//"_v"//trim(buf)
            ssa%current(i) = fresh
            return
        end do
    end subroutine ssa_fresh

    subroutine ssa_set(ssa, name, value)
        !! Force the current version of `name`, used when SSA is suspended
        !! inside a loop body.
        type(ssa_map_t), intent(inout) :: ssa
        character(len=*), intent(in) :: name, value
        integer :: i

        do i = 1, ssa%n
            if (trim(ssa%base(i)) == name) then
                ssa%current(i) = value
                return
            end if
        end do
    end subroutine ssa_set

    logical function is_known_name(names, n, name) result(yes)
        !! Membership test over a fixed-width name list.
        character(len=64), intent(in) :: names(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        do i = 1, n
            if (trim(names(i)) == name) then
                yes = .true.
                return
            end if
        end do
    end function is_known_name

    integer function ssa_version(ssa, name) result(v)
        !! Current version counter of `name`.
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: name
        integer :: i

        v = 0
        do i = 1, ssa%n
            if (trim(ssa%base(i)) == name) then
                v = ssa%version(i)
                return
            end if
        end do
    end function ssa_version

    subroutine ssa_advance_to(ssa, name, floor_version)
        !! Raise `name`'s version counter to at least `floor_version`.
        type(ssa_map_t), intent(inout) :: ssa
        character(len=*), intent(in) :: name
        integer, intent(in) :: floor_version
        integer :: i

        do i = 1, ssa%n
            if (trim(ssa%base(i)) == name) then
                ssa%version(i) = max(ssa%version(i), floor_version)
                return
            end if
        end do
    end subroutine ssa_advance_to

    subroutine ssa_lookup(ssa, name, current)
        !! The current version of `name`.
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: current
        integer :: i

        current = name
        do i = 1, ssa%n
            if (trim(ssa%base(i)) == name) then
                current = trim(ssa%current(i))
                return
            end if
        end do
    end subroutine ssa_lookup

    subroutine ssa_base_of(ssa, ssa_name, base)
        !! The declared variable an SSA name belongs to.
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: ssa_name
        character(len=:), allocatable, intent(out) :: base
        integer :: pos

        pos = index(ssa_name, "_v", back=.true.)
        if (pos > 1) then
            if (verify(ssa_name(pos + 2:), "0123456789") == 0 .and. &
                len(ssa_name) > pos + 1) then
                base = ssa_name(1:pos - 1)
                return
            end if
        end if
        base = ssa_name
    end subroutine ssa_base_of

    logical function is_dummy(p, name) result(yes)
        !! True when `name` is a dummy argument of `p`.
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

end module fortad_reverse
