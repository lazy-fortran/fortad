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
    use fortad_reverse_loop, only: loop_shape_t, analyse_loop, LOOP_OK
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
        character(len=64), allocatable :: body_lhs(:)
        integer, allocatable :: body_rhs(:)
        logical, allocatable :: body_is_accum(:)
        !! +1 for `s = s + e`, -1 for `s = s - e`, 0 for a temporary.
        integer, allocatable :: body_sign(:)
        integer :: n_body = 0
    end type loop_record_t

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
        integer :: n_rec, n_loops

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
                                 n_rec, loops, n_loops, status)
        if (.not. status%ok) return
        call build_reverse_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
                                 n_rec, loops, n_loops, spec, dependent, suffix, &
                                 active, status)
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
        integer :: i

        do i = 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                if (index(primal%stmts(i)%target, "(") > 0) then
                    status%ok = .false.
                    status%message = "reverse mode: assignment to an array "// &
                        "element is not supported yet; forward mode handles it"
                    return
                end if
            case (FAD_DO)
                call analyse_loop(primal, i, shape)
                if (shape%status /= LOOP_OK) then
                    status%ok = .false.
                    status%message = shape%message
                    return
                end if
            case (FAD_END_DO)
                continue
            case (FAD_IF, FAD_ELSE, FAD_END_IF)
                status%ok = .false.
                status%message = "reverse mode: branches need control-flow "// &
                    "reversal, which is the next milestone"
                return
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
                di = primal%decl_index(primal%stmts(j)%target)
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
                di = primal%decl_index(primal%stmts(j)%target)
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
                                   n_rec, loops, n_loops, status)
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
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=:), allocatable :: fresh, current
        type(loop_shape_t) :: shape
        integer :: i, k, di, ignored

        call ssa_init(primal, ssa)
        allocate (lhs_names(max(1, primal%n_stmts)))
        allocate (rhs_exprs(max(1, primal%n_stmts)))
        allocate (loops(max(1, primal%n_stmts)))
        n_rec = 0
        n_loops = 0

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
                i = shape%last + 1

            case default
                i = i + 1
            end select
        end do
    end subroutine build_forward_sweep

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
        character(len=:), allocatable :: fresh, incoming
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

        s%kind = FAD_DO
        s%target = rec%var
        s%lo = rec%lo
        s%hi = rec%hi
        s%step = rec%step
        ignored = adjoint%add_stmt(s)

        allocate (rec%body_lhs(shape%last - shape%first))
        allocate (rec%body_rhs(shape%last - shape%first))
        allocate (rec%body_is_accum(shape%last - shape%first))
        allocate (rec%body_sign(shape%last - shape%first))
        rec%n_body = 0

        do i = shape%first + 1, shape%last - 1
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            s%kind = FAD_ASSIGN
            s%value = copy_renamed(primal, adjoint, primal%stmts(i)%value, ssa)
            call ssa_lookup(ssa, primal%stmts(i)%target, fresh)
            s%target = fresh
            ignored = adjoint%add_stmt(s)

            rec%n_body = rec%n_body + 1
            rec%body_lhs(rec%n_body) = fresh
            rec%body_is_accum(rec%n_body) = &
                is_known_name(shape%accumulators, shape%n_accumulators, &
                              primal%stmts(i)%target)
            if (rec%body_is_accum(rec%n_body)) then
                ! `s = s +/- e`: the reverse sweep needs only `e` and the sign.
                rec%body_rhs(rec%n_body) = adjoint%exprs(s%value)%args(2)
                if (trim(adjoint%exprs(s%value)%text) == "-") then
                    rec%body_sign(rec%n_body) = -1
                else
                    rec%body_sign(rec%n_body) = 1
                end if
            else
                rec%body_rhs(rec%n_body) = s%value
                rec%body_sign(rec%n_body) = 0
            end if
        end do

        s%kind = FAD_END_DO
        s%value = 0
        ignored = adjoint%add_stmt(s)
    end subroutine emit_loop_forward

    subroutine build_reverse_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
                                   n_rec, loops, n_loops, spec, dependent, &
                                   suffix, active, status)
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
            do i = 1, loops(k)%n_body
                if (loops(k)%body_is_accum(i)) cycle
                call declare_adjoint(primal, adjoint, ssa, &
                                     trim(loops(k)%body_lhs(i)), suffix)
                s%kind = FAD_ASSIGN
                s%target = trim(loops(k)%body_lhs(i))//suffix
                s%value = zero
                ignored = adjoint%add_stmt(s)
            end do
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

        do i = n_rec, 1, -1
            if (.not. adjoint_is_live(primal, ssa, lhs_names(i), active)) cycle
            seed_expr = adjoint%add_expr(expr_var(trim(lhs_names(i))//suffix))
            call accumulate(primal, adjoint, rhs_exprs(i), seed_expr, ssa, &
                            suffix, active, n_tmp, status)
            if (.not. status%ok) return
        end do

        do k = n_loops, 1, -1
            call emit_loop_reverse(primal, adjoint, ssa, loops(k), suffix, &
                                   active, n_tmp, status)
            if (.not. status%ok) return
        end do
    end subroutine build_reverse_sweep

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
        integer :: i, ignored, seed_expr

        s%kind = FAD_DO
        s%target = rec%var
        s%lo = rec%lo
        s%hi = rec%hi
        s%step = rec%step
        ignored = adjoint%add_stmt(s)

        ! Recompute the per-iteration temporaries rather than storing them.
        do i = 1, rec%n_body
            if (rec%body_is_accum(i)) cycle
            s%kind = FAD_ASSIGN
            s%target = trim(rec%body_lhs(i))
            s%value = rec%body_rhs(i)
            ignored = adjoint%add_stmt(s)
        end do

        ! Push each accumulation's adjoint into its operands, in reverse order.
        do i = rec%n_body, 1, -1
            if (.not. rec%body_is_accum(i)) cycle
            seed_expr = adjoint%add_expr(expr_var(trim(rec%body_lhs(i))//suffix))
            if (rec%body_sign(i) < 0) seed_expr = fad_neg(adjoint, seed_expr)
            call accumulate(primal, adjoint, rec%body_rhs(i), seed_expr, ssa, &
                            suffix, active, n_tmp, status)
            if (.not. status%ok) return
        end do

        ! A temporary's adjoint is local to one iteration: push it on, then
        ! clear it so the next iteration starts from zero.
        do i = rec%n_body, 1, -1
            if (rec%body_is_accum(i)) cycle
            if (.not. adjoint_is_live(primal, ssa, rec%body_lhs(i), active)) cycle
            seed_expr = adjoint%add_expr(expr_var(trim(rec%body_lhs(i))//suffix))
            call accumulate(primal, adjoint, rec%body_rhs(i), seed_expr, ssa, &
                            suffix, active, n_tmp, status)
            if (.not. status%ok) return
            s%kind = FAD_ASSIGN
            s%target = trim(rec%body_lhs(i))//suffix
            s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
            ignored = adjoint%add_stmt(s)
        end do

        s%kind = FAD_END_DO
        s%value = 0
        ignored = adjoint%add_stmt(s)
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
                    "'; add one to fortad_rules"
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

    subroutine materialise(primal, adjoint, expr, ssa, n_tmp, out)
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
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=32) :: buf
        character(len=:), allocatable :: name
        integer :: ignored

        if (expr <= 0) then
            out = 0
            return
        end if
        if (adjoint%exprs(expr)%kind == FAD_VAR .or. &
            adjoint%exprs(expr)%kind == FAD_CONST) then
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
