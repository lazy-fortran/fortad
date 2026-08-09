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
        fad_base_name, fad_suffix_name, &
        FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, FAD_CALL, &
        FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, &
        FAD_ELSE, FAD_END_IF, FAD_CALL_STMT, FAD_INTENT_IN, &
        FAD_DIRECTIVE, FAD_SELECT_TYPE, FAD_TYPE_IS, FAD_CLASS_IS, &
        FAD_CLASS_DEFAULT, FAD_END_SELECT, &
        FAD_ALLOCATE, FAD_DEALLOCATE, FAD_MOVE_ALLOC, &
        FAD_INTENT_OUT, &
        FAD_INTENT_INOUT, FAD_INTENT_NONE, copy_decl
    use fortad_rules, only: jvp_binop, jvp_unop, jvp_call, has_rule, &
        fad_add, fad_mul, fad_div, fad_neg, fad_real, fad_fn1, fad_fn3
    use fortad_registry, only: call_rule_has, call_rule_lines, &
        call_rule_substitute
    use fortad_emit, only: emit_expr
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
        !! Whether the generated routine also returns the primal value.
        !!
        !! A caller driving an optimiser usually has the primal already, and
        !! asking for it back forces the whole forward computation to stay
        !! live. Dropping it lets dead-store elimination remove everything the
        !! gradient does not need - for a linear recurrence, that is the entire
        !! forward loop.
        logical :: with_primal = .true.
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
        !! Every level of the nest, outermost first.
        character(len=64), allocatable :: nest_var(:)
        integer, allocatable :: nest_lo(:), nest_hi(:), nest_step(:)
        integer :: n_levels = 0
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
        !! How many carried variables actually got a tape.
        integer :: n_taped_carried = 0
    end type loop_record_t

    !! Marker in `body_sign` for an array-element target, distinguishing it
    !! from a scalar temporary (0) and an accumulation term (+-1).
    integer, parameter :: ELEMENT_TARGET = 2

    !! Marker for a loop-carried variable whose per-iteration value is taped.
    integer, parameter :: CARRIED_TARGET = 3

    integer, parameter :: ORDER_STMT = 1
    integer, parameter :: ORDER_LOOP = 2
    integer, parameter :: ORDER_BRANCH = 3
    integer, parameter :: ORDER_CALL = 4
    integer, parameter :: ORDER_SELECT = 5

    type :: allocation_record_t
        !! One explicitly allocated, simple allocatable owner.
        character(len=:), allocatable :: owner
        !! For a polymorphic allocatable component, ``owner`` is the concrete
        !! derived-object shadow base and ``owner_path`` is the component
        !! descriptor whose lifetime is retained (for example
        !! ``box%field%payload``).  Keeping both avoids pretending that the
        !! enclosing derived object owns allocatable storage itself.
        character(len=:), allocatable :: owner_path
        character(len=:), allocatable :: previous_owner
        !! A literal selected element such as ``owners(2)`` for an
        !! allocatable polymorphic owner array.
        character(len=:), allocatable :: selected_owner
        character(len=:), allocatable :: source
        character(len=:), allocatable :: source_type
        logical :: active = .false.
        logical :: deallocated = .false.
        logical :: component = .false.
    end type allocation_record_t

    type :: call_record_t
        !! An opaque call whose registered rule is applied in reverse.
        character(len=:), allocatable :: name
        character(len=512), allocatable :: args(:)
    end type call_record_t

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

    type :: select_arm_record_t
        !! One dynamic-type guard and the SSA assignments in that arm.
        integer :: kind = 0
        character(len=:), allocatable :: target
        character(len=64), allocatable :: lhs(:)
        integer, allocatable :: rhs(:)
        integer :: n = 0
    end type select_arm_record_t

    type :: select_record_t
        !! Runtime type dispatch is discrete and therefore passive. Both
        !! sweeps repeat the same selector, while only arithmetic in the arm
        !! chosen at runtime contributes derivatives.
        integer :: selector = 0
        character(len=:), allocatable :: selector_alias
        type(select_arm_record_t), allocatable :: arms(:)
        integer :: n_arms = 0
    end type select_record_t

    type :: ssa_map_t
        !! Current SSA name of each declared variable, and the version counter.
        character(len=64), allocatable :: base(:)
        character(len=64), allocatable :: current(:)
        integer, allocatable :: version(:)
        character(len=256), allocatable :: active_paths(:)
        character(len=256), allocatable :: component_targets(:)
        character(len=256), allocatable :: component_snapshots(:)
        integer :: n_components = 0
        integer :: n = 0
    end type ssa_map_t

contains

    subroutine differentiate_reverse(primal, spec, adjoint, status)
        !! Build the adjoint procedure.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_spec_t), intent(in) :: spec
        type(fad_proc_t), intent(out) :: adjoint
        type(reverse_status_t), intent(out) :: status
        character(len=:), allocatable :: suffix, dependent, dependent_seed
        logical, allocatable :: active(:)
        character(len=256), allocatable :: active_paths(:)
        type(ssa_map_t) :: ssa
        integer :: i, di
        character(len=64), allocatable :: lhs_names(:)
        logical, allocatable :: is_element(:)
        integer, allocatable :: rhs_exprs(:)
        type(loop_record_t), allocatable :: loops(:)
        type(branch_record_t), allocatable :: branches(:)
        type(select_record_t), allocatable :: selections(:)
        type(call_record_t), allocatable :: calls(:)
        type(allocation_record_t), allocatable :: allocations(:)
        integer, allocatable :: order_kind(:), order_index(:)
        integer :: n_rec, n_loops, n_branches, n_selects, n_calls
        integer :: n_allocations, n_order
        logical :: complex_path, projection_path

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

        dependent_seed = ""
        if (is_component_dependent(primal, dependent)) then
            dependent_seed = component_seed_name(dependent, suffix)
        end if

        call check_supported(primal, status)
        if (.not. status%ok) return
        call refuse_polymorphic_component_read_modify_write(primal, status)
        if (.not. status%ok) return

        call seed_activity(primal, spec, dependent, active, status)
        if (.not. status%ok) return
        call independent_component_paths(spec%independents, active_paths)
        call refuse_active_polymorphic_dispatch(primal, active_paths, status)
        if (.not. status%ok) return
        call refuse_active_nested_polymorphic_component(primal, active_paths, status)
        if (.not. status%ok) return
        call refuse_active_polymorphic_ownership(primal, active, active_paths, status)
        if (.not. status%ok) return
        call check_reverse_allocation_sources(primal, active, status)
        if (.not. status%ok) return
        complex_path = complex_reverse_path(primal, dependent, active)
        projection_path = complex_real_projection_path(primal, dependent, active)
        if (complex_path .and. .not. projection_path) then
            status%ok = .false.
            status%message = complex_projection_refusal(primal, active)
            return
        end if
        ! An active optional primal is safe here because its PRESENT guard is
        ! retained in the reverse sweep.  Keep its outgoing cotangent
        ! required: an omitted primal still has a well-defined zero gradient,
        ! and a required output avoids ever assigning through an absent
        ! optional cotangent descriptor.
        adjoint%name = primal%name//"_vjp"
        if (allocated(spec%name)) adjoint%name = spec%name
        adjoint%is_function = .false.
        adjoint%is_elemental = primal%is_elemental
        adjoint%real_suffix = "d0"
        if (allocated(primal%real_suffix)) adjoint%real_suffix = primal%real_suffix
        ! The derivative names the same kinds as the primal, so it needs the
        ! same imports.
        if (primal%n_uses > 0 .and. allocated(primal%uses)) then
            allocate (character(len=256) :: adjoint%uses(primal%n_uses))
            do i = 1, primal%n_uses
                adjoint%uses(i) = primal%uses(i)
            end do
            adjoint%n_uses = primal%n_uses
        end if
        adjoint%is_pure = primal%is_pure
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind == FAD_CALL_STMT) adjoint%is_pure = .false.
        end do

        call build_signature(primal, adjoint, spec, dependent, suffix, active)
        call build_forward_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
            is_element, &
            n_rec, loops, n_loops, branches, n_branches, &
            selections, n_selects, &
            calls, n_calls, &
            allocations, n_allocations, &
            order_kind, order_index, n_order, active, suffix, &
            status)
        if (.not. status%ok) return
        allocate (ssa%active_paths(size(active_paths)))
        ssa%active_paths = active_paths
        call build_reverse_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
            is_element, &
            n_rec, loops, n_loops, branches, n_branches, &
            selections, n_selects, &
            calls, n_calls, &
            allocations, n_allocations, &
            order_kind, order_index, n_order, &
            spec, dependent, dependent_seed, suffix, active, active_paths, status)
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
            if (primal%decl_index(dependent) > 0) then
                if (is_derived_decl(primal, primal%decl_index(dependent))) then
                    status%ok = .false.
                    status%message = "dependent '"//dependent// &
                        "' is a derived object; it must name a concrete REAL component"
                    return
                end if
            else
                if (.not. supported_component_dependent(primal, dependent)) then
                    status%ok = .false.
                    if (index(trim(dependent), "%") > 0) then
                        status%message = "dependent '"//dependent// &
                            "' is not a supported concrete REAL component path"
                    else
                        status%message = "dependent '"//dependent// &
                            "' is not declared in "//primal%name
                    end if
                end if
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

    logical function supported_component_dependent(primal, dependent) result(ok)
        !! The bounded component-dependent VJP contract.
        !!
        !! A component seed is a separate rank-preserving dummy. This is safe
        !! only while the component has ordinary concrete REAL storage;
        !! aliases, dynamic type, global state, and lifetime changes need a
        !! storage-aware reverse representation of their own.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: dependent
        integer :: i, target_count
        character(len=:), allocatable :: path
        logical :: found

        ok = .false.
        if (index(trim(dependent), "%") == 0) return
        if (is_section_target(dependent)) return
        found = .false.
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            path = component_path_text(primal%exprs(i))
            if (.not. same_component_name(path, dependent)) cycle
            found = .true.
            if (.not. primal%exprs(i)%component_is_real) return
            if (primal%exprs(i)%component_is_allocatable) return
            if (primal%exprs(i)%component_is_pointer) return
            if (primal%exprs(i)%component_is_target) return
            if (primal%exprs(i)%component_is_polymorphic) return
            if (primal%exprs(i)%component_is_global) return
            if (primal%exprs(i)%component_rank > 4) return
            exit
        end do
        if (.not. found) then
            do i = 1, primal%n_stmts
                if (.not. primal%stmts(i)%target_is_component_path) cycle
                if (.not. allocated(primal%stmts(i)%target)) cycle
                if (.not. same_component_name(primal%stmts(i)%target, &
                    dependent)) cycle
                found = .true.
                if (.not. primal%stmts(i)%target_component_is_real) return
                if (primal%stmts(i)%target_component_is_allocatable .or. &
                    primal%stmts(i)%target_component_is_pointer .or. &
                    primal%stmts(i)%target_component_is_target .or. &
                    primal%stmts(i)%target_component_is_polymorphic .or. &
                    primal%stmts(i)%target_component_is_global) return
                if (primal%stmts(i)%target_component_rank > 4) return
                exit
            end do
        end if
        if (.not. found) return
        target_count = 0
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            if (same_component_name(primal%stmts(i)%target, dependent)) then
                target_count = target_count + 1
            end if
        end do
        ok = target_count == 1
    end function supported_component_dependent

    logical function is_component_dependent(primal, dependent) result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: dependent

        found = .false.
        if (index(trim(dependent), "%") == 0) return
        found = supported_component_dependent(primal, dependent)
    end function is_component_dependent

    function component_path_text(expr) result(path)
        type(fad_expr_t), intent(in) :: expr
        character(len=:), allocatable :: path

        path = ""
        if (allocated(expr%text)) path = trim(expr%text)
        if (allocated(expr%component_original_path)) then
            path = trim(expr%component_original_path)
        end if
    end function component_path_text

    function component_seed_name(path, suffix) result(name)
        !! Make a valid, stable dummy name for a component cotangent.
        character(len=*), intent(in) :: path, suffix
        character(len=:), allocatable :: name
        character :: c
        integer :: i, code, limit

        name = "fad_dep_"
        do i = 1, len_trim(path)
            c = path(i:i)
            code = iachar(c)
            if (code >= iachar("A") .and. code <= iachar("Z")) then
                c = achar(code + iachar("a") - iachar("A"))
            else if (.not. (code >= iachar("a") .and. code <= iachar("z")) .and. &
                    .not. (code >= iachar("0") .and. code <= iachar("9")) .and. &
                    c /= "_") then
                c = "_"
            end if
            name = name//c
        end do
        limit = 63 - len_trim(suffix)
        if (len_trim(name) > limit) name = name(:limit)
        name = trim(name)//trim(suffix)
    end function component_seed_name

    subroutine component_seed_decl(primal, path, name, d)
        !! Declare the incoming cotangent with the component's scalar type and
        !! rank. Assumed-shape seed arrays are safe because the generated
        !! routine is emitted in a module with an explicit interface.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: path, name
        type(fad_decl_t), intent(out) :: d
        integer :: i, rank, di
        character(len=:), allocatable :: expr_path

        di = primal%decl_index(fad_base_name(path))
        if (di > 0) then
            call copy_decl(d, primal%decls(di))
        end if
        d%name = trim(name)
        d%type_name = "real(8)"
        d%is_value = .false.
        d%is_optional = .false.
        d%is_allocatable = .false.
        d%is_contiguous = .false.
        d%is_polymorphic = .false.
        d%is_unlimited_polymorphic = .false.
        d%is_result = .false.
        d%intent = FAD_INTENT_IN
        d%is_array = .false.
        if (allocated(d%dims)) deallocate (d%dims)

        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            expr_path = component_path_text(primal%exprs(i))
            if (.not. same_component_name(expr_path, path)) cycle
            if (allocated(primal%exprs(i)%component_type_name)) then
                d%type_name = primal%exprs(i)%component_type_name
            end if
            rank = component_path_rank(primal%exprs(i), expr_path)
            if (rank > 0) then
                d%is_array = .true.
                d%dims = component_seed_dims(rank)
            end if
            return
        end do
        do i = 1, primal%n_stmts
            if (.not. primal%stmts(i)%target_is_component_path) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            if (.not. same_component_name(primal%stmts(i)%target, path)) cycle
            if (allocated(primal%stmts(i)%target_component_type_name)) then
                d%type_name = primal%stmts(i)%target_component_type_name
            end if
            rank = primal%stmts(i)%target_component_rank
            if (rank > 0) then
                d%is_array = .true.
                d%dims = component_seed_dims(rank)
            end if
            return
        end do
    end subroutine component_seed_decl

    integer function component_path_rank(expr, path) result(rank)
        type(fad_expr_t), intent(in) :: expr
        character(len=*), intent(in) :: path
        integer :: percent, open
        character(len=:), allocatable :: tail

        rank = expr%component_rank
        percent = index(trim(path), "%", back=.true.)
        if (percent > 0) then
            tail = trim(path(percent + 1:))
            open = index(tail, "(")
            if (open > 0) rank = 0
        end if
        if (rank < 0) rank = 0
    end function component_path_rank

    function component_seed_dims(rank) result(dims)
        integer, intent(in) :: rank
        character(len=:), allocatable :: dims
        integer :: i

        dims = ""
        do i = 1, rank
            if (i > 1) dims = dims//", "
            dims = dims//":"
        end do
    end function component_seed_dims

    subroutine check_supported(primal, status)
        !! Refuse what this milestone cannot do correctly, by name.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_status_t), intent(inout) :: status
        type(loop_shape_t) :: shape
        integer :: i, depth, allocation_depth, di, j, automatic_count
        integer :: explicit_lifetime_count
        character(len=64) :: owner, owner_text
        logical :: found

        allocation_depth = 0
        automatic_count = 0
        explicit_lifetime_count = 0
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%is_automatic_reallocation) then
                if (.not. (primal%stmts(i)%target_component_is_allocatable .and. &
                    explicit_component_lifetime(primal, &
                    primal%stmts(i)%target))) then
                    automatic_count = automatic_count + 1
                end if
            end if
            select case (primal%stmts(i)%kind)
            case (FAD_ALLOCATE, FAD_DEALLOCATE, FAD_MOVE_ALLOC)
                explicit_lifetime_count = explicit_lifetime_count + 1
            end select
        end do
        if (automatic_count > 1) then
            status%ok = .false.
            status%message = "reverse mode: repeated automatic reallocation "// &
                "requires allocation-state replay"
            return
        end if
        if (automatic_count > 0 .and. explicit_lifetime_count > 0) then
            status%ok = .false.
            status%message = "reverse mode: automatic reallocation cannot be combined "// &
                "with explicit allocation lifetime operations"
            return
        end if
        call check_reverse_move_alloc_shape(primal, status)
        if (.not. status%ok) return
        call check_nested_polymorphic_component_lifetime(primal, status)
        if (.not. status%ok) return

        depth = 0
        do i = 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                if (primal%stmts(i)%is_automatic_reallocation) then
                    if (primal%stmts(i)%target_component_is_allocatable) then
                        call check_component_reallocation_shape(primal%stmts(i), &
                            status)
                        if (.not. status%ok) return
                    end if
                    if (depth /= 0 .or. allocation_depth /= 0) then
                        status%ok = .false.
                        status%message = "reverse mode: path-dependent automatic "// &
                            "reallocation requires allocation-state replay"
                        return
                    end if
                end if
                ! An element write is a scatter wherever it appears. Inside a
                ! loop `analyse_loop` validates the index; outside one the
                ! index is whatever the subscript says and the adjoint goes to
                ! the matching element of the array's adjoint.
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
            case (FAD_IF, FAD_SELECT_TYPE)
                allocation_depth = allocation_depth + 1
            case (FAD_END_IF, FAD_END_SELECT)
                allocation_depth = max(0, allocation_depth - 1)
            case (FAD_ELSE, FAD_TYPE_IS, FAD_CLASS_IS, FAD_CLASS_DEFAULT)
                ! Branches are handled by re-evaluating the condition in the
                ! reverse sweep; see emit_branch_forward.
                continue
            case (FAD_CALL_STMT)
                if (.not. call_rule_has(primal%stmts(i)%target)) then
                    status%ok = .false.
                    status%message = "no reverse rule for the call to '"// &
                        primal%stmts(i)%target// &
                        "'; register one with fad_add_call_rule"
                    return
                end if
                if (depth > 0) then
                    status%ok = .false.
                    status%message = "reverse mode: structured calls inside "// &
                        "loops are not supported"
                    return
                end if
            case (FAD_ALLOCATE, FAD_DEALLOCATE, FAD_MOVE_ALLOC)
                if (depth /= 0 .or. allocation_depth /= 0) then
                    status%ok = .false.
                    status%message = "reverse mode: allocation lifetime inside "// &
                        "control flow requires a per-path replay tape"
                    return
                end if
                if (primal%stmts(i)%kind == FAD_MOVE_ALLOC) then
                    cycle
                end if
                if (.not. allocated(primal%stmts(i)%allocation_args)) then
                    status%ok = .false.
                    status%message = "reverse mode: allocation statement has no "// &
                        "simple owner"
                    return
                end if
                if (size(primal%stmts(i)%allocation_args) < 1) then
                    status%ok = .false.
                    status%message = "reverse mode: allocation statement has no "// &
                        "simple owner"
                    return
                end if
                owner_text = emit_expr(primal, &
                    primal%stmts(i)%allocation_args(1))
                owner = fad_base_name(owner_text)
                if (index(trim(owner_text), "%") > 0) then
                    if (primal%stmts(i)%allocation_target_component .and. &
                        has_move_alloc(primal)) then
                        ! The dedicated MOVE_ALLOC component shape check
                        ! validates this concrete descriptor transition.
                        cycle
                    end if
                    if (primal%stmts(i)%allocation_target_polymorphic) then
                        ! The bounded component lifetime was checked above.  It
                        ! is retained as a component descriptor, not mistaken
                        ! for an allocatable declaration named by its base.
                        cycle
                    end if
                    if (is_polymorphic_component_path(primal, owner_text)) cycle
                    if (is_concrete_allocatable_component_path(primal, owner_text) &
                        .and. has_move_alloc(primal)) cycle
                    status%ok = .false.
                    status%message = "reverse mode: allocation owner '"// &
                        trim(owner)//"' must be a simple local or dummy "// &
                        "allocatable array"
                    return
                end if
                if (len_trim(owner) == 0) then
                    status%ok = .false.
                    status%message = "reverse mode: allocation owner must be a "// &
                        "simple local or dummy allocatable array"
                    return
                end if
                di = primal%decl_index(trim(owner))
                if (di <= 0) then
                    status%ok = .false.
                    status%message = "reverse mode: allocation owner '"// &
                        trim(owner)//"' is not an allocatable declaration"
                    return
                end if
                if (.not. primal%decls(di)%is_allocatable) then
                    status%ok = .false.
                    status%message = "reverse mode: allocation owner '"// &
                        trim(owner)//"' is not an allocatable declaration"
                    return
                end if
                if (primal%decls(di)%is_polymorphic) then
                    if (.not. has_fixed_source_owner(primal, di)) then
                        status%ok = .false.
                        if (primal%decls(di)%is_unlimited_polymorphic) then
                            status%message = "reverse mode: class(*) allocatable ownership '"// &
                                trim(owner)//"' requires one fixed concrete SELECT TYPE "// &
                                "arm and a direct concrete SOURCE= acquisition"
                        else
                            status%message = "reverse mode: polymorphic allocatable ownership '"// &
                                trim(owner)//"' requires dynamic-type replay"
                        end if
                        return
                    end if
                end if
                if (primal%stmts(i)%kind == FAD_ALLOCATE) then
                    found = .false.
                    do j = 1, i - 1
                        if (.not. allocated(primal%stmts(j)%allocation_args)) cycle
                        if (size(primal%stmts(j)%allocation_args) < 1) cycle
                        if (primal%stmts(j)%kind /= FAD_ALLOCATE) cycle
                        if (trim(fad_base_name(emit_expr(primal, &
                            primal%stmts(j)%allocation_args(1)))) == &
                            trim(owner)) found = .true.
                    end do
                    if (found) then
                        status%ok = .false.
                        status%message = "reverse mode: repeated allocation of '"// &
                            trim(owner)//"' needs allocation-state replay"
                        return
                    end if
                else
                    do j = i + 1, primal%n_stmts
                        if (primal%stmts(j)%kind == FAD_DIRECTIVE) cycle
                        status%ok = .false.
                        status%message = "reverse mode: explicit deallocate of '"// &
                            trim(owner)//"' must terminate its straight-line "// &
                            "lifetime in the bounded retention slice"
                        return
                    end do
                    found = .false.
                    do j = 1, i - 1
                        if (primal%stmts(j)%kind == FAD_MOVE_ALLOC) then
                            if (allocated(primal%stmts(j)%call_args)) then
                                if (size(primal%stmts(j)%call_args) >= 2) then
                                    if (trim(fad_base_name(emit_expr(primal, &
                                        primal%stmts(j)%call_args(2)))) == &
                                        trim(owner)) found = .true.
                                end if
                            end if
                            cycle
                        end if
                        if (.not. allocated(primal%stmts(j)%allocation_args)) cycle
                        if (size(primal%stmts(j)%allocation_args) < 1) cycle
                        if (trim(fad_base_name(emit_expr(primal, &
                            primal%stmts(j)%allocation_args(1)))) /= &
                            trim(owner)) cycle
                        if (primal%stmts(j)%kind == FAD_ALLOCATE) then
                            found = .true.
                        else if (primal%stmts(j)%kind == FAD_DEALLOCATE) then
                            status%ok = .false.
                            status%message = "reverse mode: repeated deallocation "// &
                                "of '"//trim(owner)//"' needs allocation-state "// &
                                "replay"
                            return
                        end if
                    end do
                    if (.not. found) then
                        status%ok = .false.
                        status%message = "reverse mode: deallocation of '"// &
                            trim(owner)//"' has no preceding explicit allocation"
                        return
                    end if
                end if
            end select
        end do

    end subroutine check_supported

    subroutine refuse_polymorphic_component_read_modify_write(primal, status)
        !! The bounded reverse shadow does not snapshot a selected component's
        !! old value. Refuse read-modify-write while direct fixed-path stores
        !! remain supported.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_status_t), intent(inout) :: status
        integer :: i
        character(len=:), allocatable :: target

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            target = trim(primal%stmts(i)%target)
            if (index(target, achar(37)) == 0) cycle
            if (.not. selected_component_target(primal, target)) cycle
            if (.not. expression_reads_component(primal, primal%stmts(i)%value, &
                target)) cycle
            status%ok = .false.
            status%message = "reverse mode: fixed-path polymorphic component "// &
                "read-modify-write requires an old-value snapshot; use a "// &
                "direct assignment or an explicit derivative rule"
            return
        end do
    end subroutine refuse_polymorphic_component_read_modify_write

    logical function selected_component_target(primal, target) result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: target
        integer :: i

        found = .false.
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            if (trim(primal%stmts(i)%target) /= fad_base_name(target)) cycle
            if (index(trim(emit_expr(primal, primal%stmts(i)%value)), &
                achar(37)) > 0) found = .true.
            return
        end do
    end function selected_component_target

    recursive logical function expression_reads_component(primal, idx, target) &
            result(found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        character(len=*), intent(in) :: target
        integer :: i

        found = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (index(trim(emit_expr(primal, idx)), achar(37)) > 0) then
            if (same_component_name(emit_expr(primal, idx), target)) then
                found = .true.
                return
            end if
        end if
        do i = 1, size(primal%exprs(idx)%args)
            if (expression_reads_component(primal, primal%exprs(idx)%args(i), &
                target)) then
                found = .true.
                return
            end if
        end do
    end function expression_reads_component

    subroutine check_nested_polymorphic_component_lifetime(primal, status)
        !! Permit one and only one scalar polymorphic component acquisition.
        !! The component must be acquired from one declared concrete source and
        !! released once after the straight-line computation.  This is the
        !! reverse boundary for ``allocate(box%field%payload, source=child)``
        !! and one literal-indexed holder element such as
        !! ``allocate(holders(2)%payload, source=child)``.  Dynamic indices,
        !! sections, reallocation, and aliases stay refusals because their
        !! descriptor history is not represented here.  One direct
        !! ``MOVE_ALLOC`` transfer to a distinct scalar component is part of
        !! the fixed-path replay slice.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_status_t), intent(inout) :: status
        integer :: i, n_allocate, n_deallocate, allocate_at, deallocate_at
        character(len=:), allocatable :: target, lifetime_target, &
            deallocated_target

        status%ok = .true.
        n_allocate = 0
        n_deallocate = 0
        allocate_at = 0
        deallocate_at = 0
        target = ""
        lifetime_target = ""
        deallocated_target = ""

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind == FAD_ALLOCATE) then
                if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
                if (size(primal%stmts(i)%allocation_args) < 1) cycle
                target = emit_expr(primal, primal%stmts(i)%allocation_args(1))
                if (.not. primal%stmts(i)%allocation_target_polymorphic) cycle
                if (index(trim(target), "%") == 0) cycle
                n_allocate = n_allocate + 1
                allocate_at = i
                if (.not. fixed_source_component(primal, i)) then
                    status%ok = .false.
                    if (array_element_component(target)) then
                        status%message = "reverse mode: array-element polymorphic component "// &
                            "allocation '"//trim(target)//"' requires one literal index "// &
                            "for bounded reverse SOURCE= ownership; dynamic indices, "// &
                            "sections, and per-element lifetime tapes are unsupported"
                    else
                        status%message = "reverse mode: nested polymorphic component allocation '"// &
                            trim(target)//"' cannot replay SOURCE= ownership; requires one "// &
                            "scalar fixed-source acquisition with the same concrete dynamic type"
                    end if
                    return
                end if
            else if (primal%stmts(i)%kind == FAD_DEALLOCATE) then
                if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
                if (size(primal%stmts(i)%allocation_args) < 1) cycle
                deallocated_target = emit_expr(primal, &
                    primal%stmts(i)%allocation_args(1))
                if (.not. is_polymorphic_component_path(primal, &
                    deallocated_target)) cycle
                n_deallocate = n_deallocate + 1
                deallocate_at = i
            end if
        end do

        if (n_allocate == 0) return
        if (n_allocate /= 1 .or. n_deallocate /= 1) then
            status%ok = .false.
            status%message = "reverse mode: polymorphic component ownership '"// &
                trim(target)//"' does not support reallocation or repeated "// &
                "acquisition/deallocation"
            return
        end if
        lifetime_target = target
        do i = allocate_at + 1, deallocate_at - 1
            if (primal%stmts(i)%kind /= FAD_MOVE_ALLOC) cycle
            if (.not. allocated(primal%stmts(i)%call_args)) cycle
            if (size(primal%stmts(i)%call_args) /= 2) cycle
            if (.not. same_component_name(emit_expr(primal, &
                primal%stmts(i)%call_args(1)), lifetime_target)) cycle
            lifetime_target = emit_expr(primal, primal%stmts(i)%call_args(2))
            exit
        end do
        if (.not. same_component_name(lifetime_target, deallocated_target) .or. &
            deallocate_at <= allocate_at) then
            status%ok = .false.
            status%message = "reverse mode: polymorphic component ownership '"// &
                trim(target)//"' requires one matching final deallocation"
            return
        end if
    end subroutine check_nested_polymorphic_component_lifetime

    logical function fixed_source_component(primal, stmt_index, &
            required_selector) result(supported)
        !! Facts for the one supported component ownership transition.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: stmt_index
        character(len=*), intent(in), optional :: required_selector
        character(len=:), allocatable :: target, selector_target, source_type, &
            dispatch_type
        integer :: owner_di, source_di, i, select_start
        logical :: found

        supported = .false.
        if (stmt_index <= 0 .or. stmt_index > primal%n_stmts) return
        if (primal%stmts(stmt_index)%kind /= FAD_ALLOCATE) return
        if (.not. primal%stmts(stmt_index)%allocation_target_polymorphic) return
        if (.not. allocated(primal%stmts(stmt_index)%allocation_args)) return
        if (size(primal%stmts(stmt_index)%allocation_args) < 1) return
        target = emit_expr(primal, primal%stmts(stmt_index)%allocation_args(1))
        if (index(trim(target), "%") == 0) return
        if (index(trim(target), "(") > 0) then
            if (.not. array_element_component(target)) return
            if (.not. fixed_literal_array_element(target)) return
        end if
        if (primal%stmts(stmt_index)%allocation_source <= 0 .or. &
            primal%stmts(stmt_index)%allocation_mold > 0) return
        source_di = concrete_source_decl(primal, &
            primal%stmts(stmt_index)%allocation_source)
        if (source_di <= 0) return
        if (.not. source_initializer_supported(primal, source_di)) return

        owner_di = primal%decl_index(fad_base_name(target))
        if (owner_di <= 0) return
        if (primal%decls(owner_di)%is_polymorphic .or. &
            primal%decls(owner_di)%is_allocatable .or. &
            primal%decls(owner_di)%is_associate_alias .or. &
            primal%decls(owner_di)%is_select_alias) return
        if (array_element_component(target)) then
            if (.not. primal%decls(owner_di)%is_array) return
            if (.not. allocated(primal%decls(owner_di)%dims)) return
            if (index(trim(primal%decls(owner_di)%dims), ":") > 0 .or. &
                index(trim(primal%decls(owner_di)%dims), "*") > 0) return
        end if

        found = .false.
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (.not. same_component_name(emit_expr(primal, i), target)) cycle
            found = .true.
            if (.not. primal%exprs(i)%component_is_allocatable .or. &
                .not. primal%exprs(i)%component_is_polymorphic) return
            if (primal%exprs(i)%component_is_pointer .or. &
                primal%exprs(i)%component_is_target .or. &
                primal%exprs(i)%component_is_global) return
            if (primal%exprs(i)%component_rank /= 0) return
            exit
        end do
        if (.not. found) return

        source_type = type_leaf(primal%decls(source_di)%type_name)
        selector_target = target
        select_start = stmt_index + 1
        do i = stmt_index + 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_MOVE_ALLOC) cycle
            if (.not. allocated(primal%stmts(i)%call_args)) return
            if (size(primal%stmts(i)%call_args) /= 2) return
            if (.not. same_component_name(emit_expr(primal, &
                primal%stmts(i)%call_args(1)), target)) cycle
            selector_target = emit_expr(primal, primal%stmts(i)%call_args(2))
            select_start = i + 1
            exit
        end do
        if (present(required_selector)) then
            if (.not. same_component_name(selector_target, required_selector)) &
                return
        end if
        dispatch_type = ""
        found = .false.
        do i = select_start, primal%n_stmts
            if (primal%stmts(i)%kind == FAD_END_SELECT) exit
            if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
            if (.not. same_component_name(emit_expr(primal, &
                primal%stmts(i)%value), selector_target)) cycle
            call fixed_dispatch_type(primal, i, dispatch_type, found)
            exit
        end do
        if (.not. found) return
        supported = same_variable_name(source_type, type_leaf(dispatch_type))
    end function fixed_source_component

    integer function concrete_source_decl(primal, idx) result(di)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx

        di = 0
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind /= FAD_VAR) return
        di = primal%decl_index_of(primal%exprs(idx)%text)
        if (di <= 0 .or. primal%decls(di)%is_polymorphic) di = 0
    end function concrete_source_decl

    logical function source_initializer_supported(primal, source_di) result(ok)
        !! Do not let the ownership classifier hide an unsupported active
        !! initializer of the concrete SOURCE object.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: source_di
        integer :: i

        ok = .true.
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (primal%decl_index(fad_base_name(primal%stmts(i)%target)) /= &
                source_di) cycle
            if (.not. initializer_calls_supported(primal, &
                primal%stmts(i)%value)) then
                ok = .false.
                return
            end if
        end do
    end function source_initializer_supported

    recursive logical function initializer_calls_supported(primal, idx) result(ok)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        integer :: i

        ok = .true.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind == FAD_CALL .and. &
            .not. has_rule(primal%exprs(idx)%text)) then
            ok = .false.
            return
        end if
        do i = 1, size(primal%exprs(idx)%args)
            if (.not. initializer_calls_supported(primal, &
                primal%exprs(idx)%args(i))) then
                ok = .false.
                return
            end if
        end do
    end function initializer_calls_supported

    subroutine fixed_dispatch_type(primal, select_at, type_name, found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: select_at
        character(len=:), allocatable, intent(out) :: type_name
        logical, intent(out) :: found
        integer :: i, depth, concrete

        type_name = ""
        found = .false.
        concrete = 0
        depth = 1
        do i = select_at + 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_SELECT_TYPE)
                depth = depth + 1
            case (FAD_END_SELECT)
                depth = depth - 1
                if (depth == 0) exit
            case (FAD_TYPE_IS, FAD_CLASS_IS)
                if (depth /= 1) cycle
                concrete = concrete + 1
                if (allocated(primal%stmts(i)%target)) then
                    type_name = primal%stmts(i)%target
                end if
            end select
        end do
        found = concrete == 1 .and. len_trim(type_name) > 0
    end subroutine fixed_dispatch_type

    logical function is_polymorphic_component_path(primal, text) result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        integer :: i

        found = .false.
        if (index(trim(text), "%") == 0) return
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (.not. same_component_name(emit_expr(primal, i), text)) cycle
            found = primal%exprs(i)%component_is_allocatable .and. &
                primal%exprs(i)%component_is_polymorphic
            return
        end do
    end function is_polymorphic_component_path

    function type_leaf(type_name) result(leaf)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: leaf
        integer :: open, close

        leaf = trim(type_name)
        open = index(leaf, "(")
        close = index(leaf, ")", back=.true.)
        if (open > 0 .and. close > open) leaf = trim(leaf(open + 1:close - 1))
    end function type_leaf

    function shadow_component_path(path, suffix) result(shadow)
        !! Put the derivative suffix on the enclosing derived object, not on
        !! the allocatable component name: ``box%field%payload`` becomes
        !! ``box_b%field%payload``.
        character(len=*), intent(in) :: path, suffix
        character(len=:), allocatable :: shadow, base
        integer :: cut

        base = fad_base_name(path)
        cut = len_trim(base) + 1
        if (cut <= len_trim(path)) then
            shadow = trim(base)//trim(suffix)//trim(path(cut:))
        else
            shadow = trim(base)//trim(suffix)
        end if
    end function shadow_component_path

    subroutine check_component_reallocation_shape(stmt, status)
        !! Reverse replay is deliberately narrower than ordinary component
        !! assignment: only one scalar concrete REAL descriptor transition is
        !! safe without a component lifetime tape.
        type(fad_stmt_t), intent(in) :: stmt
        type(reverse_status_t), intent(inout) :: status

        status%ok = .true.
        if (.not. stmt%target_component_is_real) then
            status%ok = .false.
            status%message = "reverse mode: allocatable component reallocation "// &
                "requires a concrete REAL component"
            return
        end if
        if (stmt%target_component_rank /= 0) then
            status%ok = .false.
            status%message = "reverse mode: array-valued allocatable component "// &
                "reallocation requires component lifetime replay"
            return
        end if
        if (stmt%target_component_is_polymorphic) then
            status%ok = .false.
            status%message = "reverse mode: polymorphic allocatable component "// &
                "reallocation requires dynamic-type replay"
            return
        end if
        if (stmt%target_component_is_pointer .or. &
            stmt%target_component_is_target) then
            status%ok = .false.
            status%message = "reverse mode: pointer/TARGET component storage "// &
                "identity is not tracked"
            return
        end if
        if (stmt%target_component_is_global) then
            status%ok = .false.
            status%message = "reverse mode: global mutable component ownership "// &
                "requires an explicit derivative rule"
            return
        end if
    end subroutine check_component_reallocation_shape

    subroutine check_reverse_move_alloc_shape(primal, status)
        !! The reverse move slice has one static ownership transition only.
        type(fad_proc_t), intent(in) :: primal
        type(reverse_status_t), intent(inout) :: status
        integer :: i, n_allocate, n_deallocate, n_move, source_di, target_di
        integer :: move_at, allocate_at, deallocate_at
        character(len=:), allocatable :: source, target, owner
        logical :: found, component_mode, polymorphic_component

        n_allocate = 0
        n_deallocate = 0
        n_move = 0
        move_at = 0
        allocate_at = 0
        deallocate_at = 0
        do i = 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_ALLOCATE)
                n_allocate = n_allocate + 1
                allocate_at = i
            case (FAD_DEALLOCATE)
                n_deallocate = n_deallocate + 1
                deallocate_at = i
            case (FAD_MOVE_ALLOC)
                n_move = n_move + 1
                move_at = i
            end select
        end do
        if (n_move == 0) return
        if (n_move /= 1 .or. n_allocate /= 1 .or. n_deallocate /= 1) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc requires one straight-line "// &
                "allocation owner and one matching final deallocation"
            return
        end if
        if (.not. allocated(primal%stmts(move_at)%call_args)) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc requires one simple source "// &
                "and destination"
            return
        end if
        if (size(primal%stmts(move_at)%call_args) /= 2) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc requires one simple source "// &
                "and destination"
            return
        end if
        source = emit_expr(primal, primal%stmts(move_at)%call_args(1))
        target = emit_expr(primal, primal%stmts(move_at)%call_args(2))
        component_mode = index(trim(source), "%") > 0 .or. &
            index(trim(target), "%") > 0
        polymorphic_component = .false.
        if (component_mode .neqv. (index(trim(source), "%") > 0 .and. &
            index(trim(target), "%") > 0)) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc cannot mix component and "// &
                "simple ownership paths"
            return
        end if
        if (component_mode) then
            if (index(trim(source), "(") > 0 .or. &
                index(trim(target), "(") > 0 .or. trim(source) == trim(target)) then
                status%ok = .false.
                status%message = "reverse mode: move_alloc component owners require "// &
                    "distinct scalar paths without dynamic indices"
                return
            end if
            if (is_concrete_allocatable_component_path(primal, source) .and. &
                is_concrete_allocatable_component_path(primal, target)) then
                if (allocatable_component_rank(primal, source) /= &
                    allocatable_component_rank(primal, target)) then
                    status%ok = .false.
                    status%message = "reverse mode: move_alloc component owners require "// &
                        "matching component ranks"
                    return
                end if
                if (allocatable_component_rank(primal, source) > 0) then
                    if (.not. fixed_literal_component_shape(primal, allocate_at)) then
                        status%ok = .false.
                        status%message = "reverse mode: array component MOVE_ALLOC "// &
                            "requires one literal allocation shape"
                        return
                    end if
                end if
            else
                polymorphic_component = is_polymorphic_component_path(primal, source)
                if (.not. polymorphic_component) then
                    status%ok = .false.
                    status%message = "reverse mode: move_alloc component owners require "// &
                        "concrete scalar through rank-four REAL allocatable components"
                    return
                end if
                if (.not. is_polymorphic_component_path(primal, target)) then
                    status%ok = .false.
                    status%message = "reverse mode: polymorphic MOVE_ALLOC requires "// &
                        "matching polymorphic component paths"
                    return
                end if
                if (allocatable_component_rank(primal, source) /= 0 .or. &
                    allocatable_component_rank(primal, target) /= 0) then
                    status%ok = .false.
                    status%message = "reverse mode: polymorphic component MOVE_ALLOC "// &
                        "is limited to scalar components"
                    return
                end if
                if (.not. fixed_source_component(primal, allocate_at)) then
                    status%ok = .false.
                    status%message = "reverse mode: polymorphic component MOVE_ALLOC "// &
                        "requires one fixed concrete SOURCE= acquisition and SELECT TYPE arm"
                    return
                end if
            end if
            source_di = primal%decl_index(fad_base_name(source))
            target_di = primal%decl_index(fad_base_name(target))
            if (source_di <= 0 .or. target_di <= 0) then
                status%ok = .false.
                status%message = "reverse mode: move_alloc component owners must "// &
                    "have declared bases"
                return
            end if
            if (primal%decls(source_di)%is_polymorphic .or. &
                primal%decls(target_di)%is_polymorphic .or. &
                primal%decls(source_di)%is_select_alias .or. &
                primal%decls(target_di)%is_select_alias) then
                status%ok = .false.
                status%message = "reverse mode: move_alloc component owners must "// &
                    "have concrete, non-aliased bases"
                return
            end if
        else
            if (trim(source) == trim(target)) then
                status%ok = .false.
                status%message = "reverse mode: move_alloc requires simple, distinct "// &
                    "local or dummy allocatable owners"
                return
            end if
            source_di = primal%decl_index(trim(source))
            target_di = primal%decl_index(trim(target))
            if (source_di <= 0 .or. target_di <= 0) then
                status%ok = .false.
                status%message = "reverse mode: move_alloc owners must be declared "// &
                    "allocatable objects"
                return
            end if
            if (.not. primal%decls(source_di)%is_allocatable .or. &
                .not. primal%decls(target_di)%is_allocatable) then
                status%ok = .false.
                status%message = "reverse mode: move_alloc owners must be declared "// &
                    "allocatable objects"
                return
            end if
            if (primal%decls(source_di)%is_polymorphic .or. &
                primal%decls(target_di)%is_polymorphic .or. &
                primal%decls(source_di)%is_select_alias .or. &
                primal%decls(target_di)%is_select_alias) then
                status%ok = .false.
                status%message = "reverse mode: move_alloc does not support polymorphic "// &
                    "ownership or aliases"
                return
            end if
        end if
        if (move_at <= allocate_at .or. deallocate_at <= move_at) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc requires allocation, transfer, "// &
                "then final deallocation"
            return
        end if
        if (.not. allocated(primal%stmts(allocate_at)%allocation_args)) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc source has no explicit allocation"
            return
        end if
        owner = emit_expr(primal, primal%stmts(allocate_at)%allocation_args(1))
        if (.not. component_mode) owner = fad_base_name(owner)
        if (trim(owner) /= trim(source)) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc source must be the allocated owner"
            return
        end if
        if (.not. allocated(primal%stmts(deallocate_at)%allocation_args)) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc has no matching final deallocation"
            return
        end if
        owner = emit_expr(primal, primal%stmts(deallocate_at)%allocation_args(1))
        if (.not. component_mode) owner = fad_base_name(owner)
        if (trim(owner) /= trim(target)) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc has no matching final deallocation"
            return
        end if
        found = .false.
        do i = move_at + 1, deallocate_at - 1
            if (primal%stmts(i)%kind == FAD_ALLOCATE .or. &
                primal%stmts(i)%kind == FAD_DEALLOCATE .or. &
                primal%stmts(i)%kind == FAD_MOVE_ALLOC) found = .true.
        end do
        if (found) then
            status%ok = .false.
            status%message = "reverse mode: repeated or path-dependent allocation "// &
                "lifetime needs allocation-state replay"
        end if
    end subroutine check_reverse_move_alloc_shape

    logical function has_move_alloc(primal) result(found)
        type(fad_proc_t), intent(in) :: primal
        integer :: i

        found = .false.
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind == FAD_MOVE_ALLOC) then
                found = .true.
                return
            end if
        end do
    end function has_move_alloc

    logical function explicit_component_lifetime(primal, text) result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        integer :: i
        character(len=:), allocatable :: candidate

        found = .false.
        if (index(trim(text), "%") == 0) return
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind == FAD_ALLOCATE .or. &
                primal%stmts(i)%kind == FAD_DEALLOCATE) then
                if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
                if (size(primal%stmts(i)%allocation_args) < 1) cycle
                candidate = emit_expr(primal, &
                    primal%stmts(i)%allocation_args(1))
                if (same_component_lifetime_name(candidate, text)) then
                    found = .true.
                    return
                end if
            else if (primal%stmts(i)%kind == FAD_MOVE_ALLOC) then
                if (.not. allocated(primal%stmts(i)%call_args)) cycle
                if (size(primal%stmts(i)%call_args) /= 2) cycle
                candidate = emit_expr(primal, primal%stmts(i)%call_args(1))
                if (same_component_lifetime_name(candidate, text)) then
                    found = .true.
                    return
                end if
                candidate = emit_expr(primal, primal%stmts(i)%call_args(2))
                if (same_component_lifetime_name(candidate, text)) then
                    found = .true.
                    return
                end if
            end if
        end do
    end function explicit_component_lifetime

    logical function same_component_lifetime_name(a, b) result(equal)
        !! Match an allocation owner to one of its element references.
        character(len=*), intent(in) :: a, b
        character(len=:), allocatable :: owner_a, owner_b

        owner_a = component_lifetime_owner(a)
        owner_b = component_lifetime_owner(b)
        equal = same_component_name(owner_a, owner_b)
    end function same_component_lifetime_name

    function component_lifetime_owner(text) result(owner)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: owner
        integer :: percent, open

        owner = trim(text)
        percent = index(owner, "%")
        open = index(owner, "(")
        if (percent > 0 .and. open > percent) owner = trim(owner(:open - 1))
    end function component_lifetime_owner

    logical function is_concrete_allocatable_component_path(primal, text) &
            result(found)
        !! The concrete component lifetime slice is scalar through rank-four REAL.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        integer :: i

        found = .false.
        if (index(trim(text), "%") == 0) return
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (.not. same_component_name(emit_expr(primal, i), text)) cycle
            found = primal%exprs(i)%component_is_allocatable .and. &
                primal%exprs(i)%component_is_real .and. &
                .not. primal%exprs(i)%component_is_polymorphic .and. &
                .not. primal%exprs(i)%component_is_pointer .and. &
                .not. primal%exprs(i)%component_is_target .and. &
                .not. primal%exprs(i)%component_is_global .and. &
                (primal%exprs(i)%component_rank >= 0 .and. &
                primal%exprs(i)%component_rank <= 4)
            return
        end do
    end function is_concrete_allocatable_component_path

    integer function allocatable_component_rank(primal, text) result(rank)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        integer :: i

        rank = -1
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (.not. same_component_name(emit_expr(primal, i), text)) cycle
            rank = component_path_rank(primal%exprs(i), emit_expr(primal, i))
            return
        end do
    end function allocatable_component_rank

    logical function fixed_literal_component_shape(primal, stmt_index) result(found)
        !! An array component lifetime has statically known literal extents.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: stmt_index
        integer :: i, rank
        character(len=:), allocatable :: target

        found = .false.
        if (stmt_index <= 0 .or. stmt_index > primal%n_stmts) return
        if (.not. allocated(primal%stmts(stmt_index)%allocation_args)) return
        target = emit_expr(primal, primal%stmts(stmt_index)%allocation_args(1))
        rank = allocatable_component_rank(primal, target)
        if (rank < 1 .or. rank > 4) return
        if (size(primal%stmts(stmt_index)%allocation_args) /= rank + 1) return
        found = .true.
        do i = 2, size(primal%stmts(stmt_index)%allocation_args)
            if (.not. integer_literal_expr(primal, &
                primal%stmts(stmt_index)%allocation_args(i))) then
                found = .false.
                return
            end if
        end do
    end function fixed_literal_component_shape

    logical function array_element_component(text) result(found)
        character(len=*), intent(in) :: text
        integer :: open, percent

        open = index(trim(text), "(")
        percent = index(trim(text), "%")
        found = open > 0 .and. percent > open
    end function array_element_component

    logical function fixed_literal_array_element(text) result(found)
        !! The bounded array-owner replay uses one scalar element selected by
        !! a literal integer.  No descriptor arithmetic is inferred for
        !! computed indices, sections, vectors, or multi-dimensional paths.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: index_text
        integer :: open, close, i, digit

        found = .false.
        open = index(trim(text), "(")
        close = index(trim(text), ")")
        if (open <= 1 .or. close <= open) return
        if (index(trim(text(close + 1:)), "(") > 0) return
        index_text = trim(text(open + 1:close - 1))
        if (len_trim(index_text) == 0) return
        do i = 1, len_trim(index_text)
            digit = iachar(index_text(i:i))
            if (digit < iachar("0") .or. digit > iachar("9")) return
        end do
        found = index(trim(text(close + 1:)), "%") > 0
    end function fixed_literal_array_element

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
            if (di == 0) di = primal%decl_index( &
                fad_base_name(trim(spec%independents(i))))
            if (di == 0) then
                status%ok = .false.
                status%message = "independent '"//trim(spec%independents(i))// &
                    "' is not declared in "//primal%name
                return
            end if
            if (index(trim(spec%independents(i)), "%") == 0 .and. &
                is_derived_decl(primal, di)) then
                status%ok = .false.
                if (primal%decls(di)%is_polymorphic) then
                    status%message = "active polymorphic receiver '"// &
                        trim(spec%independents(i))// &
                        "' would perturb dynamic type; dynamic type perturbations "// &
                        "are unsupported"
                else
                    status%message = "active derived object '"// &
                        trim(spec%independents(i))// &
                        "' must name a real component"
                end if
                return
            end if
            varied(di) = .true.
        end do

        changed = .true.
        do while (changed)
            changed = .false.
            do j = 1, primal%n_stmts
                if (primal%stmts(j)%kind == FAD_MOVE_ALLOC) then
                    if (.not. allocated(primal%stmts(j)%call_args)) cycle
                    if (call_reads_any(primal, primal%stmts(j), varied)) then
                        do i = 1, size(primal%stmts(j)%call_args)
                            di = call_arg_decl_index(primal, &
                                primal%stmts(j)%call_args(i))
                            if (di <= 0) cycle
                            if (.not. varied(di)) then
                                varied(di) = .true.
                                changed = .true.
                            end if
                        end do
                    end if
                    cycle
                end if
                if (primal%stmts(j)%kind == FAD_CALL_STMT) then
                    if (.not. call_reads_any(primal, primal%stmts(j), varied)) cycle
                    do i = 1, size(primal%stmts(j)%call_args)
                        di = call_arg_decl_index(primal, &
                            primal%stmts(j)%call_args(i))
                        if (di <= 0) cycle
                        if (.not. is_real_type(primal%decls(di))) cycle
                        if (.not. varied(di)) then
                            varied(di) = .true.
                            changed = .true.
                        end if
                    end do
                    cycle
                end if
                if (primal%stmts(j)%kind == FAD_ALLOCATE) then
                    if (primal%stmts(j)%allocation_source > 0 .and. &
                        allocated(primal%stmts(j)%allocation_args)) then
                        di = call_arg_decl_index(primal, &
                            primal%stmts(j)%allocation_args(1))
                        if (di <= 0) then
                            di = primal%decl_index_of(fad_base_name(emit_expr( &
                                primal, primal%stmts(j)%allocation_args(1))))
                        end if
                        if (di > 0 .and. reads_any(primal, &
                            primal%stmts(j)%allocation_source, varied)) then
                            if (.not. varied(di)) then
                                varied(di) = .true.
                                changed = .true.
                            end if
                        end if
                    end if
                    cycle
                end if
                if (primal%stmts(j)%kind /= FAD_ASSIGN) cycle
                if (.not. reads_any(primal, primal%stmts(j)%value, varied)) cycle
                ! An array-element target must resolve to its array, or the
                ! array never becomes active and its adjoint is silently
                ! dropped - a wrong gradient that looks plausible.
                di = primal%decl_index_of(primal%stmts(j)%target)
                if (di > 0) then
                    if (.not. varied(di)) then
                        varied(di) = .true.
                        changed = .true.
                    end if
                end if
            end do
        end do

        di = primal%decl_index_of(dependent)
        if (di > 0) useful(di) = .true.
        changed = .true.
        do while (changed)
            changed = .false.
            do j = primal%n_stmts, 1, -1
                if (primal%stmts(j)%kind == FAD_MOVE_ALLOC) then
                    if (.not. allocated(primal%stmts(j)%call_args)) cycle
                    if (call_reads_any(primal, primal%stmts(j), useful)) then
                        do i = 1, size(primal%stmts(j)%call_args)
                            di = call_arg_decl_index(primal, &
                                primal%stmts(j)%call_args(i))
                            if (di <= 0) cycle
                            if (.not. useful(di)) then
                                useful(di) = .true.
                                changed = .true.
                            end if
                        end do
                    end if
                    cycle
                end if
                if (primal%stmts(j)%kind == FAD_CALL_STMT) then
                    if (.not. call_reads_any(primal, primal%stmts(j), useful)) cycle
                    do i = 1, size(primal%stmts(j)%call_args)
                        di = call_arg_decl_index(primal, &
                            primal%stmts(j)%call_args(i))
                        if (di <= 0) cycle
                        if (.not. useful(di)) then
                            useful(di) = .true.
                            changed = .true.
                        end if
                    end do
                    cycle
                end if
                if (primal%stmts(j)%kind == FAD_ALLOCATE) then
                    if (primal%stmts(j)%allocation_source > 0 .and. &
                        allocated(primal%stmts(j)%allocation_args)) then
                        di = call_arg_decl_index(primal, &
                            primal%stmts(j)%allocation_args(1))
                        ! Mark SOURCE= reads in a separate statement.  The
                        ! recursive helper updates `useful`; putting it in a
                        ! compound .AND. expression made the result depend on
                        ! compiler evaluation order (GNU marked `child`, NVHPC
                        ! short-circuited it), which dropped the component
                        ! shadow only under NVHPC.
                        if (di <= 0) then
                            di = primal%decl_index_of(fad_base_name(emit_expr( &
                                primal, primal%stmts(j)%allocation_args(1))))
                        end if
                        if (di > 0) then
                            if (mark_reads(primal, primal%stmts(j)%allocation_source, &
                                useful)) changed = .true.
                        end if
                    end if
                    cycle
                end if
                if (primal%stmts(j)%kind /= FAD_ASSIGN) cycle
                di = primal%decl_index_of(primal%stmts(j)%target)
                if (di == 0) cycle
                if (.not. useful(di)) cycle
                if (mark_reads(primal, primal%stmts(j)%value, useful)) changed = .true.
            end do
        end do

        allocate (active(max(1, primal%n_decls)))
        active = varied .and. useful
    end subroutine seed_activity

    subroutine check_reverse_allocation_sources(primal, active, status)
        !! The one supported active SOURCE= copy has a fixed concrete source
        !! and a scalar polymorphic local or dummy owner.  Its dynamic type is
        !! replayed by MOLD=owner in the adjoint shadow; all other copies still
        !! require a value-copy replay tape.
        type(fad_proc_t), intent(in) :: primal
        logical, intent(in) :: active(:)
        type(reverse_status_t), intent(inout) :: status
        integer :: i

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ALLOCATE) cycle
            if (primal%stmts(i)%allocation_source <= 0) cycle
            if (.not. reads_any(primal, primal%stmts(i)%allocation_source, &
                active)) cycle
            if (allocated(primal%stmts(i)%allocation_args)) then
                if (size(primal%stmts(i)%allocation_args) >= 1) then
                    if (primal%stmts(i)%allocation_target_polymorphic .and. &
                        index(trim(emit_expr(primal, primal%stmts(i)%allocation_args(1))), &
                        "%") > 0 .and. fixed_source_component(primal, i)) cycle
                    if (has_fixed_source_owner(primal, &
                        primal%decl_index_of(emit_expr(primal, &
                        primal%stmts(i)%allocation_args(1))))) cycle
                end if
            end if
            status%ok = .false.
            status%message = "reverse mode: active ALLOCATE(SOURCE=) requires "// &
                "a value-copy replay tape; this bounded slice only retains "// &
                "allocation state"
            return
        end do
    end subroutine check_reverse_allocation_sources

    logical function has_fixed_source_owner(primal, owner_di) result(supported)
        !! The bounded reverse ownership case: one scalar polymorphic owner or
        !! one one-dimensional allocatable owner array selected at one literal
        !! element, one ALLOCATE(owner,SOURCE=concrete_declared_type), and no
        !! move or second acquisition.  The source may carry the active
        !! component.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: owner_di
        integer :: i, target_di, source_di, allocate_at
        character(len=:), allocatable :: target, source_type, dispatch_type
        character(len=:), allocatable :: selector
        logical :: found, dispatch_found, array_owner

        supported = .false.
        if (owner_di <= 0 .or. owner_di > primal%n_decls) return
        if (.not. primal%decls(owner_di)%is_polymorphic) return
        array_owner = primal%decls(owner_di)%is_array
        if (array_owner) then
            if (.not. primal%decls(owner_di)%is_allocatable) return
            if (.not. allocated(primal%decls(owner_di)%dims)) return
            if (index(trim(primal%decls(owner_di)%dims), ",") > 0) return
        end if
        if (primal%decls(owner_di)%is_associate_alias .or. &
            primal%decls(owner_di)%is_select_alias) return
        found = .false.
        allocate_at = 0
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind == FAD_ALLOCATE) then
                if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
                target = emit_expr(primal, primal%stmts(i)%allocation_args(1))
                if (array_owner) then
                    if (.not. fixed_literal_owner_shape(primal, i)) return
                    target_di = primal%decl_index_of(fad_base_name(target))
                else
                    if (primal%exprs(primal%stmts(i)%allocation_args(1))%kind /= FAD_VAR) cycle
                    target_di = primal%decl_index_of(target)
                end if
                if (target_di /= owner_di) cycle
                if (found) return
                if (primal%stmts(i)%allocation_source <= 0 .or. &
                    primal%stmts(i)%allocation_mold > 0) return
                source_di = 0
                if (primal%stmts(i)%allocation_source > 0) then
                    if (primal%exprs(primal%stmts(i)%allocation_source)%kind == FAD_VAR) then
                        source_di = primal%decl_index_of( &
                            primal%exprs(primal%stmts(i)%allocation_source)%text)
                        if (source_di > 0) then
                            if (primal%decls(source_di)%is_polymorphic) source_di = 0
                        end if
                    end if
                end if
                if (source_di <= 0) return
                found = .true.
                allocate_at = i
            else if (primal%stmts(i)%kind == FAD_MOVE_ALLOC) then
                if (.not. allocated(primal%stmts(i)%call_args)) cycle
                if (size(primal%stmts(i)%call_args) >= 1) then
                    if (fad_base_name(emit_expr(primal, &
                        primal%stmts(i)%call_args(1))) == &
                        primal%decls(owner_di)%name) return
                end if
                if (size(primal%stmts(i)%call_args) >= 2) then
                    if (fad_base_name(emit_expr(primal, &
                        primal%stmts(i)%call_args(2))) == &
                        primal%decls(owner_di)%name) return
                end if
            end if
        end do
        if (.not. found) return

        ! A retained dynamic descriptor is useful only when the following
        ! SELECT TYPE has one statically proven concrete arm.  Replaying a
        ! multi-arm choice would silently claim ownership of a path that the
        ! bounded shadow does not model, especially for CLASS(*).
        source_type = type_leaf(primal%decls(source_di)%type_name)
        dispatch_type = ""
        dispatch_found = .false.
        do i = allocate_at + 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
            selector = emit_expr(primal, primal%stmts(i)%value)
            if (array_owner) then
                if (.not. fixed_literal_owner_selector(selector, &
                    primal%decls(owner_di)%name)) cycle
            else
                if (.not. same_variable_name(selector, &
                    primal%decls(owner_di)%name)) cycle
            end if
            call fixed_dispatch_type(primal, i, dispatch_type, dispatch_found)
            exit
        end do
        if (.not. dispatch_found) return
        supported = same_variable_name(source_type, type_leaf(dispatch_type))
    end function has_fixed_source_owner

    logical function fixed_literal_owner_shape(primal, stmt_index) result(found)
        !! One explicit literal extent is the only allocatable owner-array
        !! shape retained by the current ownership replay model.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: stmt_index

        found = .false.
        if (stmt_index <= 0 .or. stmt_index > primal%n_stmts) return
        if (.not. allocated(primal%stmts(stmt_index)%allocation_args)) return
        if (size(primal%stmts(stmt_index)%allocation_args) >= 1) then
            if (fixed_literal_owner_selector(emit_expr(primal, &
                primal%stmts(stmt_index)%allocation_args(1)), &
                fad_base_name(emit_expr(primal, &
                primal%stmts(stmt_index)%allocation_args(1))))) then
                found = .true.
                return
            end if
        end if
        if (size(primal%stmts(stmt_index)%allocation_args) /= 2) return
        found = integer_literal_expr(primal, &
            primal%stmts(stmt_index)%allocation_args(2))
    end function fixed_literal_owner_shape

    logical function fixed_literal_owner_selector(selector, owner) result(found)
        !! Match one literal element such as ``owners(2)`` to its owner.
        character(len=*), intent(in) :: selector, owner
        character(len=:), allocatable :: index_text
        integer :: open, close, i, digit

        found = .false.
        if (fad_base_name(selector) /= trim(owner)) return
        open = index(trim(selector), "(")
        close = index(trim(selector), ")")
        if (open <= 1 .or. close <= open) return
        if (len_trim(selector) /= close) return
        index_text = trim(selector(open + 1:close - 1))
        if (len_trim(index_text) == 0) return
        do i = 1, len_trim(index_text)
            digit = iachar(index_text(i:i))
            if (digit < iachar("0") .or. digit > iachar("9")) return
        end do
        found = .true.
    end function fixed_literal_owner_selector

    logical function integer_literal_expr(primal, idx) result(found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        integer :: i, digit

        found = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind /= FAD_CONST) return
        if (.not. allocated(primal%exprs(idx)%text)) return
        if (len_trim(primal%exprs(idx)%text) == 0) return
        do i = 1, len_trim(primal%exprs(idx)%text)
            digit = iachar(primal%exprs(idx)%text(i:i))
            if (digit < iachar("0") .or. digit > iachar("9")) return
        end do
        found = .true.
    end function integer_literal_expr

    logical function complex_reverse_path(primal, dependent, active) result(yes)
        !! Reverse adjoints are currently real-only. Refuse a complex path
        !! before emission instead of writing compiler-invalid `real`/`aimag`
        !! partials into a complex cotangent.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: dependent
        logical, intent(in) :: active(:)
        integer :: i, di

        yes = .false.
        di = primal%decl_index(dependent)
        if (di > 0) then
            if (decl_is_complex(primal, di)) then
                yes = .true.
                return
            end if
        end if
        do i = 1, primal%n_decls
            if (.not. active(i)) cycle
            if (decl_is_complex(primal, i)) then
                yes = .true.
                return
            end if
        end do
    end function complex_reverse_path

    logical function complex_real_projection_path(primal, dependent, active) &
            result(yes)
        !! Recognise the first bounded real-coordinate reverse case.
        !!
        !! A complex input may feed a real-valued objective through a direct
        !! `real(z)`, `dble(z)`, `aimag(z)`, or nonzero `abs(z)` projection and
        !! then ordinary real arithmetic.
        !! Its adjoint is representable as a complex number whose real and
        !! imaginary parts are the two coordinate gradients.  Do not broaden
        !! this predicate to arbitrary complex expressions: a single forward
        !! seed is not enough to transpose a non-holomorphic map.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: dependent
        logical, intent(in) :: active(:)
        integer :: i, di

        yes = .false.
        di = primal%decl_index(dependent)
        if (di <= 0) return
        if (decl_is_complex(primal, di)) return

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (.not. has_active_complex(primal, primal%stmts(i)%value, active)) &
                cycle
            if (.not. safe_real_projection_expr(primal, &
                primal%stmts(i)%value, active)) return
        end do
        yes = .true.
    end function complex_real_projection_path

    recursive logical function has_active_complex(primal, idx, active) &
            result(yes)
        !! Whether an expression reads an active complex declaration.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        integer :: i, di

        yes = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        select case (primal%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            di = primal%decl_index_of(primal%exprs(idx)%text)
            if (di > 0) then
                if (di <= size(active)) then
                    yes = active(di) .and. decl_is_complex(primal, di)
                    if (yes) return
                end if
            end if
        end select
        if (.not. allocated(primal%exprs(idx)%args)) return
        do i = 1, size(primal%exprs(idx)%args)
            if (has_active_complex(primal, primal%exprs(idx)%args(i), active)) then
                yes = .true.
                return
            end if
        end do
    end function has_active_complex

    recursive logical function safe_real_projection_expr(primal, idx, active) &
            result(yes)
        !! Verify that every active complex leaf is directly projected to real
        !! coordinates.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        integer :: i
        character(len=:), allocatable :: name

        yes = .true.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (.not. has_active_complex(primal, idx, active)) return

        select case (primal%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            ! A complex leaf is only valid beneath a real/dble/aimag call. Seeing
            ! one here means the caller used complex arithmetic directly.
            yes = .false.
        case (FAD_CALL)
            name = lower_name(primal%exprs(idx)%text)
            if (.not. allocated(primal%exprs(idx)%args)) then
                yes = .false.
            else if (name == "real" .or. name == "dble" .or. name == "aimag") then
                if (size(primal%exprs(idx)%args) == 1) then
                    yes = simple_active_complex(primal, &
                        primal%exprs(idx)%args(1), active)
                else
                    yes = .false.
                end if
            else if (name == "abs") then
                if (size(primal%exprs(idx)%args) == 1) then
                    if (known_zero_expr(primal, primal%exprs(idx)%args(1))) then
                        yes = .false.
                    else
                        yes = simple_active_complex(primal, &
                            primal%exprs(idx)%args(1), active)
                    end if
                else
                    yes = .false.
                end if
            else
                yes = .false.
            end if
        case default
            if (.not. allocated(primal%exprs(idx)%args)) return
            do i = 1, size(primal%exprs(idx)%args)
                if (.not. safe_real_projection_expr(primal, &
                    primal%exprs(idx)%args(i), active)) then
                    yes = .false.
                    return
                end if
            end do
        end select
    end function safe_real_projection_expr

    logical function simple_active_complex(primal, idx, active) result(yes)
        !! The accepted projection operand is one active complex object.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        integer :: di

        yes = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind /= FAD_VAR .and. &
            primal%exprs(idx)%kind /= FAD_INDEX) return
        di = primal%decl_index_of(primal%exprs(idx)%text)
        if (di <= 0 .or. di > size(active)) return
        yes = active(di) .and. decl_is_complex(primal, di)
    end function simple_active_complex

    function complex_projection_refusal(primal, active) result(message)
        !! Name the first complex reverse boundary that rejected the path.
        type(fad_proc_t), intent(in) :: primal
        logical, intent(in) :: active(:)
        character(len=:), allocatable :: message
        integer :: i

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (has_active_zero_abs(primal, primal%stmts(i)%value, active)) then
                message = "reverse mode: abs(z) is not differentiable at "// &
                    "zero; the bounded complex abs path requires a nonzero "// &
                    "active scalar argument"
                return
            end if
        end do
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (has_active_conjg(primal, primal%stmts(i)%value, active)) then
                message = "reverse mode: active complex conjg is unsupported; "// &
                    "only direct real(z), dble(z), aimag(z), or nonzero abs(z) "// &
                    "projections are supported"
                return
            end if
        end do
        message = "reverse mode: active complex arithmetic is unsupported; "// &
            "only direct real(z), dble(z), aimag(z), or nonzero abs(z) "// &
            "projections are supported"
    end function complex_projection_refusal

    recursive logical function has_active_zero_abs(primal, idx, active) &
            result(yes)
        !! Whether an active complex path contains an ``abs`` of a known zero.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        character(len=:), allocatable :: name
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind == FAD_CALL) then
            name = lower_name(primal%exprs(idx)%text)
            if (name == "abs") then
                if (allocated(primal%exprs(idx)%args)) then
                    if (size(primal%exprs(idx)%args) == 1) then
                        if (has_active_complex(primal, &
                            primal%exprs(idx)%args(1), active)) then
                            if (known_zero_expr(primal, &
                                primal%exprs(idx)%args(1))) then
                                yes = .true.
                                return
                            end if
                        end if
                    end if
                end if
            end if
        end if
        if (.not. allocated(primal%exprs(idx)%args)) return
        do i = 1, size(primal%exprs(idx)%args)
            if (has_active_zero_abs(primal, primal%exprs(idx)%args(i), active)) then
                yes = .true.
                return
            end if
        end do
    end function has_active_zero_abs

    recursive logical function has_active_conjg(primal, idx, active) result(yes)
        !! Whether an active complex path contains an unsupported ``conjg``.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        character(len=:), allocatable :: name
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind == FAD_CALL) then
            name = lower_name(primal%exprs(idx)%text)
            if (name == "conjg") then
                if (allocated(primal%exprs(idx)%args)) then
                    if (size(primal%exprs(idx)%args) == 1) then
                        if (has_active_complex(primal, &
                            primal%exprs(idx)%args(1), active)) then
                            yes = .true.
                            return
                        end if
                    end if
                end if
            end if
        end if
        if (.not. allocated(primal%exprs(idx)%args)) return
        do i = 1, size(primal%exprs(idx)%args)
            if (has_active_conjg(primal, primal%exprs(idx)%args(i), active)) then
                yes = .true.
                return
            end if
        end do
    end function has_active_conjg

    recursive logical function known_zero_expr(primal, idx) result(yes)
        !! Prove a small set of zero-valued expressions.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        character(len=:), allocatable :: name

        yes = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        select case (primal%exprs(idx)%kind)
        case (FAD_CONST)
            yes = zero_literal(primal%exprs(idx)%text)
        case (FAD_BINOP)
            if (.not. allocated(primal%exprs(idx)%args)) return
            if (size(primal%exprs(idx)%args) /= 2) return
            if (trim(primal%exprs(idx)%text) == "-" .and. &
                primal%exprs(idx)%args(1) == primal%exprs(idx)%args(2)) then
                yes = .true.
            end if
        case (FAD_UNOP)
            if (.not. allocated(primal%exprs(idx)%args)) return
            if (size(primal%exprs(idx)%args) /= 1) return
            yes = known_zero_expr(primal, primal%exprs(idx)%args(1))
        case (FAD_CALL)
            if (.not. allocated(primal%exprs(idx)%args)) return
            name = lower_name(primal%exprs(idx)%text)
            if (name /= "cmplx") return
            if (size(primal%exprs(idx)%args) < 2) return
            yes = known_zero_expr(primal, primal%exprs(idx)%args(1))
            if (.not. yes) return
            yes = known_zero_expr(primal, primal%exprs(idx)%args(2))
        end select
    end function known_zero_expr

    logical function zero_literal(text) result(yes)
        !! Recognise real zero spellings used by lowered constants.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: normalized
        real :: value
        integer :: ios

        yes = .false.
        normalized = trim(adjustl(text))
        read (normalized, *, iostat=ios) value
        if (ios /= 0) return
        if (value > 0.0 .or. value < 0.0) return
        yes = .true.
    end function zero_literal

    pure function lower_name(value) result(out)
        !! ASCII lowercase for the two projection intrinsic names.
        character(len=*), intent(in) :: value
        character(len=len(value)) :: out
        integer :: i, code

        out = value
        do i = 1, len(value)
            code = iachar(out(i:i))
            if (code >= iachar("A") .and. code <= iachar("Z")) then
                out(i:i) = achar(code + iachar("a") - iachar("A"))
            end if
        end do
        out = trim(out)
    end function lower_name

    logical function decl_is_complex(primal, di) result(yes)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: di

        yes = .false.
        if (di <= 0 .or. di > primal%n_decls) return
        if (.not. allocated(primal%decls(di)%type_name)) return
        yes = index(primal%decls(di)%type_name, "complex") == 1 .or. &
            index(primal%decls(di)%type_name, "COMPLEX") == 1
    end function decl_is_complex

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
            di = p%decl_index_of(p%exprs(idx)%text)
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

    logical function call_reads_any(p, s, flags) result(yes)
        !! True when any actual of an opaque call reads a flagged declaration.
        type(fad_proc_t), intent(in) :: p
        type(fad_stmt_t), intent(in) :: s
        logical, intent(in) :: flags(:)
        integer :: i

        yes = .false.
        if (.not. allocated(s%call_args)) return
        do i = 1, size(s%call_args)
            if (reads_any(p, s%call_args(i), flags)) then
                yes = .true.
                return
            end if
        end do
    end function call_reads_any

    integer function call_arg_decl_index(p, idx) result(di)
        !! Declaration index for a simple structured-call actual.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx

        di = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_VAR) then
            di = p%decl_index_of(p%exprs(idx)%text)
        end if
    end function call_arg_decl_index

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
            di = p%decl_index_of(p%exprs(idx)%text)
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
        character(len=:), allocatable :: base, dependent_base
        integer :: i, j, n, di, ignored
        logical :: seen, dependent_component

        allocate (names(2*(size(primal%params) + size(spec%independents) + 4)))
        n = 0
        dependent_component = index(trim(dependent), "%") > 0
        dependent_base = fad_base_name(dependent)

        do i = 1, size(primal%params)
            ! A dependent passed as an `intent(out)` dummy is the primal value.
            ! Gradient-only callers do not want it back.
            if (.not. spec%with_primal .and. &
                trim(primal%params(i)) == dependent) cycle
            n = n + 1
            names(n) = trim(primal%params(i))
            di = primal%decl_index(trim(primal%params(i)))
            if (di == 0) cycle
            call copy_decl(d, primal%decls(di))
            ! The adjoint routine recomputes the primal, so an argument the
            ! primal only wrote is still only written here.
            ignored = adjoint%add_decl(d)
        end do

        ! The dependent: value out, adjoint seed in.  A component dependent
        ! gets a separate rank-preserving seed dummy; a component cannot be a
        ! dummy argument in Fortran, and adding `dependent//suffix` would emit
        ! an invalid designator such as `soldat(1)%a_b`.
        di = primal%decl_index(dependent_base)
        if (di > 0 .and. .not. dependent_component) then
            if (.not. is_dummy(primal, dependent) .and. spec%with_primal) then
                n = n + 1
                names(n) = dependent
                call copy_decl(d, primal%decls(di))
                d%intent = FAD_INTENT_OUT
                d%is_result = .false.
                ignored = adjoint%add_decl(d)
            end if
            n = n + 1
            names(n) = dependent//suffix
            call copy_decl(d, primal%decls(di))
            d%name = dependent//suffix
            d%intent = FAD_INTENT_IN
            d%is_result = .false.
            d%is_optional = .false.
            ignored = adjoint%add_decl(d)
        end if

        if (dependent_component) then
            n = n + 1
            names(n) = component_seed_name(dependent, suffix)
            call component_seed_decl(primal, dependent, names(n), d)
            ignored = adjoint%add_decl(d)
        end if

        ! One outgoing adjoint per independent.
        do i = 1, size(spec%independents)
            di = primal%decl_index_of(trim(spec%independents(i)))
            if (di == 0) cycle
            base = fad_base_name(trim(spec%independents(i)))
            seen = .false.
            do j = 1, i - 1
                if (fad_base_name(trim(spec%independents(j))) == base) then
                    seen = .true.
                    exit
                end if
            end do
            if (seen) cycle
            n = n + 1
            names(n) = trim(base)//suffix
            call copy_decl(d, primal%decls(di))
            d%name = trim(base)//suffix
            ! VALUE belongs to the primal argument. An outgoing adjoint is
            ! written by this routine and must remain a normal dummy.
            d%is_value = .false.
            if (is_derived_decl(primal, di) .and. &
                (component_independent_base(spec%independents, base) .or. &
                (dependent_component .and. same_component_name(base, &
                dependent_base)))) then
                ! An allocatable component of a derived adjoint must survive
                ! procedure entry.  The bounded slice requires the caller to
                ! allocate that concrete shadow before the VJP call.
                d%intent = FAD_INTENT_INOUT
            else
                d%intent = FAD_INTENT_OUT
            end if
            d%is_result = .false.
            d%is_optional = .false.
            ignored = adjoint%add_decl(d)
        end do

        ! Compile-time parameters are passive locals, so activity analysis
        ! does not select them as adjoint declarations.  They can still be
        ! referenced by an active primal expression (for example a vector
        ! subscript), and their initializer must remain available verbatim.
        do i = 1, primal%n_decls
            if (.not. primal%decls(i)%is_parameter) cycle
            if (is_dummy(primal, primal%decls(i)%name)) cycle
            call copy_decl(d, primal%decls(i))
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            d%is_optional = .false.
            ignored = adjoint%add_decl(d)
        end do

        allocate (character(len=64) :: adjoint%params(n))
        do i = 1, n
            adjoint%params(i) = names(i)
        end do
    end subroutine build_signature

    logical function component_independent_base(independents, base) result(found)
        character(len=*), intent(in) :: independents(:)
        character(len=*), intent(in) :: base
        integer :: i

        found = .false.
        do i = 1, size(independents)
            if (index(trim(independents(i)), "%") == 0) cycle
            if (fad_base_name(trim(independents(i))) == trim(base)) then
                found = .true.
                return
            end if
        end do
    end function component_independent_base

    subroutine build_forward_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
            is_element, &
            n_rec, loops, n_loops, branches, n_branches, &
            selections, n_selects, &
            calls, n_calls, &
            allocations, n_allocations, &
            order_kind, order_index, n_order, active, suffix, &
            status)
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
        !! Whether each record's target is an array element rather than a
        !! variable. An element is not versioned and its adjoint is a scatter.
        logical, allocatable, intent(out) :: is_element(:)
        integer, intent(out) :: n_rec
        type(loop_record_t), allocatable, intent(out) :: loops(:)
        integer, intent(out) :: n_loops
        type(branch_record_t), allocatable, intent(out) :: branches(:)
        integer, intent(out) :: n_branches
        type(select_record_t), allocatable, intent(out) :: selections(:)
        integer, intent(out) :: n_selects
        type(call_record_t), allocatable, intent(out) :: calls(:)
        integer, intent(out) :: n_calls
        type(allocation_record_t), allocatable, intent(out) :: allocations(:)
        integer, intent(out) :: n_allocations
        !! Blocks in forward program order, so the reverse sweep can walk them
        !! backwards. Reversing all straight-line statements and only then the
        !! loops and branches is wrong whenever a construct sits between two
        !! assignments, which it usually does.
        integer, allocatable, intent(out) :: order_kind(:), order_index(:)
        integer, intent(out) :: n_order
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(fad_stmt_t) :: snap
        character(len=:), allocatable :: actual
        type(fad_decl_t) :: d
        character(len=:), allocatable :: fresh, current, snapshot
        logical :: component_target
        type(loop_shape_t) :: shape
        integer :: i, k, di, ignored, after

        call ssa_init(primal, ssa)
        call emit_initial_component_snapshots(primal, adjoint, ssa)
        allocate (lhs_names(max(1, primal%n_stmts)))
        allocate (rhs_exprs(max(1, primal%n_stmts)))
        allocate (is_element(max(1, primal%n_stmts)))
        is_element = .false.
        allocate (loops(max(1, primal%n_stmts)))
        allocate (branches(max(1, primal%n_stmts)))
        allocate (selections(max(1, primal%n_stmts)))
        allocate (calls(max(1, primal%n_stmts)))
        allocate (allocations(max(1, primal%n_stmts)))
        allocate (order_kind(max(1, primal%n_stmts)))
        allocate (order_index(max(1, primal%n_stmts)))
        n_rec = 0
        n_loops = 0
        n_branches = 0
        n_selects = 0
        n_calls = 0
        n_allocations = 0
        n_order = 0

        i = 1
        do while (i <= primal%n_stmts)
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                ! An element write names a storage location, not a variable, so
                ! there is nothing to give a version to: `point(1)` means the
                ! same place every time it is written. It is emitted verbatim
                ! and its adjoint is a scatter into the same element of the
                ! array's adjoint.
                is_element(n_rec + 1) = index(primal%stmts(i)%target, "(") > 0 .or. &
                    index(primal%stmts(i)%target, "%") > 0
                if (is_element(n_rec + 1)) then
                    di = primal%decl_index_of( &
                        primal%stmts(i)%target)
                else
                    di = primal%decl_index_of( &
                        primal%stmts(i)%target)
                end if
                if (di == 0) then
                    status%ok = .false.
                    status%message = "assignment to undeclared '"// &
                        primal%stmts(i)%target//"'"
                    return
                end if
                s%kind = FAD_ASSIGN
                component_target = is_element(n_rec + 1) .and. &
                    index(trim(primal%stmts(i)%target), "%") > 0
                if (component_target) then
                    call component_snapshot_lookup(ssa, primal%stmts(i)%target, &
                        snapshot)
                    if (len_trim(snapshot) == 0) then
                        call add_component_snapshot(ssa, primal%stmts(i)%target, &
                            snapshot)
                        d%name = snapshot
                        d%type_name = "real(8)"
                        d%is_array = .false.
                        d%is_allocatable = .false.
                        d%is_result = .false.
                        d%is_optional = .false.
                        d%intent = FAD_INTENT_NONE
                        ignored = adjoint%add_decl(d)
                        snap%kind = FAD_ASSIGN
                        snap%target = snapshot
                        snap%value = adjoint%add_expr(expr_var( &
                            primal%stmts(i)%target))
                        ignored = adjoint%add_stmt(snap)
                    end if
                end if
                s%value = copy_renamed(primal, adjoint, primal%stmts(i)%value, ssa)
                if (component_target) call remove_component_snapshot(ssa, &
                    primal%stmts(i)%target)
                if (is_element(n_rec + 1)) then
                    fresh = primal%stmts(i)%target
                    call copy_decl(d, primal%decls(di))
                    d%is_result = .false.
                    ignored = adjoint%add_decl(d)
                else
                    call ssa_fresh(ssa, primal%stmts(i)%target, fresh)
                    call copy_decl(d, primal%decls(di))
                    d%name = fresh
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    d%is_optional = .false.
                    ignored = adjoint%add_decl(d)
                end if
                s%target = fresh
                ignored = adjoint%add_stmt(s)
                n_rec = n_rec + 1
                lhs_names(n_rec) = fresh
                rhs_exprs(n_rec) = s%value
                n_order = n_order + 1
                order_kind(n_order) = ORDER_STMT
                order_index(n_order) = n_rec
                i = i + 1

            case (FAD_DO)
                call analyse_loop(primal, i, shape, active)
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

            case (FAD_SELECT_TYPE)
                n_selects = n_selects + 1
                call emit_select_forward(primal, adjoint, ssa, i, &
                    selections(n_selects), after, status)
                if (.not. status%ok) return
                n_order = n_order + 1
                order_kind(n_order) = ORDER_SELECT
                order_index(n_order) = n_selects
                i = after

            case (FAD_CALL_STMT)
                ! Calls are opaque, but a registered rule makes their primal
                ! statement and reverse statement sequence explicit. Keep the
                ! current SSA names in both records so a call after a scalar
                ! assignment still refers to the value that reaches it.
                if (.not. call_rule_has(primal%stmts(i)%target)) then
                    status%ok = .false.
                    status%message = "no reverse rule for the call to '"// &
                        primal%stmts(i)%target//"'"
                    return
                end if
                n_calls = n_calls + 1
                calls(n_calls)%name = primal%stmts(i)%target
                allocate (calls(n_calls)%args(size(primal%stmts(i)%call_args)))
                s%kind = FAD_CALL_STMT
                s%target = primal%stmts(i)%target
                if (allocated(s%call_args)) deallocate (s%call_args)
                allocate (s%call_args(size(primal%stmts(i)%call_args)))
                do k = 1, size(s%call_args)
                    if (primal%exprs(primal%stmts(i)%call_args(k))%kind /= &
                        FAD_VAR) then
                        status%ok = .false.
                        status%message = "reverse mode: structured call arguments "// &
                            "must be simple variables"
                        return
                    end if
                    s%call_args(k) = copy_renamed( &
                        primal, adjoint, primal%stmts(i)%call_args(k), ssa)
                    actual = emit_expr(adjoint, s%call_args(k))
                    calls(n_calls)%args(k) = trim(actual)
                end do
                ignored = adjoint%add_stmt(s)
                n_order = n_order + 1
                order_kind(n_order) = ORDER_CALL
                order_index(n_order) = n_calls
                i = i + 1

            case (FAD_ALLOCATE)
                n_allocations = n_allocations + 1
                call emit_allocation_forward(primal, adjoint, ssa, &
                    primal%stmts(i), active, suffix, &
                    allocations(n_allocations), status)
                if (.not. status%ok) return
                i = i + 1

            case (FAD_MOVE_ALLOC)
                call emit_move_alloc_forward(primal, adjoint, ssa, &
                    primal%stmts(i), active, suffix, allocations, &
                    n_allocations, status)
                if (.not. status%ok) return
                i = i + 1

            case (FAD_DEALLOCATE)
                ! Retain both descriptors and payloads until the reverse sweep
                ! has consumed every expression that may read this owner.
                ! `append_allocation_cleanup` performs the original explicit
                ! deallocation after all adjoints have been propagated.
                if (allocated(primal%stmts(i)%allocation_args)) then
                    if (size(primal%stmts(i)%allocation_args) >= 1) then
                        actual = emit_expr(primal, primal%stmts(i)%allocation_args(1))
                        do k = 1, n_allocations
                            if ((allocations(k)%component .and. &
                                trim(allocations(k)%owner_path) == trim(actual)) .or. &
                                (.not. allocations(k)%component .and. &
                                trim(allocations(k)%owner) == &
                                trim(fad_base_name(actual)))) then
                                allocations(k)%deallocated = .true.
                            end if
                        end do
                    end if
                end if
                i = i + 1

            case default
                i = i + 1
            end select
        end do
    end subroutine build_forward_sweep

    subroutine emit_allocation_forward(primal, adjoint, ssa, ps, active, &
            suffix, record, status)
        !! Retain one owner and create its allocatable adjoint alongside it.
        !! The primal owner is intentionally not deallocated until the reverse
        !! sweep has finished: this is the smallest replay representation that
        !! keeps descriptor, shape, and payload available without a new tape
        !! IR.  The adjoint owner is allocated with MOLD=owner and zeroed.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(fad_stmt_t), intent(in) :: ps
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        type(allocation_record_t), intent(out) :: record
        type(reverse_status_t), intent(inout) :: status
        type(fad_decl_t) :: d
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: owner, owner_text, derivative_target
        integer :: di, i, ignored, source_di
        logical :: indexed_owner, component_owner

        status%ok = .true.
        owner_text = emit_expr(primal, ps%allocation_args(1))
        owner = fad_base_name(owner_text)
        component_owner = ps%allocation_target_component .and. &
            index(trim(owner_text), "%") > 0
        record%owner = trim(owner)
        record%owner_path = trim(owner_text)
        record%component = component_owner
        if (.not. component_owner) then
            do i = 1, primal%n_stmts
                if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
                if (fixed_literal_owner_selector(emit_expr(primal, &
                    primal%stmts(i)%value), owner)) then
                    record%selected_owner = emit_expr(primal, &
                        primal%stmts(i)%value)
                    exit
                end if
            end do
        end if
        if (ps%allocation_source > 0) then
            record%source = trim(emit_expr(primal, ps%allocation_source))
            source_di = concrete_source_decl(primal, ps%allocation_source)
            if (source_di > 0) record%source_type = &
                type_leaf(primal%decls(source_di)%type_name)
        end if
        di = primal%decl_index(trim(owner))
        if (di <= 0) then
            status%ok = .false.
            status%message = "reverse mode: allocation owner '"//trim(owner)// &
                "' is not declared"
            return
        end if

        call copy_decl(d, primal%decls(di))
        if (adjoint%decl_index(trim(owner)) == 0) then
            ignored = adjoint%add_decl(d)
        end if
        call reset_reverse_statement(s)
        s%kind = FAD_ALLOCATE
        indexed_owner = primal%exprs(ps%allocation_args(1))%kind == FAD_INDEX
        if (component_owner) then
            allocate (s%allocation_args(size(ps%allocation_args)))
            s%allocation_args(1) = copy_renamed(primal, adjoint, &
                ps%allocation_args(1), ssa)
            do i = 2, size(ps%allocation_args)
                s%allocation_args(i) = copy_renamed(primal, adjoint, &
                    ps%allocation_args(i), ssa)
            end do
        else if (indexed_owner) then
            if (allocated(primal%exprs(ps%allocation_args(1))%args)) then
                allocate (s%allocation_args(1 + size( &
                    primal%exprs(ps%allocation_args(1))%args)))
                s%allocation_args(1) = adjoint%add_expr(expr_var(trim(owner)))
                do i = 1, size(primal%exprs(ps%allocation_args(1))%args)
                    s%allocation_args(i + 1) = copy_renamed(primal, adjoint, &
                        primal%exprs(ps%allocation_args(1))%args(i), ssa)
                end do
            else
                allocate (s%allocation_args(1))
                s%allocation_args(1) = adjoint%add_expr(expr_var(trim(owner)))
            end if
        else
            allocate (s%allocation_args(size(ps%allocation_args)))
            s%allocation_args(1) = adjoint%add_expr(expr_var(trim(owner)))
            do i = 2, size(ps%allocation_args)
                s%allocation_args(i) = copy_renamed(primal, adjoint, &
                    ps%allocation_args(i), ssa)
            end do
        end if
        if (ps%allocation_source > 0) s%allocation_source = &
            copy_renamed(primal, adjoint, ps%allocation_source, ssa)
        if (ps%allocation_mold > 0) s%allocation_mold = &
            copy_renamed(primal, adjoint, ps%allocation_mold, ssa)
        ignored = adjoint%add_stmt(s)

        record%active = active(di)
        if (ps%allocation_source > 0) then
            source_di = concrete_source_decl(primal, ps%allocation_source)
            if (source_di > 0) record%active = record%active .or. active(source_di)
        end if
        if (.not. record%active) return

        d%name = trim(owner)//trim(suffix)
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_optional = .false.
        d%is_value = .false.
        if (d%is_polymorphic .and. ps%allocation_source > 0 .and. &
            .not. primal%decls(di)%is_array) then
            source_di = 0
            if (primal%exprs(ps%allocation_source)%kind == FAD_VAR) then
                source_di = primal%decl_index_of( &
                    primal%exprs(ps%allocation_source)%text)
            end if
            if (source_di > 0) then
                d%type_name = primal%decls(source_di)%type_name
                d%is_polymorphic = .false.
                d%is_unlimited_polymorphic = .false.
            end if
        end if
        if (adjoint%decl_index(d%name) == 0) then
            ignored = adjoint%add_decl(d)
        end if

        call reset_reverse_statement(s)
        s%kind = FAD_ALLOCATE
        if (component_owner) then
            allocate (s%allocation_args(1))
            derivative_target = shadow_component_path(trim(owner_text), suffix)
            s%allocation_args(1) = adjoint%add_expr(expr_var( &
                derivative_target))
        else if (indexed_owner) then
            allocate (s%allocation_args(1 + size( &
                primal%exprs(ps%allocation_args(1))%args)))
            s%allocation_args(1) = adjoint%add_expr(expr_var(trim(d%name)))
            do i = 1, size(primal%exprs(ps%allocation_args(1))%args)
                s%allocation_args(i + 1) = copy_renamed(primal, adjoint, &
                    primal%exprs(ps%allocation_args(1))%args(i), ssa)
            end do
        else if (primal%decls(di)%is_array .and. &
                size(ps%allocation_args) > 1) then
            allocate (s%allocation_args(size(ps%allocation_args)))
            s%allocation_args(1) = adjoint%add_expr(expr_var(trim(d%name)))
            do i = 2, size(ps%allocation_args)
                s%allocation_args(i) = copy_renamed(primal, adjoint, &
                    ps%allocation_args(i), ssa)
            end do
        else
            allocate (s%allocation_args(1))
            s%allocation_args(1) = adjoint%add_expr(expr_var(d%name))
        end if
        if (d%is_polymorphic) then
            s%allocation_mold = adjoint%add_expr(expr_var(trim(owner)))
        else if (ps%allocation_source > 0) then
            s%allocation_source = copy_renamed(primal, adjoint, &
                ps%allocation_source, ssa)
        else if (component_owner .and. allocatable_component_rank(primal, &
                owner_text) > 0) then
            s%allocation_mold = copy_renamed(primal, adjoint, &
                ps%allocation_args(1), ssa)
        else if (.not. component_owner) then
            s%allocation_mold = adjoint%add_expr(expr_var(trim(owner)))
        end if
        ignored = adjoint%add_stmt(s)

        if (component_owner) then
            call emit_zero_component_shadow(primal, adjoint, active, suffix, &
                record)
        else if (d%is_polymorphic .and. primal%decls(di)%is_array) then
            call emit_zero_owner_array_shadow(primal, adjoint, suffix, record)
        end if

        ! A polymorphic shadow cannot be assigned a scalar zero.  Its fixed
        ! SOURCE= dynamic type is retained by MOLD=owner; the active concrete
        ! component is initialized by zero_component_adjoints below.
        if (.not. component_owner .and. .not. d%is_polymorphic .and. &
            index(trim(d%type_name), "type(") /= 1) then
            call reset_reverse_statement(s)
            s%kind = FAD_ASSIGN
            s%target = d%name
            s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
            ignored = adjoint%add_stmt(s)
        end if
        if (component_owner .and. .not. ps%allocation_target_polymorphic .and. &
            is_concrete_allocatable_component_path(primal, owner_text)) then
            call reset_reverse_statement(s)
            s%kind = FAD_ASSIGN
            s%target = shadow_component_path(trim(owner_text), suffix)
            s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
            ignored = adjoint%add_stmt(s)
        end if
    end subroutine emit_allocation_forward

    subroutine emit_zero_owner_array_shadow(primal, adjoint, suffix, record)
        !! Reset concrete REAL leaves of a polymorphic allocatable array shadow
        !! while its dynamic type is still available through SELECT TYPE.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        character(len=*), intent(in) :: suffix
        type(allocation_record_t), intent(in) :: record
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: source, target, tail, alias, component_text
        integer :: i, ignored, n_real
        logical :: opened

        if (.not. allocated(record%source) .or. &
            .not. allocated(record%source_type)) return
        source = trim(record%source)
        target = trim(record%owner)//trim(suffix)
        alias = "fad_owner_array_shadow_b"
        opened = .false.
        n_real = 0
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (.not. primal%stmts(i)%target_is_component_path) cycle
            if (.not. primal%stmts(i)%target_component_is_real) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            if (index(trim(primal%stmts(i)%target), source//"%") /= 1) cycle
            component_text = trim(primal%stmts(i)%target)
            if (.not. opened) then
                call reset_reverse_statement(s)
                s%kind = FAD_SELECT_TYPE
                s%value = adjoint%add_expr(expr_var(target))
                s%target = alias
                ignored = adjoint%add_stmt(s)
                call reset_reverse_statement(s)
                s%kind = FAD_TYPE_IS
                s%target = record%source_type
                ignored = adjoint%add_stmt(s)
                opened = .true.
            end if
            tail = component_text(len_trim(source) + 1:)
            call reset_reverse_statement(s)
            s%kind = FAD_ASSIGN
            s%target = alias//tail
            s%value = adjoint%add_expr(expr_const( &
                "0.0"//adjoint%real_suffix))
            ignored = adjoint%add_stmt(s)
            n_real = n_real + 1
        end do
        if (opened .and. n_real > 0) then
            call reset_reverse_statement(s)
            s%kind = FAD_END_SELECT
            ignored = adjoint%add_stmt(s)
        end if
    end subroutine emit_zero_owner_array_shadow

    subroutine emit_zero_component_shadow(primal, adjoint, active, suffix, &
            record)
        !! A component shadow is allocated with SOURCE= so its dynamic type
        !! is proven, then its real scalar payload leaves are reset before
        !! reverse accumulation.  The payload is never assigned a scalar
        !! directly while it is CLASS(base_t).
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        type(allocation_record_t), intent(in) :: record
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: source, target, tail, alias, component_text
        integer :: i, ignored, n_real
        logical :: opened

        associate (unused_active => active, unused_suffix => suffix)
        end associate
        if (.not. record%component .or. .not. allocated(record%source) .or. &
            .not. allocated(record%source_type)) return
        source = trim(record%source)
        target = shadow_component_path(trim(record%owner_path), suffix)
        alias = "fad_component_shadow_b"
        opened = .false.
        n_real = 0
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (.not. primal%stmts(i)%target_is_component_path) cycle
            if (.not. primal%stmts(i)%target_component_is_real) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            if (index(trim(primal%stmts(i)%target), source//"%") /= 1) cycle
            component_text = trim(primal%stmts(i)%target)
            if (.not. opened) then
                call reset_reverse_statement(s)
                s%kind = FAD_SELECT_TYPE
                s%value = adjoint%add_expr(expr_var(target))
                s%target = alias
                ignored = adjoint%add_stmt(s)
                call reset_reverse_statement(s)
                s%kind = FAD_TYPE_IS
                s%target = record%source_type
                ignored = adjoint%add_stmt(s)
                opened = .true.
            end if
            tail = component_text(len_trim(source) + 1:)
            call reset_reverse_statement(s)
            s%kind = FAD_ASSIGN
            s%target = alias//tail
            s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
            ignored = adjoint%add_stmt(s)
            n_real = n_real + 1
        end do
        if (opened .and. n_real > 0) then
            call reset_reverse_statement(s)
            s%kind = FAD_END_SELECT
            ignored = adjoint%add_stmt(s)
        end if
    end subroutine emit_zero_component_shadow

    subroutine emit_move_alloc_forward(primal, adjoint, ssa, ps, active, &
            suffix, allocations, n_allocations, status)
        !! Transfer the retained primal and tangent owners together.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(fad_stmt_t), intent(in) :: ps
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        type(allocation_record_t), intent(inout) :: allocations(:)
        integer, intent(in) :: n_allocations
        type(reverse_status_t), intent(inout) :: status
        type(fad_decl_t) :: d
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: source, target
        integer :: source_di, target_di, i, ignored
        logical :: owner_found, component_owner

        status%ok = .true.
        source = emit_expr(primal, ps%call_args(1))
        target = emit_expr(primal, ps%call_args(2))
        component_owner = index(trim(source), "%") > 0
        source_di = primal%decl_index(fad_base_name(source))
        target_di = primal%decl_index(fad_base_name(target))
        if (source_di <= 0 .or. target_di <= 0) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc owners are not declared"
            return
        end if
        owner_found = .false.
        do i = 1, n_allocations
            if ((component_owner .and. allocations(i)%component .and. &
                trim(allocations(i)%owner_path) == trim(source)) .or. &
                (.not. component_owner .and. .not. allocations(i)%component .and. &
                trim(allocations(i)%owner) == trim(source))) then
                owner_found = .true.
                if (component_owner) then
                    allocations(i)%previous_owner = trim(allocations(i)%owner_path)
                    allocations(i)%owner = trim(target)
                    allocations(i)%owner_path = trim(target)
                else
                    allocations(i)%previous_owner = trim(source)
                    allocations(i)%owner = trim(target)
                end if
                ! The allocation may already be active through its concrete
                ! SOURCE= payload even when the enclosing base itself is not
                ! marked active.  MOVE_ALLOC transfers that shadow; it must
                ! not erase the earlier ownership fact.
                allocations(i)%active = allocations(i)%active .or. &
                    active(source_di) .or. active(target_di)
                exit
            end if
        end do
        if (.not. owner_found) then
            status%ok = .false.
            status%message = "reverse mode: move_alloc source has no retained "// &
                "allocation owner"
            return
        end if

        call copy_decl(d, primal%decls(target_di))
        if (component_owner) d%name = fad_base_name(target)
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_optional = .false.
        if (adjoint%decl_index(trim(target)) == 0) then
            ignored = adjoint%add_decl(d)
        end if
        call reset_reverse_statement(s)
        s%kind = FAD_MOVE_ALLOC
        allocate (s%call_args(2))
        s%call_args(1) = copy_renamed(primal, adjoint, ps%call_args(1), ssa)
        s%call_args(2) = copy_renamed(primal, adjoint, ps%call_args(2), ssa)
        ignored = adjoint%add_stmt(s)

        if (.not. allocations(i)%active) return
        if (component_owner) then
            d%name = fad_base_name(target)//trim(suffix)
        else
            d%name = trim(target)//trim(suffix)
        end if
        d%is_allocatable = .not. component_owner
        if (adjoint%decl_index(d%name) == 0) then
            ignored = adjoint%add_decl(d)
        end if
        call reset_reverse_statement(s)
        s%kind = FAD_MOVE_ALLOC
        allocate (s%call_args(2))
        if (component_owner) then
            s%call_args(1) = adjoint%add_expr(expr_var( &
                shadow_component_path(trim(source), suffix)))
            s%call_args(2) = adjoint%add_expr(expr_var( &
                shadow_component_path(trim(target), suffix)))
        else
            s%call_args(1) = adjoint%add_expr(expr_var(trim(source)//trim(suffix)))
            s%call_args(2) = adjoint%add_expr(expr_var(trim(target)//trim(suffix)))
        end if
        ignored = adjoint%add_stmt(s)
    end subroutine emit_move_alloc_forward

    subroutine append_allocation_cleanup(primal, adjoint, allocations, &
            n_allocations, suffix)
        !! Finish the original explicit lifetime after all reverse reads.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(allocation_record_t), intent(in) :: allocations(:)
        integer, intent(in) :: n_allocations
        character(len=*), intent(in) :: suffix
        type(fad_stmt_t) :: s
        integer :: i, di, ignored

        do i = n_allocations, 1, -1
            if (.not. allocations(i)%deallocated) cycle
            if (allocations(i)%component) then
                di = primal%decl_index(fad_base_name(trim(allocations(i)%owner_path)))
            else
                di = primal%decl_index(trim(allocations(i)%owner))
            end if
            if (di <= 0) cycle
            if (allocations(i)%component) then
                if (allocations(i)%active .and. adjoint%decl_index( &
                    fad_base_name(trim(allocations(i)%owner_path))// &
                    trim(suffix)) > 0) then
                    call reset_reverse_statement(s)
                    s%kind = FAD_DEALLOCATE
                    allocate (s%allocation_args(1))
                    s%allocation_args(1) = adjoint%add_expr(expr_var( &
                        shadow_component_path(trim(allocations(i)%owner_path), suffix)))
                    ignored = adjoint%add_stmt(s)
                end if
                call reset_reverse_statement(s)
                s%kind = FAD_DEALLOCATE
                allocate (s%allocation_args(1))
                s%allocation_args(1) = adjoint%add_expr(expr_var( &
                    trim(allocations(i)%owner_path)))
                ignored = adjoint%add_stmt(s)
                cycle
            end if
            if (allocations(i)%active .and. adjoint%decl_index( &
                trim(allocations(i)%owner)//trim(suffix)) > 0) then
                call reset_reverse_statement(s)
                s%kind = FAD_DEALLOCATE
                allocate (s%allocation_args(1))
                s%allocation_args(1) = adjoint%add_expr(expr_var( &
                    trim(allocations(i)%owner)//trim(suffix)))
                ignored = adjoint%add_stmt(s)
            end if
            call reset_reverse_statement(s)
            s%kind = FAD_DEALLOCATE
            allocate (s%allocation_args(1))
            s%allocation_args(1) = adjoint%add_expr(expr_var( &
                trim(allocations(i)%owner)))
            ignored = adjoint%add_stmt(s)
        end do
    end subroutine append_allocation_cleanup

    subroutine emit_source_component_adjoint_for_target(primal, adjoint, &
            lhs, suffix, allocations, n_allocations)
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        character(len=*), intent(in) :: lhs, suffix
        type(allocation_record_t), intent(in) :: allocations(:)
        integer, intent(in) :: n_allocations
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: source, payload, target, owner_path
        character(len=:), allocatable :: source_alias
        character(len=32) :: source_number
        integer :: i, percent, ignored, owner_di

        percent = index(trim(lhs), "%")
        if (percent <= 0) return
        do i = 1, n_allocations
            if (.not. allocated(allocations(i)%source)) cycle
            source = trim(allocations(i)%source)
            if (index(trim(lhs), source//"%") /= 1) cycle
            payload = trim(lhs(percent:))
            owner_path = allocations(i)%owner
            if (allocations(i)%component .and. &
                allocated(allocations(i)%owner_path)) then
                owner_path = allocations(i)%owner_path
            end if
            if (allocations(i)%component) then
                target = shadow_component_path(trim(owner_path), suffix)//payload
                if (.not. allocated(allocations(i)%source_type)) return
                write (source_number, '(i0)') i
                source_alias = "fad_source_component_b_"//trim(source_number)
                call reset_reverse_statement(s)
                s%kind = FAD_SELECT_TYPE
                s%value = adjoint%add_expr(expr_var( &
                    shadow_component_path(trim(owner_path), suffix)))
                s%target = source_alias
                ignored = adjoint%add_stmt(s)
                call reset_reverse_statement(s)
                s%kind = FAD_TYPE_IS
                s%target = allocations(i)%source_type
                ignored = adjoint%add_stmt(s)
                call reset_reverse_statement(s)
                s%kind = FAD_ASSIGN
                s%target = shadow_component_path(trim(lhs), suffix)
                s%value = adjoint%add_expr(expr_var(source_alias//payload))
                ignored = adjoint%add_stmt(s)
                call reset_reverse_statement(s)
                s%kind = FAD_END_SELECT
                ignored = adjoint%add_stmt(s)
            else
                if (allocated(allocations(i)%selected_owner)) then
                    target = shadow_component_path( &
                        trim(allocations(i)%selected_owner), suffix)//payload
                else
                    target = trim(owner_path)//trim(suffix)//payload
                end if
                ! An element selected from a polymorphic owner array remains
                ! polymorphic in the reverse shadow.  Keep the source copy
                ! inside the same concrete guard; emitting OWNER_B(i)%field
                ! after END SELECT is invalid because the declared base type
                ! has no knowledge of the leaf component.
                owner_di = primal%decl_index(trim(allocations(i)%owner))
                if (allocated(allocations(i)%selected_owner) .and. &
                    allocated(allocations(i)%source_type) .and. owner_di > 0 .and. &
                    primal%decls(owner_di)%is_array .and. &
                    primal%decls(owner_di)%is_polymorphic) then
                    write (source_number, '(i0)') i
                    source_alias = "fad_source_owner_b_"//trim(source_number)
                    call reset_reverse_statement(s)
                    s%kind = FAD_SELECT_TYPE
                    s%value = adjoint%add_expr(expr_var(shadow_component_path( &
                        trim(allocations(i)%selected_owner), suffix)))
                    s%target = source_alias
                    ignored = adjoint%add_stmt(s)
                    call reset_reverse_statement(s)
                    s%kind = FAD_TYPE_IS
                    s%target = allocations(i)%source_type
                    ignored = adjoint%add_stmt(s)
                    call reset_reverse_statement(s)
                    s%kind = FAD_ASSIGN
                    s%target = fad_suffix_name(trim(lhs), suffix)
                    s%value = adjoint%add_expr(expr_var(source_alias//payload))
                    ignored = adjoint%add_stmt(s)
                    call reset_reverse_statement(s)
                    s%kind = FAD_END_SELECT
                    ignored = adjoint%add_stmt(s)
                    return
                end if
                call reset_reverse_statement(s)
                s%kind = FAD_ASSIGN
                s%target = fad_suffix_name(trim(lhs), suffix)
                s%value = adjoint%add_expr(expr_var(target))
                ignored = adjoint%add_stmt(s)
            end if
            return
        end do
    end subroutine emit_source_component_adjoint_for_target

    subroutine reset_reverse_statement(s)
        type(fad_stmt_t), intent(out) :: s

        s%kind = 0
        s%value = 0
        s%lo = 0
        s%hi = 0
        s%step = 0
        if (allocated(s%target)) deallocate (s%target)
        if (allocated(s%call_args)) deallocate (s%call_args)
        if (allocated(s%allocation_args)) deallocate (s%allocation_args)
        s%allocation_source = 0
        s%allocation_mold = 0
    end subroutine reset_reverse_statement

    subroutine emit_select_forward(primal, adjoint, ssa, first, rec, after, &
            status)
        !! Re-emit one SELECT TYPE and put every guard arm in an independent
        !! SSA state. The dynamic type is a passive runtime choice; all arms
        !! must advance the same variables by the same number of versions so a
        !! single post-select state exists without differentiating that choice.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(inout) :: ssa
        integer, intent(in) :: first
        type(select_record_t), intent(out) :: rec
        integer, intent(out) :: after
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        type(ssa_map_t) :: before, arm_ssa, common
        integer, allocatable :: guard_at(:)
        integer :: a, body_last, depth, end_at, i, ignored, n_guards
        logical :: has_default

        after = first + 1
        allocate (guard_at(max(1, primal%n_stmts - first)))
        depth = 0
        end_at = 0
        n_guards = 0
        has_default = .false.
        do i = first, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_SELECT_TYPE)
                depth = depth + 1
            case (FAD_END_SELECT)
                depth = depth - 1
                if (depth == 0) then
                    end_at = i
                    exit
                end if
            case (FAD_TYPE_IS, FAD_CLASS_IS, FAD_CLASS_DEFAULT)
                if (depth /= 1) cycle
                n_guards = n_guards + 1
                guard_at(n_guards) = i
                if (primal%stmts(i)%kind == FAD_CLASS_DEFAULT) then
                    has_default = .true.
                end if
            end select
        end do
        if (end_at == 0) then
            status%ok = .false.
            status%message = "unterminated select type construct"
            return
        end if
        if (n_guards == 0) then
            status%ok = .false.
            status%message = "reverse mode: select type has no guard arms"
            return
        end if
        after = end_at + 1

        rec%n_arms = n_guards
        if (.not. has_default) rec%n_arms = rec%n_arms + 1
        allocate (rec%arms(rec%n_arms))
        rec%selector = copy_renamed(primal, adjoint, &
            primal%stmts(first)%value, ssa)
        if (allocated(primal%stmts(first)%target)) then
            rec%selector_alias = primal%stmts(first)%target
        end if
        before = ssa

        s%kind = FAD_SELECT_TYPE
        s%value = rec%selector
        if (allocated(rec%selector_alias)) s%target = rec%selector_alias
        ignored = adjoint%add_stmt(s)

        do a = 1, n_guards
            rec%arms(a)%kind = primal%stmts(guard_at(a))%kind
            if (allocated(primal%stmts(guard_at(a))%target)) then
                rec%arms(a)%target = primal%stmts(guard_at(a))%target
            end if
            s%kind = rec%arms(a)%kind
            s%value = 0
            if (allocated(s%target)) deallocate (s%target)
            if (allocated(rec%arms(a)%target)) s%target = rec%arms(a)%target
            ignored = adjoint%add_stmt(s)

            body_last = end_at - 1
            if (a < n_guards) body_last = guard_at(a + 1) - 1
            allocate (rec%arms(a)%lhs(max(1, body_last - guard_at(a))))
            allocate (rec%arms(a)%rhs(max(1, body_last - guard_at(a))))
            arm_ssa = before
            call emit_arm(primal, adjoint, arm_ssa, guard_at(a) + 1, &
                body_last, rec%arms(a)%lhs, rec%arms(a)%rhs, &
                rec%arms(a)%n, status)
            if (.not. status%ok) return
            if (a == 1) then
                common = arm_ssa
            else
                if (.not. select_ssa_state_compatible(primal, common, arm_ssa, &
                    end_at)) then
                    status%ok = .false.
                    status%message = "reverse mode: select type arms must assign "// &
                        "the same variables the same number of times"
                    return
                end if
            end if
        end do

        if (.not. has_default) then
            a = rec%n_arms
            rec%arms(a)%kind = FAD_CLASS_DEFAULT
            allocate (rec%arms(a)%lhs(max(1, primal%n_decls)))
            allocate (rec%arms(a)%rhs(max(1, primal%n_decls)))
            s%kind = FAD_CLASS_DEFAULT
            s%value = 0
            if (allocated(s%target)) deallocate (s%target)
            ignored = adjoint%add_stmt(s)
            do i = 1, primal%n_decls
                if (trim(common%current(i)) == trim(before%current(i))) cycle
                s%kind = FAD_ASSIGN
                s%target = trim(common%current(i))
                s%value = adjoint%add_expr(expr_var(trim(before%current(i))))
                ignored = adjoint%add_stmt(s)
                rec%arms(a)%n = rec%arms(a)%n + 1
                rec%arms(a)%lhs(rec%arms(a)%n) = trim(common%current(i))
                rec%arms(a)%rhs(rec%arms(a)%n) = s%value
            end do
        end if

        s%kind = FAD_END_SELECT
        s%value = 0
        if (allocated(s%target)) deallocate (s%target)
        ignored = adjoint%add_stmt(s)
        ssa = common
    end subroutine emit_select_forward

    logical function same_ssa_state(a, b) result(same)
        type(ssa_map_t), intent(in) :: a, b
        integer :: i

        same = .false.
        if (a%n /= b%n) return
        do i = 1, a%n
            if (trim(a%current(i)) /= trim(b%current(i))) return
            if (a%version(i) /= b%version(i)) return
        end do
        same = .true.
    end function same_ssa_state

    logical function select_ssa_state_compatible(primal, a, b, end_at) result(ok)
        !! A type-bound implementation may introduce a private result local
        !! while it is inlined into one SELECT TYPE arm.  Such a local is not
        !! part of the state after the dispatch and may therefore have a
        !! different name in each arm.  Require exact SSA agreement for every
        !! value used after the select; private arm-only locals are ignored.
        type(fad_proc_t), intent(in) :: primal
        type(ssa_map_t), intent(in) :: a, b
        integer, intent(in) :: end_at
        integer :: i

        ok = .false.
        if (a%n /= b%n) return
        do i = 1, a%n
            if (trim(a%current(i)) == trim(b%current(i)) .and. &
                a%version(i) == b%version(i)) cycle
            if (select_name_used_after(primal, trim(a%base(i)), end_at)) return
        end do
        ok = .true.
    end function select_ssa_state_compatible

    logical function select_name_used_after(primal, name, end_at) result(used)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: name
        integer, intent(in) :: end_at
        integer :: i

        used = .false.
        do i = end_at + 1, primal%n_stmts
            if (allocated(primal%stmts(i)%target)) then
                if (same_variable_name(fad_base_name(primal%stmts(i)%target), &
                    name)) then
                    used = .true.
                    return
                end if
            end if
            if (select_expr_mentions(primal, primal%stmts(i)%value, name)) then
                used = .true.
                return
            end if
            if (select_expr_mentions(primal, primal%stmts(i)%lo, name) .or. &
                select_expr_mentions(primal, primal%stmts(i)%hi, name) .or. &
                select_expr_mentions(primal, primal%stmts(i)%step, name)) then
                used = .true.
                return
            end if
            if (allocated(primal%stmts(i)%call_args)) then
                if (any_call_arg_mentions(primal, primal%stmts(i)%call_args, &
                    name)) then
                    used = .true.
                    return
                end if
            end if
        end do
    end function select_name_used_after

    recursive logical function select_expr_mentions(primal, idx, name) &
            result(found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind == FAD_VAR .or. &
            primal%exprs(idx)%kind == FAD_INDEX) then
            if (same_variable_name(fad_base_name(primal%exprs(idx)%text), name)) then
                found = .true.
                return
            end if
        end if
        if (.not. allocated(primal%exprs(idx)%args)) return
        do i = 1, size(primal%exprs(idx)%args)
            if (select_expr_mentions(primal, primal%exprs(idx)%args(i), name)) then
                found = .true.
                return
            end if
        end do
    end function select_expr_mentions

    logical function any_call_arg_mentions(primal, args, name) result(found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: args(:)
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, size(args)
            if (select_expr_mentions(primal, args(i), name)) then
                found = .true.
                return
            end if
        end do
    end function any_call_arg_mentions

    logical function same_variable_name(a, b) result(equal)
        character(len=*), intent(in) :: a, b
        integer :: i

        equal = len_trim(a) == len_trim(b)
        if (.not. equal) return
        do i = 1, len_trim(a)
            if (lower_name_char(a(i:i)) /= lower_name_char(b(i:i))) then
                equal = .false.
                return
            end if
        end do
    end function same_variable_name

    character function lower_name_char(c)
        character, intent(in) :: c

        lower_name_char = c
        if (c >= "A" .and. c <= "Z") then
            lower_name_char = achar(iachar(c) + iachar("a") - iachar("A"))
        end if
    end function lower_name_char

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
            call copy_decl(d, primal%decls(i))
            d%name = merged
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            d%is_optional = .false.
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
            di = primal%decl_index_of(primal%stmts(i)%target)
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
            call copy_decl(d, primal%decls(di))
            d%name = fresh
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            d%is_optional = .false.
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
            call copy_decl(d, primal%decls(di))
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            d%is_optional = .false.
            ignored = adjoint%add_decl(d)
        else
            ! Fixed-form Tapenade regressions commonly rely on implicit
            ! integer typing for a loop index.  Generated procedures use
            ! implicit none, so synthesize the missing local declaration.
            ignored = adjoint%add_decl_fields(rec%var, "integer", &
                FAD_INTENT_NONE, .false., .false., .false., .false., "")
        end if

        ! Each accumulator gets one local for the whole loop, seeded from its
        ! incoming SSA value. After the loop that local is the current version.
        allocate (rec%accum_names(max(1, shape%n_accumulators)))
        do k = 1, shape%n_accumulators
            call ssa_lookup(ssa, trim(shape%accumulators(k)), incoming)
            call ssa_fresh(ssa, trim(shape%accumulators(k)), fresh)
            di = primal%decl_index(trim(shape%accumulators(k)))
            call copy_decl(d, primal%decls(di))
            d%name = fresh
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            d%is_optional = .false.
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
            call copy_decl(d, primal%decls(di))
            d%intent = FAD_INTENT_NONE
            d%is_result = .false.
            d%is_optional = .false.
            ignored = adjoint%add_decl(d)
        end do

        ! A taped loop needs one array per carried variable, sized from the
        ! loop bounds. The tape is the price of a recurrence that cannot be
        ! recomputed; everything else in this module exists to avoid paying it.
        ! A tape is worth setting up whenever some temporary is expensive to
        ! rebuild, not only when a carried variable forces one. Recomputing a
        ! division or a transcendental in the reverse sweep costs tens of
        ! cycles; a load costs a few. Tapes that turn out unused are removed by
        ! dead-array elimination, so being generous here is free.
        ! The tape index is built from one loop variable, so a tape can only
        ! address a single level. A nest would need one index per level, and
        ! taping under a single index silently keeps just the innermost
        ! iteration's value - a wrong gradient, not an error. Carried variables
        ! in a nest are already refused for the same reason; temporaries have
        ! the cheaper way out of being recomputed, so take it.
        rec%taped = shape%n_carried > shape%n_linear
        if (shape%n_headers > 1) then
            rec%taped = .false.
        else if (.not. rec%taped) then
            do k = 1, shape%n_temporaries
                if (worth_taping(primal, shape, trim(shape%temporaries(k)))) then
                    rec%taped = .true.
                    exit
                end if
            end do
        end if
        if (shape%n_carried > 0) allocate (carried_entry(max(1, shape%n_carried)))
        if (.not. allocated(carried_entry)) allocate (carried_entry(1))
        if (rec%taped) then
            block
                use fortad_emit, only: emit_expr
                character(len=:), allocatable :: lo_text, hi_text
                lo_text = emit_expr(adjoint, rec%lo)
                hi_text = emit_expr(adjoint, rec%hi)
                rec%tape_index = rec%var//" - ("//lo_text//") + 1"
                do k = 1, shape%n_temporaries
                    ! A loop that already pays for a tape should not also pay
                    ! to recompute. One more array of the same length turns a
                    ! whole recomputed body into a load, and the store was
                    ! going to touch that cache line anyway.
                    di = primal%decl_index(trim(shape%temporaries(k)))
                    if (di == 0) cycle
                    if (primal%decls(di)%is_array) cycle
                    if (.not. is_real_type(primal%decls(di))) cycle
                    if (.not. worth_taping(primal, shape, &
                        trim(shape%temporaries(k)))) cycle
                    call copy_decl(d, primal%decls(di))
                    d%name = trim(shape%temporaries(k))//"_tape"
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    d%is_optional = .false.
                    d%is_array = .true.
                    d%dims = "("//hi_text//") - ("//lo_text//") + 1"
                    ignored = adjoint%add_decl(d)
                end do
                do k = 1, shape%n_carried
                    ! A carried variable the body is linear in needs no tape:
                    ! its adjoint coefficient is built from values that do not
                    ! depend on it, so the reverse sweep never reads it.
                    if (is_known_name(shape%linear, shape%n_linear, &
                        trim(shape%carried(k)))) cycle
                    di = primal%decl_index(trim(shape%carried(k)))
                    if (di == 0) cycle
                    call copy_decl(d, primal%decls(di))
                    d%name = trim(shape%carried(k))//"_tape"
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    d%is_optional = .false.
                    d%is_array = .true.
                    d%dims = "("//hi_text//") - ("//lo_text//") + 1"
                    ignored = adjoint%add_decl(d)
                end do
            end block
        end if

        ! Every carried variable needs its own local, its seed from before the
        ! loop, and its SSA suspension - whether or not it is taped. Only the
        ! tape array above depends on that.
        if (shape%n_carried > 0) then
            if (.not. allocated(carried_entry)) then
                allocate (carried_entry(max(1, shape%n_carried)))
            end if
            do k = 1, shape%n_carried
                di = primal%decl_index(trim(shape%carried(k)))
                if (di == 0) cycle
                ! Seed the carrier from whatever the variable held before the
                ! loop, or the first iteration reads an undefined value.
                call ssa_lookup(ssa, trim(shape%carried(k)), incoming)
                carried_entry(k) = incoming
                if (incoming /= trim(shape%carried(k))) then
                    s%kind = FAD_ASSIGN
                    s%target = trim(shape%carried(k))
                    s%value = adjoint%add_expr(expr_var(incoming))
                    ignored = adjoint%add_stmt(s)
                end if
                ! The loop-entry value lives under the plain name; each
                ! assignment inside the body gets its own SSA version, so the
                ! reverse sweep can tell the pre-update value from the
                ! post-update one. Conflating them was the first bug here.
                call ssa_set(ssa, trim(shape%carried(k)), &
                    trim(shape%carried(k)))
                call copy_decl(d, primal%decls(di))
                d%intent = FAD_INTENT_NONE
                d%is_result = .false.
                d%is_optional = .false.
                ignored = adjoint%add_decl(d)
            end do
        end if

        ! Reproduce every level of the nest, outermost first.
        allocate (rec%nest_var(shape%n_headers), rec%nest_lo(shape%n_headers))
        allocate (rec%nest_hi(shape%n_headers), rec%nest_step(shape%n_headers))
        rec%n_levels = shape%n_headers
        do k = 1, shape%n_headers
            associate (h => primal%stmts(shape%header_stmt(k)))
                rec%nest_var(k) = h%target
                rec%nest_lo(k) = copy_renamed(primal, adjoint, h%lo, ssa)
                rec%nest_hi(k) = copy_renamed(primal, adjoint, h%hi, ssa)
                rec%nest_step(k) = 0
                if (h%step /= 0) then
                    rec%nest_step(k) = copy_renamed(primal, adjoint, h%step, ssa)
                end if
                di = primal%decl_index(h%target)
                if (di > 0) then
                    call copy_decl(d, primal%decls(di))
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    d%is_optional = .false.
                    ignored = adjoint%add_decl(d)
                else
                    ignored = adjoint%add_decl_fields(h%target, "integer", &
                        FAD_INTENT_NONE, .false., .false., .false., .false., "")
                end if
            end associate
        end do

        do k = 1, rec%n_levels
            s%kind = FAD_DO
            s%target = trim(rec%nest_var(k))
            s%lo = rec%nest_lo(k)
            s%hi = rec%nest_hi(k)
            s%step = rec%nest_step(k)
            if (k == 1) then
                rec%do_stmt = adjoint%add_stmt(s)
            else
                ignored = adjoint%add_stmt(s)
            end if
        end do

        ! Store each carried value as it enters the iteration.
        if (rec%taped) then
            do k = 1, shape%n_carried
                if (adjoint%decl_index(trim(shape%carried(k))//"_tape") == 0) cycle
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
                    cdi = primal%decl_index_of( &
                        primal%stmts(i)%target)
                    call copy_decl(cd, primal%decls(cdi))
                    cd%name = fresh
                    cd%intent = FAD_INTENT_NONE
                    cd%is_result = .false.
                    cd%is_optional = .false.
                    ignored = adjoint%add_decl(cd)
                end block
            else if (index(primal%stmts(i)%target, "(") > 0) then
                fresh = primal%stmts(i)%target
                ! An element target is not renamed, so its array still needs a
                ! declaration in the generated procedure when it was a local.
                block
                    integer :: base_di
                    type(fad_decl_t) :: bd
                    base_di = primal%decl_index_of(fresh)
                    if (base_di > 0) then
                        call copy_decl(bd, primal%decls(base_di))
                        bd%is_result = .false.
                        ignored = adjoint%add_decl(bd)
                    end if
                end block
            else if (is_known_name(shape%temporaries, shape%n_temporaries, &
                    primal%stmts(i)%target)) then
                ! A per-iteration temporary gets a version per write, like any
                ! straight-line assignment. Holding it to one name is what made
                ! a second write ambiguous, and the loop was refused for it -
                ! bundle adjustment writes `qx` three times. An accumulator is
                ! different and keeps its single name: it is the same storage
                ! across iterations by definition.
                call ssa_fresh(ssa, primal%stmts(i)%target, fresh)
                block
                    integer :: tdi
                    type(fad_decl_t) :: td
                    tdi = primal%decl_index_of( &
                        primal%stmts(i)%target)
                    if (tdi > 0) then
                        call copy_decl(td, primal%decls(tdi))
                        td%name = fresh
                        td%intent = FAD_INTENT_NONE
                        td%is_result = .false.
                        td%is_optional = .false.
                        ignored = adjoint%add_decl(td)
                    end if
                end block
            else
                call ssa_lookup(ssa, primal%stmts(i)%target, fresh)
            end if
            s%target = fresh
            ignored = adjoint%add_stmt(s)

            ! In a taped loop, store a scalar temporary as it is produced.
            if (rec%taped) then
                if (is_known_name(shape%temporaries, shape%n_temporaries, &
                    primal%stmts(i)%target)) then
                    if (adjoint%decl_index(primal%stmts(i)%target//"_tape") > 0) then
                        block
                            type(fad_stmt_t) :: ts
                            ts%kind = FAD_ASSIGN
                            ts%target = primal%stmts(i)%target//"_tape("// &
                                rec%tape_index//")"
                            ts%value = adjoint%add_expr(expr_var(fresh))
                            ignored = adjoint%add_stmt(ts)
                        end block
                    end if
                end if
            end if

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
            if (adjoint%decl_index(trim(shape%carried(k))//"_tape") > 0) then
                rec%n_taped_carried = rec%n_taped_carried + 1
            end if
            s%kind = FAD_ASSIGN
            s%target = trim(shape%carried(k))
            s%value = adjoint%add_expr(expr_var(current))
            ignored = adjoint%add_stmt(s)
            ! After the loop the plain name holds the final value.
            call ssa_set(ssa, trim(shape%carried(k)), trim(shape%carried(k)))
        end do

        do k = 1, rec%n_levels
            s%kind = FAD_END_DO
            s%value = 0
            rec%end_do_stmt = adjoint%add_stmt(s)
        end do
    end subroutine emit_loop_forward

    subroutine build_reverse_sweep(primal, adjoint, ssa, lhs_names, rhs_exprs, &
            is_element, &
            n_rec, loops, n_loops, branches, n_branches, &
            selections, n_selects, &
            calls, n_calls, &
            allocations, n_allocations, &
            order_kind, order_index, n_order, &
            spec, dependent, dependent_seed, suffix, active, active_paths, status)
        !! Walk backwards, accumulating adjoints.
        !!
        !! Straight-line statements are inverted directly against their SSA
        !! values. A reduction loop becomes a second loop whose body recomputes
        !! the per-iteration temporaries and scatters the accumulator's adjoint
        !! into the array adjoints.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(inout) :: lhs_names(:)
        logical, intent(in) :: is_element(:)
        integer, intent(inout) :: rhs_exprs(:)
        integer, intent(in) :: n_rec
        type(loop_record_t), intent(inout) :: loops(:)
        integer, intent(in) :: n_loops
        type(branch_record_t), intent(in) :: branches(:)
        integer, intent(in) :: n_branches
        type(select_record_t), intent(in) :: selections(:)
        integer, intent(in) :: n_selects
        type(call_record_t), intent(in) :: calls(:)
        integer, intent(in) :: n_calls
        type(allocation_record_t), intent(in) :: allocations(:)
        integer, intent(in) :: n_allocations
        integer, intent(in) :: order_kind(:), order_index(:), n_order
        type(reverse_spec_t), intent(in) :: spec
        character(len=*), intent(in) :: dependent, dependent_seed, suffix
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: active_paths(:)
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: final_name, shadow_name, target_name
        integer :: i, k, di, ignored, zero, n_tmp, seed_expr, seed_copy
        logical :: component_target, dependent_component

        dependent_component = index(trim(dependent), "%") > 0

        call retarget_move_reverse_records(adjoint, lhs_names, rhs_exprs, n_rec, &
            loops, n_loops, allocations, n_allocations)

        if (.not. dependent_component) then
            call ssa_lookup(ssa, dependent, final_name)
            if (final_name /= dependent) then
                s%kind = FAD_ASSIGN
                s%target = dependent
                s%value = adjoint%add_expr(expr_var(final_name))
                ignored = adjoint%add_stmt(s)
            end if
        end if

        ! Zero every adjoint before anything accumulates into it.
        zero = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
        do i = 1, n_rec
            if (.not. adjoint_is_live(primal, ssa, lhs_names(i), active)) cycle
            if (index(trim(lhs_names(i)), "%") > 0) cycle
            ! An element write is a storage scatter, not an SSA scalar.  The
            ! forward sweep deliberately keeps its target as `x(1)` (rather
            ! than inventing an SSA name), so declaring `x(1)_b` here would
            ! emit invalid Fortran.  Allocate one owning adjoint array and
            ! let the reverse scatter use `x_b(1)` instead.
            if (index(trim(lhs_names(i)), "(") > 0) then
                call declare_array_adjoint(primal, adjoint, &
                    target_base(trim(lhs_names(i))), suffix, zero)
                cycle
            end if
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
        do k = 1, n_selects
            call zero_select_adjoints(primal, adjoint, ssa, selections(k), &
                suffix, active, zero)
        end do
        do k = 1, n_calls
            call zero_call_adjoints(primal, adjoint, ssa, calls(k), suffix, zero)
        end do
        call zero_component_adjoints(primal, adjoint, active, suffix, zero, &
            active_paths, dependent)
        do i = 1, size(spec%independents)
            if (index(trim(spec%independents(i)), "%") > 0) cycle
            di = primal%decl_index(trim(spec%independents(i)))
            if (di == 0) cycle
            s%kind = FAD_ASSIGN
            ! A bare name assigns the whole array whatever its rank; "(:)"
            ! would silently assume rank one and fail to compile at rank two.
            s%target = trim(spec%independents(i))//suffix
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do

        if (.not. dependent_component) then
            call ssa_lookup(ssa, dependent, final_name)
            s%kind = FAD_ASSIGN
            s%target = final_name//suffix
            s%value = adjoint%add_expr(expr_var(dependent//suffix))
            ignored = adjoint%add_stmt(s)
        end if

        n_tmp = 0

        ! Exact reverse of forward program order.
        do k = n_order, 1, -1
            select case (order_kind(k))
            case (ORDER_STMT)
                i = order_index(k)
                if (is_section_target(lhs_names(i))) then
                    ! A contiguous section assignment is a vector store, not a
                    ! scalar scatter.  Propagate its whole incoming section
                    ! seed directly; scalar materialisation would lose the
                    ! shape and can route the seed through the wrong SSA name.
                    call declare_seed_shadow(primal, adjoint, dependent, suffix)
                    seed_expr = adjoint%add_expr(expr_var(shadow_element( &
                        trim(lhs_names(i)), suffix, dependent)))
                    call accumulate(primal, adjoint, rhs_exprs(i), seed_expr, &
                        ssa, suffix, active, n_tmp, status)
                    if (.not. status%ok) return
                    s%kind = FAD_ASSIGN
                    s%target = shadow_element(trim(lhs_names(i)), suffix, &
                        dependent)
                    s%value = zero
                    ignored = adjoint%add_stmt(s)
                    cycle
                end if
                if (is_element(i)) then
                    call emit_source_component_adjoint_for_target(primal, adjoint, &
                        lhs_names(i), suffix, allocations, n_allocations)
                    ! The adjoint of `point(1) = e` is the same element of the
                    ! array's adjoint, propagated into `e` and then cleared:
                    ! the store killed whatever was there before it.
                    if (dependent_component .and. same_component_name( &
                        lhs_names(i), dependent)) then
                        seed_expr = adjoint%add_expr(expr_var(dependent_seed))
                    else
                        call declare_seed_shadow(primal, adjoint, dependent, suffix)
                        seed_expr = adjoint%add_expr(expr_var(shadow_element( &
                            trim(lhs_names(i)), suffix, dependent)))
                    end if
                    ! The target's adjoint currently holds the seed for the
                    ! *new* value.  Copy it before propagating the RHS: an
                    ! expression such as `x(1) = x(1) + 2*a` otherwise emits
                    ! `x_b(1) = x_b(1) + x_b(1)`, double-counting the same
                    ! storage location instead of replacing its seed.
                    target_name = trim(adjoint_element(trim(lhs_names(i)), suffix))
                    component_target = index(trim(lhs_names(i)), "%") > 0
                    if (.not. component_target .and. &
                        index(trim(lhs_names(i)), "(") > 0 .and. &
                        trim(shadow_element(trim(lhs_names(i)), suffix, dependent)) == &
                        target_name) then
                        shadow_name = array_seed_shadow_element( &
                            trim(lhs_names(i)), suffix)
                        call declare_array_seed_shadow(primal, adjoint, &
                            fad_base_name(trim(lhs_names(i))), suffix)
                        block
                            type(fad_stmt_t) :: cs
                            integer :: cignored
                            cs%kind = FAD_ASSIGN
                            cs%target = shadow_array_name( &
                                fad_base_name(trim(lhs_names(i))), suffix)
                            cs%value = adjoint%add_expr(expr_var( &
                                fad_base_name(trim(lhs_names(i)))//trim(suffix)))
                            cignored = adjoint%add_stmt(cs)
                        end block
                        seed_copy = adjoint%add_expr(expr_var(shadow_name))
                        block
                            type(fad_stmt_t) :: zs
                            integer :: zignored
                            zs%kind = FAD_ASSIGN
                            zs%target = target_name
                            zs%value = adjoint%add_expr( &
                                expr_const("0.0"//adjoint%real_suffix))
                            zignored = adjoint%add_stmt(zs)
                        end block
                    else if (component_target) then
                        if (dependent_component) then
                            ! The incoming component seed is an intent(in)
                            ! dummy. It must not be routed through or cleared
                            ! as a whole-object adjoint shadow.
                            seed_copy = seed_expr
                        else
                            call materialise(primal, adjoint, seed_expr, ssa, &
                                n_tmp, seed_copy, force=.true.)
                            block
                                type(fad_stmt_t) :: zs
                                integer :: zignored
                                zs%kind = FAD_ASSIGN
                                zs%target = target_name
                                zs%value = adjoint%add_expr( &
                                    expr_const("0.0"//adjoint%real_suffix))
                                zignored = adjoint%add_stmt(zs)
                            end block
                        end if
                    else
                        call materialise(primal, adjoint, seed_expr, ssa, n_tmp, &
                            seed_copy, force=.true.)
                    end if
                    call accumulate(primal, adjoint, rhs_exprs(i), seed_copy, &
                        ssa, suffix, active, n_tmp, status)
                    if (.not. status%ok) return
                    if (.not. component_target) then
                        block
                            type(fad_stmt_t) :: zs
                            integer :: zignored
                            zs%kind = FAD_ASSIGN
                            zs%target = shadow_element(trim(lhs_names(i)), suffix, &
                                dependent)
                            zs%value = adjoint%add_expr( &
                                expr_const("0.0"//adjoint%real_suffix))
                            zignored = adjoint%add_stmt(zs)
                        end block
                    end if
                    cycle
                end if
                if (.not. adjoint_is_live(primal, ssa, lhs_names(i), active)) cycle
                seed_expr = adjoint%add_expr(expr_var(trim(lhs_names(i))//suffix))
                call accumulate(primal, adjoint, rhs_exprs(i), seed_expr, ssa, &
                    suffix, active, n_tmp, status)
            case (ORDER_BRANCH)
                call emit_branch_reverse(primal, adjoint, ssa, &
                    branches(order_index(k)), suffix, &
                    active, n_tmp, status)
            case (ORDER_SELECT)
                call emit_select_reverse(primal, adjoint, ssa, &
                    selections(order_index(k)), suffix, active, n_tmp, status)
            case (ORDER_LOOP)
                call emit_loop_reverse(primal, adjoint, ssa, &
                    loops(order_index(k)), suffix, &
                    active, n_tmp, status)
            case (ORDER_CALL)
                call emit_call_reverse(adjoint, calls(order_index(k)), &
                    suffix, status)
            end select
            if (.not. status%ok) return
        end do

        ! Fusing the adjoint of a reduction back into its own primal loop turns
        ! two passes over the input arrays into one. The seed is loop-invariant
        ! and the adjoint work is per-element, so the fused loop computes the
        ! same values in the same order and stays parallelisable. Memory
        ! bandwidth, not arithmetic, is what bounds these kernels, so halving
        ! the traffic is the single largest win available here.
        if (n_loops == 1 .and. loops(1)%n_carried == 0 .and. &
            loops(1)%n_levels == 1 .and. &
            can_fuse(primal, loops(1), dependent)) then
            call fuse_loop(adjoint, loops(1), suffix)
        end if

        call append_allocation_cleanup(primal, adjoint, allocations, &
            n_allocations, suffix)
    end subroutine build_reverse_sweep

    subroutine retarget_move_reverse_records(adjoint, lhs_names, rhs_exprs, &
            n_rec, loops, n_loops, allocations, n_allocations)
        !! Reverse records made before MOVE_ALLOC still name the source
        !! descriptor.  The payload has the destination name by the time the
        !! reverse sweep runs, so retarget only those records, never the
        !! already-emitted forward statements.
        type(fad_proc_t), intent(inout) :: adjoint
        character(len=*), intent(inout) :: lhs_names(:)
        integer, intent(inout) :: rhs_exprs(:)
        integer, intent(in) :: n_rec
        type(loop_record_t), intent(inout) :: loops(:)
        integer, intent(in) :: n_loops
        type(allocation_record_t), intent(in) :: allocations(:)
        integer, intent(in) :: n_allocations
        integer :: i, k

        do k = 1, n_allocations
            if (.not. allocated(allocations(k)%previous_owner)) cycle
            do i = 1, n_rec
                lhs_names(i) = replace_move_owner(lhs_names(i), &
                    allocations(k)%previous_owner, allocations(k)%owner)
                rhs_exprs(i) = retarget_move_expr(adjoint, rhs_exprs(i), &
                    allocations(k)%previous_owner, allocations(k)%owner)
            end do
            do i = 1, n_loops
                call retarget_move_loop(adjoint, loops(i), &
                    allocations(k)%previous_owner, allocations(k)%owner)
            end do
        end do
    end subroutine retarget_move_reverse_records

    subroutine retarget_move_loop(adjoint, rec, source, target)
        type(fad_proc_t), intent(inout) :: adjoint
        type(loop_record_t), intent(inout) :: rec
        character(len=*), intent(in) :: source, target
        integer :: i

        do i = 1, rec%n_body
            rec%body_lhs(i) = replace_move_owner(rec%body_lhs(i), source, target)
            rec%body_rhs(i) = retarget_move_expr(adjoint, rec%body_rhs(i), &
                source, target)
        end do
    end subroutine retarget_move_loop

    recursive integer function retarget_move_expr(adjoint, idx, source, target) &
            result(out)
        type(fad_proc_t), intent(inout) :: adjoint
        integer, intent(in) :: idx
        character(len=*), intent(in) :: source, target
        type(fad_expr_t) :: e
        integer, allocatable :: args(:)
        integer :: i

        out = 0
        if (idx <= 0 .or. idx > adjoint%n_exprs) return
        e = adjoint%exprs(idx)
        if ((e%kind == FAD_VAR .or. e%kind == FAD_INDEX) .and. &
            allocated(e%text)) then
            e%text = replace_move_owner(e%text, source, target)
        end if
        allocate (args(size(e%args)))
        do i = 1, size(args)
            args(i) = retarget_move_expr(adjoint, e%args(i), source, target)
        end do
        e%args = args
        out = adjoint%add_expr(e)
    end function retarget_move_expr

    function replace_move_owner(text, source, target) result(out)
        character(len=*), intent(in) :: text, source, target
        character(len=:), allocatable :: out
        integer :: n

        out = text
        n = len_trim(source)
        if (len_trim(text) < n) return
        if (text(:n) /= source) return
        if (len_trim(text) == n) then
            out = target
        else if (text(n + 1:n + 1) == "(") then
            out = target//text(n + 1:)
        end if
    end function replace_move_owner

    subroutine zero_component_adjoints(primal, adjoint, active, suffix, zero, &
            active_paths, dependent)
        !! Initialise component adjoints without assigning a scalar to the
        !! derived object itself.  A derived tangent is a shadow object; only
        !! the real components that occur on the active path are zeroed here.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        integer, intent(in) :: zero
        character(len=*), intent(in) :: active_paths(:)
        character(len=*), intent(in) :: dependent
        character(len=256) :: paths(256)
        character(len=:), allocatable :: path, base
        type(fad_stmt_t) :: s
        integer :: i, j, n, di, ignored, suffix_pos, percent_pos
        logical :: seen

        paths = ""
        n = 0
        do i = 1, primal%n_exprs
            if (primal%exprs(i)%kind /= FAD_VAR .and. &
                primal%exprs(i)%kind /= FAD_INDEX) cycle
            if (index(primal%exprs(i)%text, "%") == 0) cycle
            if (is_polymorphic_component_path(primal, &
                primal%exprs(i)%text)) cycle
            ! A polymorphic dispatch receiver is selected again in the reverse
            ! sweep. Its cotangent is selected in parallel there; emitting a
            ! bare receiver-alias shadow here would be invalid.
            if (is_select_alias_path(primal, primal%exprs(i)%text)) cycle
            if (is_nested_polymorphic_receiver_path(primal, &
                primal%exprs(i)%text)) cycle
            if (is_scalar_polymorphic_receiver_path(primal, &
                primal%exprs(i)%text)) cycle
            if (explicit_component_lifetime(primal, &
                primal%exprs(i)%text)) cycle
            base = fad_base_name(primal%exprs(i)%text)
            di = primal%decl_index(base)
            if (di <= 0) cycle
            if (.not. component_path_is_active(primal, primal%exprs(i)%text, &
                active, active_paths)) cycle
            if (index(trim(dependent), "%") > 0) then
                if (same_component_name(primal%exprs(i)%text, dependent)) cycle
            end if
            path = fad_suffix_name(primal%exprs(i)%text, suffix)
            seen = .false.
            do j = 1, n
                if (trim(paths(j)) == trim(path)) then
                    seen = .true.
                    exit
                end if
            end do
            if (seen .or. n == size(paths)) cycle
            n = n + 1
            paths(n) = path
        end do
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (index(primal%stmts(i)%target, "%") == 0) cycle
            if (is_polymorphic_component_path(primal, &
                primal%stmts(i)%target)) cycle
            if (is_select_alias_path(primal, primal%stmts(i)%target)) cycle
            if (is_scalar_polymorphic_receiver_path(primal, &
                primal%stmts(i)%target)) cycle
            if (is_nested_polymorphic_receiver_path(primal, &
                primal%stmts(i)%target)) cycle
            if (explicit_component_lifetime(primal, &
                primal%stmts(i)%target)) cycle
            base = fad_base_name(primal%stmts(i)%target)
            di = primal%decl_index(base)
            if (di <= 0) cycle
            if (index(trim(dependent), "%") > 0) then
                if (same_component_name(primal%stmts(i)%target, dependent)) cycle
            end if
            if (.not. component_path_is_active(primal, primal%stmts(i)%target, &
                active, active_paths)) cycle
            path = fad_suffix_name(primal%stmts(i)%target, suffix)
            seen = .false.
            do j = 1, n
                if (trim(paths(j)) == trim(path)) then
                    seen = .true.
                    exit
                end if
            end do
            if (seen .or. n == size(paths)) cycle
            n = n + 1
            paths(n) = path
        end do
        do i = 1, n
            base = trim(paths(i))
            suffix_pos = index(base, trim(suffix))
            if (suffix_pos > 0) base = base(:suffix_pos - 1)//base(suffix_pos + &
                len_trim(suffix):)
            base = fad_base_name(base)
            di = primal%decl_index(base)
            if (di > 0 .and. adjoint%decl_index(trim(base)//trim(suffix)) == 0) then
                block
                    type(fad_decl_t) :: d
                    integer :: dignored
                    call copy_decl(d, primal%decls(di))
                    d%name = trim(base)//trim(suffix)
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    d%is_optional = .false.
                    d%is_value = .false.
                    dignored = adjoint%add_decl(d)
                end block
            end if
            path = trim(paths(i))
            percent_pos = index(path, "%")
            if (percent_pos > 0) then
                suffix_pos = index(path(percent_pos:), trim(suffix))
                if (suffix_pos > 0) then
                    suffix_pos = percent_pos + suffix_pos - 1
                    path = path(:suffix_pos - 1)//path(suffix_pos + &
                        len_trim(suffix):)
                    path = path(:percent_pos - 1)//trim(suffix)// &
                        path(percent_pos:)
                end if
            end if
            s%kind = FAD_ASSIGN
            s%target = trim(path)
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do
    end subroutine zero_component_adjoints

    logical function is_scalar_polymorphic_receiver_path(primal, text) &
            result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: base, selector
        integer :: di, i

        found = .false.
        if (index(trim(text), "%") <= 0) return
        base = fad_base_name(text)
        di = primal%decl_index(base)
        if (di <= 0) return
        selector = base
        if (primal%decls(di)%is_select_alias) then
            ! A SELECT TYPE associate name is not itself marked polymorphic:
            ! it has the concrete arm type.  For a fixed scalar receiver,
            ! however, its alias target still identifies the polymorphic
            ! object whose cotangent is emitted by emit_select_reverse.
            if (.not. allocated(primal%decls(di)%alias_target)) return
            selector = trim(primal%decls(di)%alias_target)
            di = primal%decl_index(fad_base_name(selector))
            if (di <= 0) return
        end if
        if (.not. primal%decls(di)%is_polymorphic) return
        if (primal%decls(di)%is_array) return
        if (primal%decls(di)%is_allocatable) return
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
            if (fad_base_name(emit_expr(primal, primal%stmts(i)%value)) == &
                fad_base_name(selector)) then
                found = .true.
                return
            end if
        end do
    end function is_scalar_polymorphic_receiver_path

    logical function is_nested_polymorphic_receiver_path(primal, text) &
            result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: base, target
        integer :: di, i

        found = .false.
        base = fad_base_name(text)
        di = primal%decl_index(base)
        if (di <= 0 .or. .not. primal%decls(di)%is_select_alias) return
        if (.not. allocated(primal%decls(di)%alias_target)) return
        target = trim(primal%decls(di)%alias_target)
        if (index(target, "%") <= 0) return
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (trim(emit_expr(primal, i)) /= target) cycle
            found = primal%exprs(i)%component_is_polymorphic
            return
        end do
    end function is_nested_polymorphic_receiver_path

    subroutine refuse_active_polymorphic_dispatch(primal, active_paths, status)
        !! A scalar CLASS cotangent can be paired with the primal selector
        !! only on one proven concrete path. Multiple runtime arms would
        !! require a dynamic-type cotangent descriptor, so refuse them before
        !! reverse SSA emission rather than producing an invalid shadow.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: active_paths(:)
        type(reverse_status_t), intent(inout) :: status
        character(len=:), allocatable :: selector, base, path
        integer :: i, j, k, depth, concrete, di
        logical :: active_receiver

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
            selector = emit_expr(primal, primal%stmts(i)%value)
            base = fad_base_name(selector)
            di = primal%decl_index(base)
            if (di <= 0) cycle
            if (.not. primal%decls(di)%is_polymorphic) cycle
            if (primal%decls(di)%is_array) cycle
            active_receiver = .false.
            do j = 1, primal%n_exprs
                if (primal%exprs(j)%kind /= FAD_VAR .and. &
                    primal%exprs(j)%kind /= FAD_INDEX) cycle
                if (index(trim(primal%exprs(j)%text), "%") == 0) cycle
                path = resolve_component_path(primal, primal%exprs(j)%text)
                if (fad_base_name(path) /= trim(base)) cycle
                do k = 1, size(active_paths)
                    if (same_component_name(path, active_paths(k))) then
                        active_receiver = .true.
                        exit
                    end if
                end do
                if (active_receiver) exit
            end do
            if (.not. active_receiver) cycle
            concrete = 0
            depth = 1
            j = i + 1
            do while (j <= primal%n_stmts .and. depth > 0)
                select case (primal%stmts(j)%kind)
                case (FAD_SELECT_TYPE)
                    depth = depth + 1
                case (FAD_END_SELECT)
                    depth = depth - 1
                case (FAD_TYPE_IS, FAD_CLASS_IS)
                    if (depth == 1) concrete = concrete + 1
                end select
                j = j + 1
            end do
            if (concrete /= 1) then
                status%ok = .false.
                status%message = "reverse mode: active polymorphic receiver "// &
                    "requires one fixed concrete runtime path; dynamic "// &
                    "dispatch shadows are unsupported"
                return
            end if
        end do
    end subroutine refuse_active_polymorphic_dispatch

    subroutine refuse_active_polymorphic_ownership(primal, active, active_paths, &
            status)
        !! Reverse mode needs the same dynamic-type boundary as forward mode.
        !! A borrowed fixed-source owner is safe; an active polymorphic
        !! allocatable without one would require replaying its descriptor.
        type(fad_proc_t), intent(in) :: primal
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: active_paths(:)
        type(reverse_status_t), intent(inout) :: status
        integer :: i, j
        character(len=:), allocatable :: type_label
        logical :: owner_active

        do i = 1, primal%n_decls
            if (primal%decls(i)%is_select_alias) cycle
            if (.not. primal%decls(i)%is_allocatable) cycle
            if (.not. primal%decls(i)%is_polymorphic) cycle
            owner_active = active(i)
            if (.not. owner_active) then
                do j = 1, size(active_paths)
                    if (index(trim(active_paths(j)), "%") <= 0) cycle
                    if (fad_base_name(active_paths(j)) == &
                        trim(primal%decls(i)%name)) then
                        owner_active = .true.
                        exit
                    end if
                end do
            end if
            if (.not. owner_active) cycle
            if (has_fixed_source_owner(primal, i)) cycle
            type_label = "class(T)"
            if (primal%decls(i)%is_unlimited_polymorphic) type_label = "class(*)"
            status%ok = .false.
            status%message = "reverse mode: active polymorphic allocatable "// &
                "ownership '"//trim(primal%decls(i)%name)//"' ("// &
                trim(type_label)//"): current IR cannot replay the primal "// &
                "dynamic type descriptor without a fixed SOURCE= owner"
            return
        end do
    end subroutine refuse_active_polymorphic_ownership

    subroutine refuse_active_nested_polymorphic_component(primal, active_paths, status)
        !! The reverse shadow for a nested polymorphic component is a paired
        !! selector in a caller-owned concrete holder.  Do not infer that
        !! pairing through aliases, dynamic indexing, ownership changes, or a
        !! dispatch with more than one concrete arm.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: active_paths(:)
        type(reverse_status_t), intent(inout) :: status
        character(len=:), allocatable :: selector, path
        integer :: i, j, k, depth, concrete, base_di
        logical :: active_component, found, fixed_ownership

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
            selector = emit_expr(primal, primal%stmts(i)%value)
            if (index(trim(selector), "%") <= 0) cycle
            found = .false.
            active_component = .false.
            do j = 1, primal%n_exprs
                if (.not. primal%exprs(j)%is_component_path) cycle
                if (trim(emit_expr(primal, j)) /= trim(selector)) cycle
                found = .true.
                if (.not. primal%exprs(j)%component_is_polymorphic) exit
                concrete = 0
                depth = 1
                do k = i + 1, primal%n_stmts
                    select case (primal%stmts(k)%kind)
                    case (FAD_SELECT_TYPE)
                        depth = depth + 1
                    case (FAD_END_SELECT)
                        depth = depth - 1
                        if (depth == 0) exit
                    case (FAD_TYPE_IS, FAD_CLASS_IS)
                        if (depth == 1) concrete = concrete + 1
                    end select
                end do
                if (concrete /= 1) then
                    status%ok = .false.
                    status%message = "reverse mode: nested polymorphic component "// &
                        "path '"//trim(selector)//"' requires one fixed concrete "// &
                        "runtime path; unresolved dispatch is unsupported"
                    return
                end if
                do k = 1, size(active_paths)
                    path = trim(active_paths(k))
                    if (index(path, trim(selector)//"%") == 1) then
                        active_component = .true.
                        exit
                    end if
                end do
                if (.not. active_component) exit
                if (primal%exprs(j)%component_is_pointer .or. &
                    primal%exprs(j)%component_is_target) then
                    status%ok = .false.
                    status%message = "reverse mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' uses pointer or TARGET storage"
                    return
                end if
                if (index(trim(selector), "(") > 0) then
                    status%ok = .false.
                    status%message = "reverse mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' has dynamic bounds or indexing"
                    return
                end if
                base_di = primal%decl_index(fad_base_name(selector))
                if (base_di <= 0 .or. primal%decls(base_di)%is_polymorphic .or. &
                    primal%decls(base_di)%is_allocatable .or. &
                    primal%decls(base_di)%is_associate_alias .or. &
                    primal%decls(base_di)%is_select_alias) then
                    status%ok = .false.
                    status%message = "reverse mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' has unresolved owner alias or ownership"
                    return
                end if
                fixed_ownership = .false.
                do k = 1, primal%n_stmts
                    if (primal%stmts(k)%kind /= FAD_ALLOCATE) cycle
                    if (.not. allocated(primal%stmts(k)%allocation_args)) cycle
                    if (size(primal%stmts(k)%allocation_args) < 1) cycle
                    if (fixed_source_component(primal, k, selector)) then
                        fixed_ownership = .true.
                        exit
                    end if
                end do
                if (fixed_ownership) exit
                do k = 1, primal%n_stmts
                    if (primal%stmts(k)%kind /= FAD_ALLOCATE .and. &
                        primal%stmts(k)%kind /= FAD_DEALLOCATE .and. &
                        primal%stmts(k)%kind /= FAD_MOVE_ALLOC) cycle
                    status%ok = .false.
                    status%message = "reverse mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' crosses ownership/lifetime "// &
                        "operations; caller-owned borrowed components only"
                    return
                end do
                exit
            end do
            if (found .and. .not. active_component) cycle
        end do
    end subroutine refuse_active_nested_polymorphic_component

    subroutine zero_call_adjoints(primal, adjoint, ssa, rec, suffix, zero)
        !! Declare and clear adjoints for the arguments of an opaque call.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(call_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        integer, intent(in) :: zero
        type(fad_stmt_t) :: s
        integer :: i, di, ignored

        do i = 1, size(rec%args)
            di = primal%decl_index(trim(rec%args(i)))
            if (di == 0) cycle
            if (.not. is_real_type(primal%decls(di))) cycle
            call declare_adjoint(primal, adjoint, ssa, trim(rec%args(i)), suffix)
            s%kind = FAD_ASSIGN
            s%target = trim(rec%args(i))//suffix
            s%value = zero
            ignored = adjoint%add_stmt(s)
        end do
    end subroutine zero_call_adjoints

    subroutine emit_call_reverse(adjoint, rec, suffix, status)
        !! Emit the registered reverse statements for one opaque call.
        type(fad_proc_t), intent(inout) :: adjoint
        type(call_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        type(reverse_status_t), intent(inout) :: status
        character(len=512), allocatable :: args(:), tangents(:), adjoints(:)
        character(len=:), allocatable :: line
        type(fad_stmt_t) :: s
        integer :: i, ignored, n_lines

        n_lines = call_rule_lines(rec%name, "adjoint")
        if (n_lines == 0) then
            status%ok = .false.
            status%message = "no reverse rule body for the call to '"// &
                rec%name//"'"
            return
        end if
        allocate (args(size(rec%args)), tangents(size(rec%args)), &
            adjoints(size(rec%args)))
        do i = 1, size(rec%args)
            args(i) = rec%args(i)
            tangents(i) = trim(rec%args(i))//suffix
            adjoints(i) = trim(rec%args(i))//suffix
        end do
        do i = 1, n_lines
            line = call_rule_substitute(rec%name, "adjoint", i, args, tangents, &
                adjoints)
            s%kind = FAD_ASSIGN
            s%target = "!fad_raw"
            s%value = adjoint%add_expr(expr_const(line))
            ignored = adjoint%add_stmt(s)
        end do
    end subroutine emit_call_reverse

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

    subroutine zero_select_adjoints(primal, adjoint, ssa, rec, suffix, active, &
            zero)
        !! Declare and clear the SSA adjoints introduced in SELECT TYPE arms.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(select_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: active(:)
        integer, intent(in) :: zero
        type(fad_stmt_t) :: s
        character(len=64), allocatable :: seen(:)
        integer :: a, capacity, i, ignored, n_seen

        capacity = 0
        do a = 1, rec%n_arms
            capacity = capacity + rec%arms(a)%n
        end do
        allocate (seen(max(1, capacity)))
        n_seen = 0
        do a = 1, rec%n_arms
            do i = 1, rec%arms(a)%n
                if (index(trim(rec%arms(a)%lhs(i)), "%") > 0) then
                    if (is_select_alias_path(primal, rec%arms(a)%lhs(i)) .or. &
                        is_nested_polymorphic_receiver_path(primal, &
                        rec%arms(a)%lhs(i))) cycle
                end if
                if (.not. adjoint_is_live(primal, ssa, rec%arms(a)%lhs(i), &
                    active)) cycle
                if (is_known_name(seen, n_seen, &
                    trim(rec%arms(a)%lhs(i)))) cycle
                n_seen = n_seen + 1
                seen(n_seen) = trim(rec%arms(a)%lhs(i))
                call declare_adjoint(primal, adjoint, ssa, &
                    trim(rec%arms(a)%lhs(i)), suffix)
                s%kind = FAD_ASSIGN
                s%target = trim(rec%arms(a)%lhs(i))//suffix
                s%value = zero
                ignored = adjoint%add_stmt(s)
            end do
        end do
    end subroutine zero_select_adjoints

    subroutine emit_select_reverse(primal, adjoint, ssa, rec, suffix, active, &
            n_tmp, status)
        !! Repeat the passive runtime dispatch and unwind only its chosen arm.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(in) :: ssa
        type(select_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: active(:)
        integer, intent(inout) :: n_tmp
        type(reverse_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: receiver_alias, cotangent_alias
        character(len=:), allocatable :: cotangent_selector
        character(len=:), allocatable :: seed_target
        logical :: receiver_cotangent
        integer :: a, i, ignored, seed_expr, component_seed, selector_expr

        call receiver_cotangent_context(primal, rec, suffix, ssa%active_paths, &
            active, &
            receiver_cotangent, receiver_alias, &
            cotangent_alias, cotangent_selector)

        s%kind = FAD_SELECT_TYPE
        s%value = rec%selector
        if (allocated(rec%selector_alias)) s%target = rec%selector_alias
        ignored = adjoint%add_stmt(s)
        do a = 1, rec%n_arms
            s%kind = rec%arms(a)%kind
            s%value = 0
            if (allocated(s%target)) deallocate (s%target)
            if (allocated(rec%arms(a)%target)) s%target = rec%arms(a)%target
            ignored = adjoint%add_stmt(s)
            if (receiver_cotangent .and. (rec%arms(a)%kind == FAD_TYPE_IS .or. &
                rec%arms(a)%kind == FAD_CLASS_IS)) then
                selector_expr = adjoint%add_expr(expr_var(cotangent_selector))
                s%kind = FAD_SELECT_TYPE
                s%value = selector_expr
                s%target = cotangent_alias
                ignored = adjoint%add_stmt(s)
                s%kind = rec%arms(a)%kind
                s%value = 0
                s%target = rec%arms(a)%target
                ignored = adjoint%add_stmt(s)
                call zero_receiver_cotangent(primal, adjoint, ssa%active_paths, &
                    active, &
                    receiver_alias, cotangent_alias)
            end if
            do i = rec%arms(a)%n, 1, -1
                if (.not. adjoint_is_live(primal, ssa, rec%arms(a)%lhs(i), &
                    active)) cycle
                seed_target = trim(rec%arms(a)%lhs(i))//suffix
                if (receiver_cotangent .and. (rec%arms(a)%kind == FAD_TYPE_IS .or. &
                    rec%arms(a)%kind == FAD_CLASS_IS)) then
                    seed_target = reverse_component_target( &
                        trim(rec%arms(a)%lhs(i)), suffix, receiver_alias, &
                        cotangent_alias)
                end if
                seed_expr = adjoint%add_expr(expr_var(seed_target))
                if (receiver_cotangent .and. (rec%arms(a)%kind == FAD_TYPE_IS .or. &
                    rec%arms(a)%kind == FAD_CLASS_IS)) then
                    if (index(trim(rec%arms(a)%lhs(i)), "%") > 0) then
                        ! A selected component store first accumulates the
                        ! cotangent from later reads into the selected shadow.
                        ! Snapshot that location before differentiating the
                        ! store RHS: otherwise an owner-array element can be
                        ! read as both the incoming seed and the contribution
                        ! just added by the later read.
                        call materialise(primal, adjoint, seed_expr, ssa, n_tmp, &
                            component_seed, force=.true.)
                        call accumulate(primal, adjoint, rec%arms(a)%rhs(i), &
                            component_seed, ssa, suffix, active, n_tmp, status, &
                            receiver_alias, cotangent_alias)
                    else
                        call accumulate(primal, adjoint, rec%arms(a)%rhs(i), &
                            seed_expr, ssa, suffix, active, n_tmp, status, &
                            receiver_alias, cotangent_alias)
                    end if
                    ! A fixed-path component assignment kills the incoming
                    ! component cotangent before SOURCE= ownership replay.
                    ! Keeping it would incorrectly differentiate the copied
                    ! pre-assignment payload as well as the assignment RHS.
                    if (index(trim(rec%arms(a)%lhs(i)), "%") > 0) then
                        s%kind = FAD_ASSIGN
                        s%target = seed_target
                        s%value = adjoint%add_expr( &
                            expr_const("0.0"//adjoint%real_suffix))
                        ignored = adjoint%add_stmt(s)
                    end if
                else
                    call accumulate(primal, adjoint, rec%arms(a)%rhs(i), &
                        seed_expr, ssa, suffix, active, n_tmp, status)
                end if
                if (.not. status%ok) return
            end do
            if (receiver_cotangent .and. (rec%arms(a)%kind == FAD_TYPE_IS .or. &
                rec%arms(a)%kind == FAD_CLASS_IS)) then
                s%kind = FAD_CLASS_DEFAULT
                s%value = 0
                if (allocated(s%target)) deallocate (s%target)
                ignored = adjoint%add_stmt(s)
                s%kind = FAD_END_SELECT
                s%value = 0
                ignored = adjoint%add_stmt(s)
            end if
        end do
        s%kind = FAD_END_SELECT
        s%value = 0
        if (allocated(s%target)) deallocate (s%target)
        ignored = adjoint%add_stmt(s)
    end subroutine emit_select_reverse

    subroutine receiver_cotangent_context(primal, rec, suffix, active_paths, &
            active, &
            enabled, receiver_alias, cotangent_alias, &
            cotangent_selector)
        !! Identify the P8.3f receiver shape and name the matching selected
        !! cotangent. The lowerer has already proved a literal indexed,
        !! one-dimensional, borrowed CLASS receiver and one dispatch target;
        !! these checks keep the reverse emitter honest if the IR is reused.
        type(fad_proc_t), intent(in) :: primal
        type(select_record_t), intent(in) :: rec
        character(len=*), intent(in) :: suffix
        character(len=*), intent(in) :: active_paths(:)
        logical, intent(in) :: active(:)
        logical, intent(out) :: enabled
        character(len=:), allocatable, intent(out) :: receiver_alias
        character(len=:), allocatable, intent(out) :: cotangent_alias
        character(len=:), allocatable, intent(out) :: cotangent_selector
        character(len=:), allocatable :: selector
        integer :: alias_di, receiver_di, concrete_targets, i, j, source_di
        logical :: nested_receiver, component_found, ownership_active

        enabled = .false.
        receiver_alias = ""
        cotangent_alias = ""
        cotangent_selector = ""
        if (.not. allocated(rec%selector_alias)) return
        concrete_targets = 0
        do i = 1, rec%n_arms
            if (rec%arms(i)%kind == FAD_TYPE_IS .or. &
                rec%arms(i)%kind == FAD_CLASS_IS) concrete_targets = &
                concrete_targets + 1
        end do
        if (concrete_targets /= 1) return
        receiver_alias = trim(rec%selector_alias)
        alias_di = primal%decl_index(receiver_alias)
        if (alias_di <= 0) return
        if (primal%decls(alias_di)%is_select_alias) then
            if (.not. allocated(primal%decls(alias_di)%alias_target)) return
            selector = trim(primal%decls(alias_di)%alias_target)
        else
            if (.not. primal%decls(alias_di)%is_polymorphic) return
            selector = receiver_alias
        end if
        nested_receiver = .false.
        component_found = .false.
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (trim(emit_expr(primal, i)) /= trim(selector)) cycle
            component_found = .true.
            nested_receiver = primal%exprs(i)%component_is_polymorphic
            exit
        end do
        receiver_di = primal%decl_index(fad_base_name(selector))
        if (receiver_di <= 0) return
        if (nested_receiver) then
            if (primal%decls(receiver_di)%is_polymorphic .or. &
                primal%decls(receiver_di)%is_allocatable .or. &
                primal%decls(receiver_di)%is_associate_alias .or. &
                primal%decls(receiver_di)%is_select_alias) return
            if (.not. component_found) return
            if (index(selector, "(") > 0) then
                if (.not. fixed_literal_array_element(selector)) return
            end if
        else
            if (.not. primal%decls(receiver_di)%is_polymorphic) return
            if (primal%decls(receiver_di)%is_allocatable) then
                if (.not. primal%decls(receiver_di)%is_array) return
                if (.not. has_fixed_source_owner(primal, receiver_di)) return
                if (.not. fixed_literal_owner_selector(selector, &
                    primal%decls(receiver_di)%name)) return
            end if
            if (primal%decls(receiver_di)%is_associate_alias .or. &
                primal%decls(receiver_di)%is_select_alias) return
        end if
        if (.not. nested_receiver .and. primal%decls(receiver_di)%is_array) then
            if (index(selector, "(") <= 0) return
            if (index(selector, ",") > 0) return
        else if (.not. nested_receiver) then
            if (index(selector, "(") > 0) return
        end if
        if (.not. receiver_context_has_active(primal, receiver_alias, &
            active_paths, active)) then
            ! An owned polymorphic component can have its active source on the
            ! enclosing concrete object rather than on the selected alias
            ! declaration.  The fixed-source ownership fact is the same proof
            ! used to allocate the paired cotangent descriptor.
            ownership_active = .false.
            if (nested_receiver) then
                do j = 1, primal%n_stmts
                    if (primal%stmts(j)%kind /= FAD_ALLOCATE) cycle
                    if (.not. allocated(primal%stmts(j)%allocation_args)) cycle
                    if (size(primal%stmts(j)%allocation_args) < 1) cycle
                    if (.not. same_component_name(emit_expr(primal, &
                        primal%stmts(j)%allocation_args(1)), selector)) cycle
                    if (.not. fixed_source_component(primal, j)) cycle
                    source_di = concrete_source_decl(primal, &
                        primal%stmts(j)%allocation_source)
                    if (source_di > 0) ownership_active = active(source_di)
                    exit
                end do
            else if (primal%decls(receiver_di)%is_allocatable) then
                if (primal%decls(receiver_di)%is_array) then
                    do j = 1, primal%n_stmts
                        if (primal%stmts(j)%kind /= FAD_ALLOCATE) cycle
                        if (.not. allocated(primal%stmts(j)%allocation_args)) cycle
                        if (size(primal%stmts(j)%allocation_args) < 1) cycle
                        if (fad_base_name(emit_expr(primal, &
                            primal%stmts(j)%allocation_args(1))) /= &
                            primal%decls(receiver_di)%name) cycle
                        if (.not. has_fixed_source_owner(primal, receiver_di)) cycle
                        source_di = concrete_source_decl(primal, &
                            primal%stmts(j)%allocation_source)
                        if (source_di > 0) ownership_active = active(source_di)
                        exit
                    end do
                end if
            end if
            if (.not. ownership_active) return
        end if
        cotangent_alias = receiver_alias//trim(suffix)
        cotangent_selector = fad_suffix_name(selector, suffix)
        enabled = len_trim(cotangent_selector) > 0
    end subroutine receiver_cotangent_context

    logical function receiver_context_has_active(primal, receiver_alias, &
            active_paths, active) result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: receiver_alias
        character(len=*), intent(in) :: active_paths(:)
        logical, intent(in) :: active(:)
        character(len=:), allocatable :: canonical, active_path
        integer :: i, j, di

        found = .false.
        do i = 1, primal%n_exprs
            if (primal%exprs(i)%kind /= FAD_VAR .and. &
                primal%exprs(i)%kind /= FAD_INDEX) cycle
            if (fad_base_name(primal%exprs(i)%text) /= trim(receiver_alias)) cycle
            if (index(trim(primal%exprs(i)%text), "%") <= 0) cycle
            canonical = resolve_component_path(primal, primal%exprs(i)%text)
            di = primal%decl_index(fad_base_name(canonical))
            if (di > 0 .and. active(di)) then
                found = .true.
                return
            end if
            if (nested_polymorphic_component_active(primal, canonical, active)) then
                found = .true.
                return
            end if
            do j = 1, size(active_paths)
                active_path = resolve_component_path(primal, active_paths(j))
                if (same_component_name(canonical, active_path)) then
                    found = .true.
                    return
                end if
            end do
        end do
    end function receiver_context_has_active

    logical function nested_polymorphic_component_active(primal, path, active) &
            result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: path
        logical, intent(in) :: active(:)
        integer :: i, j, source_di
        character(len=:), allocatable :: allocation_path, ownership_path

        found = .false.
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ALLOCATE) cycle
            if (.not. primal%stmts(i)%allocation_target_polymorphic) cycle
            if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
            if (size(primal%stmts(i)%allocation_args) < 1) cycle
            allocation_path = emit_expr(primal, primal%stmts(i)%allocation_args(1))
            ownership_path = allocation_path
            do j = i + 1, primal%n_stmts
                if (primal%stmts(j)%kind /= FAD_MOVE_ALLOC) cycle
                if (.not. allocated(primal%stmts(j)%call_args)) cycle
                if (size(primal%stmts(j)%call_args) /= 2) cycle
                if (.not. same_component_name(emit_expr(primal, &
                    primal%stmts(j)%call_args(1)), ownership_path)) cycle
                ownership_path = emit_expr(primal, primal%stmts(j)%call_args(2))
                exit
            end do
            if (.not. same_component_name(ownership_path, path)) then
                if (index(trim(path), trim(ownership_path)//"%") /= 1) cycle
            end if
            if (primal%stmts(i)%allocation_source <= 0) cycle
            source_di = concrete_source_decl(primal, &
                primal%stmts(i)%allocation_source)
            found = (source_di > 0 .and. active(source_di)) .or. &
                reads_any(primal, primal%stmts(i)%allocation_source, active)
            return
        end do
    end function nested_polymorphic_component_active

    subroutine zero_receiver_cotangent(primal, adjoint, active_paths, active, &
            receiver_alias, cotangent_alias)
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        character(len=*), intent(in) :: active_paths(:)
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: receiver_alias, cotangent_alias
        character(len=64) :: paths(64)
        character(len=:), allocatable :: source, canonical, target, base, tail
        type(fad_stmt_t) :: s
        integer :: i, n, ignored, cut

        paths = ""
        n = 0
        do i = 1, primal%n_exprs
            if (primal%exprs(i)%kind /= FAD_VAR .and. &
                primal%exprs(i)%kind /= FAD_INDEX) cycle
            source = trim(primal%exprs(i)%text)
            if (fad_base_name(source) /= trim(receiver_alias)) cycle
            if (index(source, "%") <= 0) cycle
            canonical = resolve_component_path(primal, source)
            if (.not. active_path_matches(active_paths, canonical)) then
                if (primal%decl_index(fad_base_name(canonical)) <= 0) cycle
                if (.not. active(primal%decl_index(fad_base_name(canonical)))) cycle
            end if
            base = fad_base_name(source)
            cut = len_trim(base) + 1
            tail = source(cut:)
            target = trim(cotangent_alias)//trim(tail)
            if (is_known_name(paths, n, target)) cycle
            if (n == size(paths)) exit
            n = n + 1
            paths(n) = target
            s%kind = FAD_ASSIGN
            s%target = target
            s%value = adjoint%add_expr(expr_const("0.0"//adjoint%real_suffix))
            ignored = adjoint%add_stmt(s)
        end do
    end subroutine zero_receiver_cotangent

    logical function active_path_matches(paths, canonical) result(found)
        character(len=*), intent(in) :: paths(:)
        character(len=*), intent(in) :: canonical
        integer :: i

        found = .false.
        do i = 1, size(paths)
            if (same_component_name(paths(i), canonical)) then
                found = .true.
                return
            end if
        end do
    end function active_path_matches

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
        call copy_decl(d, primal%decls(di))
        d%name = base//suffix
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_optional = .false.
        ignored = adjoint%add_decl(d)
        s%kind = FAD_ASSIGN
        s%target = base//suffix
        s%value = zero
        ignored = adjoint%add_stmt(s)
    end subroutine declare_array_adjoint

    subroutine declare_array_seed_shadow(primal, adjoint, base, suffix)
        !! Declare a copy used to preserve an element seed across a local
        !! array store.  Keeping the copy as a whole array prevents the
        !! optimiser from substituting `x_b(i)` back through the zero that
        !! kills the destination before the old RHS is accumulated.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        character(len=*), intent(in) :: base, suffix
        type(fad_decl_t) :: d
        integer :: di, ignored

        if (adjoint%decl_index(shadow_array_name(base, suffix)) > 0) return
        di = primal%decl_index(base)
        if (di == 0) return
        call copy_decl(d, primal%decls(di))
        d%name = shadow_array_name(base, suffix)
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_optional = .false.
        ignored = adjoint%add_decl(d)
    end subroutine declare_array_seed_shadow

    function shadow_array_name(base, suffix) result(name)
        character(len=*), intent(in) :: base, suffix
        character(len=:), allocatable :: name
        name = trim(base)//trim(suffix)//"_in"
    end function shadow_array_name

    function array_seed_shadow_element(target, suffix) result(name)
        character(len=*), intent(in) :: target, suffix
        character(len=:), allocatable :: name
        integer :: open
        open = index(target, "(")
        if (open > 0) then
            name = trim(target(:open - 1))//trim(suffix)//"_in"//target(open:)
        else
            name = trim(target)//trim(suffix)//"_in"
        end if
    end function array_seed_shadow_element

    function adjoint_element(target, suffix) result(name)
        !! `c(i)` becomes `c_b(i)`: the suffix goes on the array name, not on
        !! the whole reference.
        character(len=*), intent(in) :: target, suffix
        character(len=:), allocatable :: name
        name = fad_suffix_name(target, suffix)
    end function adjoint_element

    function shadow_element(target, suffix, dependent) result(name)
        !! As `adjoint_element`, but through a local copy for the dependent.
        !!
        !! Reading an element's adjoint and then clearing it is how a store's
        !! adjoint works: the store killed whatever the element held, so its
        !! adjoint stops there. For the dependent that adjoint is the caller's
        !! incoming seed, declared `intent(in)`, and clearing it would both fail
        !! to compile and mean writing to the caller's argument. The sweep works
        !! on a copy instead.
        character(len=*), intent(in) :: target, suffix, dependent
        character(len=:), allocatable :: name, base
        integer :: pos

        pos = index(target, "(")
        base = target
        if (pos > 0) base = target(1:pos - 1)
        if (index(trim(dependent), "%") == 0 .and. &
            trim(base) == trim(dependent)) then
            name = trim(base)//suffix//"_in"
            if (pos > 0) name = name//target(pos:)
        else
            name = adjoint_element(target, suffix)
        end if
    end function shadow_element

    logical function is_section_target(target) result(yes)
        !! Whether an assignment target contains a range rather than an index.
        character(len=*), intent(in) :: target
        integer :: open, colon

        yes = .false.
        open = index(target, "(")
        if (open <= 0) return
        colon = index(target(open + 1:), ":")
        if (colon > 0) yes = .true.
    end function is_section_target

    subroutine declare_seed_shadow(primal, adjoint, dependent, suffix)
        !! Declare and fill the dependent's local adjoint copy.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        character(len=*), intent(in) :: dependent, suffix
        type(fad_decl_t) :: d
        type(fad_stmt_t) :: s
        integer :: di, ignored

        ! A component designator cannot be declared as a standalone dummy;
        ! component dependents use the separate shaped seed instead.
        if (index(trim(dependent), "%") > 0) return
        if (adjoint%decl_index(dependent//suffix//"_in") > 0) return
        di = primal%decl_index(dependent)
        if (di == 0) return
        call copy_decl(d, primal%decls(di))
        d%name = dependent//suffix//"_in"
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_optional = .false.
        ignored = adjoint%add_decl(d)
        s%kind = FAD_ASSIGN
        s%target = dependent//suffix//"_in"
        s%value = adjoint%add_expr(expr_var(dependent//suffix))
        ignored = adjoint%add_stmt(s)
    end subroutine declare_seed_shadow

    logical function worth_taping(primal, shape, name) result(yes)
        !! Whether storing a temporary beats recomputing it.
        !!
        !! Measured rather than assumed. Taping every temporary sped the LSTM
        !! kernel up by 12% and the bundle-adjustment kernel by 20%, and slowed
        !! Euler and the Brusselator down by the same order. The difference is
        !! what the temporary costs to rebuild: a transcendental, a division or
        !! a power is tens of cycles and worth a load, while a multiply-add is
        !! one and the store is pure loss.
        type(fad_proc_t), intent(in) :: primal
        type(loop_shape_t), intent(in) :: shape
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        do i = shape%first + 1, shape%last - 1
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (primal%stmts(i)%target /= name) cycle
            yes = expensive(primal, primal%stmts(i)%value)
            return
        end do
    end function worth_taping

    recursive logical function expensive(p, idx) result(yes)
        !! Whether an expression costs enough to be worth storing.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_CALL)
            yes = .true.
            return
        case (FAD_BINOP)
            select case (trim(p%exprs(idx)%text))
            case ("/", "**")
                yes = .true.
                return
            end select
        end select
        do i = 1, size(p%exprs(idx)%args)
            if (expensive(p, p%exprs(idx)%args(i))) then
                yes = .true.
                return
            end if
        end do
    end function expensive

    logical function is_real_type(d) result(yes)
        !! Only real temporaries are worth taping; an integer index is cheaper
        !! to recompute than to store.
        type(fad_decl_t), intent(in) :: d

        yes = .false.
        if (.not. allocated(d%type_name)) return
        yes = index(d%type_name, "real") == 1 .or. index(d%type_name, "REAL") == 1
    end function is_real_type

    logical function is_derived_decl(p, di) result(yes)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: di
        character(len=:), allocatable :: t, compact
        integer :: i

        yes = .false.
        if (di <= 0 .or. di > p%n_decls) return
        if (.not. allocated(p%decls(di)%type_name)) return
        t = p%decls(di)%type_name
        compact = ""
        do i = 1, len_trim(t)
            if (t(i:i) == " " .or. t(i:i) == achar(9)) cycle
            compact = compact//t(i:i)
        end do
        if (len_trim(compact) < 5) return
        yes = compact(:5) == "type(" .or. compact(:5) == "TYPE("
        if (.not. yes .and. len_trim(compact) >= 6) then
            yes = compact(:6) == "class(" .or. compact(:6) == "CLASS("
        end if
    end function is_derived_decl

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
        type(fad_stmt_t) :: directive

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

        ! Put the directive immediately before the actual loop. Statements
        ! generated after the forward body (constant partials, for example)
        ! may otherwise land between the directive and its loop and make the
        ! OpenMP construct invalid.
        if (can_parallelize_fused(rec)) then
            ! Fortran forbids OpenMP parallel regions in PURE procedures. The
            ! generated arithmetic remains side-effect free apart from its
            ! declared outputs, but the directive requires the procedure to
            ! carry the non-pure OpenMP contract.
            adjoint%is_pure = .false.
            do i = 1, n_new
                if (fused(i)%kind /= FAD_DO) cycle
                block
                    integer :: j
                    do j = n_new, i, -1
                        fused(j + 1) = fused(j)
                    end do
                end block
                directive%kind = FAD_DIRECTIVE
                directive%target = omp_fused_directive(rec)
                fused(i) = directive
                n_new = n_new + 1
                exit
            end do
        end if

        adjoint%stmts(1:n_new) = fused(1:n_new)
        adjoint%n_stmts = n_new
    end subroutine fuse_loop

    logical function can_parallelize_fused(rec) result(yes)
        !! Whether a fused reduction loop has a race-free OpenMP form.
        type(loop_record_t), intent(in) :: rec
        integer :: i

        yes = rec%n_levels == 1 .and. rec%n_carried == 0
        if (.not. yes) return
        do i = 1, rec%n_body
            if (rec%body_is_accum(i) .and. rec%body_sign(i) < 0) then
                yes = .false.
                return
            end if
        end do
    end function can_parallelize_fused

    function omp_fused_directive(rec) result(text)
        !! Mark a fused reduction loop for late OpenMP clause emission.
        type(loop_record_t), intent(in) :: rec
        character(len=:), allocatable :: text
        integer :: i

        text = "!$omp parallel do"
        do i = 1, rec%shape%n_accumulators
            if (.not. allocated(rec%accum_names)) cycle
            if (len_trim(rec%accum_names(i)) == 0) cycle
            text = text//"|"//trim(rec%accum_names(i))
        end do
    end function omp_fused_directive

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
        do i = 1, rec%n_levels
            s%kind = FAD_DO
            s%target = trim(rec%nest_var(i))
            ! Only a *taped* carried variable forces the reverse loop to run
            ! backwards. A linear recurrence propagates its adjoint through a
            ! constant coefficient, and a tape of per-iteration temporaries is
            ! indexed rather than ordered, so neither constrains direction.
            if (rec%n_carried > 0) then
                s%lo = rec%nest_hi(i)
                s%hi = rec%nest_lo(i)
                s%step = adjoint%add_expr(expr_const("-1"))
            else
                s%lo = rec%nest_lo(i)
                s%hi = rec%nest_hi(i)
                s%step = rec%nest_step(i)
            end if
            ignored = adjoint%add_stmt(s)
        end do

        ! Restore each carried value as it was on entry to this iteration, so
        ! the body can be recomputed exactly as the forward sweep ran it.
        if (rec%taped) then
            do i = 1, rec%n_carried
                if (adjoint%decl_index(trim(rec%carried_name(i))//"_tape") == 0) cycle
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
            if (rec%taped .and. &
                adjoint%decl_index(trim(rec%body_lhs(i))//"_tape") > 0) then
                s%value = adjoint%add_expr(expr_var( &
                    trim(rec%body_lhs(i))//"_tape("//rec%tape_index//")"))
            else
                s%value = rec%body_rhs(i)
            end if
            ignored = adjoint%add_stmt(s)
        end do

        ! The adjoint arriving from the next iteration belongs to this
        ! iteration's final version, so hand it over and clear the carrier.
        ! This is what makes the recurrence's adjoint propagate, and it is
        ! needed whether or not the variable was taped: a linear recurrence
        ! still carries its adjoint backwards, it just does so through a
        ! coefficient that costs nothing to rebuild.
        if (rec%n_carried > 0) then
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

        do i = 1, rec%n_levels
            s%kind = FAD_END_DO
            s%value = 0
            ignored = adjoint%add_stmt(s)
        end do

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
        di = primal%decl_index_of(base)
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
        di = primal%decl_index_of(base)
        if (di == 0) return
        call copy_decl(d, primal%decls(di))
        d%name = ssa_name//suffix
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_optional = .false.
        ignored = adjoint%add_decl(d)
    end subroutine declare_adjoint

    recursive subroutine accumulate(primal, adjoint, idx, seed, ssa, suffix, &
            active, n_tmp, status, reverse_receiver_alias, &
            reverse_cotangent_alias)
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
        character(len=*), intent(in), optional :: reverse_receiver_alias
        character(len=*), intent(in), optional :: reverse_cotangent_alias
        type(fad_stmt_t) :: s
        integer, allocatable :: dargs(:), node_args(:)
        integer :: i, j, one, partial, contrib, child_seed, ignored, di
        integer :: node_kind, lhs, two
        character(len=:), allocatable :: base, node_text, leaf_text, target_text

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
            base = fad_base_name(base)
            if (allocated(adjoint%exprs(idx)%component_original_path)) then
                node_text = adjoint%exprs(idx)%component_original_path
                base = fad_base_name(node_text)
            end if
            di = primal%decl_index(base)
            if (di > 0) then
                if (index(trim(node_text), "%") > 0) then
                    if (.not. (present(reverse_receiver_alias) .and. &
                        present(reverse_cotangent_alias) .and. &
                        len_trim(reverse_receiver_alias) > 0 .and. &
                        same_component_name(fad_base_name(node_text), &
                        reverse_receiver_alias))) then
                        if (.not. component_path_is_active(primal, node_text, &
                            active, ssa%active_paths)) return
                    end if
                else if (.not. active(di)) then
                    return
                end if
            end if
            target_text = reverse_component_target(node_text, suffix, &
                reverse_receiver_alias, reverse_cotangent_alias)
            s%kind = FAD_ASSIGN
            s%target = target_text
            lhs = adjoint%add_expr(expr_var(target_text))
            s%value = fad_add(adjoint, lhs, seed)
            ignored = adjoint%add_stmt(s)

        case (FAD_BINOP)
            one = fad_real(adjoint, "1.0")
            if (trim(node_text) == "*" .and. node_args(1) == node_args(2)) then
                ! x*x: both operands are the same value, so the two adjoint
                ! contributions are identical. Push one, doubled, rather than
                ! the same accumulation twice.
                if (carries_adjoint(primal, adjoint, node_args(1), ssa, active)) then
                    two = fad_real(adjoint, "2.0")
                    partial = fad_mul(adjoint, two, node_args(1))
                    contrib = fad_mul(adjoint, seed, partial)
                    call materialise(primal, adjoint, contrib, ssa, n_tmp, &
                        child_seed)
                    call accumulate(primal, adjoint, node_args(1), child_seed, &
                        ssa, suffix, active, n_tmp, status, &
                        reverse_receiver_alias, reverse_cotangent_alias)
                end if
                return
            end if
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
                    suffix, active, n_tmp, status, reverse_receiver_alias, &
                    reverse_cotangent_alias)
                if (.not. status%ok) return
            end do

        case (FAD_UNOP)
            one = fad_real(adjoint, "1.0")
            partial = jvp_unop(adjoint, node_text, node_args(1), one)
            if (partial == 0) return
            contrib = fad_mul(adjoint, seed, partial)
            call materialise(primal, adjoint, contrib, ssa, n_tmp, child_seed)
            call accumulate(primal, adjoint, node_args(1), child_seed, ssa, &
                suffix, active, n_tmp, status, reverse_receiver_alias, &
                reverse_cotangent_alias)

        case (FAD_CALL)
            if (.not. has_rule(node_text)) then
                status%ok = .false.
                status%message = "no derivative rule for '"//node_text// &
                    "'; register one with fad_add_rule, or keep it out of "// &
                    "the active path"
                return
            end if
            if (lower_name(node_text) == "aimag") then
                ! The forward tangent of AIMAG is real, but its transpose
                ! maps a real seed to the imaginary coordinate of a complex
                ! input. Build that coordinate explicitly instead of asking
                ! the forward rule for AIMAG(1.0), which is compiler-invalid.
                if (size(node_args) == 1) then
                    if (carries_adjoint(primal, adjoint, node_args(1), ssa, &
                        active)) then
                        block
                            integer :: zero_real, seed_kind, complex_seed
                            zero_real = fad_real(adjoint, "0.0")
                            seed_kind = fad_fn1(adjoint, "kind", seed)
                            complex_seed = fad_fn3(adjoint, "cmplx", &
                                zero_real, seed, seed_kind)
                            call accumulate(primal, adjoint, node_args(1), &
                                complex_seed, ssa, suffix, active, n_tmp, &
                                status, reverse_receiver_alias, &
                                reverse_cotangent_alias)
                        end block
                    end if
                end if
                return
            end if
            if (lower_name(node_text) == "abs") then
                ! For a nonzero complex input, abs(z) has the real-coordinate
                ! transpose z/abs(z).  The zero case is rejected during path
                ! validation instead of inventing a subgradient at the cusp.
                if (size(node_args) == 1) then
                    if (has_active_complex(primal, node_args(1), active)) then
                        if (carries_adjoint(primal, adjoint, node_args(1), ssa, &
                            active)) then
                            block
                                integer :: denominator, radial, complex_seed
                                denominator = fad_fn1(adjoint, "abs", node_args(1))
                                radial = fad_mul(adjoint, seed, node_args(1))
                                complex_seed = fad_div(adjoint, radial, denominator)
                                call accumulate(primal, adjoint, node_args(1), &
                                    complex_seed, ssa, suffix, active, n_tmp, &
                                    status, reverse_receiver_alias, &
                                    reverse_cotangent_alias)
                            end block
                        end if
                        return
                    end if
                end if
            end if
            if (trim(node_text) == "sum") then
                if (size(node_args) > 0) then
                    call accumulate(primal, adjoint, node_args(1), seed, ssa, &
                        suffix, active, n_tmp, status, reverse_receiver_alias, &
                        reverse_cotangent_alias)
                end if
                return
            end if
            if (lower_name(node_text) == "spread") then
                ! The transpose of SPREAD sums the output cotangent over
                ! the replicated dimension.  A generic forward-rule partial
                ! would incorrectly return an expanded array and cannot be
                ! accumulated into the rank-one source.
                if (size(node_args) >= 3) then
                    if (carries_adjoint(primal, adjoint, node_args(1), ssa, &
                        active)) then
                        block
                            integer :: sum_args(2), sum_seed
                            character(len=4) :: sum_names(2)
                            sum_args(1) = seed
                            sum_args(2) = node_args(2)
                            sum_names = [character(len=4) :: "", "dim"]
                            sum_seed = adjoint%add_expr(expr_call( &
                                "sum", sum_args, sum_names))
                            call accumulate(primal, adjoint, node_args(1), &
                                sum_seed, ssa, suffix, active, n_tmp, status, &
                                reverse_receiver_alias, reverse_cotangent_alias)
                        end block
                    end if
                end if
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
                    suffix, active, n_tmp, status, reverse_receiver_alias, &
                    reverse_cotangent_alias)
                if (.not. status%ok) return
            end do

        case (FAD_INDEX)
            ! `a(i)` contributes to `a_b(i)`. Inside a reduction loop each
            ! iteration touches a different element, so these scatters carry no
            ! loop-carried dependence.
            call ssa_base_of(ssa, node_text, base)
            base = fad_base_name(base)
            di = primal%decl_index(base)
            if (di > 0) then
                if (index(trim(emit_expr(adjoint, idx)), "%") > 0) then
                    if (.not. component_path_is_active(primal, emit_expr(adjoint, idx), &
                        active, ssa%active_paths)) return
                else if (.not. active(di)) then
                    return
                end if
            end if
            block
                type(fad_expr_t) :: target_expr
                integer :: read_idx
                target_expr%kind = FAD_INDEX
                target_expr%text = reverse_component_target(node_text, suffix, &
                    reverse_receiver_alias, reverse_cotangent_alias)
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
        character(len=:), allocatable :: base, leaf_text
        integer :: i, di

        yes = .false.
        if (idx <= 0 .or. idx > adjoint%n_exprs) return
        select case (adjoint%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            call ssa_base_of(ssa, adjoint%exprs(idx)%text, base)
            base = fad_base_name(base)
            leaf_text = emit_expr(adjoint, idx)
            if (adjoint%exprs(idx)%kind == FAD_VAR .and. &
                allocated(adjoint%exprs(idx)%component_original_path)) then
                leaf_text = adjoint%exprs(idx)%component_original_path
                base = fad_base_name(leaf_text)
            end if
            di = primal%decl_index(base)
            if (di > 0) then
                if (index(trim(leaf_text), "%") > 0) then
                    yes = component_path_is_active(primal, leaf_text, active, &
                        ssa%active_paths)
                else
                    yes = active(di)
                end if
                if (yes) then
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
        !! Bind a compound seed to a local temporary.
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
        integer :: ignored, array_di
        logical :: force_bind

        associate (unused => ssa)
        end associate
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
        d%is_optional = .false.
        array_di = array_decl_in_expr(primal, adjoint, expr, ssa)
        if (array_di > 0) then
            !! An array partial needs an allocatable temporary: its shape is
            !! inherited from an assumed-shape active input at runtime.  An
            !! ordinary local with DIMENSION(:,:) would be invalid Fortran.
            d%is_array = .true.
            d%is_allocatable = .true.
            if (allocated(primal%decls(array_di)%dims)) then
                d%dims = deferred_dims(primal%decls(array_di)%dims)
            end if
        end if
        ignored = adjoint%add_decl(d)

        s%kind = FAD_ASSIGN
        s%target = name
        s%value = expr
        ignored = adjoint%add_stmt(s)
        out = adjoint%add_expr(expr_var(name))
    end subroutine materialise

    recursive integer function array_decl_in_expr(primal, adjoint, idx, ssa) &
            result(found)
        !! Find an active array declaration contributing to an expression.
        !! Materialised reverse partials use that declaration's deferred shape
        !! to remain valid for runtime extents.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(in) :: adjoint
        integer, intent(in) :: idx
        type(ssa_map_t), intent(in) :: ssa
        character(len=:), allocatable :: base
        integer :: i, di, adjoint_di, arg_idx

        found = 0
        if (idx <= 0) return
        if (idx > adjoint%n_exprs) return
        select case (adjoint%exprs(idx)%kind)
        case (FAD_VAR)
            call ssa_base_of(ssa, adjoint%exprs(idx)%text, base)
            di = primal%decl_index_of(base)
            if (di > 0) then
                if (primal%decls(di)%is_array) then
                    found = di
                    return
                end if
            end if
            ! A reverse seed such as ``v_b`` is an adjoint declaration, not
            ! a primal name.  Match its proven shape to a primal array so a
            ! compound seed/partial product gets an array temporary.
            adjoint_di = adjoint%decl_index(adjoint%exprs(idx)%text)
            if (adjoint_di > 0 .and. adjoint%decls(adjoint_di)%is_array) then
                do i = 1, primal%n_decls
                    if (.not. primal%decls(i)%is_array) cycle
                    if (allocated(adjoint%decls(adjoint_di)%dims) .and. &
                        allocated(primal%decls(i)%dims)) then
                        if (trim(adjoint%decls(adjoint_di)%dims) /= &
                            trim(primal%decls(i)%dims)) cycle
                    end if
                    found = i
                    return
                end do
            end if
        case (FAD_INDEX)
            call ssa_base_of(ssa, adjoint%exprs(idx)%text, base)
            di = primal%decl_index_of(base)
            if (di > 0) then
                if (adjoint%exprs(idx)%is_array_section .and. &
                    primal%decls(di)%is_array) then
                    found = di
                    return
                end if
            end if
            ! A vector subscript is an array-valued FAD_INDEX without the
            ! range marker used for sections.  Its result is still an array,
            ! so reverse materialisation must allocate a shaped temporary
            ! when the seed is multiplied by its partial.
            if (.not. allocated(adjoint%exprs(idx)%args)) return
            do i = 1, size(adjoint%exprs(idx)%args)
                arg_idx = adjoint%exprs(idx)%args(i)
                if (arg_idx <= 0 .or. arg_idx > adjoint%n_exprs) cycle
                if (adjoint%exprs(arg_idx)%kind /= FAD_VAR) cycle
                call ssa_base_of(ssa, adjoint%exprs(arg_idx)%text, base)
                di = primal%decl_index_of(base)
                if (di <= 0) cycle
                if (.not. primal%decls(di)%is_array) cycle
                call ssa_base_of(ssa, adjoint%exprs(idx)%text, base)
                di = primal%decl_index_of(base)
                if (di > 0 .and. primal%decls(di)%is_array) then
                    found = di
                    return
                end if
            end do
        end select
        if (.not. allocated(adjoint%exprs(idx)%args)) return
        do i = 1, size(adjoint%exprs(idx)%args)
            found = array_decl_in_expr(primal, adjoint, &
                adjoint%exprs(idx)%args(i), ssa)
            if (found > 0) return
        end do
    end function array_decl_in_expr

    function deferred_dims(dims) result(out)
        !! Convert a declared rank to deferred dimensions for an allocatable.
        character(len=*), intent(in) :: dims
        character(len=:), allocatable :: out
        integer :: i, rank

        rank = 1
        do i = 1, len_trim(dims)
            if (dims(i:i) == ",") rank = rank + 1
        end do
        out = ""
        do i = 1, rank
            if (i > 1) out = out//","
            out = out//":"
        end do
    end function deferred_dims

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
        e%is_array_section = src%exprs(idx)%is_array_section
        if (e%kind == FAD_VAR .or. e%kind == FAD_INDEX) then
            call component_snapshot_lookup(ssa, emit_expr(src, idx), name)
            if (len_trim(name) > 0) then
                e%kind = FAD_VAR
                e%text = name
                e%component_original_path = emit_expr(src, idx)
                allocate (args(0))
                e%args = args
                out = dst%add_expr(e)
                return
            end if
        end if
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

    subroutine add_component_snapshot(ssa, target, snapshot)
        type(ssa_map_t), intent(inout) :: ssa
        character(len=*), intent(in) :: target
        character(len=:), allocatable, intent(out) :: snapshot
        character(len=32) :: number

        write (number, '(i0)') ssa%n_components + 1
        snapshot = "fad_component_in_"//trim(number)
        ssa%n_components = ssa%n_components + 1
        if (ssa%n_components > size(ssa%component_targets)) then
            ssa%n_components = size(ssa%component_targets)
            snapshot = "fad_component_in_"//trim(number)
        end if
        ssa%component_targets(ssa%n_components) = trim(target)
        ssa%component_snapshots(ssa%n_components) = snapshot
    end subroutine add_component_snapshot

    subroutine emit_initial_component_snapshots(primal, adjoint, ssa)
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: adjoint
        type(ssa_map_t), intent(inout) :: ssa
        type(fad_decl_t) :: d
        type(fad_stmt_t) :: snap
        character(len=:), allocatable :: snapshot
        integer :: i, di, ignored

        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (index(trim(primal%stmts(i)%target), "%") == 0) cycle
            if (index(trim(primal%stmts(i)%target), "(") == 0) cycle
            di = primal%decl_index_of(primal%stmts(i)%target)
            if (di == 0) cycle
            call add_component_snapshot(ssa, primal%stmts(i)%target, snapshot)
            call copy_decl(d, primal%decls(di))
            d%name = snapshot
            d%type_name = "real(8)"
            d%is_array = .false.
            d%is_allocatable = .false.
            d%is_result = .false.
            d%is_optional = .false.
            d%intent = FAD_INTENT_NONE
            ignored = adjoint%add_decl(d)
            snap%kind = FAD_ASSIGN
            snap%target = snapshot
            snap%value = adjoint%add_expr(expr_var(primal%stmts(i)%target))
            ignored = adjoint%add_stmt(snap)
        end do
    end subroutine emit_initial_component_snapshots

    subroutine remove_component_snapshot(ssa, target)
        type(ssa_map_t), intent(inout) :: ssa
        character(len=*), intent(in) :: target
        integer :: i

        do i = 1, ssa%n_components
            if (trim(ssa%component_targets(i)) /= trim(target)) cycle
            ssa%component_targets(i:ssa%n_components - 1) = &
                ssa%component_targets(i + 1:ssa%n_components)
            ssa%component_snapshots(i:ssa%n_components - 1) = &
                ssa%component_snapshots(i + 1:ssa%n_components)
            ssa%n_components = ssa%n_components - 1
            return
        end do
    end subroutine remove_component_snapshot

    subroutine component_snapshot_lookup(ssa, target, snapshot)
        type(ssa_map_t), intent(in) :: ssa
        character(len=*), intent(in) :: target
        character(len=:), allocatable, intent(out) :: snapshot
        integer :: i

        snapshot = ""
        do i = ssa%n_components, 1, -1
            if (trim(ssa%component_targets(i)) == trim(target)) then
                snapshot = trim(ssa%component_snapshots(i))
                return
            end if
        end do
    end subroutine component_snapshot_lookup

    subroutine independent_component_paths(independents, paths)
        character(len=*), intent(in) :: independents(:)
        character(len=256), allocatable, intent(out) :: paths(:)
        integer :: i, n

        n = 0
        do i = 1, size(independents)
            if (index(trim(independents(i)), "%") > 0) n = n + 1
        end do
        allocate (paths(n))
        n = 0
        do i = 1, size(independents)
            if (index(trim(independents(i)), "%") == 0) cycle
            n = n + 1
            paths(n) = trim(independents(i))
        end do
    end subroutine independent_component_paths

    logical function component_path_is_active(primal, text, active, paths) &
            result(yes)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: paths(:)
        character(len=:), allocatable :: canonical_text, expression_path, &
            active_path
        integer :: i, di
        logical :: component

        component = .false.
        yes = .false.
        canonical_text = resolve_component_path(primal, text)
        if (nested_polymorphic_component_active(primal, canonical_text, active)) then
            yes = .true.
            return
        end if
        if (polymorphic_owner_array_component_active(primal, canonical_text, &
            active)) then
            yes = .true.
            return
        end if
        ! An explicit independent component path must remain active even when
        ! the lowered expression has no component-path metadata of its own.
        ! Do not classify every percent expression as a component here: the
        ! ordinary derived-object fallback below is what keeps implicit
        ! component activity (for example child%scale) working.
        do di = 1, size(paths)
            active_path = resolve_component_path(primal, paths(di))
            if (same_component_name(active_path, canonical_text)) then
                yes = .true.
                return
            end if
        end do
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            expression_path = resolve_component_path(primal, &
                primal%exprs(i)%text)
            if (.not. same_component_name(expression_path, canonical_text)) cycle
            component = .true.
            do di = 1, size(paths)
                active_path = resolve_component_path(primal, paths(di))
                if (same_component_name(active_path, canonical_text)) then
                    yes = .true.
                    exit
                end if
            end do
            exit
        end do
        if (component) then
            ! A concrete scalar component of an active derived shadow
            ! carries an adjoint even when no component was listed
            ! independently. This is needed after MOVE_ALLOC transfers
            ! ownership to a different enclosing object. Do not extend
            ! that fallback to ordinary derived fields: their activity
            ! must still come from an explicit component path.
            if (is_concrete_allocatable_component_path(primal, &
                canonical_text)) then
                di = primal%decl_index(fad_base_name(canonical_text))
                if (di > 0) yes = active(di)
            end if
            return
        end if
        di = primal%decl_index_of(canonical_text)
        if (di > 0) then
            yes = active(di)
        end if
    end function component_path_is_active

    logical function polymorphic_owner_array_component_active(primal, path, &
            active) result(found)
        !! A fixed SOURCE= allocation makes the concrete payload of an owner
        !! array an active reverse path when its source is active.  The
        !! SELECT TYPE alias is lowered separately, so recover that fact here
        !! for carry analysis and component cotangent routing.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: path
        logical, intent(in) :: active(:)
        character(len=:), allocatable :: selector, owner
        integer :: percent, owner_di, source_di, i

        found = .false.
        percent = index(trim(path), "%")
        if (percent <= 1) return
        selector = trim(path(:percent - 1))
        owner = fad_base_name(selector)
        owner_di = primal%decl_index(owner)
        if (owner_di <= 0) return
        if (.not. primal%decls(owner_di)%is_allocatable .or. &
            .not. primal%decls(owner_di)%is_polymorphic .or. &
            .not. primal%decls(owner_di)%is_array) return
        if (.not. fixed_literal_owner_selector(selector, owner)) return
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ALLOCATE) cycle
            if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
            if (size(primal%stmts(i)%allocation_args) < 1) cycle
            if (fad_base_name(emit_expr(primal, &
                primal%stmts(i)%allocation_args(1))) /= owner) cycle
            if (primal%stmts(i)%allocation_source <= 0) return
            source_di = concrete_source_decl(primal, &
                primal%stmts(i)%allocation_source)
            found = source_di > 0 .and. active(source_di)
            if (.not. found) found = reads_any(primal, &
                primal%stmts(i)%allocation_source, active)
            return
        end do
    end function polymorphic_owner_array_component_active

    function resolve_component_path(primal, text) result(path)
        !! Resolve a component read through a SELECT TYPE alias back to the
        !! original receiver designator. FortAD's direct polymorphic-call
        !! lowering names the selected object (for example
        !! fad_dispatch_receiver_1), while the active path is supplied by
        !! the caller as a(2)%scale.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: path, base, tail
        integer :: di, cut

        path = trim(text)
        base = fad_base_name(path)
        di = primal%decl_index(base)
        if (di <= 0) return
        if (.not. primal%decls(di)%is_select_alias) return
        if (.not. allocated(primal%decls(di)%alias_target)) return
        cut = len_trim(base) + 1
        if (cut <= len_trim(path)) then
            tail = path(cut:)
        else
            tail = ""
        end if
        path = map_section_alias_path(primal%decls(di)%alias_target, tail)
    end function resolve_component_path

    function map_section_alias_path(alias_target, tail) result(mapped)
        !! Map a literal element of a rank-one SELECT TYPE section to the
        !! original array element, e.g. item(1)%scale from model(2:3) to
        !! model(2)%scale. Open bounds, strides, vectors, and nested sections
        !! are deliberately outside this descriptor-free path.
        character(len=*), intent(in) :: alias_target, tail
        character(len=:), allocatable :: mapped, section, index_text
        integer :: open, colon, relative, lower, ios, absolute
        character(len=32) :: number

        mapped = trim(alias_target)//trim(tail)
        open = index(alias_target, "(")
        colon = index(alias_target, ":")
        if (open <= 1 .or. colon <= open) return
        if (len_trim(tail) < 3) return
        if (tail(1:1) /= "(") return
        if (index(tail, ":") > 0 .or. index(tail, ",") > 0) return
        if (index(tail, ")") <= 2) return
        index_text = trim(tail(2:index(tail, ")") - 1))
        read (index_text, *, iostat=ios) relative
        if (ios /= 0) return
        section = trim(alias_target(open + 1:colon - 1))
        read (section, *, iostat=ios) lower
        if (ios /= 0) return
        absolute = lower + relative - 1
        write (number, '(i0)') absolute
        mapped = trim(alias_target(:open - 1))//"("//trim(number)//")"// &
            tail(index(tail, ")") + 1:)
    end function map_section_alias_path

    logical function is_select_alias_path(primal, text) result(yes)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        integer :: di, i

        yes = .false.
        di = primal%decl_index(fad_base_name(text))
        if (di > 0) then
            if (primal%decls(di)%is_select_alias .and. &
                allocated(primal%decls(di)%alias_target)) then
                ! Scalar SELECT TYPE aliases need the same routed cotangent
                ! handling as section aliases; all such paths stay in the
                ! paired TYPE IS arm.
                yes = .true.
                return
            end if
        end if
        ! Some lowered selector aliases are represented on SELECT TYPE but
        ! are absent from the declaration table. The statement still proves
        ! the fixed path, so route component cotangents into that arm.
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_SELECT_TYPE) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            if (trim(primal%stmts(i)%target) /= fad_base_name(text)) cycle
            if (index(trim(emit_expr(primal, primal%stmts(i)%value)), &
                achar(37)) > 0) yes = .true.
            return
        end do
    end function is_select_alias_path

    function reverse_component_target(text, suffix, receiver_alias, &
            cotangent_alias) result(target)
        !! Route an active component in a selected receiver to the matching
        !! selected cotangent alias. Outside this bounded dispatch context
        !! ordinary suffixing remains unchanged.
        character(len=*), intent(in) :: text, suffix
        character(len=*), intent(in), optional :: receiver_alias, cotangent_alias
        character(len=:), allocatable :: target, base
        integer :: cut

        target = fad_suffix_name(text, suffix)
        if (.not. present(receiver_alias)) return
        if (.not. present(cotangent_alias)) return
        if (len_trim(receiver_alias) == 0 .or. len_trim(cotangent_alias) == 0) return
        base = fad_base_name(text)
        if (.not. same_component_name(base, receiver_alias)) return
        if (index(trim(text), "%") <= 0) return
        cut = len_trim(base) + 1
        target = trim(cotangent_alias)//trim(text(cut:))
    end function reverse_component_target

    logical function same_component_name(a, b) result(equal)
        character(len=*), intent(in) :: a, b
        integer :: i

        equal = len_trim(a) == len_trim(b)
        if (.not. equal) return
        do i = 1, len_trim(a)
            if (lower_name_char(a(i:i)) /= lower_name_char(b(i:i))) then
                equal = .false.
                return
            end if
        end do
    end function same_component_name

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
        allocate (ssa%component_targets(max(1, ssa%n)))
        allocate (ssa%component_snapshots(max(1, ssa%n)))
        ssa%base = ""
        ssa%current = ""
        ssa%version = 0
        ssa%component_targets = ""
        ssa%component_snapshots = ""
        ssa%n_components = 0
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

        associate (unused => ssa)
        end associate
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
