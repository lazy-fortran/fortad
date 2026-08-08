module fortad_forward
    !! Forward (tangent) mode: build the JVP procedure from the primal IR.
    !!
    !! For each primal statement `v = e` the tangent statement `v_d = D(e)` is
    !! emitted immediately before it, so `v_d` still sees the old `v` where the
    !! rule needs it and no value has to be saved. Statements whose tangent is a
    !! structural zero produce no code at all: that is activity analysis falling
    !! out of the zero-aware rule builders rather than being a separate pass.
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, &
        expr_const, expr_var, expr_binop, expr_unop, expr_call, &
        fad_base_name, fad_suffix_name, &
        FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, FAD_CALL, &
        FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, &
        FAD_ELSE, FAD_END_IF, FAD_CALL_STMT, FAD_INTENT_IN, &
        FAD_INTENT_OUT, FAD_INTENT_INOUT, FAD_INTENT_NONE, &
        FAD_SELECT_TYPE, FAD_TYPE_IS, FAD_CLASS_IS, FAD_CLASS_DEFAULT, &
        FAD_END_SELECT, FAD_ALLOCATE, FAD_DEALLOCATE, FAD_MOVE_ALLOC
    use fortad_rules, only: jvp_binop, jvp_unop, jvp_call, has_rule
    use fortad_registry, only: call_rule_has, call_rule_lines, &
        call_rule_substitute
    use fortad_emit, only: emit_expr
    implicit none
    private

    public :: differentiate_forward, forward_spec_t, forward_status_t

    type :: forward_spec_t
        !! What to differentiate, and with respect to what.
        character(len=:), allocatable :: independents(:)
        character(len=:), allocatable :: dependents(:)
        !! Suffix for tangent names. `x` becomes `x_d` by default.
        character(len=:), allocatable :: suffix
        !! Name of the generated procedure. Defaults to `<primal>_jvp`.
        character(len=:), allocatable :: name
        !! Vector mode: carry `n_dir` tangent directions through one primal
        !! sweep instead of one. Every tangent gains a leading direction
        !! dimension, which is the contiguous axis in Fortran, so the emitted
        !! array expressions vectorise across directions. One primal traversal
        !! then serves k directions at cost `primal + k*active`, rather than
        !! `k*(primal + active)`.
        logical :: vector = .false.
        !! Name of the direction-count dummy argument in vector mode.
        character(len=:), allocatable :: ndir_name
        !! Whether the generated routine also returns the primal value.
        !!
        !! A consumer that already has the value, or that wants a routine
        !! matching a tangent-only contract, does not want it back - and asking
        !! for it keeps the whole primal computation live.
        logical :: with_primal = .true.
    end type forward_spec_t

    type :: forward_status_t
        logical :: ok = .false.
        character(len=:), allocatable :: message
    end type forward_status_t

contains

    subroutine differentiate_forward(primal, spec, tangent, status)
        !! Build the tangent procedure.
        type(fad_proc_t), intent(in) :: primal
        type(forward_spec_t), intent(in) :: spec
        type(fad_proc_t), intent(out) :: tangent
        type(forward_status_t), intent(out) :: status
        character(len=:), allocatable :: suffix, ndir
        character(len=256), allocatable :: active_paths(:)
        character(len=256) :: decl_name, decl_type, decl_dims, tangent_type
        logical, allocatable :: active(:)
        integer :: i, ignored, di

        status%ok = .true.
        suffix = "_d"
        if (allocated(spec%suffix)) suffix = spec%suffix
        ndir = "n_dir"
        if (allocated(spec%ndir_name)) ndir = spec%ndir_name

        if (.not. allocated(spec%independents)) then
            status%ok = .false.
            status%message = "no independent variables given"
            return
        end if

        call seed_activity(primal, spec, active, status)
        if (.not. status%ok) return
        call independent_component_paths(spec%independents, active_paths)
        call refuse_active_polymorphic_dispatch(primal, active_paths, status)
        if (.not. status%ok) return
        call refuse_active_nested_polymorphic_component(primal, active_paths, active, &
            status)
        if (.not. status%ok) return
        call refuse_active_polymorphic_ownership(primal, active, status)
        if (.not. status%ok) return
        if (spec%vector) then
            do i = 1, primal%n_stmts
                if (primal%stmts(i)%kind == FAD_ALLOCATE .or. &
                    primal%stmts(i)%kind == FAD_DEALLOCATE .or. &
                    primal%stmts(i)%kind == FAD_MOVE_ALLOC) then
                    status%ok = .false.
                    status%message = "vector mode with explicit allocation lifetime is "// &
                        "not supported; use scalar directions"
                    return
                end if
            end do
            do i = 1, primal%n_decls
                if (active(i) .and. is_derived_decl(primal, i)) then
                    status%ok = .false.
                    status%message = "vector mode for derived components is "// &
                        "not supported; use scalar directions"
                    return
                end if
            end do
        end if
        tangent%name = primal%name//"_jvp"
        if (allocated(spec%name)) tangent%name = spec%name
        tangent%is_function = .false.
        tangent%is_elemental = primal%is_elemental
        tangent%real_suffix = "d0"
        if (allocated(primal%real_suffix)) tangent%real_suffix = primal%real_suffix
        ! The derivative names the same kinds as the primal, so it needs the
        ! same imports.
        if (primal%n_uses > 0 .and. allocated(primal%uses)) then
            allocate (character(len=256) :: tangent%uses(primal%n_uses))
            do i = 1, primal%n_uses
                tangent%uses(i) = primal%uses(i)
            end do
            tangent%n_uses = primal%n_uses
        end if
        tangent%is_pure = primal%is_pure
        if (tangent%is_pure) tangent%is_pure = .not. has_calls(primal)

        call build_signature(primal, tangent, active, suffix, spec%vector, ndir, &
            spec%with_primal)
        call build_body(primal, tangent, active, active_paths, suffix, &
            spec%vector, status)
        if (.not. status%ok) return

        ! Every local the primal declared is still a local of the tangent
        ! procedure, active or not: an inactive local still holds a primal
        ! value the active statements read.
        do i = 1, primal%n_decls
            if (primal%decls(i)%is_select_alias) cycle
            if (is_dummy(primal, primal%decls(i)%name)) cycle
            if (primal%decls(i)%is_result) cycle
            decl_name = trim(primal%decls(i)%name)
            decl_type = ""
            if (allocated(primal%decls(i)%type_name)) then
                decl_type = primal%decls(i)%type_name
            end if
            decl_dims = ""
            if (allocated(primal%decls(i)%dims)) then
                decl_dims = primal%decls(i)%dims
            end if
            ignored = tangent%add_decl_fields(decl_name, decl_type, &
                FAD_INTENT_NONE, primal%decls(i)%is_value, &
                primal%decls(i)%is_array, primal%decls(i)%is_contiguous, &
                .false., decl_dims, .false., primal%decls(i)%is_allocatable)
            if (active(i)) then
                tangent_type = decl_type
                if (primal%decls(i)%is_polymorphic) then
                    if (len_trim(fixed_source_type(primal, i)) > 0) then
                        tangent_type = fixed_source_type(primal, i)
                    end if
                end if
                call add_tangent_decl(tangent, decl_name, tangent_type, &
                    primal%decls(i)%is_value, &
                    primal%decls(i)%is_array, &
                    primal%decls(i)%is_contiguous, &
                    decl_dims, suffix, &
                    FAD_INTENT_NONE, spec%vector, ndir, &
                    is_optional=.false., &
                    is_allocatable=primal%decls(i)%is_allocatable)
            end if
        end do
    end subroutine differentiate_forward

    subroutine seed_activity(primal, spec, active, status)
        !! Mark declarations reachable from an independent.
        !!
        !! Forward "varied" dataflow: a variable is active if it is an
        !! independent, or if it is assigned from an expression that reads an
        !! active variable. Iterated to a fixed point so loop-carried
        !! dependencies converge.
        type(fad_proc_t), intent(in) :: primal
        type(forward_spec_t), intent(in) :: spec
        logical, allocatable, intent(out) :: active(:)
        type(forward_status_t), intent(inout) :: status
        integer :: i, j, di
        logical :: changed

        allocate (active(max(1, primal%n_decls)))
        active = .false.

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
            active(di) = .true.
        end do

        changed = .true.
        do while (changed)
            changed = .false.
            do j = 1, primal%n_stmts
                if (primal%stmts(j)%kind == FAD_ALLOCATE) then
                    if (primal%stmts(j)%allocation_source > 0 .and. &
                        allocated(primal%stmts(j)%allocation_args)) then
                        di = arg_decl_index(primal, &
                            primal%stmts(j)%allocation_args(1))
                        if (di > 0 .and. expr_reads_active(primal, &
                            primal%stmts(j)%allocation_source, active)) then
                            if (.not. active(di)) then
                                active(di) = .true.
                                changed = .true.
                            end if
                        end if
                    end if
                    cycle
                end if
                if (primal%stmts(j)%kind == FAD_MOVE_ALLOC) then
                    di = arg_decl_index(primal, primal%stmts(j)%call_args(1))
                    if (di > 0) then
                        if (active(di)) call mark_move_alloc_peer(primal, &
                            primal%stmts(j), active, changed)
                    else
                        di = arg_decl_index(primal, primal%stmts(j)%call_args(2))
                        if (di > 0) then
                            if (active(di)) call mark_move_alloc_peer(primal, &
                                primal%stmts(j), active, changed)
                        end if
                    end if
                    cycle
                end if
                if (primal%stmts(j)%kind == FAD_CALL_STMT) then
                    ! A call is opaque, so which arguments it writes is unknown.
                    ! If any argument is active, every argument is treated as
                    ! active: the alternative is guessing, and guessing wrong
                    ! drops a derivative silently.
                    if (.not. call_reads_active(primal, primal%stmts(j), active)) cycle
                    do i = 1, size(primal%stmts(j)%call_args)
                        di = arg_decl_index(primal, primal%stmts(j)%call_args(i))
                        if (di <= 0) cycle
                        if (.not. is_real_decl(primal, di)) cycle
                        if (.not. active(di)) then
                            active(di) = .true.
                            changed = .true.
                        end if
                    end do
                    cycle
                end if
                if (primal%stmts(j)%kind /= FAD_ASSIGN) cycle
                if (.not. expr_reads_active(primal, primal%stmts(j)%value, active)) cycle
                di = primal%decl_index_of( &
                    primal%stmts(j)%target)
                if (di > 0) then
                    if (.not. active(di)) then
                        active(di) = .true.
                        changed = .true.
                    end if
                end if
            end do
        end do
    end subroutine seed_activity

    subroutine refuse_active_polymorphic_dispatch(primal, active_paths, status)
        !! An active scalar CLASS shadow is valid only when upstream facts
        !! prove one concrete runtime implementation. Multiple arms would
        !! require a dynamic-type tangent descriptor, which this IR does not
        !! model, so reject that boundary before emission.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: active_paths(:)
        type(forward_status_t), intent(inout) :: status
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
                path = resolve_component_alias(primal, primal%exprs(j)%text)
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
                status%message = "forward mode: active polymorphic receiver "// &
                    "requires one fixed concrete runtime path; dynamic "// &
                    "dispatch shadows are unsupported"
                return
            end if
        end do
    end subroutine refuse_active_polymorphic_dispatch

    subroutine refuse_active_nested_polymorphic_component(primal, active_paths, &
            active, status)
        !! A borrowed polymorphic component can be paired with its tangent
        !! only when its owner and dynamic path are fixed.  The tangent carries
        !! a caller-owned shadow of the concrete holder; it never owns or
        !! replays the component descriptor.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: active_paths(:)
        logical, intent(in) :: active(:)
        type(forward_status_t), intent(inout) :: status
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
                active_component = .false.
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
                    status%message = "forward mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' uses pointer or TARGET storage"
                    return
                end if
                if (index(trim(selector), "(") > 0) then
                    status%ok = .false.
                    status%message = "forward mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' has dynamic bounds or indexing"
                    return
                end if
                base_di = primal%decl_index(fad_base_name(selector))
                if (base_di <= 0 .or. primal%decls(base_di)%is_polymorphic .or. &
                    primal%decls(base_di)%is_allocatable .or. &
                    primal%decls(base_di)%is_associate_alias .or. &
                    primal%decls(base_di)%is_select_alias) then
                    status%ok = .false.
                    status%message = "forward mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' has unresolved owner alias or ownership"
                    return
                end if
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
                    status%message = "forward mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' requires one fixed concrete "// &
                        "runtime path; unresolved dispatch is unsupported"
                    return
                end if
                fixed_ownership = .false.
                do k = 1, primal%n_stmts
                    if (primal%stmts(k)%kind /= FAD_ALLOCATE) cycle
                    if (.not. allocated(primal%stmts(k)%allocation_args)) cycle
                    if (size(primal%stmts(k)%allocation_args) < 1) cycle
                    if (trim(emit_expr(primal, primal%stmts(k)%allocation_args(1))) /= &
                        trim(selector)) cycle
                    if (has_fixed_source_component(primal, k, active)) then
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
                    status%message = "forward mode: active nested polymorphic "// &
                        "component path '"//trim(selector)//"' crosses ownership/lifetime "// &
                        "operations; caller-owned borrowed components only"
                    return
                end do
                exit
            end do
            if (found .and. .not. active_component) cycle
        end do
    end subroutine refuse_active_nested_polymorphic_component

    subroutine refuse_active_polymorphic_ownership(primal, active, status)
        !! A polymorphic allocatable needs a tangent descriptor with the same
        !! dynamic type as the primal.  The current IR can copy a passive
        !! selector, but it cannot pair SELECT TYPE guards or replay dynamic
        !! allocation state for an active shadow.  Refuse that exact semantic
        !! case after activity analysis; do not classify it from source text.
        type(fad_proc_t), intent(in) :: primal
        logical, intent(in) :: active(:)
        type(forward_status_t), intent(inout) :: status
        integer :: i
        character(len=:), allocatable :: type_label

        do i = 1, primal%n_decls
            if (primal%decls(i)%is_select_alias) cycle
            if (.not. active(i)) cycle
            if (.not. primal%decls(i)%is_allocatable) cycle
            if (.not. primal%decls(i)%is_polymorphic) cycle
            if (has_fixed_source_owner(primal, i, active)) cycle
            type_label = "class(T)"
            if (primal%decls(i)%is_unlimited_polymorphic) type_label = "class(*)"
            status%ok = .false.
            status%message = "forward mode: active polymorphic allocatable "// &
                "ownership '"//trim(primal%decls(i)%name)//"' ("// &
                trim(type_label)//") at line "//itoa(primal%decls(i)%line)// &
                ": current IR cannot synchronize a tangent dynamic type "// &
                "with the primal ownership descriptor; use a concrete "// &
                "type(t) owner or await the dynamic ownership boundary"
            return
        end do
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ALLOCATE) cycle
            if (.not. primal%stmts(i)%allocation_target_polymorphic) cycle
            if (primal%stmts(i)%allocation_source <= 0) cycle
            if (.not. expr_reads_active(primal, primal%stmts(i)%allocation_source, active)) cycle
            if (has_fixed_source_component(primal, i, active)) cycle
            type_label = "class(T)"
            if (primal%stmts(i)%allocation_target_unlimited_polymorphic) then
                type_label = "class(*)"
            end if
            status%ok = .false.
            status%message = "forward mode: active polymorphic allocatable component ownership "// &
                "("//trim(type_label)//") at line "//itoa(primal%stmts(i)%line)// &
                ": current IR cannot synchronize a component dynamic type with the primal ownership descriptor"
            return
        end do
    end subroutine refuse_active_polymorphic_ownership

    logical function has_fixed_source_component(primal, stmt_index, active) result(supported)
        !! One component acquisition from a declared concrete SOURCE object.
        !! The component classification is a FortFront storage fact carried by
        !! the allocation statement, including array-element components.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: stmt_index
        logical, intent(in) :: active(:)
        integer :: i, holder_di, source_di
        character(len=:), allocatable :: target_text

        supported = .false.
        if (stmt_index <= 0 .or. stmt_index > primal%n_stmts) return
        if (.not. primal%stmts(stmt_index)%allocation_target_polymorphic) return
        if (.not. allocated(primal%stmts(stmt_index)%allocation_args)) return
        target_text = emit_expr(primal, primal%stmts(stmt_index)%allocation_args(1))
        holder_di = primal%decl_index_of(target_text)
        if (holder_di <= 0) return
        if (primal%decls(holder_di)%is_polymorphic) return
        if (primal%stmts(stmt_index)%allocation_source <= 0) return
        if (primal%stmts(stmt_index)%allocation_mold > 0) return
        source_di = fixed_source_decl(primal, &
            primal%stmts(stmt_index)%allocation_source)
        if (source_di <= 0) return
        if (.not. expr_reads_active(primal, &
            primal%stmts(stmt_index)%allocation_source, active)) return
        if (.not. source_activity_supported(primal, source_di)) return
        do i = 1, primal%n_stmts
            if (i == stmt_index) cycle
            if (primal%stmts(i)%kind /= FAD_ALLOCATE) cycle
            if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
            if (emit_expr(primal, primal%stmts(i)%allocation_args(1)) == &
                target_text) return
        end do
        supported = .true.
    end function has_fixed_source_component

    logical function source_activity_supported(primal, source_di) result(ok)
        !! Keep ownership support from masking an unrelated unsupported
        !! active initializer such as ``seed = child_t(...)``.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: source_di
        integer :: i

        ok = .true.
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (primal%decl_index(fad_base_name(primal%stmts(i)%target)) /= &
                source_di) cycle
            if (.not. expression_calls_supported(primal, &
                primal%stmts(i)%value)) then
                ok = .false.
                return
            end if
        end do
    end function source_activity_supported

    recursive logical function expression_calls_supported(primal, idx) result(ok)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        integer :: i

        ok = .true.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind == FAD_CALL) then
            if (.not. has_rule(primal%exprs(idx)%text)) then
                ok = .false.
                return
            end if
        end if
        do i = 1, size(primal%exprs(idx)%args)
            if (.not. expression_calls_supported(primal, &
                primal%exprs(idx)%args(i))) then
                ok = .false.
                return
            end if
        end do
    end function expression_calls_supported

    logical function has_fixed_source_owner(primal, owner_di, active) result(supported)
        !! The bounded active case is an allocatable polymorphic local whose
        !! only acquisition is ALLOCATE(owner, SOURCE=concrete_value).  The
        !! declared type of the source is a fixed dynamic type, so the
        !! existing tangent allocation descriptor can be paired by emitting
        !! SOURCE=concrete_value_d.  No factory, polymorphic source, or
        !! ownership transfer is inferred here.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: owner_di
        logical, intent(in) :: active(:)
        integer :: i, target_di, source_di
        character(len=:), allocatable :: owner
        logical :: found

        supported = .false.
        if (owner_di <= 0 .or. owner_di > primal%n_decls) return
        owner = primal%decls(owner_di)%name
        found = .false.
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind == FAD_ALLOCATE) then
                if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
                if (primal%exprs(primal%stmts(i)%allocation_args(1))%kind /= FAD_VAR) return
                target_di = primal%decl_index_of( &
                    primal%exprs(primal%stmts(i)%allocation_args(1))%text)
                if (target_di /= owner_di) cycle
                if (found) return
                if (primal%stmts(i)%allocation_source <= 0) return
                if (primal%stmts(i)%allocation_mold > 0) return
                source_di = fixed_source_decl(primal, &
                    primal%stmts(i)%allocation_source)
                if (source_di <= 0) return
                if (.not. expr_reads_active(primal, &
                    primal%stmts(i)%allocation_source, active)) return
                found = .true.
            else if (primal%stmts(i)%kind == FAD_MOVE_ALLOC) then
                if (.not. allocated(primal%stmts(i)%call_args)) cycle
                if (allocation_arg_is_owner(primal, primal%stmts(i)%call_args(1), &
                    owner) .or. allocation_arg_is_owner(primal, &
                    primal%stmts(i)%call_args(2), owner)) return
            end if
        end do
        supported = found
    end function has_fixed_source_owner

    function fixed_source_type(primal, owner_di) result(type_name)
        !! Return the concrete declared source type for the one supported
        !! acquisition, or an empty string when no such ownership event exists.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: owner_di
        character(len=:), allocatable :: type_name
        integer :: i, target_di, source_di

        type_name = ""
        do i = 1, primal%n_stmts
            if (primal%stmts(i)%kind /= FAD_ALLOCATE) cycle
            if (.not. allocated(primal%stmts(i)%allocation_args)) cycle
            if (primal%exprs(primal%stmts(i)%allocation_args(1))%kind /= FAD_VAR) then
                type_name = ""
                return
            end if
            target_di = primal%decl_index_of( &
                primal%exprs(primal%stmts(i)%allocation_args(1))%text)
            if (target_di /= owner_di) cycle
            if (primal%stmts(i)%allocation_source <= 0 .or. &
                primal%stmts(i)%allocation_mold > 0) then
                type_name = ""
                return
            end if
            source_di = fixed_source_decl(primal, &
                primal%stmts(i)%allocation_source)
            if (source_di <= 0) then
                type_name = ""
                return
            end if
            if (len_trim(type_name) > 0) then
                if (type_name /= primal%decls(source_di)%type_name) then
                    type_name = ""
                    return
                end if
            else
                type_name = primal%decls(source_di)%type_name
            end if
        end do
    end function fixed_source_type

    integer function fixed_source_decl(primal, idx) result(di)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx

        di = 0
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind /= FAD_VAR) return
        di = primal%decl_index_of(primal%exprs(idx)%text)
        if (di <= 0 .or. primal%decls(di)%is_polymorphic) di = 0
    end function fixed_source_decl

    logical function allocation_arg_is_owner(primal, idx, owner) result(found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        character(len=*), intent(in) :: owner

        found = .false.
        if (idx <= 0 .or. idx > primal%n_exprs) return
        if (primal%exprs(idx)%kind /= FAD_VAR) return
        found = fad_base_name(primal%exprs(idx)%text) == trim(owner)
    end function allocation_arg_is_owner

    function itoa(n) result(text)
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '(i0)') n
        text = trim(buffer)
    end function itoa

    subroutine mark_move_alloc_peer(primal, stmt, active, changed)
        type(fad_proc_t), intent(in) :: primal
        type(fad_stmt_t), intent(in) :: stmt
        logical, intent(inout) :: active(:)
        logical, intent(inout) :: changed
        integer :: i, di

        do i = 1, 2
            di = arg_decl_index(primal, stmt%call_args(i))
            if (di <= 0 .or. di > size(active)) cycle
            if (.not. active(di)) then
                active(di) = .true.
                changed = .true.
            end if
        end do
    end subroutine mark_move_alloc_peer

    logical function has_calls(p) result(yes)
        !! True when the procedure calls something fortad cannot see into.
        type(fad_proc_t), intent(in) :: p
        integer :: i

        yes = .false.
        do i = 1, p%n_stmts
            if (p%stmts(i)%kind == FAD_CALL_STMT) then
                yes = .true.
                return
            end if
        end do
    end function has_calls

    logical function call_reads_active(p, s, active) result(yes)
        !! True when any actual argument of a call reads an active variable.
        type(fad_proc_t), intent(in) :: p
        type(fad_stmt_t), intent(in) :: s
        logical, intent(in) :: active(:)
        integer :: i

        yes = .false.
        if (.not. allocated(s%call_args)) return
        do i = 1, size(s%call_args)
            if (expr_reads_active(p, s%call_args(i), active)) then
                yes = .true.
                return
            end if
        end do
    end function call_reads_active

    integer function arg_decl_index(p, idx) result(di)
        !! Declaration index of an actual argument that is a plain variable.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx

        di = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            di = p%decl_index_of(p%exprs(idx)%text)
        end select
    end function arg_decl_index

    logical function is_real_decl(p, di) result(yes)
        !! Only real arguments carry derivatives; an integer dimension does not.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: di

        yes = .false.
        if (di <= 0 .or. di > p%n_decls) return
        if (.not. allocated(p%decls(di)%type_name)) return
        yes = index(p%decls(di)%type_name, "real") == 1 .or. &
            index(p%decls(di)%type_name, "REAL") == 1
    end function is_real_decl

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

    recursive logical function expr_reads_active(p, idx, active) result(yes)
        !! True when the expression reads any active variable.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        integer :: i, di

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        associate (e => p%exprs(idx))
            select case (e%kind)
            case (FAD_VAR, FAD_INDEX)
                di = p%decl_index_of(e%text)
                if (di > 0) yes = decl_active(p, di, active)
                if (yes) return
            end select
            do i = 1, size(e%args)
                if (expr_reads_active(p, e%args(i), active)) then
                    yes = .true.
                    return
                end if
            end do
        end associate
    end function expr_reads_active

    subroutine build_signature(primal, tangent, active, suffix, vector, ndir, &
            with_primal)
        !! Dummy arguments: every primal argument, each active one followed by
        !! its tangent, then the result and its tangent for a function. In
        !! vector mode the direction count leads the list.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        character(len=*), intent(in) :: ndir
        logical, intent(in) :: with_primal
        character(len=64), allocatable :: names(:)
        character(len=256) :: decl_name, decl_type, decl_dims
        integer :: i, n, di, ignored
        logical :: decl_value, decl_array, decl_contiguous

        allocate (names(2*(size(primal%params) + 3)))
        n = 0
        if (vector) then
            n = n + 1
            names(n) = ndir
            ignored = tangent%add_decl_fields(ndir, "integer", FAD_INTENT_IN, &
                .false., .false., .false., .false., "")
        end if
        do i = 1, size(primal%params)
            di = primal%decl_index(trim(primal%params(i)))
            ! An active `intent(out)` dummy is a primal value the caller asked
            ! not to be given back. Inactive ones stay: a status flag is not a
            ! derivative output and dropping it would change the contract.
            if (di > 0) then
                decl_name = trim(primal%decls(di)%name)
                decl_type = ""
                if (allocated(primal%decls(di)%type_name)) then
                    decl_type = primal%decls(di)%type_name
                end if
                decl_dims = ""
                if (allocated(primal%decls(di)%dims)) then
                    decl_dims = primal%decls(di)%dims
                end if
                decl_value = primal%decls(di)%is_value
                decl_array = primal%decls(di)%is_array
                decl_contiguous = primal%decls(di)%is_contiguous
            end if
            if (di > 0 .and. .not. with_primal) then
                if (active(di) .and. primal%decls(di)%intent == FAD_INTENT_OUT) then
                    n = n + 1
                    names(n) = trim(primal%params(i))//suffix
                    call add_tangent_decl(tangent, decl_name, decl_type, &
                        decl_value, decl_array, decl_contiguous, &
                        decl_dims, suffix, &
                        FAD_INTENT_OUT, vector, ndir, &
                        is_optional=.false., &
                        is_allocatable=primal%decls(di)%is_allocatable)
                    ! Dropped from the signature but still written by the primal
                    ! statements, so it stays as a local. Whether those writes
                    ! survive is dead-store elimination's decision, not this
                    ! routine's - and an undeclared name would not compile.
                    ignored = tangent%add_decl_fields(decl_name, decl_type, &
                        FAD_INTENT_NONE, decl_value, decl_array, decl_contiguous, &
                        .false., decl_dims, .false., primal%decls(di)%is_allocatable)
                    cycle
                end if
            end if
            n = n + 1
            names(n) = trim(primal%params(i))
            if (di == 0) cycle
            ignored = tangent%add_decl_fields(decl_name, decl_type, &
                primal%decls(di)%intent, decl_value, decl_array, decl_contiguous, &
                primal%decls(di)%is_result, decl_dims, &
                primal%decls(di)%is_optional, primal%decls(di)%is_allocatable)
            if (.not. active(di)) cycle
            n = n + 1
            names(n) = trim(primal%params(i))//suffix
            call add_tangent_decl(tangent, decl_name, decl_type, decl_value, &
                decl_array, decl_contiguous, decl_dims, suffix, &
                tangent_intent(primal%decls(di)%intent), vector, ndir, &
            ! The tangent is read only on the source PRESENT path, so it
            ! has the same optional interface as its primal dummy.
            is_optional=primal%decls(di)%is_optional, &
                is_allocatable=primal%decls(di)%is_allocatable)
        end do

        if (primal%is_function) then
            di = primal%decl_index(primal%result_name)
            if (with_primal) then
                n = n + 1
                names(n) = primal%result_name
            end if
            if (di > 0) then
                decl_name = trim(primal%decls(di)%name)
                decl_type = ""
                if (allocated(primal%decls(di)%type_name)) then
                    decl_type = primal%decls(di)%type_name
                end if
                decl_dims = ""
                if (allocated(primal%decls(di)%dims)) then
                    decl_dims = primal%decls(di)%dims
                end if
                decl_value = primal%decls(di)%is_value
                decl_array = primal%decls(di)%is_array
                decl_contiguous = primal%decls(di)%is_contiguous
                if (with_primal) ignored = tangent%add_decl_fields( &
                    decl_name, decl_type, FAD_INTENT_OUT, decl_value, decl_array, &
                    decl_contiguous, .true., decl_dims, .false., &
                    primal%decls(di)%is_allocatable)
                n = n + 1
                names(n) = primal%result_name//suffix
                call add_tangent_decl(tangent, decl_name, decl_type, decl_value, &
                    decl_array, decl_contiguous, decl_dims, suffix, &
                    FAD_INTENT_OUT, vector, ndir, &
                    is_allocatable=primal%decls(di)%is_allocatable)
            end if
        end if

        allocate (character(len=64) :: tangent%params(n))
        do i = 1, n
            tangent%params(i) = names(i)
        end do
    end subroutine build_signature

    integer function tangent_intent(primal_intent) result(out)
        !! A tangent argument carries the intent of its primal, except that an
        !! `intent(in)` primal still needs its tangent read in.
        integer, intent(in) :: primal_intent

        select case (primal_intent)
        case (FAD_INTENT_IN)
            out = FAD_INTENT_IN
        case (FAD_INTENT_OUT)
            out = FAD_INTENT_OUT
        case default
            out = FAD_INTENT_INOUT
        end select
    end function tangent_intent

    subroutine add_tangent_decl(tangent, name, type_name, is_value, is_array, &
            is_contiguous, dims, suffix, intent_code, &
            vector, ndir, is_optional, is_allocatable)
        !! Declare the tangent counterpart of a primal entity.
        !!
        !! In vector mode the direction axis goes **first**, because Fortran
        !! stores the leftmost index contiguously and the direction axis is the
        !! one every tangent expression sweeps. `a(n)` becomes `a_d(n_dir, n)`,
        !! and `a_d(:, i)` is then a contiguous vector the compiler can load
        !! and fuse as a unit.
        type(fad_proc_t), intent(inout) :: tangent
        character(len=*), intent(in) :: name, type_name, dims
        logical, intent(in) :: is_value, is_array, is_contiguous
        character(len=*), intent(in) :: suffix
        integer, intent(in) :: intent_code
        logical, intent(in), optional :: vector
        character(len=*), intent(in), optional :: ndir
        logical, intent(in), optional :: is_optional
        logical, intent(in), optional :: is_allocatable
        integer :: ignored
        logical :: vec, optional_arg, allocatable_arg

        associate (unused => is_value)
        end associate
        vec = .false.
        if (present(vector)) vec = vector
        optional_arg = .false.
        if (present(is_optional)) optional_arg = is_optional
        allocatable_arg = .false.
        if (present(is_allocatable)) allocatable_arg = is_allocatable

        ! VALUE belongs to the primal dummy, not to its tangent. A tangent is
        ! written by the generated routine, so VALUE would conflict with the
        ! required INTENT(INOUT) contract.
        if (vec) then
            if (is_array .and. len_trim(dims) > 0) then
                ignored = tangent%add_decl_fields(trim(name)//suffix, type_name, &
                    intent_code, .false., .true., .false., .false., &
                    ndir//", "//trim(dims), optional_arg, allocatable_arg)
            else
                ignored = tangent%add_decl_fields(trim(name)//suffix, type_name, &
                    intent_code, .false., .true., .false., .false., ndir, &
                    optional_arg, allocatable_arg)
            end if
            ! Contiguity of the primal says nothing about the tangent block,
            ! and a wrong `contiguous` is a promise the caller may not keep.
        else
            ignored = tangent%add_decl_fields(trim(name)//suffix, type_name, &
                intent_code, .false., is_array, is_contiguous, .false., dims, &
                optional_arg, allocatable_arg)
        end if
    end subroutine add_tangent_decl

    subroutine build_body(primal, tangent, active, active_paths, suffix, &
            vector, status)
        !! Walk the primal statements, emitting tangent then primal.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: active_paths(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        integer :: i, dexpr, ignored, di, paired_selector
        logical :: paired_select, inner_select_open
        character(len=:), allocatable :: paired_alias

        paired_select = .false.
        inner_select_open = .false.
        paired_selector = 0
        paired_alias = ""

        do i = 1, primal%n_stmts
            associate (ps => primal%stmts(i))
                select case (ps%kind)
                case (FAD_ASSIGN)
                    di = primal%decl_index(target_base(ps%target))
                    if (di > 0) then
                        if (target_path_active(primal, ps%target, di, active, &
                            active_paths)) then
                            dexpr = tangent_of(primal, tangent, ps%value, active, &
                                suffix, vector, status, active_paths)
                            if (.not. status%ok) return
                            s%kind = FAD_ASSIGN
                            s%target = tangent_name(ps%target, suffix, vector)
                            if (dexpr == 0) then
                                s%value = tangent%add_expr( &
                                    expr_const("0.0"//tangent%real_suffix))
                            else
                                s%value = dexpr
                            end if
                            ignored = tangent%add_stmt(s)
                        end if
                    end if
                    s%kind = FAD_ASSIGN
                    s%target = ps%target
                    s%value = copy_expr(primal, tangent, ps%value)
                    ignored = tangent%add_stmt(s)

                case (FAD_DO)
                    ! Legacy Tapenade-style sources often rely on implicit
                    ! typing for the loop index (`i`, `j`, ...).  The primal
                    ! may therefore have no declaration to copy, but the
                    ! generated procedure has implicit none and must still
                    ! declare the index as an integer.
                    if (tangent%decl_index(ps%target) == 0) then
                        ignored = tangent%add_decl_fields(ps%target, "integer", &
                            FAD_INTENT_NONE, .false., .false., .false., &
                            .false., "")
                    end if
                    s%kind = FAD_DO
                    s%target = ps%target
                    s%lo = copy_expr(primal, tangent, ps%lo)
                    s%hi = copy_expr(primal, tangent, ps%hi)
                    s%step = 0
                    if (ps%step /= 0) s%step = copy_expr(primal, tangent, ps%step)
                    ignored = tangent%add_stmt(s)

                case (FAD_END_DO, FAD_END_IF, FAD_ELSE)
                    s%kind = ps%kind
                    s%value = 0
                    if (allocated(ps%target)) s%target = ps%target
                    ignored = tangent%add_stmt(s)

                case (FAD_TYPE_IS, FAD_CLASS_IS)
                    if (inner_select_open) then
                        s%kind = FAD_END_SELECT
                        s%value = 0
                        ignored = tangent%add_stmt(s)
                        inner_select_open = .false.
                    end if
                    s%kind = ps%kind
                    s%value = 0
                    if (allocated(ps%target)) s%target = ps%target
                    ignored = tangent%add_stmt(s)
                    if (paired_select) then
                        s%kind = FAD_SELECT_TYPE
                        s%value = paired_selector
                        s%target = tangent_name(paired_alias, suffix, vector)
                        ignored = tangent%add_stmt(s)
                        s%kind = ps%kind
                        s%value = 0
                        if (allocated(ps%target)) s%target = ps%target
                        ignored = tangent%add_stmt(s)
                        inner_select_open = .true.
                    end if

                case (FAD_CLASS_DEFAULT)
                    if (inner_select_open) then
                        s%kind = FAD_END_SELECT
                        s%value = 0
                        ignored = tangent%add_stmt(s)
                        inner_select_open = .false.
                    end if
                    s%kind = FAD_CLASS_DEFAULT
                    s%value = 0
                    if (allocated(ps%target)) s%target = ps%target
                    ignored = tangent%add_stmt(s)

                case (FAD_IF)
                    s%kind = FAD_IF
                    s%value = copy_expr(primal, tangent, ps%value)
                    ignored = tangent%add_stmt(s)

                case (FAD_SELECT_TYPE)
                    s%kind = FAD_SELECT_TYPE
                    s%value = copy_expr(primal, tangent, ps%value)
                    if (allocated(ps%target)) s%target = ps%target
                    paired_select = .false.
                    inner_select_open = .false.
                    paired_selector = paired_tangent_selector(primal, tangent, &
                        ps%value, active, suffix, vector)
                    if (paired_selector > 0 .and. allocated(ps%target)) then
                        paired_select = .true.
                        paired_alias = ps%target
                    end if
                    di = arg_decl_index(primal, ps%value)
                    if (.not. paired_select .and. di > 0 .and. &
                        decl_active(primal, di, active) .and. &
                        len_trim(fixed_source_type(primal, di)) == 0) then
                        s%value = tangent_of(primal, tangent, ps%value, active, &
                            suffix, vector, status)
                        if (.not. status%ok) return
                    end if
                    ignored = tangent%add_stmt(s)

                case (FAD_CALL_STMT)
                    call emit_call_tangent(primal, tangent, ps, active, suffix, &
                        vector, status)
                    if (.not. status%ok) return

                case (FAD_ALLOCATE)
                    call emit_allocate_tangent(primal, tangent, ps, active, &
                        suffix, vector, status)
                    if (.not. status%ok) return

                case (FAD_DEALLOCATE)
                    call emit_deallocate_tangent(primal, tangent, ps, active, &
                        suffix, vector, status)
                    if (.not. status%ok) return

                case (FAD_MOVE_ALLOC)
                    call emit_move_alloc_tangent(primal, tangent, ps, active, &
                        suffix, vector, status)
                    if (.not. status%ok) return

                case (FAD_END_SELECT)
                    if (inner_select_open) then
                        s%kind = FAD_END_SELECT
                        s%value = 0
                        ignored = tangent%add_stmt(s)
                        inner_select_open = .false.
                    end if
                    s%kind = FAD_END_SELECT
                    s%value = 0
                    ignored = tangent%add_stmt(s)
                    paired_select = .false.
                    paired_selector = 0
                    paired_alias = ""

                case default
                    status%ok = .false.
                    status%message = "forward mode: unsupported statement kind"
                    return
                end select
            end associate
        end do
    end subroutine build_body

    integer function paired_tangent_selector(primal, tangent, idx, active, suffix, vector) &
            result(out)
        !! Copy a component selector onto the active holder shadow.  A paired
        !! SELECT TYPE then gives the primal and tangent aliases the same
        !! concrete child type without pretending that a polymorphic descriptor
        !! itself has an arithmetic tangent.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        character(len=:), allocatable :: text, base, shadow
        type(fad_expr_t) :: e
        integer :: i, cut, open

        out = 0
        text = emit_expr(primal, idx)
        cut = len_trim(text) + 1
        open = index(text, "(")
        if (open > 0) cut = min(cut, open)
        open = index(text, "%")
        if (open > 0) cut = min(cut, open)
        if (cut > 1) then
            base = trim(text(:cut - 1))
        else
            base = trim(text)
        end if
        shadow = tangent_name(base, suffix, vector)
        if (len_trim(base) == 0) return
        if (primal%decl_index(base) <= 0) return
        if (primal%decls(primal%decl_index(base))%is_allocatable) return
        if (primal%decls(primal%decl_index(base))%is_associate_alias) return
        if (primal%decls(primal%decl_index(base))%is_select_alias) return
        if (.not. decl_active(primal, primal%decl_index(base), active)) return
        if (len_trim(text) <= len_trim(base)) then
            if (.not. primal%decls(primal%decl_index(base))%is_polymorphic) return
            e%kind = primal%exprs(idx)%kind
            e%text = shadow
            e%rank = primal%exprs(idx)%rank
            out = tangent%add_expr(e)
            return
        end if
        if (idx <= 0 .or. idx > primal%n_exprs) return
        e%kind = primal%exprs(idx)%kind
        if (e%kind == FAD_INDEX) then
            e%text = shadow
        else
            e%text = shadow//text(len_trim(base) + 1:)
        end if
        e%rank = primal%exprs(idx)%rank
        if (allocated(primal%exprs(idx)%args)) then
            allocate(e%args(size(primal%exprs(idx)%args)))
            do i = 1, size(e%args)
                e%args(i) = copy_expr(primal, tangent, primal%exprs(idx)%args(i))
            end do
        end if
        out = tangent%add_expr(e)
    end function paired_tangent_selector

    subroutine emit_allocate_tangent(primal, tangent, ps, active, suffix, &
            vector, status)
        !! Mirror an explicit allocation for an active owner.  The derivative
        !! descriptor follows the primal descriptor; SOURCE= is differentiated
        !! when its source is active, while MOLD= and shape expressions are
        !! passive allocation metadata.  An uninitialised tangent is cleared
        !! immediately so later uses cannot observe undefined storage.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        type(fad_stmt_t), intent(in) :: ps
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        integer :: i, di, ignored, target_expr, dexpr
        character(len=:), allocatable :: target_text
        logical :: target_active, source_active

        target_text = emit_expr(primal, ps%allocation_args(1))
        di = primal%decl_index_of(target_text)
        target_active = .false.
        if (di > 0) target_active = decl_active(primal, di, active)

        s%kind = FAD_ALLOCATE
        allocate (s%allocation_args(size(ps%allocation_args)))
        s%allocation_args(1) = tangent%add_expr(expr_var( &
            tangent_name(target_text, suffix, vector)))
        do i = 2, size(ps%allocation_args)
            s%allocation_args(i) = copy_expr(primal, tangent, &
                ps%allocation_args(i))
        end do
        if (ps%allocation_source > 0) then
            source_active = expr_reads_active(primal, ps%allocation_source, active)
            if (source_active) then
                dexpr = tangent_of(primal, tangent, ps%allocation_source, active, &
                    suffix, vector, status)
                if (.not. status%ok) return
                s%allocation_source = dexpr
            else
                target_expr = tangent%add_expr(expr_var(target_text))
                s%allocation_mold = target_expr
            end if
        else if (ps%allocation_mold > 0) then
            s%allocation_mold = copy_expr(primal, tangent, ps%allocation_mold)
        end if
        if (target_active) then
            ignored = tangent%add_stmt(s)
            if (s%allocation_source == 0) then
                s%kind = FAD_ASSIGN
                s%target = tangent_name(target_text, suffix, vector)
                s%value = tangent%add_expr(expr_const( &
                    "0.0"//tangent%real_suffix))
                ignored = tangent%add_stmt(s)
            end if
        end if

        call reset_statement(s)
        call copy_allocation_stmt(primal, tangent, ps, s)
        ignored = tangent%add_stmt(s)
    end subroutine emit_allocate_tangent

    subroutine emit_deallocate_tangent(primal, tangent, ps, active, suffix, &
            vector, status)
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        type(fad_stmt_t), intent(in) :: ps
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: target
        integer :: di, ignored

        status%ok = .true.
        target = emit_expr(primal, ps%allocation_args(1))
        di = primal%decl_index_of(target)
        if (di > 0) then
            if (decl_active(primal, di, active)) then
                call reset_statement(s)
                s%kind = FAD_DEALLOCATE
                allocate (s%allocation_args(1))
                s%allocation_args(1) = tangent%add_expr(expr_var( &
                    tangent_name(target, suffix, vector)))
                ignored = tangent%add_stmt(s)
            end if
        end if
        call reset_statement(s)
        s%kind = FAD_DEALLOCATE
        allocate (s%allocation_args(1))
        s%allocation_args(1) = copy_expr(primal, tangent, ps%allocation_args(1))
        ignored = tangent%add_stmt(s)
    end subroutine emit_deallocate_tangent

    subroutine emit_move_alloc_tangent(primal, tangent, ps, active, suffix, &
            vector, status)
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        type(fad_stmt_t), intent(in) :: ps
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        character(len=:), allocatable :: source, target
        integer :: source_di, target_di, ignored
        logical :: source_active, target_active

        source = emit_expr(primal, ps%call_args(1))
        target = emit_expr(primal, ps%call_args(2))
        source_di = primal%decl_index_of(source)
        target_di = primal%decl_index_of(target)
        source_active = .false.
        if (source_di > 0) source_active = active(source_di)
        target_active = .false.
        if (target_di > 0) target_active = active(target_di)
        if (source_active .neqv. target_active) then
            status%ok = .false.
            status%message = "forward mode: move_alloc requires source and "// &
                "destination to share an active derivative shadow"
            return
        end if
        if (source_active) then
            call reset_statement(s)
            s%kind = FAD_MOVE_ALLOC
            allocate (s%call_args(2))
            s%call_args(1) = tangent%add_expr(expr_var( &
                tangent_name(source, suffix, vector)))
            s%call_args(2) = tangent%add_expr(expr_var( &
                tangent_name(target, suffix, vector)))
            ignored = tangent%add_stmt(s)
        end if
        call reset_statement(s)
        s%kind = FAD_MOVE_ALLOC
        allocate (s%call_args(2))
        s%call_args(1) = copy_expr(primal, tangent, ps%call_args(1))
        s%call_args(2) = copy_expr(primal, tangent, ps%call_args(2))
        ignored = tangent%add_stmt(s)
    end subroutine emit_move_alloc_tangent

    subroutine copy_allocation_stmt(primal, tangent, ps, out)
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        type(fad_stmt_t), intent(in) :: ps
        type(fad_stmt_t), intent(out) :: out
        integer :: i

        call reset_statement(out)
        out%kind = FAD_ALLOCATE
        allocate (out%allocation_args(size(ps%allocation_args)))
        do i = 1, size(ps%allocation_args)
            out%allocation_args(i) = copy_expr(primal, tangent, &
                ps%allocation_args(i))
        end do
        if (ps%allocation_source > 0) out%allocation_source = &
            copy_expr(primal, tangent, ps%allocation_source)
        if (ps%allocation_mold > 0) out%allocation_mold = &
            copy_expr(primal, tangent, ps%allocation_mold)
    end subroutine copy_allocation_stmt

    subroutine reset_statement(s)
        !! A statement is reused while emitting primal and shadow events.  Its
        !! allocatable payloads must be cleared before another payload is
        !! allocated; intrinsic assignment of a statement retains those
        !! descriptors.
        type(fad_stmt_t), intent(inout) :: s

        if (allocated(s%call_args)) deallocate (s%call_args)
        if (allocated(s%allocation_args)) deallocate (s%allocation_args)
        s%allocation_source = 0
        s%allocation_mold = 0
        s%allocation_target_polymorphic = .false.
        s%allocation_target_unlimited_polymorphic = .false.
        if (allocated(s%target)) deallocate (s%target)
        s%value = 0
    end subroutine reset_statement

    subroutine emit_call_tangent(primal, tangent, ps, active, suffix, vector, &
            status)
        !! Apply a registered statement rule to a subroutine call.
        !!
        !! The call is opaque to fortad: it emits the registered tangent
        !! statements and then the call itself. Without a rule it refuses,
        !! because assuming a call is inactive would silently drop whatever
        !! derivative flows through it.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        type(fad_stmt_t), intent(in) :: ps
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        character(len=512), allocatable :: args(:), tangents(:), adjoints(:)
        type(fad_stmt_t) :: s
        integer :: i, di, ignored, n_args

        associate (unused => vector)
        end associate
        if (.not. call_rule_has(ps%target)) then
            status%ok = .false.
            status%message = "no derivative rule for the call to '"//ps%target// &
                "'; register one with fad_add_call_rule, or keep it out of "// &
                "the active path"
            return
        end if

        n_args = size(ps%call_args)
        allocate (args(n_args), tangents(n_args), adjoints(n_args))
        do i = 1, n_args
            args(i) = emit_expr(primal, ps%call_args(i))
            tangents(i) = trim(args(i))//suffix
            adjoints(i) = trim(args(i))//"_b"
            di = primal%decl_index(trim(args(i)))
            if (di > 0) then
                if (.not. active(di)) tangents(i) = "<inactive>"
            end if
        end do

        ! The primal call goes first, unlike an assignment. A rule generally
        ! needs the call's *outputs*: the tangent of a linear solve is
        ! `A x_d = b_d - A_d x`, which reads the solution `x`. A rule needing a
        ! pre-call value must save it itself, and this is documented rather
        ! than inferred, because fortad cannot see which arguments a call
        ! writes.
        s%kind = FAD_CALL_STMT
        s%target = ps%target
        block
            integer, allocatable :: cargs(:)
            allocate (cargs(n_args))
            do i = 1, n_args
                cargs(i) = copy_expr(primal, tangent, ps%call_args(i))
            end do
            s%call_args = cargs
        end block
        ignored = tangent%add_stmt(s)

        do i = 1, call_rule_lines(ps%target, "tangent")
            s%kind = FAD_ASSIGN
            s%target = "!fad_raw"
            s%value = tangent%add_expr(expr_const( &
                call_rule_substitute(ps%target, "tangent", i, args, tangents, &
                adjoints)))
            ignored = tangent%add_stmt(s)
        end do
    end subroutine emit_call_tangent

    recursive integer function tangent_of(primal, tangent, idx, active, suffix, &
            vector, status, active_paths) result(out)
        !! Tangent of a primal expression, as an expression in `tangent`.
        !!
        !! In vector mode a tangent leaf carries the whole direction block:
        !! `x_d` becomes `x_d(:)` and `a(i)` becomes `a_d(:, i)`. Every rule
        !! above then combines array tangents with scalar primal factors, which
        !! is exactly the shape Fortran vectorises.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        character(len=*), intent(in), optional :: active_paths(:)
        integer, allocatable :: args(:), dargs(:)
        integer :: i, di, a, b, da, db
        type(fad_expr_t) :: e

        out = 0
        if (idx <= 0 .or. idx > primal%n_exprs) return

        associate (pe => primal%exprs(idx))
            select case (pe%kind)
            case (FAD_CONST)
                out = 0

            case (FAD_VAR)
                di = primal%decl_index_of(pe%text)
                if (di > 0) then
                    if (component_expr_is_active(primal, idx, active, &
                        active_paths)) then
                        if (vector) then
                            allocate (args(1))
                            args(1) = tangent%add_expr(expr_const(":"))
                            e%kind = FAD_INDEX
                            e%text = fad_suffix_name(pe%text, suffix)
                            e%args = args
                            out = tangent%add_expr(e)
                        else
                            out = tangent%add_expr(expr_var( &
                                fad_suffix_name(pe%text, suffix)))
                        end if
                    end if
                end if

            case (FAD_INDEX)
                di = primal%decl_index_of(pe%text)
                if (di > 0) then
                    if (component_expr_is_active(primal, idx, active, &
                        active_paths)) then
                        if (vector) then
                            allocate (args(size(pe%args) + 1))
                            args(1) = tangent%add_expr(expr_const(":"))
                            do i = 1, size(pe%args)
                                args(i + 1) = copy_expr(primal, tangent, pe%args(i))
                            end do
                        else
                            allocate (args(size(pe%args)))
                            do i = 1, size(pe%args)
                                args(i) = copy_expr(primal, tangent, pe%args(i))
                            end do
                        end if
                        e%kind = FAD_INDEX
                        e%text = fad_suffix_name(pe%text, suffix)
                        e%args = args
                        out = tangent%add_expr(e)
                    end if
                end if

            case (FAD_BINOP)
                a = copy_expr(primal, tangent, pe%args(1))
                b = copy_expr(primal, tangent, pe%args(2))
                da = tangent_of(primal, tangent, pe%args(1), active, suffix, vector, &
                    status, active_paths)
                if (.not. status%ok) return
                db = tangent_of(primal, tangent, pe%args(2), active, suffix, vector, &
                    status, active_paths)
                if (.not. status%ok) return
                out = jvp_binop(tangent, pe%text, a, b, da, db)

            case (FAD_UNOP)
                a = copy_expr(primal, tangent, pe%args(1))
                da = tangent_of(primal, tangent, pe%args(1), active, suffix, vector, &
                    status, active_paths)
                if (.not. status%ok) return
                out = jvp_unop(tangent, pe%text, a, da)

            case (FAD_CALL)
                allocate (args(size(pe%args)), dargs(size(pe%args)))
                do i = 1, size(pe%args)
                    args(i) = copy_expr(primal, tangent, pe%args(i))
                    dargs(i) = tangent_of(primal, tangent, pe%args(i), active, &
                        suffix, vector, status, active_paths)
                    if (.not. status%ok) return
                end do
                if (all(dargs == 0)) then
                    out = 0
                else if (has_rule(pe%text)) then
                    out = jvp_call(tangent, pe%text, args, dargs)
                else
                    status%ok = .false.
                    status%message = "no derivative rule for '"//pe%text// &
                        "'; register one with fad_add_rule, or keep it out of "// &
                        "the active path"
                    return
                end if
            end select
        end associate
    end function tangent_of

    recursive integer function copy_expr(src, dst, idx) result(out)
        !! Copy a primal expression into the tangent procedure's arena.
        type(fad_proc_t), intent(in) :: src
        type(fad_proc_t), intent(inout) :: dst
        integer, intent(in) :: idx
        type(fad_expr_t) :: e
        integer, allocatable :: args(:)
        integer :: i

        out = 0
        if (idx <= 0 .or. idx > src%n_exprs) return
        e%kind = src%exprs(idx)%kind
        e%text = src%exprs(idx)%text
        if (allocated(src%exprs(idx)%args)) then
            allocate (args(size(src%exprs(idx)%args)))
        else
            allocate (args(0))
        end if
        do i = 1, size(args)
            args(i) = copy_expr(src, dst, src%exprs(idx)%args(i))
        end do
        e%args = args
        out = dst%add_expr(e)
    end function copy_expr

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

    function target_base(target) result(base)
        !! The variable name of an assignment target, without any subscript.
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: base
        base = fad_base_name(target)
    end function target_base

    function tangent_name(target, suffix, vector) result(name)
        !! Tangent counterpart of an assignment target, keeping any subscript
        !! and, in vector mode, prefixing the direction axis.
        character(len=*), intent(in) :: target, suffix
        logical, intent(in) :: vector
        character(len=:), allocatable :: name
        name = fad_suffix_name(target, suffix, vector)
    end function tangent_name

    logical function decl_active(p, di, active) result(yes)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: di
        logical, intent(in) :: active(:)
        integer :: base_di

        yes = .false.
        if (di <= 0 .or. di > p%n_decls) return
        if (di <= size(active)) yes = active(di)
        if (yes .or. .not. p%decls(di)%is_select_alias) return
        if (.not. allocated(p%decls(di)%alias_target)) return
        base_di = p%decl_index_of(p%decls(di)%alias_target)
        if (base_di > 0 .and. base_di <= size(active)) yes = active(base_di)
    end function decl_active

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
        character(len=*), intent(in), optional :: paths(:)
        character(len=:), allocatable :: mapped_text
        integer :: i, di
        logical :: component

        component = .false.
        yes = .false.
        mapped_text = resolve_component_alias(primal, text)
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (.not. same_component_name(primal%exprs(i)%text, text)) cycle
            component = .true.
            if (.not. present(paths)) then
                yes = .true.
            else
                do di = 1, size(paths)
                    if (same_component_name(paths(di), mapped_text)) then
                        yes = .true.
                        exit
                    end if
                end do
            end if
            exit
        end do
        if (component) return
        di = primal%decl_index_of(text)
        if (di > 0) yes = decl_active(primal, di, active)
    end function component_path_is_active

    function resolve_component_alias(primal, text) result(mapped)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: mapped, base, tail
        integer :: di, cut

        mapped = trim(text)
        base = fad_base_name(text)
        di = primal%decl_index(base)
        if (di <= 0) return
        if (.not. primal%decls(di)%is_select_alias) return
        if (.not. allocated(primal%decls(di)%alias_target)) return
        if (len_trim(text) <= len_trim(base)) return
        cut = len_trim(base) + 1
        tail = text(cut:)
        mapped = map_section_alias_path(primal%decls(di)%alias_target, tail)
    end function resolve_component_alias

    function map_section_alias_path(alias_target, tail) result(mapped)
        !! Map a literal element of a rank-one SELECT TYPE section back to the
        !! original array.  For example, ``item(1)%scale`` selected from
        !! ``model(2:3)`` denotes ``model(2)%scale``.  The IR has no descriptor
        !! arithmetic, so open bounds, strides, vector subscripts, and nested
        !! sections stay outside this bounded path.
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

    logical function target_path_active(primal, text, di, active, paths) &
            result(yes)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: text
        integer, intent(in) :: di
        logical, intent(in) :: active(:)
        character(len=*), intent(in), optional :: paths(:)

        yes = component_path_is_active(primal, text, active, paths)
        if (index(trim(text), "%") == 0) then
            yes = decl_active(primal, di, active)
        end if
    end function target_path_active

    logical function component_expr_is_active(primal, idx, active, paths) &
            result(yes)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        character(len=*), intent(in), optional :: paths(:)
        character(len=:), allocatable :: text

        text = emit_expr(primal, idx)
        yes = component_path_is_active(primal, text, active, paths)
    end function component_expr_is_active

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

    character function lower_name_char(c)
        character, intent(in) :: c

        lower_name_char = c
        if (c >= "A" .and. c <= "Z") then
            lower_name_char = achar(iachar(c) + iachar("a") - iachar("A"))
        end if
    end function lower_name_char

end module fortad_forward
