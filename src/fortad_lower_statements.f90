module fortad_lower_statements
    !! Lower statement and expression trees from fortfront into fortad IR.
    use fortfront, only: ast_arena_t, module_node, assignment_node, &
        binary_op_node, identifier_node, literal_node, call_or_subscript_node, &
        declaration_node, do_loop_node, if_node, parameter_declaration_node, &
        subroutine_call_node, use_statement_node, comment_node, &
        allocate_statement_node, deallocate_statement_node, &
        pointer_assignment_node, &
        return_node, &
        get_select_type_info, get_type_guard_info, component_access_query_t, &
        query_component_access, query_derived_type, query_type_binding, &
        derived_type_query_t, type_binding_query_t, declaration_query_t, &
        query_declaration, query_program_unit, program_unit_query_t, &
        binding_hierarchy_query_t, query_type_binding_hierarchy, &
        generic_call_query_t, query_generic_call, resolved_type_query_t, &
        query_resolved_type, &
        procedure_call_target_query_t, query_procedure_call_target, &
        procedure_target_query_t, query_procedure_target, &
        type_bound_call_query_t, query_type_bound_call, &
        get_source_line
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
        expr_const, expr_var, expr_binop, expr_call, fad_base_name, copy_decl, &
        FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, &
        FAD_END_IF, FAD_VAR, FAD_INDEX, FAD_CALL_STMT, FAD_INTENT_NONE, &
        FAD_INTENT_IN, FAD_INTENT_OUT, FAD_INTENT_INOUT, &
        FAD_SELECT_TYPE, FAD_TYPE_IS, FAD_CLASS_IS, FAD_CLASS_DEFAULT, &
        FAD_END_SELECT, FAD_ALLOCATE, FAD_DEALLOCATE, FAD_MOVE_ALLOC
    use fortad_lower_types, only: lower_status_t
    use fortad_use_store, only: ensure_use_capacity
    use fortad_emit, only: emit_expr
    use frontend_compiler_queries, only: storage_query_t, query_storage, &
        component_path_query_t, query_component_path, &
        array_slice_query_t, array_bounds_query_t, range_expression_query_t, &
        query_array_slice, query_array_bounds, query_range_expression, &
        nullify_query_t, query_nullify, STORAGE_POINTER, STORAGE_MODULE, &
        STORAGE_SAVE, STORAGE_COMMON
    use frontend_compiler_resolution, only: BINDING_DECLARATION, &
        BINDING_FUNCTION, BINDING_SUBROUTINE, BINDING_ASSOCIATE_NAME, &
        declaration_binding_t, resolve_identifier_binding
    implicit none
    private

    public :: lower_body
    public :: inherit_module_uses

contains

    recursive subroutine lower_body(arena, body_indices, proc, status, &
            allow_terminal_return)
        !! Lower a statement list.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        logical, intent(in), optional :: allow_terminal_return
        logical :: terminal_return_allowed
        integer :: i

        status%ok = .true.
        terminal_return_allowed = .true.
        if (present(allow_terminal_return)) then
            terminal_return_allowed = allow_terminal_return
        end if
        do i = 1, size(body_indices)
            if (body_indices(i) > 0) then
                if (body_indices(i) <= arena%size) then
                    if (arena%has_node_at(body_indices(i))) then
                        select type (return_stmt => arena%entries(body_indices(i))%node)
                            type is (return_node)
                            if (return_stmt%has_selector) then
                                status%ok = .false.
                                status%message = "unsupported alternate RETURN at line "// &
                                    itoa(return_stmt%line)
                                return
                            end if
                            if (terminal_return_allowed) then
                                if (return_is_terminal(arena, body_indices, i)) cycle
                            end if
                            status%ok = .false.
                            status%message = "unsupported non-terminal RETURN at line "// &
                                itoa(return_stmt%line)//": control flow changes the active path"
                            return
                        class default
                        end select
                    end if
                end if
            end if
            call lower_stmt(arena, body_indices(i), proc, status)
            if (.not. status%ok) return
        end do
    end subroutine lower_body

    logical function return_is_terminal(arena, body_indices, position) result(terminal)
        !! A plain RETURN is derivative-neutral only when it is the last
        !! executable statement in this body. Comments may follow it.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:), position
        integer :: i, idx

        terminal = .true.
        do i = position + 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. arena%has_node_at(idx)) cycle
            if (trim(arena%entries(idx)%node_type) == "comment") cycle
            terminal = .false.
            return
        end do
    end function return_is_terminal

    recursive subroutine lower_stmt(arena, idx, proc, status)
        !! Lower one statement, or refuse it by name.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        type(type_bound_call_query_t) :: type_bound
        type(procedure_target_query_t) :: callback_target
        type(nullify_query_t) :: nullify_info
        integer :: ignored, k

        status%ok = .true.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        if (trim(arena%entries(idx)%node_type) == "select_type") then
            call lower_select_type(arena, idx, proc, status)
            return
        end if
        nullify_info = query_nullify(arena, idx)
        if (nullify_info%found) then
            status%ok = .false.
            status%message = "unsupported NULLIFY at line "//itoa(node_line(arena, idx))// &
                ": callback target flow is not tracked"
            return
        end if
        select type (n => arena%entries(idx)%node)
            type is (comment_node)
            return

            type is (use_statement_node)
            call add_use(proc, n)

            type is (declaration_node)
            if (n%is_pointer .or. n%is_target) then
                if (n%is_pointer) then
                    if (is_procedure_pointer_type(n)) then
                        ! A resolved callback assignment is passive metadata;
                        ! the call is rewritten to its concrete target below.
                        ! No procedure-pointer declaration belongs in the AD
                        ! procedure interface or IR.
                        return
                    end if
                end if
                if (allocated(n%var_names)) then
                    if (size(n%var_names) > 0) then
                        call refuse_alias_declaration(n%var_names(1), n%line, &
                            n%is_pointer, n%is_target, status)
                    else
                        call refuse_alias_declaration(n%var_name, n%line, &
                            n%is_pointer, n%is_target, status)
                    end if
                else
                    call refuse_alias_declaration(n%var_name, n%line, &
                        n%is_pointer, n%is_target, status)
                end if
                return
            end if
            if (allocated(n%var_names)) then
                do k = 1, size(n%var_names)
                    call fill_decl(n, idx, trim(n%var_names(k)), arena, d)
                    ignored = proc%add_decl(d)
                end do
            else
                call fill_decl(n, idx, n%var_name, arena, d)
                ignored = proc%add_decl(d)
            end if

            type is (allocate_statement_node)
            call lower_allocate_statement(arena, n, proc, s, status)
            if (.not. status%ok) return
            ignored = proc%add_stmt(s)

            type is (deallocate_statement_node)
            call lower_deallocate_statement(arena, n, proc, s, status)
            if (.not. status%ok) return
            ignored = proc%add_stmt(s)

            type is (assignment_node)
            s%kind = FAD_ASSIGN
            s%line = n%line
            s%is_automatic_reallocation = .false.
            call lower_target(arena, n%target_index, proc, s, status)
            if (.not. status%ok) return
            if (whole_allocatable_target(arena, n%target_index, proc)) then
                if (.not. automatic_reallocation_supported(arena, &
                    n%target_index, proc, status%message)) then
                    status%ok = .false.
                    return
                end if
                s%is_automatic_reallocation = .true.
            end if
            s%value = lower_expr(arena, n%value_index, proc, status)
            if (.not. status%ok) return
            ignored = proc%add_stmt(s)

            type is (if_node)
            if (allocated(n%elseif_blocks)) then
                if (size(n%elseif_blocks) > 0) then
                    status%ok = .false.
                    status%message = "unsupported 'else if' chain at line "// &
                        itoa(node_line(arena, idx))// &
                        "; rewrite as nested if/else"
                    return
                end if
            end if
            s%kind = FAD_IF
            s%line = n%line
            s%value = lower_expr(arena, n%condition_index, proc, status)
            if (.not. status%ok) return
            ignored = proc%add_stmt(s)
            call lower_body(arena, n%then_body_indices, proc, status, &
                allow_terminal_return=.false.)
            if (.not. status%ok) return
            if (allocated(n%else_body_indices)) then
                if (size(n%else_body_indices) > 0) then
                    block
                        type(fad_stmt_t) :: e
                        e%kind = FAD_ELSE
                        ignored = proc%add_stmt(e)
                    end block
                    call lower_body(arena, n%else_body_indices, proc, status, &
                        allow_terminal_return=.false.)
                    if (.not. status%ok) return
                end if
            end if
            block
                type(fad_stmt_t) :: e
                e%kind = FAD_END_IF
                ignored = proc%add_stmt(e)
            end block

            type is (subroutine_call_node)
            s%line = n%line
            type_bound = query_type_bound_call(arena, idx)
            if (type_bound%found .or. type_bound%is_unresolved .or. &
                index(n%name, "%") > 0) then
                call lower_type_bound_subroutine(arena, idx, proc, s, status)
                if (.not. status%ok) return
            else
                block
                    integer, allocatable :: cargs(:)
                    character(len=64), allocatable :: cnames(:)
                    call lower_call_arguments(arena, n%arg_indices, proc, cargs, &
                        cnames, status)
                    if (.not. status%ok) return
                    s%call_args = cargs
                    s%call_arg_names = cnames
                end block
            end if
            if (same_name(n%name, "move_alloc") .and. &
                size(n%arg_indices) == 2 .and. .not. &
                (type_bound%found .or. type_bound%is_unresolved)) then
                s%kind = FAD_MOVE_ALLOC
                if (.not. allocation_object_declared(proc, s%call_args(1)) .or. &
                    .not. allocation_object_declared(proc, s%call_args(2))) then
                    call refuse_allocation(n%line, "move_alloc requires allocatable "// &
                        "objects", status)
                    return
                end if
            else if (.not. (type_bound%found .or. type_bound%is_unresolved .or. &
                    index(n%name, "%") > 0)) then
                s%kind = FAD_CALL_STMT
                call resolve_generic_call(arena, idx, n%name, s%target, status)
                if (.not. status%ok) return
                call resolve_callback_call(arena, idx, n%name, &
                    size(n%arg_indices), .true., s%target, status)
                if (.not. status%ok) return
            end if
            if (s%kind > 0) ignored = proc%add_stmt(s)

            type is (pointer_assignment_node)
            callback_target = query_procedure_target(arena, idx)
            if (callback_target%found) then
                if (callback_target%is_resolved) return
                status%ok = .false.
                status%message = "unsupported procedure-pointer callback assignment at line "// &
                    itoa(n%line)//": target flow is unresolved"
                if (callback_target%is_null) then
                    status%message = "unsupported procedure-pointer callback assignment at line "// &
                        itoa(n%line)//": NULL() callback targets are not differentiated"
                end if
                return
            end if
            status%ok = .false.
            status%message = "unsupported pointer association at line "// &
                itoa(n%line)//": storage identity is not tracked"
            return

            type is (do_loop_node)
            s%kind = FAD_DO
            s%target = n%var_name
            s%lo = lower_expr(arena, n%start_expr_index, proc, status)
            if (.not. status%ok) return
            s%hi = lower_expr(arena, n%end_expr_index, proc, status)
            if (.not. status%ok) return
            if (n%step_expr_index > 0) then
                s%step = lower_expr(arena, n%step_expr_index, proc, status)
                if (.not. status%ok) return
            end if
            ignored = proc%add_stmt(s)
            call lower_body(arena, n%body_indices, proc, status, &
                allow_terminal_return=.false.)
            if (.not. status%ok) return
            block
                type(fad_stmt_t) :: e
                e%kind = FAD_END_DO
                ignored = proc%add_stmt(e)
            end block

        class default
            status%ok = .false.
            status%message = "unsupported statement at line "//itoa(node_line(arena, idx))
        end select
    end subroutine lower_stmt

    subroutine refuse_alias_declaration(name, line, is_pointer, is_target, status)
        !! Refuse declarations whose storage may be reached through aliases.
        !!
        !! The IR names values, not storage locations. Emitting a derivative
        !! for a POINTER or TARGET declaration without tracking association
        !! could silently send an update to the wrong object, so this boundary
        !! is explicit until P7.3 adds storage-identity analysis.
        character(len=*), intent(in) :: name
        integer, intent(in) :: line
        logical, intent(in) :: is_pointer, is_target
        type(lower_status_t), intent(inout) :: status

        status%ok = .false.
        if (is_pointer) then
            status%message = "unsupported aliasing declaration '"//trim(name)// &
                "' at line "//itoa(line)//": pointer association storage "// &
                "identity is not tracked"
        else if (is_target) then
            status%message = "unsupported aliasing declaration '"//trim(name)// &
                "' at line "//itoa(line)//": TARGET alias storage identity "// &
                "is not tracked"
        else
            status%message = "unsupported aliasing declaration '"//trim(name)// &
                "' at line "//itoa(line)//": storage identity is not tracked"
        end if
    end subroutine refuse_alias_declaration

    subroutine lower_select_type(arena, idx, proc, status)
        !! Preserve a SELECT TYPE as structural IR. Its selector is discrete:
        !! differentiation happens only inside the runtime-selected guard.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        integer, allocatable :: guards(:), body(:)
        character(len=:), allocatable :: guard_kind
        type(fad_stmt_t) :: s
        integer :: selector, selector_expr, default_index, type_index, i, ignored
        integer :: selector_decl
        character(len=:), allocatable :: selector_name
        type(fad_decl_t) :: saved_selector
        logical :: saved_selector_decl

        status%ok = .true.
        call get_select_type_info(arena, idx, selector, guards, default_index)
        if (selector <= 0) then
            status%ok = .false.
            status%message = "select type at line "//itoa(node_line(arena, idx))// &
                " has no selector"
            return
        end if

        s%kind = FAD_SELECT_TYPE
        if (allocated(s%target)) deallocate (s%target)
        call select_type_selector_info(arena, selector, selector_expr, &
            selector_name)
        s%value = lower_expr(arena, selector_expr, proc, status)
        if (.not. status%ok) return
        if (allocated(selector_name)) then
            selector_decl = proc%decl_index(selector_name)
            if (selector_decl == 0) then
                block
                    use fortad_emit, only: emit_expr
                    type(fad_decl_t) :: alias_decl
                    alias_decl%name = selector_name
                    alias_decl%type_name = "class(*)"
                    alias_decl%is_select_alias = .true.
                    alias_decl%alias_target = emit_expr(proc, s%value)
                    ignored = proc%add_decl(alias_decl)
                    selector_decl = proc%decl_index(selector_name)
                end block
            end if
        end if
        if (allocated(selector_name)) s%target = selector_name
        ignored = proc%add_stmt(s)

        ! A SELECT TYPE associate name has a concrete static type inside its
        ! guard arm.  Keep that fact only while lowering the arm so a
        ! type-bound call such as ``model%value(x)`` can bind to the child's
        ! implementation without changing the generated derivative interface
        ! (the selector remains CLASS(base_t) at the procedure boundary).
        saved_selector_decl = .false.
        if (allocated(selector_name)) then
            selector_decl = proc%decl_index(selector_name)
            if (selector_decl > 0) then
                call copy_decl(saved_selector, proc%decls(selector_decl))
                saved_selector_decl = .true.
            end if
        end if

        do i = 1, size(guards)
            call get_type_guard_info(arena, guards(i), guard_kind, type_index, body)
            if (type_index <= 0) then
                status%ok = .false.
                status%message = "select type guard at line "// &
                    itoa(node_line(arena, guards(i)))//" has no type name"
                return
            end if
            if (.not. arena%has_node_at(type_index)) then
                status%ok = .false.
                status%message = "select type guard at line "// &
                    itoa(node_line(arena, guards(i)))//" has no type name"
                return
            end if
            select case (trim(guard_kind))
            case ("type_is")
                s%kind = FAD_TYPE_IS
            case ("class_is")
                s%kind = FAD_CLASS_IS
            case default
                status%ok = .false.
                status%message = "unsupported select type guard '"// &
                    trim(guard_kind)//"' at line "// &
                    itoa(node_line(arena, guards(i)))
                return
            end select
            select type (name => arena%entries(type_index)%node)
                type is (identifier_node)
                s%target = name%name
            class default
                status%ok = .false.
                status%message = "select type guard at line "// &
                    itoa(node_line(arena, guards(i)))// &
                    " has an unsupported type name"
                return
            end select
            s%value = 0
            ignored = proc%add_stmt(s)
            if (saved_selector_decl) then
                call set_select_type_alias(proc%decls(selector_decl), s%target)
            end if
            call lower_body(arena, body, proc, status, &
                allow_terminal_return=.false.)
            if (saved_selector_decl) then
                call copy_decl(proc%decls(selector_decl), saved_selector)
            end if
            if (.not. status%ok) return
        end do

        if (default_index > 0) then
            call get_type_guard_info(arena, default_index, guard_kind, &
                type_index, body)
            s%kind = FAD_CLASS_DEFAULT
            s%value = 0
            if (allocated(s%target)) deallocate (s%target)
            ignored = proc%add_stmt(s)
            call lower_body(arena, body, proc, status, &
                allow_terminal_return=.false.)
            if (.not. status%ok) return
        end if

        s%kind = FAD_END_SELECT
        s%value = 0
        ignored = proc%add_stmt(s)
    end subroutine lower_select_type

    subroutine select_type_selector_info(arena, selector, expression, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: selector
        integer, intent(out) :: expression
        character(len=:), allocatable, intent(out) :: name

        expression = selector
        if (selector <= 0 .or. selector > arena%size) return
        if (.not. arena%has_node_at(selector)) return
        select type (node => arena%entries(selector)%node)
            type is (identifier_node)
            name = trim(node%name)
            type is (pointer_assignment_node)
            expression = node%target_index
            if (node%pointer_index > 0 .and. &
                arena%has_node_at(node%pointer_index)) then
                select type (alias => arena%entries(node%pointer_index)%node)
                    type is (identifier_node)
                    name = trim(alias%name)
                class default
                end select
            end if
        class default
        end select
    end subroutine select_type_selector_info

    subroutine set_select_type_alias(decl, type_name)
        type(fad_decl_t), intent(inout) :: decl
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: alias_type

        alias_type = "type("//trim(type_name)//")"
        if (allocated(decl%type_name)) deallocate (decl%type_name)
        decl%type_name = alias_type
    end subroutine set_select_type_alias

    subroutine fill_decl(n, idx, name, arena, d)
        !! Translate a fortfront declaration node into a fortad declaration.
        type(declaration_node), intent(in) :: n
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        type(ast_arena_t), intent(in) :: arena
        type(fad_decl_t), intent(out) :: d
        type(declaration_query_t) :: query
        type(storage_query_t) :: storage

        d%name = name
        d%line = n%line
        d%type_name = n%type_name
        d%is_value = n%is_value
        d%is_optional = n%is_optional
        if (n%has_kind .and. n%kind_value > 0) then
            d%type_name = n%type_name//"("//itoa(n%kind_value)//")"
        end if
        d%is_array = n%is_array
        d%is_contiguous = n%is_contiguous
        d%is_allocatable = n%is_allocatable
        storage = query_storage(arena, idx)
        if (storage%found) then
            d%is_polymorphic = storage%is_polymorphic
            d%is_unlimited_polymorphic = storage%is_unlimited_polymorphic
        end if
        d%intent = FAD_INTENT_NONE
        if (n%has_intent .and. allocated(n%intent)) then
            select case (trim(n%intent))
            case ("in")
                d%intent = FAD_INTENT_IN
            case ("out")
                d%intent = FAD_INTENT_OUT
            case ("inout")
                d%intent = FAD_INTENT_INOUT
            end select
        end if
        query = query_declaration(arena, idx)
        if (query%found .and. query%is_array .and. &
            allocated(query%dimension_indices)) then
            if (size(query%dimension_indices) == 0) then
                d%dims = declaration_dims_from_source(arena, n%line, name)
                return
            end if
            d%dims = dims_text(arena, query%dimension_indices)
            if (trim(d%dims) == ":") then
                d%dims = declaration_dims_from_source(arena, n%line, name)
            end if
        end if
        if (d%is_array) then
            if (.not. allocated(d%dims)) then
                d%dims = declaration_dims_from_source(arena, n%line, name)
            else if (len_trim(d%dims) == 0) then
                d%dims = declaration_dims_from_source(arena, n%line, name)
            end if
        end if
    end subroutine fill_decl

    logical function is_procedure_pointer_type(n) result(is_callback)
        !! Whether a declaration is a procedure pointer, rather than a data
        !! pointer whose storage identity still belongs to the aliasing
        !! refusal boundary.
        type(declaration_node), intent(in) :: n
        character(len=:), allocatable :: normalized
        integer :: i

        is_callback = .false.
        if (.not. n%is_pointer) return
        if (.not. allocated(n%type_name)) return
        normalized = ""
        do i = 1, len_trim(n%type_name)
            if (n%type_name(i:i) == " ") cycle
            normalized = normalized//lower_char(n%type_name(i:i))
        end do
        if (index(normalized, "procedure") == 1) is_callback = .true.
    end function is_procedure_pointer_type

    subroutine lower_allocate_statement(arena, node, proc, s, status)
        !! Lower one-owner ALLOCATE.  Multiple allocation objects, STAT=,
        !! ERRMSG=, and polymorphic TYPE-spec forms remain a named boundary;
        !! they need a transaction/state representation rather than a text
        !! copy.  SOURCE= and MOLD= are retained as ownership metadata so the
        !! forward shadow can acquire the same shape and initial values.
        type(ast_arena_t), intent(in) :: arena
        type(allocate_statement_node), intent(in) :: node
        type(fad_proc_t), intent(inout) :: proc
        type(fad_stmt_t), intent(out) :: s
        type(lower_status_t), intent(out) :: status
        integer :: i

        status%ok = .true.
        s%kind = FAD_ALLOCATE
        s%line = node%line
        if (node%stat_var_index > 0 .or. node%errmsg_var_index > 0) then
            call refuse_allocation(node%line, "STAT=/ERRMSG=", status)
            return
        end if
        if (allocated(node%type_spec)) then
            if (len_trim(node%type_spec) > 0) then
                call refuse_allocation(node%line, "TYPE-spec", status)
                return
            end if
        end if
        if (.not. allocated(node%var_indices)) then
            call refuse_allocation(node%line, "missing allocation object", status)
            return
        end if
        if (size(node%var_indices) /= 1) then
            call refuse_allocation(node%line, "multiple allocation objects", status)
            return
        end if
        allocate (s%allocation_args(1))
        s%allocation_args(1) = lower_expr(arena, node%var_indices(1), proc, status)
        if (.not. status%ok) return
        if (.not. allocation_object_declared(proc, s%allocation_args(1))) then
            block
                type(storage_query_t) :: component_storage
                component_storage = query_storage(arena, node%var_indices(1))
                if (.not. component_storage%found .or. &
                    .not. component_storage%is_allocatable .or. &
                    .not. component_storage%is_polymorphic) then
                    call refuse_allocation(node%line, "non-allocatable target", status)
                    return
                end if
                ! query_storage resolves both a component and a component of
                ! an array element.  Keep that frontend fact in the IR; later
                ! passes must not rediscover it from rendered source text.
                s%allocation_target_polymorphic = .true.
                s%allocation_target_unlimited_polymorphic = &
                    component_storage%is_unlimited_polymorphic
            end block
        end if
        if (allocated(node%shape_indices)) then
            if (size(node%shape_indices) > 0) then
                block
                    integer, allocatable :: shapes(:)
                    allocate (shapes(size(node%shape_indices)))
                    do i = 1, size(node%shape_indices)
                        shapes(i) = lower_expr(arena, node%shape_indices(i), &
                            proc, status)
                        if (.not. status%ok) return
                    end do
                    s%allocation_args = [s%allocation_args, shapes]
                end block
            end if
        end if
        if (node%source_expr_index > 0) then
            s%allocation_source = lower_expr(arena, node%source_expr_index, &
                proc, status)
        else if (node%mold_expr_index > 0) then
            s%allocation_mold = lower_expr(arena, node%mold_expr_index, &
                proc, status)
        end if
    end subroutine lower_allocate_statement

    subroutine lower_deallocate_statement(arena, node, proc, s, status)
        !! Lower one-owner DEALLOCATE without status side channels.
        type(ast_arena_t), intent(in) :: arena
        type(deallocate_statement_node), intent(in) :: node
        type(fad_proc_t), intent(inout) :: proc
        type(fad_stmt_t), intent(out) :: s
        type(lower_status_t), intent(out) :: status

        status%ok = .true.
        s%kind = FAD_DEALLOCATE
        s%line = node%line
        if (node%stat_var_index > 0 .or. node%errmsg_var_index > 0) then
            call refuse_allocation(node%line, "STAT=/ERRMSG=", status)
            return
        end if
        if (.not. allocated(node%var_indices)) then
            call refuse_allocation(node%line, "missing deallocation object", status)
            return
        end if
        if (size(node%var_indices) /= 1) then
            call refuse_allocation(node%line, "multiple deallocation objects", status)
            return
        end if
        allocate (s%allocation_args(1))
        s%allocation_args(1) = lower_expr(arena, node%var_indices(1), proc, status)
        if (.not. status%ok) return
        if (.not. allocation_object_declared(proc, s%allocation_args(1))) then
            block
                type(storage_query_t) :: component_storage
                component_storage = query_storage(arena, node%var_indices(1))
                if (.not. component_storage%found .or. &
                    .not. component_storage%is_allocatable .or. &
                    .not. component_storage%is_polymorphic) then
                    call refuse_allocation(node%line, "non-allocatable target", status)
                    return
                end if
            end block
        end if
    end subroutine lower_deallocate_statement

    subroutine refuse_allocation(line, construct, status)
        integer, intent(in) :: line
        character(len=*), intent(in) :: construct
        type(lower_status_t), intent(inout) :: status

        status%ok = .false.
        status%message = "unsupported allocation lifetime form '"//trim(construct)// &
            "' at line "//itoa(line)//": ownership side effects are not represented"
    end subroutine refuse_allocation

    logical function whole_allocatable_target(arena, idx, proc) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(in) :: proc
        type(storage_query_t) :: storage
        character(len=:), allocatable :: name

        found = .false.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        select type (node => arena%entries(idx)%node)
            type is (identifier_node)
            name = node%name
        class default
            return
        end select
        storage = query_storage(arena, idx)
        if (storage%found) then
            found = storage%is_allocatable
        else
            found = proc%decl_index(trim(name)) > 0
            if (found) found = proc%decls(proc%decl_index(trim(name)))%is_allocatable
        end if
    end function whole_allocatable_target

    logical function automatic_reallocation_supported(arena, idx, proc, message) &
            result(supported)
        !! Accept only a concrete scalar, rank-one, or rank-two local/dummy
        !! owner.  The
        !! descriptor and payload are then re-created by ordinary Fortran
        !! assignment in both the primal and generated derivative procedures;
        !! no separate allocation-state tape is needed for this bounded slice.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(in) :: proc
        character(len=:), allocatable, intent(out) :: message
        type(storage_query_t) :: storage
        character(len=:), allocatable :: name, dims
        integer :: di, i, rank

        supported = .false.
        message = ""
        select type (node => arena%entries(idx)%node)
            type is (identifier_node)
            name = trim(node%name)
        class default
            message = "unsupported automatic reallocation at line "// &
                itoa(arena%get_node_line(idx))//": allocatable components and "// &
                "computed owners require component lifetime tracking"
            return
        end select

        storage = query_storage(arena, idx)
        di = proc%decl_index(name)
        if (.not. storage%found .and. di <= 0) then
            message = "unsupported automatic reallocation at line "// &
                itoa(arena%get_node_line(idx))//": FortFront did not prove the "// &
                "allocatable owner's storage"
            return
        end if
        if (storage%found) then
            if (storage%is_pointer .or. storage%is_target) then
                message = "unsupported automatic reallocation for '"//trim(name)// &
                    "': pointer/target alias storage identity is not tracked"
                return
            end if
        end if
        if ((storage%found .and. (storage%is_module_state .or. &
            storage%is_save_state .or. storage%is_common_state))) then
            message = "unsupported automatic reallocation for '"//trim(name)// &
                "': global mutable ownership requires an explicit derivative rule"
            return
        end if
        if (storage%found) then
            if (storage%is_polymorphic .or. storage%is_unlimited_polymorphic) then
                message = "unsupported automatic reallocation for '"//trim(name)// &
                    "': polymorphic ownership requires dynamic-type replay"
                return
            end if
        end if
        if (di > 0) then
            if (proc%decls(di)%is_polymorphic) then
                message = "unsupported automatic reallocation for '"//trim(name)// &
                    "': polymorphic ownership requires dynamic-type replay"
                return
            end if
        end if

        if (di <= 0) then
            message = "unsupported automatic reallocation for '"//trim(name)// &
                "': owner is not a local or dummy declaration"
            return
        end if
        if (.not. proc%decls(di)%is_allocatable) then
            message = "unsupported automatic reallocation for '"//trim(name)// &
                "': owner is not allocatable"
            return
        end if
        if (.not. proc%decls(di)%is_array) then
            supported = .true.
            return
        end if
        if (.not. allocated(proc%decls(di)%dims)) then
            message = "unsupported automatic reallocation for '"//trim(name)// &
                "': rank is not proven by FortFront"
            return
        end if
        dims = trim(proc%decls(di)%dims)
        rank = 1
        do i = 1, len_trim(dims)
            if (dims(i:i) == ",") rank = rank + 1
        end do
        if (rank > 2) then
            message = "unsupported automatic reallocation for '"//trim(name)// &
                "': only concrete scalar through rank-two owners are supported"
            return
        end if
        supported = .true.
    end function automatic_reallocation_supported

    logical function allocation_object_declared(proc, idx) result(found)
        type(fad_proc_t), intent(in) :: proc
        integer, intent(in) :: idx
        integer :: di

        found = .false.
        if (idx <= 0 .or. idx > proc%n_exprs) return
        if (.not. allocated(proc%exprs(idx)%text)) return
        di = proc%decl_index(fad_base_name(proc%exprs(idx)%text))
        ! Component ownership needs a containing-object lifetime model.  Do
        ! not accept it merely because the expression contains a percent sign.
        found = .false.
        if (di > 0) then
            found = proc%decls(di)%is_allocatable .and. &
                index(proc%exprs(idx)%text, "%") == 0
        end if
    end function allocation_object_declared

    function declaration_dims_from_source(arena, line_number, name) result(dims)
        !! Fallback for declaration bounds not attached to the parser node.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: dims, line
        logical :: found
        integer :: i, j, open, depth, name_len

        dims = ""
        call get_source_line(arena, line_number, line, found)
        if (.not. found) return
        name_len = len_trim(name)
        if (name_len == 0) return
        do i = 1, len_trim(line) - name_len + 1
            if (.not. same_name(line(i:i + name_len - 1), name)) cycle
            if (i > 1) then
                if (is_name_char(line(i - 1:i - 1))) cycle
            end if
            j = i + name_len
            if (j <= len_trim(line)) then
                if (is_name_char(line(j:j))) cycle
            end if
            do while (j <= len_trim(line))
                if (line(j:j) /= " ") exit
                j = j + 1
            end do
            if (j > len_trim(line)) cycle
            if (line(j:j) /= "(") cycle
            open = j
            depth = 1
            do j = j + 1, len_trim(line)
                if (line(j:j) == "(") depth = depth + 1
                if (line(j:j) == ")") depth = depth - 1
                if (depth == 0) then
                    dims = line(open + 1:j - 1)
                    return
                end if
            end do
        end do
    end function declaration_dims_from_source

    logical function is_name_char(c) result(yes)
        character, intent(in) :: c

        yes = (c >= "a" .and. c <= "z") .or. &
            (c >= "A" .and. c <= "Z") .or. &
            (c >= "0" .and. c <= "9") .or. c == "_"
    end function is_name_char

    subroutine inherit_module_uses(arena, procedure_index, proc)
        !! A module-level USE is visible to every contained procedure.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        type(fad_proc_t), intent(inout) :: proc
        integer :: i, j, index

        if (.not. arena%has_node_at(procedure_index)) return
        if (arena%entries(procedure_index)%parent_index <= 0) return
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (m => arena%entries(i)%node)
                type is (module_node)
                if (.not. allocated(m%procedure_indices)) cycle
                if (.not. any(m%procedure_indices == procedure_index)) cycle
                if (.not. allocated(m%declaration_indices)) cycle
                do j = 1, size(m%declaration_indices)
                    index = m%declaration_indices(j)
                    if (index <= 0 .or. index > arena%size) cycle
                    if (.not. arena%has_node_at(index)) cycle
                    select type (n => arena%entries(index)%node)
                        type is (use_statement_node)
                        call add_use(proc, n)
                    class default
                        cycle
                    end select
                end do
            class default
                cycle
            end select
        end do
    end subroutine inherit_module_uses

    function dims_text(arena, indices) result(text)
        !! Reproduce a dimension list as source text.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        character(len=:), allocatable :: text
        integer :: i

        text = ""
        do i = 1, size(indices)
            if (i > 1) text = text//","
            text = text//simple_expr_text(arena, indices(i))
        end do
        if (len(text) == 0) text = ":"
    end function dims_text

    recursive function simple_expr_text(arena, idx) result(text)
        !! Source text of a bound expression, for verbatim re-emission.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=:), allocatable :: text

        text = ":"
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        select type (n => arena%entries(idx)%node)
            type is (identifier_node)
            text = n%name
            type is (literal_node)
            text = n%value
            type is (binary_op_node)
            text = simple_expr_text(arena, n%left_index)//trim(n%operator)// &
                simple_expr_text(arena, n%right_index)
        end select
    end function simple_expr_text

    subroutine add_use(proc, n)
        !! Record one `use` statement, rendered back to source text.
        type(fad_proc_t), intent(inout) :: proc
        type(use_statement_node), intent(in) :: n

        call ensure_use_capacity(proc)
        proc%n_uses = proc%n_uses + 1
        proc%uses(proc%n_uses) = render_use_node(n)
    end subroutine add_use

    function render_use_node(n) result(line)
        !! Render a concrete USE node without copying it into a query record.
        type(use_statement_node), intent(in) :: n
        character(len=:), allocatable :: line
        character(len=4096) :: items
        integer :: i, items_length

        line = ""
        if (.not. allocated(n%module_name)) return
        line = "use"
        if (n%is_intrinsic) line = line//", intrinsic"
        line = line//" :: "//trim(n%module_name)
        items = ""
        items_length = 0
        if (allocated(n%rename_list)) then
            do i = 1, size(n%rename_list) - 1, 2
                if (items_length > 0) call append_literal(items, items_length, ", ")
                call append_trimmed(items, items_length, n%rename_list(i)%s)
                call append_literal(items, items_length, " => ")
                call append_trimmed(items, items_length, n%rename_list(i + 1)%s)
            end do
        end if
        if (allocated(n%only_list)) then
            do i = 1, size(n%only_list)
                if (items_length > 0) call append_literal(items, items_length, ", ")
                call append_trimmed(items, items_length, n%only_list(i)%s)
            end do
        end if
        if (n%has_only .and. items_length > 0) then
            line = line//", only: "//items(:items_length)
        end if
    end function render_use_node

    subroutine append_literal(buffer, used, piece)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: used
        character(len=*), intent(in) :: piece
        integer :: count

        count = len(piece)
        if (count == 0) return
        if (used + count > len(buffer)) error stop &
            "fortad_lower_statements: USE statement is too long"
        buffer(used + 1:used + count) = piece
        used = used + count
    end subroutine append_literal

    subroutine append_trimmed(buffer, used, piece)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: used
        character(len=*), intent(in) :: piece
        integer :: count

        count = len_trim(piece)
        if (count == 0) return
        call append_literal(buffer, used, piece(:count))
    end subroutine append_trimmed

    subroutine lower_target(arena, idx, proc, s, status)
        !! Lower an assignment target: a plain name or an array element.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(fad_stmt_t), intent(inout) :: s
        type(lower_status_t), intent(inout) :: status
        type(fad_expr_t) :: e
        integer :: i
        integer, allocatable :: subs(:)
        integer :: section_expr

        call validate_component_reference(arena, idx, status)
        if (.not. status%ok) return

        if (idx <= 0 .or. idx > arena%size) then
            status%ok = .false.
            status%message = "empty assignment target"
            return
        end if
        if (is_section_node(arena, idx)) then
            section_expr = lower_array_section(arena, idx, proc, status)
            if (status%ok) s%target = emit_expr(proc, section_expr)
            return
        end if
        select type (n => arena%entries(idx)%node)
            type is (identifier_node)
            s%target = n%name
            type is (call_or_subscript_node)
            allocate (subs(size(n%arg_indices)))
            do i = 1, size(n%arg_indices)
                subs(i) = lower_expr(arena, n%arg_indices(i), proc, status)
                if (.not. status%ok) return
            end do
            if (n%base_expr_index == 0) then
                if (is_array_name(proc, n%name)) then
                    if (has_vector_subscript(arena, n%arg_indices, subs, proc)) then
                        call refuse_vector_subscript(arena, idx, status)
                        return
                    end if
                end if
            end if
            if (is_component_base(arena, n%base_expr_index)) then
                if (size(n%arg_indices) == 0) then
                    s%target = component_reference_text(arena, &
                        n%base_expr_index, n%name, proc, status)
                    if (.not. status%ok) return
                    return
                end if
                e%kind = FAD_INDEX
                e%text = component_reference_text(arena, &
                    n%base_expr_index, n%name, proc, status)
                if (.not. status%ok) return
            else
                e%kind = FAD_INDEX
                e%text = n%name
            end if
            e%args = subs
            s%target = render_index(proc, e)
        class default
            if (trim(arena%entries(idx)%node_type) == "component_access") then
                s%target = render_component_access(arena, idx, proc, status)
            else
                status%ok = .false.
                status%message = "unsupported assignment target"
            end if
        end select
    end subroutine lower_target

    function render_index(proc, e) result(text)
        !! Array element reference as text, so a target stays a simple string.
        use fortad_emit, only: emit_expr
        type(fad_proc_t), intent(in) :: proc
        type(fad_expr_t), intent(in) :: e
        character(len=:), allocatable :: text
        integer :: i

        text = e%text//"("
        do i = 1, size(e%args)
            if (i > 1) text = text//", "
            text = text//emit_expr(proc, e%args(i))
        end do
        text = text//")"
    end function render_index

    recursive integer function lower_expr(arena, idx, proc, status) result(out)
        !! Lower an expression, returning its index in the IR expression arena.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        integer, allocatable :: args(:)
        character(len=64), allocatable :: arg_names(:)
        type(fad_expr_t) :: e
        integer :: i
        character(len=:), allocatable :: call_name
        character(len=:), allocatable :: callback_name

        out = 0
        if (idx <= 0 .or. idx > arena%size) then
            status%ok = .false.
            status%message = "empty expression"
            return
        end if
        if (.not. arena%has_node_at(idx)) then
            status%ok = .false.
            status%message = "empty expression node"
            return
        end if

        call validate_component_reference(arena, idx, status)
        if (.not. status%ok) return

        if (is_section_node(arena, idx)) then
            out = lower_array_section(arena, idx, proc, status)
            return
        end if

        if (trim(arena%entries(idx)%node_type) == "component_access") then
            block
                character(len=:), allocatable :: component
                component = render_component_access(arena, idx, proc, status)
                if (.not. status%ok) return
                out = proc%add_expr(expr_var(component))
                call annotate_component_expr(arena, idx, proc%exprs(out))
            end block
            return
        end if

        select type (n => arena%entries(idx)%node)
            type is (identifier_node)
            out = proc%add_expr(expr_var(n%name))
            type is (literal_node)
            out = proc%add_expr(expr_const(n%value))
            type is (binary_op_node)
            if (is_defined_operator(n%operator)) then
                status%ok = .false.
                status%message = "unsupported operator '"//trim(n%operator)// &
                    "' in an active expression"
                return
            end if
            block
                integer :: l, r
                l = lower_expr(arena, n%left_index, proc, status)
                if (.not. status%ok) return
                r = lower_expr(arena, n%right_index, proc, status)
                if (.not. status%ok) return
                out = proc%add_expr(expr_binop(trim(n%operator), l, r))
            end block
            type is (call_or_subscript_node)
            call lower_call_arguments(arena, n%arg_indices, proc, args, &
                arg_names, status)
            if (.not. status%ok) return
            call resolve_generic_call(arena, idx, n%name, call_name, status)
            if (.not. status%ok) return
            callback_name = call_name
            call resolve_callback_call(arena, idx, callback_name, &
                size(n%arg_indices), .false., call_name, status)
            if (.not. status%ok) return
            if (n%base_expr_index == 0) then
                if (is_array_name(proc, n%name)) then
                    if (has_vector_subscript(arena, n%arg_indices, args, proc)) then
                        call refuse_vector_subscript(arena, idx, status)
                        return
                    end if
                end if
            end if
            if (n%base_expr_index > 0) then
                if (is_component_base(arena, n%base_expr_index)) then
                    if (is_type_bound_reference(arena, n%base_expr_index, proc)) then
                        out = lower_type_bound_call(arena, idx, n, proc, status)
                        if (.not. status%ok) return
                    else if (size(n%arg_indices) == 0) then
                        e%kind = FAD_VAR
                        e%text = component_reference_text(arena, &
                            n%base_expr_index, n%name, proc, status)
                        if (.not. status%ok) return
                        out = proc%add_expr(e)
                        call annotate_component_expr(arena, n%base_expr_index, &
                            proc%exprs(out))
                    else
                        e%kind = FAD_INDEX
                        e%text = component_reference_text(arena, &
                            n%base_expr_index, n%name, proc, status)
                        if (.not. status%ok) return
                        e%args = args
                        out = proc%add_expr(e)
                        call annotate_component_expr(arena, n%base_expr_index, &
                            proc%exprs(out))
                    end if
                else
                    out = lower_type_bound_call(arena, idx, n, proc, status)
                    if (.not. status%ok) return
                end if
            else if (is_array_name(proc, n%name)) then
                e%kind = FAD_INDEX
                e%text = n%name
                e%args = args
                out = proc%add_expr(e)
            else
                out = proc%add_expr(expr_call(call_name, args, arg_names))
            end if
        class default
            status%ok = .false.
            status%message = "unsupported expression at line "// &
                itoa(node_line(arena, idx))
        end select
    end function lower_expr

    subroutine lower_call_arguments(arena, arg_indices, proc, args, names, status)
        !! Lower actuals while retaining the formal name of keyword actuals.
        !! FortFront represents ``f(x=1)`` as an assignment node, so lowering
        !! only its RHS would silently turn a reordered call back into a
        !! positional one before same-file inlining sees it.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arg_indices(:)
        type(fad_proc_t), intent(inout) :: proc
        integer, allocatable, intent(out) :: args(:)
        character(len=64), allocatable, intent(out) :: names(:)
        type(lower_status_t), intent(inout) :: status
        integer :: i
        character(len=64) :: keyword

        allocate (args(size(arg_indices)))
        allocate (names(size(arg_indices)))
        names = ""
        do i = 1, size(arg_indices)
            args(i) = lower_actual(arena, arg_indices(i), proc, keyword, status)
            if (.not. status%ok) return
            if (len_trim(keyword) > 0) names(i) = keyword
        end do
    end subroutine lower_call_arguments

    recursive integer function lower_actual(arena, idx, proc, keyword, status) &
            result(out)
        !! Extract a keyword's formal name, then lower its value expression.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        character(len=*), intent(out) :: keyword
        type(lower_status_t), intent(inout) :: status
        integer :: value_idx

        out = 0
        keyword = ""
        value_idx = idx
        if (idx > 0) then
            if (idx <= arena%size) then
                if (arena%has_node_at(idx)) then
                    select type (n => arena%entries(idx)%node)
                        type is (assignment_node)
                        ! Expression calls do not currently mark this AST node
                        ! with is_keyword_argument; within an actual-argument
                        ! list an assignment node is nevertheless unambiguously
                        ! the Fortran keyword form.
                        if (n%target_index <= 0 .or. &
                            n%target_index > arena%size .or. &
                            .not. arena%has_node_at(n%target_index)) then
                            status%ok = .false.
                            status%message = "keyword actual has no formal name"
                            return
                        end if
                        select type (target => arena%entries(n%target_index)%node)
                            type is (identifier_node)
                            keyword = target%name
                            value_idx = n%value_index
                        class default
                            status%ok = .false.
                            status%message = "keyword actual needs a simple formal name"
                            return
                        end select
                    end select
                end if
            end if
        end if
        out = lower_expr(arena, value_idx, proc, status)
    end function lower_actual

    subroutine resolve_callback_call(arena, idx, fallback, actual_count, &
            is_subroutine, name, status)
        !! Consume FortFront's bounded callback identity fact.  A resolved
        !! callback is reduced to an ordinary same-file call so existing
        !! lowering, inlining, and differentiation paths remain unchanged.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx, actual_count
        character(len=*), intent(in) :: fallback
        logical, intent(in) :: is_subroutine
        character(len=:), allocatable, intent(out) :: name
        type(lower_status_t), intent(inout) :: status
        type(procedure_call_target_query_t) :: query
        type(program_unit_query_t) :: target_unit
        type(declaration_query_t) :: target_declaration
        integer :: expected_kind, formal_count
        logical :: interface_matches

        name = trim(fallback)
        query = query_procedure_call_target(arena, idx)
        if (.not. query%found) then
            if (.not. query%is_unresolved) return
            status%ok = .false.
            status%message = "unsupported procedure-pointer callback call '"// &
                trim(query%pointer_name)//"' at line "//itoa(node_line(arena, idx))// &
                ": target flow is unresolved; require one preceding unconditional "// &
                "same-scope direct assignment"
            return
        end if

        expected_kind = BINDING_FUNCTION
        if (is_subroutine) expected_kind = BINDING_SUBROUTINE
        interface_matches = query%target_binding_kind == expected_kind
        if (.not. interface_matches) then
            ! An external procedure is represented by its EXTERNAL
            ! declaration, not a procedure node.  A nonempty type spec is the
            ! frontend's function-result fact; a blank one is the bounded
            ! external-subroutine fact.
            if (query%target_binding_kind == BINDING_DECLARATION .and. &
                query%target_declaration_index > 0) then
                target_declaration = query_declaration(arena, &
                    query%target_declaration_index)
                if (target_declaration%found) then
                    if (is_subroutine) then
                        interface_matches = .true.
                        if (allocated(target_declaration%type_name)) then
                            interface_matches = len_trim(target_declaration%type_name) == 0
                        end if
                    else
                        interface_matches = allocated(target_declaration%type_name)
                        if (interface_matches) then
                            interface_matches = len_trim(target_declaration%type_name) > 0
                        end if
                    end if
                end if
            end if
        end if
        if (.not. interface_matches) then
            status%ok = .false.
            status%message = "procedure-pointer callback interface mismatch for '"// &
                trim(query%pointer_name)//"' at line "//itoa(node_line(arena, idx))
            return
        end if

        ! For targets defined in this source, require the actual count to
        ! agree with the concrete procedure. External declarations have no
        ! formal list in this arena; FortFront has nevertheless proved their
        ! procedure kind and external identity, which is the available
        ! interface fact for this bounded consumer.
        if (query%target_procedure_index > 0) then
            target_unit = query_program_unit(arena, query%target_procedure_index)
            if (target_unit%found) then
                formal_count = 0
                if (allocated(target_unit%parameter_indices)) then
                    formal_count = size(target_unit%parameter_indices)
                end if
                if (formal_count /= actual_count) then
                    status%ok = .false.
                    status%message = "procedure-pointer callback interface mismatch for '"// &
                        trim(query%procedure_name)//"' at line "// &
                        itoa(node_line(arena, idx))//": argument count differs"
                    return
                end if
            end if
        end if
        name = trim(query%procedure_name)
    end subroutine resolve_callback_call

    subroutine resolve_generic_call(arena, idx, fallback, name, status)
        !! Replace one same-arena generic call by its unique exact procedure.
        !! All other generic resolutions are named refusal boundaries.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=*), intent(in) :: fallback
        character(len=:), allocatable, intent(out) :: name
        type(lower_status_t), intent(inout) :: status
        type(generic_call_query_t) :: query
        integer :: i
        logical :: unknown_types

        name = trim(fallback)
        query = query_generic_call(arena, idx)
        if (.not. query%found) return
        if (.not. query%is_generic) return

        if (query%selected_procedure_node_index > 0) then
            if (.not. query%is_ambiguous) then
                do i = 1, size(query%candidates)
                    if (query%candidates(i)%procedure_node_index /= &
                        query%selected_procedure_node_index) cycle
                    if (allocated(query%candidates(i)%procedure_name)) then
                        name = trim(query%candidates(i)%procedure_name)
                        return
                    end if
                end do
            end if
        end if

        if (query%is_ambiguous) then
            status%message = "ambiguous generic call '"//trim(query%generic_name)// &
                "': no derivative output"
        else
            unknown_types = .false.
            do i = 1, size(query%candidates)
                if (query%candidates(i)%has_unknown_types) then
                    unknown_types = .true.
                    exit
                end if
            end do
            if (unknown_types) then
                status%message = "unknown-type generic call '"// &
                    trim(query%generic_name)//"': no derivative output"
            else if (generic_call_has_array_actual(arena, idx)) then
                status%message = "elemental-expansion generic call '"// &
                    trim(query%generic_name)//"' is unsupported: no derivative output"
            else
                status%message = "conversion-required generic call '"// &
                    trim(query%generic_name)//"' has no exact candidate: no derivative output"
            end if
        end if
        status%ok = .false.
    end subroutine resolve_generic_call

    logical function generic_call_has_array_actual(arena, idx) result(found)
        !! Detect a rank-expanded actual for a generic with no exact match.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(resolved_type_query_t) :: resolved
        integer, allocatable :: actuals(:)
        integer :: i, value_idx

        found = .false.
        allocate (actuals(0))
        if (.not. arena%has_node_at(idx)) return
        select type (node => arena%entries(idx)%node)
            type is (subroutine_call_node)
            if (allocated(node%arg_indices)) actuals = node%arg_indices
            type is (call_or_subscript_node)
            if (allocated(node%arg_indices)) actuals = node%arg_indices
        class default
            return
        end select
        do i = 1, size(actuals)
            value_idx = actuals(i)
            if (value_idx > 0 .and. value_idx <= arena%size) then
                if (arena%has_node_at(value_idx)) then
                    select type (actual => arena%entries(value_idx)%node)
                        type is (assignment_node)
                        if (actual%value_index > 0) value_idx = actual%value_index
                    end select
                end if
            end if
            resolved = query_resolved_type(arena, value_idx)
            if (resolved%found) then
                if (resolved%rank > 0) then
                    found = .true.
                    return
                end if
            end if
        end do
    end function generic_call_has_array_actual

    logical function is_defined_operator(operator) result(found)
        character(len=*), intent(in) :: operator
        character(len=:), allocatable :: text

        text = trim(operator)
        found = .false.
        if (len(text) < 3) return
        if (text(1:1) /= "." .or. text(len(text):len(text)) /= ".") return
        select case (lower_ascii(text))
        case (".and.", ".or.", ".eqv.", ".neqv.", ".eq.", ".ne.", &
                ".lt.", ".le.", ".gt.", ".ge.")
            return
        end select
        found = .true.
    end function is_defined_operator

    function lower_ascii(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: i

        out = text
        do i = 1, len(text)
            out(i:i) = lower_char(out(i:i))
        end do
    end function lower_ascii

    logical function is_section_node(arena, idx) result(found)
        !! Whether an AST node denotes an array section rather than an element.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=:), allocatable :: node_type

        found = .false.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        node_type = trim(arena%entries(idx)%node_type)
        found = index(node_type, "array_slice") > 0 .or. &
            index(node_type, "range_subscript") > 0
    end function is_section_node

    recursive integer function lower_array_section(arena, idx, proc, status) &
            result(out)
        !! Lower one proven-contiguous rank-one or rank-two section into an
        !! indexed value.
        !!
        !! A range is kept as a passive textual IR argument because section
        !! bounds select storage; they are not differentiable values on the
        !! fixed execution path. The accepted base cases are deliberately
        !! narrow: no component expression, no vector or stride subscript, and
        !! only storage whose contiguity is known from the declaration.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        type(array_slice_query_t) :: slice
        type(array_bounds_query_t) :: bounds
        type(range_expression_query_t) :: range
        type(storage_query_t) :: storage
        type(declaration_binding_t) :: binding
        type(fad_expr_t) :: e
        character(len=:), allocatable :: base_name
        character(len=:), allocatable :: binding_error
        integer :: base_expr, decl_idx, bound_expr, section_rank, i

        out = 0
        status%ok = .true.
        if (trim(arena%entries(idx)%node_type) /= "array_slice") then
            call refuse_array_section(arena, idx, status)
            return
        end if

        slice = query_array_slice(arena, idx)
        if (.not. slice%found) then
            call refuse_section(arena, idx, "the frontend did not provide slice facts", &
                status)
            return
        end if
        if (slice%is_character_substring) then
            call refuse_section(arena, idx, &
                "character substrings are not array storage", status)
            return
        end if
        if (.not. allocated(slice%bounds_node_indices)) then
            call refuse_section(arena, idx, "missing section bounds", status)
            return
        end if
        section_rank = size(slice%bounds_node_indices)
        if (section_rank < 1) then
            call refuse_section(arena, idx, "the section has no dimensions", status)
            return
        end if
        if (section_rank > 2) then
            call refuse_section(arena, idx, &
                "rank greater than two is not supported", status)
            return
        end if
        if (.not. arena%has_node_at(slice%base_node_index)) then
            call refuse_section(arena, idx, &
                "the section base is not a declared object", status)
            return
        end if
        select type (base => arena%entries(slice%base_node_index)%node)
            type is (identifier_node)
            base_name = trim(base%name)
        class default
            call refuse_section(arena, idx, &
                "component or computed bases have untracked storage identity", status)
            return
        end select

        ! FortFront's storage query is keyed by the declaration node, while
        ! the section fact points at the identifier use. Resolve that use
        ! first so rank and alias facts are taken from the frontend rather
        ! than reconstructed from rendered source text.
        call resolve_identifier_binding(arena, slice%base_node_index, binding, &
            binding_error)
        if (binding%found) then
            if (binding%binding_kind == BINDING_ASSOCIATE_NAME) then
                call refuse_section(arena, idx, &
                    "ASSOCIATE or computed bases have untracked storage identity", &
                    status)
                return
            end if
            storage = query_storage(arena, binding%declaration_node_index)
        end if

        decl_idx = proc%decl_index(base_name)
        if (decl_idx <= 0) then
            call refuse_section(arena, idx, "the section base is not declared", status)
            return
        end if
        if (.not. proc%decls(decl_idx)%is_array) then
            call refuse_section(arena, idx, "the section base is not an array", status)
            return
        end if
        if (storage%found) then
            if (storage%rank /= section_rank) then
                call refuse_section(arena, idx, &
                    "section rank disagrees with FortFront storage facts", status)
                return
            end if
            if (storage%is_pointer .or. storage%is_target) then
                call refuse_section(arena, idx, &
                    "pointer/target alias storage identity is not tracked", status)
                return
            end if
            if (storage%is_module_state .or. storage%is_save_state .or. &
                storage%is_common_state) then
                call refuse_section(arena, idx, &
                    "global mutable storage identity is not tracked", status)
                return
            end if
        end if
        if (.not. section_base_contiguous(proc, decl_idx)) then
            call refuse_section(arena, idx, &
                "the section base is not declared contiguous or owning", status)
            return
        end if

        base_expr = lower_expr(arena, slice%base_node_index, proc, status)
        if (.not. status%ok) return
        e%kind = FAD_INDEX
        e%text = emit_expr(proc, base_expr)
        allocate (e%args(section_rank))
        do i = 1, section_rank
            if (section_bound_is_vector(arena, &
                slice%bounds_node_indices(i), proc)) then
                call refuse_vector_subscript(arena, idx, status)
                return
            end if
            bounds = query_array_bounds(arena, slice%bounds_node_indices(i))
            if (bounds%found) then
                if (bounds%stride_node_index > 0) then
                    call refuse_section(arena, idx, &
                        "a stride makes the section noncontiguous", status)
                    return
                end if
                if (bounds%is_assumed_size .or. bounds%is_assumed_rank) then
                    call refuse_section(arena, idx, &
                        "assumed-size or assumed-rank storage is not proven", status)
                    return
                end if
                bound_expr = lower_section_bound(arena, bounds, proc, status)
            else
                range = query_range_expression(arena, &
                    slice%bounds_node_indices(i))
                if (.not. range%found) then
                    call refuse_section(arena, idx, &
                        "the frontend did not provide bound facts", status)
                    return
                end if
                if (range%stride_node_index > 0) then
                    call refuse_section(arena, idx, &
                        "a stride makes the section noncontiguous", status)
                    return
                end if
                bound_expr = lower_range_bound(arena, range, proc, status)
            end if
            if (.not. status%ok) return
            e%args(i) = bound_expr
        end do
        out = proc%add_expr(e)
    end function lower_array_section

    logical function section_bound_is_vector(arena, idx, proc) result(found)
        !! Whether a slice dimension is an array-valued vector subscript.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(in) :: proc
        integer :: decl_idx

        found = .false.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        select type (node => arena%entries(idx)%node)
            type is (identifier_node)
            decl_idx = proc%decl_index(node%name)
            if (decl_idx > 0) found = proc%decls(decl_idx)%is_array
            type is (call_or_subscript_node)
            if (node%base_expr_index == 0) then
                decl_idx = proc%decl_index(node%name)
                if (decl_idx > 0) then
                    found = proc%decls(decl_idx)%is_array
                end if
            end if
        class default
        end select
    end function section_bound_is_vector

    integer function lower_section_bound(arena, bounds, proc, status) result(out)
        !! Keep a non-strided range as one passive IR argument, e.g. `2:n`.
        type(ast_arena_t), intent(in) :: arena
        type(array_bounds_query_t), intent(in) :: bounds
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        type(fad_expr_t) :: e
        integer :: lower, upper
        character(len=:), allocatable :: lower_text, upper_text

        out = 0
        lower_text = ""
        upper_text = ""
        if (bounds%lower_bound_node_index > 0) then
            lower = lower_expr(arena, bounds%lower_bound_node_index, proc, status)
            if (.not. status%ok) return
            lower_text = emit_expr(proc, lower)
        end if
        if (bounds%upper_bound_node_index > 0) then
            upper = lower_expr(arena, bounds%upper_bound_node_index, proc, status)
            if (.not. status%ok) return
            upper_text = emit_expr(proc, upper)
        end if
        e%kind = FAD_VAR
        e%text = lower_text//":"//upper_text
        allocate (e%args(0))
        out = proc%add_expr(e)
    end function lower_section_bound

    integer function lower_range_bound(arena, range, proc, status) result(out)
        !! Keep a parser range expression as one passive IR argument.
        type(ast_arena_t), intent(in) :: arena
        type(range_expression_query_t), intent(in) :: range
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        type(fad_expr_t) :: e
        integer :: start, finish
        character(len=:), allocatable :: start_text, finish_text

        out = 0
        start_text = ""
        finish_text = ""
        if (range%start_node_index > 0) then
            start = lower_expr(arena, range%start_node_index, proc, status)
            if (.not. status%ok) return
            start_text = emit_expr(proc, start)
        end if
        if (range%end_node_index > 0) then
            finish = lower_expr(arena, range%end_node_index, proc, status)
            if (.not. status%ok) return
            finish_text = emit_expr(proc, finish)
        end if
        e%kind = FAD_VAR
        e%text = start_text//":"//finish_text
        allocate (e%args(0))
        out = proc%add_expr(e)
    end function lower_range_bound

    logical function section_base_contiguous(proc, decl_idx) result(yes)
        !! Whether a declaration proves contiguous storage without aliases.
        type(fad_proc_t), intent(in) :: proc
        integer, intent(in) :: decl_idx
        character(len=:), allocatable :: dims

        yes = .false.
        if (decl_idx <= 0 .or. decl_idx > proc%n_decls) return
        if (proc%decls(decl_idx)%is_allocatable) then
            yes = .true.
            return
        end if
        if (proc%decls(decl_idx)%is_contiguous) then
            yes = .true.
            return
        end if
        if (.not. allocated(proc%decls(decl_idx)%dims)) return
        dims = trim(proc%decls(decl_idx)%dims)
        if (len_trim(dims) == 0) return
        if (index(dims, ":") > 0 .or. index(dims, "*") > 0) return
        ! Explicit shape is contiguous in every rank.  Assumed/deferred shape
        ! remains rejected unless FortFront copied an explicit CONTIGUOUS or
        ! allocatable ownership fact above.
        yes = .true.
    end function section_base_contiguous

    subroutine refuse_array_section(arena, idx, status)
        !! Refuse a range-subscript or otherwise unclassified section.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(lower_status_t), intent(inout) :: status

        call refuse_section(arena, idx, "section storage identity is not represented", &
            status)
    end subroutine refuse_array_section

    subroutine refuse_section(arena, idx, reason, status)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=*), intent(in) :: reason
        type(lower_status_t), intent(inout) :: status

        status%ok = .false.
        status%message = "unsupported array section at line "// &
            itoa(node_line(arena, idx))//": "//trim(reason)
    end subroutine refuse_section

    logical function has_vector_subscript(arena, arg_indices, lowered_args, proc) &
            result(found)
        !! Whether an array access receives an array-valued subscript.
        !!
        !! A vector subscript has no range node: ``x(idx)`` is represented as
        !! an identifier argument, so declaration rank must be consulted after
        !! lowering the arguments. An indexed expression is also array-valued
        !! and is conservatively refused here.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arg_indices(:), lowered_args(:)
        type(fad_proc_t), intent(in) :: proc
        integer :: i, node_idx, expr_idx, decl_idx
        character(len=:), allocatable :: name

        found = .false.
        do i = 1, size(arg_indices)
            node_idx = arg_indices(i)
            if (node_idx <= 0 .or. node_idx > arena%size) cycle
            if (.not. arena%has_node_at(node_idx)) cycle
            select type (node => arena%entries(node_idx)%node)
                type is (identifier_node)
                name = node%name
                decl_idx = proc%decl_index(name)
                if (decl_idx > 0) then
                    if (proc%decls(decl_idx)%is_array) then
                        found = .true.
                        return
                    end if
                end if
            end select

            if (i > size(lowered_args)) cycle
            expr_idx = lowered_args(i)
            if (expr_idx <= 0 .or. expr_idx > proc%n_exprs) cycle
            if (proc%exprs(expr_idx)%kind == FAD_INDEX) then
                found = .true.
                return
            end if
        end do
    end function has_vector_subscript

    subroutine refuse_vector_subscript(arena, idx, status)
        !! Refuse array-valued subscripts until storage identity is tracked.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(lower_status_t), intent(inout) :: status

        status%ok = .false.
        status%message = "unsupported vector subscript at line "// &
            itoa(node_line(arena, idx))//": section storage identity is not "// &
            "tracked"
    end subroutine refuse_vector_subscript

    recursive integer function lower_type_bound_call(arena, call_index, node, proc, status) &
            result(out)
        !! Lower one concrete same-file type-bound function call.
        !!
        !! The bounded contract is deliberately narrow: the receiver is a
        !! statically declared `type(t)` object, the binding uses PASS or
        !! NOPASS, and the implementation is a local function. A local
        !! override on an abstract/deferred parent and a statically resolved
        !! inherited binding are also accepted. A simple polymorphic receiver
        !! is expanded from FortFront's concrete dispatch-target facts; generic,
        !! unknown, empty, and incompatible target sets remain refusals.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_index
        type(call_or_subscript_node), intent(in) :: node
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        type(component_access_query_t) :: access
        type(derived_type_query_t) :: dtype
        type(derived_type_query_t) :: candidate_dtype
        type(binding_hierarchy_query_t) :: hierarchy
        type(program_unit_query_t) :: unit
        type(program_unit_query_t) :: candidate_unit
        character(len=:), allocatable :: object_type, type_name, method, impl
        character(len=:), allocatable :: receiver_name
        integer, allocatable :: args(:)
        character(len=64), allocatable :: arg_names(:)
        integer :: receiver, receiver_decl, i, j
        integer :: type_matches, function_matches
        logical :: found_type, found_function
        logical :: named_pass
        type(type_bound_call_query_t) :: dispatch

        out = 0
        access = query_component_access(arena, node%base_expr_index)
        if (.not. access%found .or. access%base_node_index <= 0) then
            call refuse_type_bound(status, node%name, &
                "the receiver is not a component access")
            return
        end if
        if (trim(arena%entries(access%base_node_index)%node_type) /= "identifier") then
            call refuse_type_bound(status, node%name, &
                "only a simple concrete receiver is supported")
            return
        end if
        select type (receiver_node => arena%entries(access%base_node_index)%node)
            type is (identifier_node)
            receiver_name = trim(receiver_node%name)
        class default
            call refuse_type_bound(status, node%name, &
                "only a simple concrete receiver is supported")
            return
        end select
        receiver_decl = proc%decl_index(receiver_name)
        if (receiver_decl <= 0) then
            call refuse_type_bound(status, node%name, &
                "the receiver is not a declared object")
            return
        end if
        if (proc%decls(receiver_decl)%is_array) then
            call refuse_type_bound(status, node%name, &
                "array receivers are unsupported")
            return
        end if
        if (proc%decls(receiver_decl)%is_allocatable) then
            call refuse_type_bound(status, node%name, &
                "allocatable receivers are unsupported")
            return
        end if
        call static_object_type(arena, access%base_node_index, proc, object_type)
        if (.not. allocated(object_type)) then
            call refuse_type_bound(status, node%name, &
                "the receiver has no statically declared type")
            return
        end if
        if (is_polymorphic_type(object_type)) then
            dispatch = query_type_bound_call(arena, call_index)
            call lower_polymorphic_function_dispatch(arena, node, proc, dispatch, &
                status, out)
            return
        end if
        type_name = canonical_type_name(object_type)
        if (.not. allocated(type_name)) then
            if (is_polymorphic_type(object_type)) then
                call refuse_type_bound(status, node%name, &
                    "the concrete type is not defined in this source")
            else
                call refuse_type_bound(status, node%name, &
                    "the receiver must be a concrete type(t) object")
            end if
            return
        end if
        if (len_trim(type_name) == 0) then
            if (is_polymorphic_type(object_type)) then
                call refuse_type_bound(status, node%name, &
                    "the concrete type is not defined in this source")
            else
                call refuse_type_bound(status, node%name, &
                    "the receiver must be a concrete type(t) object")
            end if
            return
        end if
        method = trim(access%component_name)
        found_type = .false.
        type_matches = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "derived_type") cycle
            candidate_dtype = query_derived_type(arena, i)
            if (.not. candidate_dtype%found) cycle
            if (.not. same_name(candidate_dtype%name, type_name)) cycle
            type_matches = type_matches + 1
            if (found_type) cycle
            dtype = candidate_dtype
            found_type = .true.
        end do
        if (.not. found_type) then
            call refuse_type_bound(status, method, &
                "the concrete type is not defined in this source")
            return
        end if
        if (type_matches > 1) then
            call refuse_type_bound(status, method, &
                "the concrete type name is ambiguous in this source")
            return
        end if
        hierarchy = query_type_binding_hierarchy(arena, dtype%node_index, method)
        if (.not. hierarchy%found) then
            call refuse_type_bound(status, method, "no type-bound binding")
            return
        end if
        if (hierarchy%is_ambiguous) then
            call refuse_type_bound(status, method, "ambiguous type-bound binding")
            return
        end if
        if (hierarchy%is_generic) then
            call refuse_type_bound(status, method, "generic bindings are unsupported")
            return
        end if
        if (hierarchy%is_deferred) then
            call refuse_type_bound(status, method, "deferred bindings are unsupported")
            return
        end if
        named_pass = hierarchy%pass_arg .and. allocated(hierarchy%pass_name)
        if (named_pass) named_pass = len_trim(hierarchy%pass_name) > 0
        impl = trim(hierarchy%implementation)
        if (len_trim(impl) == 0) then
            ! FortFront retains the effective binding name for implicit
            ! `procedure :: name` bindings even when no explicit alias is
            ! available in the implementation field.
            impl = trim(hierarchy%binding_name)
        end if
        if (len_trim(impl) == 0) then
            call refuse_type_bound(status, method, &
                "the binding implementation is unresolved")
            return
        end if
        found_function = .false.
        function_matches = 0
        do j = 1, arena%size
            if (.not. arena%has_node_at(j)) cycle
            if (trim(arena%entries(j)%node_type) /= "function_def") cycle
            candidate_unit = query_program_unit(arena, j)
            if (candidate_unit%found .and. same_name(candidate_unit%name, impl)) then
                function_matches = function_matches + 1
                if (found_function) cycle
                unit = candidate_unit
                found_function = .true.
            end if
        end do
        if (.not. found_function) then
            call refuse_type_bound(status, method, &
                "the binding implementation is not a same-file function")
            return
        end if
        if (function_matches > 1) then
            call refuse_type_bound(status, method, &
                "the binding implementation name is ambiguous in this source")
            return
        end if
        if (hierarchy%pass_arg) then
            receiver = lower_expr(arena, access%base_node_index, proc, status)
            if (.not. status%ok) return
            if (named_pass) then
                call lower_named_pass_arguments(arena, node%arg_indices, node%name, &
                    proc, unit, &
                    hierarchy%pass_name, receiver, args, arg_names, status)
                if (.not. status%ok) return
            else
                allocate (args(size(node%arg_indices) + 1))
                allocate (arg_names(size(node%arg_indices) + 1))
                args(1) = receiver
                arg_names(1) = ""
                do i = 1, size(node%arg_indices)
                    args(i + 1) = lower_actual(arena, node%arg_indices(i), proc, &
                        arg_names(i + 1), status)
                    if (.not. status%ok) return
                end do
            end if
        else
            ! NOPASS bindings do not receive the object expression.  Keeping
            ! the receiver out of the ordinary call is essential: the
            ! implementation's first dummy is the first explicit actual.
            allocate (args(size(node%arg_indices)))
            allocate (arg_names(size(node%arg_indices)))
            do i = 1, size(node%arg_indices)
                args(i) = lower_actual(arena, node%arg_indices(i), proc, &
                    arg_names(i), status)
                if (.not. status%ok) return
            end do
        end if
        out = proc%add_expr(expr_call(impl, args, arg_names))
    end function lower_type_bound_call

    subroutine lower_polymorphic_function_dispatch(arena, node, proc, query, &
            status, result_expr)
        !! Materialize a direct CLASS receiver function call as a structural
        !! SELECT TYPE.  The selector is passive; each concrete same-file
        !! target remains visible to inlining and to both derivative modes.
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        type(fad_proc_t), intent(inout) :: proc
        type(type_bound_call_query_t), intent(in) :: query
        type(lower_status_t), intent(inout) :: status
        integer, intent(out) :: result_expr
        type(derived_type_query_t) :: dtype
        type(binding_hierarchy_query_t) :: target_binding
        type(program_unit_query_t) :: unit
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        integer, allocatable :: args(:)
        character(len=64), allocatable :: arg_names(:)
        character(len=:), allocatable :: receiver, method, implementation
        character(len=:), allocatable :: result_type, temp
        integer :: i, ignored
        logical :: found_function

        result_expr = 0
        call validate_dispatch_query(query, status)
        if (.not. status%ok) return
        receiver = trim(query%receiver_name)
        method = trim(query%binding_name)
        if (len_trim(receiver) == 0) then
            call refuse_type_bound(status, method, "the dispatch receiver is unresolved")
            return
        end if

        result_type = ""
        do i = 1, size(query%dispatch_target_type_indices)
            dtype = query_derived_type(arena, query%dispatch_target_type_indices(i))
            if (.not. dtype%found) then
                call refuse_type_bound(status, method, "dispatch target type is unknown")
                return
            end if
            if (.not. allocated(dtype%name)) then
                call refuse_type_bound(status, method, "dispatch target type is unknown")
                return
            end if
            if (len_trim(dtype%name) == 0) then
                call refuse_type_bound(status, method, "dispatch target type is unknown")
                return
            end if
            target_binding = query_type_binding_hierarchy(arena, dtype%node_index, method)
            call validate_dispatch_target(arena, query, target_binding, &
                query%dispatch_target_implementations(i), method, unit, &
                found_function, status)
            if (.not. status%ok) return
            if (.not. found_function) then
                call refuse_type_bound(status, method, &
                    "the dispatch target is not a same-file function")
                return
            end if
            if (i == 1) then
                result_type = procedure_result_type(arena, unit)
                if (len_trim(result_type) == 0) result_type = "real(8)"
            end if
        end do

        temp = fresh_dispatch_name(proc)
        d%name = temp
        d%type_name = result_type
        ignored = proc%add_decl(d)

        s%kind = FAD_SELECT_TYPE
        s%value = proc%add_expr(expr_var(receiver))
        s%target = receiver
        ignored = proc%add_stmt(s)
        do i = 1, size(query%dispatch_target_type_indices)
            dtype = query_derived_type(arena, query%dispatch_target_type_indices(i))
            target_binding = query_type_binding_hierarchy(arena, dtype%node_index, method)
            implementation = trim(query%dispatch_target_implementations(i))
            call find_function_unit(arena, implementation, unit, found_function)
            if (.not. found_function) then
                call refuse_type_bound(status, method, &
                    "the dispatch target is not a same-file function")
                return
            end if
            call lower_dispatch_pass_arguments(arena, node%arg_indices, method, &
                proc, unit, target_binding, receiver, args, arg_names, status)
            if (.not. status%ok) return
            s%kind = FAD_TYPE_IS
            s%value = 0
            s%target = dtype%name
            ignored = proc%add_stmt(s)
            s%kind = FAD_ASSIGN
            s%target = temp
            s%value = proc%add_expr(expr_call(implementation, args, arg_names))
            ignored = proc%add_stmt(s)
        end do
        s%kind = FAD_CLASS_DEFAULT
        s%value = 0
        if (allocated(s%target)) deallocate (s%target)
        ignored = proc%add_stmt(s)
        s%kind = FAD_ASSIGN
        s%target = temp
        s%value = proc%add_expr(expr_const("0.0d0"))
        ignored = proc%add_stmt(s)
        s%kind = FAD_END_SELECT
        s%value = 0
        if (allocated(s%target)) deallocate (s%target)
        ignored = proc%add_stmt(s)
        result_expr = proc%add_expr(expr_var(temp))
    end subroutine lower_polymorphic_function_dispatch

    subroutine lower_dispatch_pass_arguments(arena, actual_indices, method, proc, &
            unit, binding, receiver, args, names, status)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:)
        character(len=*), intent(in) :: method, receiver
        type(fad_proc_t), intent(inout) :: proc
        type(program_unit_query_t), intent(in) :: unit
        type(binding_hierarchy_query_t), intent(in) :: binding
        integer, allocatable, intent(out) :: args(:)
        character(len=64), allocatable, intent(out) :: names(:)
        type(lower_status_t), intent(inout) :: status
        logical :: named_pass
        integer :: i

        named_pass = binding%pass_arg .and. allocated(binding%pass_name)
        if (named_pass) named_pass = len_trim(binding%pass_name) > 0
        if (named_pass) then
            call lower_named_pass_arguments(arena, actual_indices, method, proc, &
                unit, binding%pass_name, proc%add_expr(expr_var(receiver)), &
                args, names, status)
            return
        end if
        if (binding%pass_arg) then
            allocate (args(size(actual_indices) + 1), names(size(actual_indices) + 1))
            args(1) = proc%add_expr(expr_var(receiver))
            names(1) = ""
            do i = 1, size(actual_indices)
                args(i + 1) = lower_actual(arena, actual_indices(i), proc, &
                    names(i + 1), status)
                if (.not. status%ok) return
            end do
        else
            call lower_dispatch_actuals(arena, actual_indices, proc, args, names, status)
        end if
    end subroutine lower_dispatch_pass_arguments

    subroutine lower_polymorphic_subroutine_dispatch(arena, actual_indices, &
            proc, query, status)
        !! Materialize a direct CLASS receiver subroutine call as a structural
        !! SELECT TYPE.  The caller's normal statement list then contains one
        !! concrete same-file call per arm, ready for ordinary inlining.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:)
        type(fad_proc_t), intent(inout) :: proc
        type(type_bound_call_query_t), intent(in) :: query
        type(lower_status_t), intent(inout) :: status
        type(derived_type_query_t) :: dtype
        type(binding_hierarchy_query_t) :: target_binding
        type(program_unit_query_t) :: unit
        type(fad_stmt_t) :: s
        integer, allocatable :: args(:)
        character(len=64), allocatable :: arg_names(:)
        character(len=:), allocatable :: receiver, method, implementation
        integer :: i, ignored
        logical :: found_subroutine

        call validate_dispatch_query(query, status)
        if (.not. status%ok) return
        receiver = trim(query%receiver_name)
        method = trim(query%binding_name)
        s%kind = FAD_SELECT_TYPE
        s%value = proc%add_expr(expr_var(receiver))
        s%target = receiver
        ignored = proc%add_stmt(s)
        do i = 1, size(query%dispatch_target_type_indices)
            dtype = query_derived_type(arena, query%dispatch_target_type_indices(i))
            target_binding = query_type_binding_hierarchy(arena, dtype%node_index, method)
            implementation = trim(query%dispatch_target_implementations(i))
            call validate_dispatch_subroutine_target(arena, query, target_binding, &
                implementation, method, unit, found_subroutine, status)
            if (.not. status%ok) return
            if (.not. found_subroutine) then
                call refuse_type_bound(status, method, &
                    "the dispatch target is not a same-file subroutine")
                return
            end if
            call lower_dispatch_pass_arguments(arena, actual_indices, method, proc, &
                unit, target_binding, receiver, args, arg_names, status)
            if (.not. status%ok) return
            s%kind = FAD_TYPE_IS
            s%value = 0
            s%target = dtype%name
            ignored = proc%add_stmt(s)
            s%kind = FAD_CALL_STMT
            s%target = implementation
            s%call_args = args
            s%call_arg_names = arg_names
            ignored = proc%add_stmt(s)
        end do
        s%kind = FAD_CLASS_DEFAULT
        s%value = 0
        if (allocated(s%target)) deallocate (s%target)
        if (allocated(s%call_args)) deallocate (s%call_args)
        if (allocated(s%call_arg_names)) deallocate (s%call_arg_names)
        ignored = proc%add_stmt(s)
        s%kind = FAD_END_SELECT
        s%value = 0
        if (allocated(s%target)) deallocate (s%target)
        if (allocated(s%call_args)) deallocate (s%call_args)
        if (allocated(s%call_arg_names)) deallocate (s%call_arg_names)
        ignored = proc%add_stmt(s)
    end subroutine lower_polymorphic_subroutine_dispatch

    subroutine lower_dispatch_actuals(arena, actual_indices, proc, args, names, status)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:)
        type(fad_proc_t), intent(inout) :: proc
        integer, allocatable, intent(out) :: args(:)
        character(len=64), allocatable, intent(out) :: names(:)
        type(lower_status_t), intent(inout) :: status
        integer :: i

        allocate (args(size(actual_indices)), names(size(actual_indices)))
        names = ""
        do i = 1, size(actual_indices)
            args(i) = lower_actual(arena, actual_indices(i), proc, names(i), status)
            if (.not. status%ok) return
        end do
    end subroutine lower_dispatch_actuals

    subroutine validate_dispatch_query(query, status)
        type(type_bound_call_query_t), intent(in) :: query
        type(lower_status_t), intent(inout) :: status
        character(len=:), allocatable :: method

        method = trim(query%binding_name)
        if (.not. query%found) then
            call refuse_type_bound(status, method, &
                "dispatch target set is unknown; concrete type is not defined in this source")
            return
        end if
        if (query%is_generic .or. query%is_ambiguous) then
            call refuse_type_bound(status, method, &
                "generic or ambiguous dispatch targets are unsupported")
            return
        end if
        if (query%is_unresolved) then
            call refuse_type_bound(status, method, &
                "dispatch target set is unknown; concrete type is not defined in this source")
            return
        end if
        if (size(query%dispatch_target_type_indices) == 0) then
            if (query%is_deferred) then
                call refuse_type_bound(status, method, &
                    "deferred binding has an empty dispatch target set")
            else
                call refuse_type_bound(status, method, &
                    "dispatch target set is empty")
            end if
            return
        end if
        if (size(query%dispatch_target_type_indices) /= &
            size(query%dispatch_target_implementations)) then
            call refuse_type_bound(status, method, "dispatch target set is unknown")
        end if
    end subroutine validate_dispatch_query

    subroutine validate_dispatch_target(arena, query, binding, implementation, &
            method, unit, found_function, status)
        type(ast_arena_t), intent(in) :: arena
        type(type_bound_call_query_t), intent(in) :: query
        type(binding_hierarchy_query_t), intent(in) :: binding
        character(len=*), intent(in) :: implementation, method
        type(program_unit_query_t), intent(out) :: unit
        logical, intent(out) :: found_function
        type(lower_status_t), intent(inout) :: status

        found_function = .false.
        if (.not. binding%found) then
            call refuse_type_bound(status, method, "dispatch target binding is unknown")
            return
        end if
        if (binding%is_generic .or. binding%is_ambiguous) then
            call refuse_type_bound(status, method, &
                "generic or ambiguous dispatch target is unsupported")
            return
        end if
        if (binding%is_deferred) then
            call refuse_type_bound(status, method, &
                "deferred dispatch target is unsupported")
            return
        end if
        if (binding%pass_arg .neqv. query%pass_arg) then
            call refuse_type_bound(status, method, &
                "unsupported PASS dummy compatibility across dispatch targets")
            return
        end if
        call find_function_unit(arena, trim(implementation), unit, found_function)
    end subroutine validate_dispatch_target

    subroutine validate_dispatch_subroutine_target(arena, query, binding, &
            implementation, method, unit, found_subroutine, status)
        type(ast_arena_t), intent(in) :: arena
        type(type_bound_call_query_t), intent(in) :: query
        type(binding_hierarchy_query_t), intent(in) :: binding
        character(len=*), intent(in) :: implementation, method
        type(program_unit_query_t), intent(out) :: unit
        logical, intent(out) :: found_subroutine
        type(lower_status_t), intent(inout) :: status

        found_subroutine = .false.
        if (.not. binding%found) then
            call refuse_type_bound(status, method, "dispatch target binding is unknown")
            return
        end if
        if (binding%is_generic .or. binding%is_ambiguous) then
            call refuse_type_bound(status, method, &
                "generic or ambiguous dispatch target is unsupported")
            return
        end if
        if (binding%is_deferred) then
            call refuse_type_bound(status, method, &
                "deferred dispatch target is unsupported")
            return
        end if
        if (binding%pass_arg .neqv. query%pass_arg) then
            call refuse_type_bound(status, method, &
                "unsupported PASS dummy compatibility across dispatch targets")
            return
        end if
        call find_subroutine_unit(arena, trim(implementation), unit, found_subroutine)
    end subroutine validate_dispatch_subroutine_target

    subroutine find_function_unit(arena, name, unit, found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        type(program_unit_query_t), intent(out) :: unit
        logical, intent(out) :: found
        type(program_unit_query_t) :: candidate
        integer :: i, matches

        found = .false.
        matches = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "function_def") cycle
            candidate = query_program_unit(arena, i)
            if (.not. candidate%found) cycle
            if (.not. same_name(candidate%name, name)) cycle
            matches = matches + 1
            if (matches == 1) unit = candidate
        end do
        found = matches == 1
    end subroutine find_function_unit

    function procedure_result_type(arena, unit) result(type_name)
        type(ast_arena_t), intent(in) :: arena
        type(program_unit_query_t), intent(in) :: unit
        type(declaration_query_t) :: declaration
        character(len=:), allocatable :: type_name
        integer :: i

        type_name = ""
        if (allocated(unit%return_type)) type_name = trim(unit%return_type)
        if (.not. allocated(unit%declaration_indices)) return
        do i = 1, size(unit%declaration_indices)
            declaration = query_declaration(arena, unit%declaration_indices(i))
            if (.not. declaration%found) cycle
            if (.not. allocated(unit%result_name)) cycle
            if (.not. allocated(declaration%name)) cycle
            if (.not. same_name(declaration%name, unit%result_name)) cycle
            if (allocated(declaration%type_name)) type_name = declaration%type_name
            return
        end do
    end function procedure_result_type

    subroutine find_subroutine_unit(arena, name, unit, found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        type(program_unit_query_t), intent(out) :: unit
        logical, intent(out) :: found
        type(program_unit_query_t) :: candidate
        integer :: i, matches

        found = .false.
        matches = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "subroutine_def") cycle
            candidate = query_program_unit(arena, i)
            if (.not. candidate%found) cycle
            if (.not. same_name(candidate%name, name)) cycle
            matches = matches + 1
            if (matches == 1) unit = candidate
        end do
        found = matches == 1
    end subroutine find_subroutine_unit

    function fresh_dispatch_name(proc) result(name)
        type(fad_proc_t), intent(in) :: proc
        character(len=:), allocatable :: name
        integer :: i

        do i = 1, 10000
            name = "fad_dispatch_result_"//itoa(i)
            if (proc%decl_index(name) == 0) return
        end do
        name = "fad_dispatch_result"
    end function fresh_dispatch_name

    subroutine lower_type_bound_subroutine(arena, idx, proc, s, status)
        !! Lower one concrete same-file type-bound subroutine call.  Explicit
        !! CALL nodes carry the receiver and binding as one designator name;
        !! FortFront's resolved query supplies the same PASS/NOPASS facts that
        !! function-call lowering obtains from the component-access node.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(fad_stmt_t), intent(inout) :: s
        type(lower_status_t), intent(inout) :: status
        type(derived_type_query_t) :: dtype, candidate_dtype
        type(binding_hierarchy_query_t) :: hierarchy
        type(program_unit_query_t) :: unit
        type(program_unit_query_t) :: candidate_unit
        character(len=:), allocatable :: object_type, type_name, method, impl
        character(len=:), allocatable :: receiver_name
        integer, allocatable :: actual_indices(:), args(:)
        character(len=64), allocatable :: arg_names(:)
        integer :: receiver, receiver_decl, i, matches, separator
        integer :: type_matches
        logical :: named_pass, found_subroutine
        type(type_bound_call_query_t) :: dispatch

        select type (node => arena%entries(idx)%node)
            type is (subroutine_call_node)
            separator = index(trim(node%name), "%")
            if (separator <= 1 .or. separator >= len_trim(node%name)) then
                call refuse_type_bound(status, node%name, &
                    "the receiver or binding is unresolved")
                return
            end if
            receiver_name = trim(node%name(:separator - 1))
            method = trim(node%name(separator + 1:))
            if (allocated(node%arg_indices)) then
                actual_indices = node%arg_indices
            else
                allocate (actual_indices(0))
            end if
        class default
            call refuse_type_bound(status, "<unknown>", &
                "the call node is not a subroutine")
            return
        end select
        receiver_decl = proc%decl_index(receiver_name)
        if (receiver_decl <= 0) then
            call refuse_type_bound(status, method, &
                "the receiver is not a declared object")
            return
        end if
        if (proc%decls(receiver_decl)%is_array) then
            call refuse_type_bound(status, method, &
                "array receivers are unsupported")
            return
        end if
        if (proc%decls(receiver_decl)%is_allocatable) then
            call refuse_type_bound(status, method, &
                "allocatable receivers are unsupported")
            return
        end if
        if (.not. allocated(proc%decls(receiver_decl)%type_name)) then
            call refuse_type_bound(status, method, &
                "the receiver has no statically declared type")
            return
        end if
        object_type = proc%decls(receiver_decl)%type_name
        if (is_polymorphic_type(object_type)) then
            dispatch = query_type_bound_call(arena, idx)
            call lower_polymorphic_subroutine_dispatch(arena, actual_indices, &
                proc, dispatch, status)
            if (status%ok) s%kind = 0
            return
        end if
        type_name = canonical_type_name(object_type)
        if (.not. allocated(type_name)) then
            call refuse_type_bound(status, method, &
                "the receiver must be a concrete type(t) object")
            return
        end if

        type_matches = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "derived_type") cycle
            candidate_dtype = query_derived_type(arena, i)
            if (.not. candidate_dtype%found) cycle
            if (.not. same_name(candidate_dtype%name, type_name)) cycle
            type_matches = type_matches + 1
            if (type_matches == 1) dtype = candidate_dtype
        end do
        if (type_matches == 0) then
            call refuse_type_bound(status, method, &
                "the concrete type is not defined in this source")
            return
        end if
        if (type_matches > 1) then
            call refuse_type_bound(status, method, &
                "the concrete type name is ambiguous in this source")
            return
        end if
        hierarchy = query_type_binding_hierarchy(arena, dtype%node_index, method)
        if (.not. hierarchy%found) then
            call refuse_type_bound(status, method, "no type-bound binding")
            return
        end if
        if (hierarchy%is_ambiguous) then
            call refuse_type_bound(status, method, "ambiguous type-bound binding")
            return
        end if
        if (hierarchy%is_generic) then
            call refuse_type_bound(status, method, "generic bindings are unsupported")
            return
        end if
        if (hierarchy%is_deferred) then
            call refuse_type_bound(status, method, "deferred bindings are unsupported")
            return
        end if
        impl = trim(hierarchy%implementation)
        if (len_trim(impl) == 0) impl = trim(hierarchy%binding_name)
        found_subroutine = .false.
        matches = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "subroutine_def") cycle
            candidate_unit = query_program_unit(arena, i)
            if (.not. candidate_unit%found) cycle
            if (.not. same_name(candidate_unit%name, impl)) cycle
            matches = matches + 1
            if (found_subroutine) cycle
            unit = candidate_unit
            found_subroutine = .true.
        end do
        if (.not. found_subroutine) then
            call refuse_type_bound(status, method, &
                "the binding implementation is not a same-file subroutine")
            return
        end if
        if (matches > 1) then
            call refuse_type_bound(status, method, &
                "the binding implementation name is ambiguous in this source")
            return
        end if

        receiver = proc%add_expr(expr_var(receiver_name))
        named_pass = hierarchy%pass_arg .and. allocated(hierarchy%pass_name)
        if (named_pass) named_pass = len_trim(hierarchy%pass_name) > 0
        if (hierarchy%pass_arg) then
            if (named_pass) then
                call lower_named_pass_arguments(arena, actual_indices, method, proc, &
                    unit, hierarchy%pass_name, receiver, args, arg_names, status)
                if (.not. status%ok) return
            else
                allocate (args(size(actual_indices) + 1))
                allocate (arg_names(size(actual_indices) + 1))
                args(1) = receiver
                arg_names(1) = ""
                do i = 1, size(actual_indices)
                    args(i + 1) = lower_actual(arena, actual_indices(i), proc, &
                        arg_names(i + 1), status)
                    if (.not. status%ok) return
                end do
            end if
        else
            allocate (args(size(actual_indices)))
            allocate (arg_names(size(actual_indices)))
            do i = 1, size(actual_indices)
                args(i) = lower_actual(arena, actual_indices(i), proc, &
                    arg_names(i), status)
                if (.not. status%ok) return
            end do
        end if
        s%kind = FAD_CALL_STMT
        s%target = impl
        s%call_args = args
        s%call_arg_names = arg_names
    end subroutine lower_type_bound_subroutine

    subroutine lower_named_pass_arguments(arena, actual_indices, call_name, &
            proc, unit, pass_name, &
            receiver, args, arg_names, status)
        !! Normalize a named-PASS call to keyword actuals in implementation
        !! dummy order. This keeps positional actuals legal when the passed
        !! object dummy is not first, and gives the inliner the same formal
        !! mapping as the Fortran call.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:)
        character(len=*), intent(in) :: call_name
        type(fad_proc_t), intent(inout) :: proc
        type(program_unit_query_t), intent(in) :: unit
        character(len=*), intent(in) :: pass_name
        integer, intent(in) :: receiver
        integer, allocatable, intent(out) :: args(:)
        character(len=64), allocatable, intent(out) :: arg_names(:)
        type(lower_status_t), intent(inout) :: status
        type(declaration_query_t) :: decl
        integer, allocatable :: actuals(:), formal_actual(:)
        character(len=64), allocatable :: formal_names(:)
        character(len=64) :: keyword
        integer :: n_formal, n_actual, pass_formal
        integer :: i, j, formal, next_formal
        logical :: named

        n_formal = 0
        if (allocated(unit%parameter_indices)) n_formal = size(unit%parameter_indices)
        n_actual = size(actual_indices)
        allocate (formal_names(n_formal))
        formal_names = ""
        pass_formal = 0
        do i = 1, n_formal
            decl = query_declaration(arena, unit%parameter_indices(i))
            if (decl%found) then
                if (allocated(decl%name)) formal_names(i) = trim(decl%name)
            end if
            if (same_name(formal_names(i), pass_name)) pass_formal = i
        end do
        if (pass_formal == 0) then
            call refuse_type_bound(status, call_name, &
                "named PASS dummy is not present in the implementation")
            return
        end if
        if (n_actual > n_formal - 1) then
            call refuse_type_bound(status, call_name, &
                "named PASS call has too many explicit actuals")
            return
        end if

        allocate (actuals(n_actual), formal_actual(n_formal))
        formal_actual = 0
        next_formal = 1
        do i = 1, n_actual
            actuals(i) = lower_actual(arena, actual_indices(i), proc, keyword, status)
            if (.not. status%ok) return
            named = len_trim(keyword) > 0
            if (named) then
                formal = 0
                do j = 1, n_formal
                    if (same_name(keyword, formal_names(j))) then
                        formal = j
                        exit
                    end if
                end do
                if (formal == pass_formal .or. formal == 0) then
                    call refuse_type_bound(status, call_name, &
                        "named PASS call has an invalid keyword actual")
                    return
                end if
            else
                do while (next_formal <= n_formal)
                    if (next_formal /= pass_formal .and. &
                        formal_actual(next_formal) == 0) exit
                    next_formal = next_formal + 1
                end do
                formal = next_formal
                next_formal = next_formal + 1
            end if
            if (formal <= 0 .or. formal > n_formal) then
                call refuse_type_bound(status, call_name, &
                    "named PASS call has an unknown actual")
                return
            end if
            if (formal_actual(formal) /= 0) then
                call refuse_type_bound(status, call_name, &
                    "named PASS call has a duplicate actual")
                return
            end if
            formal_actual(formal) = i
        end do

        allocate (args(n_actual + 1), arg_names(n_actual + 1))
        j = 0
        do formal = 1, n_formal
            if (formal == pass_formal) then
                j = j + 1
                args(j) = receiver
                arg_names(j) = formal_names(formal)
            else if (formal_actual(formal) > 0) then
                j = j + 1
                args(j) = actuals(formal_actual(formal))
                arg_names(j) = formal_names(formal)
            end if
        end do
    end subroutine lower_named_pass_arguments

    logical function is_type_bound_reference(arena, base_idx, proc) result(found)
        !! Distinguish ``object%binding(args)`` from an array component
        !! ``object%values(i)`` before lowering either one.  A local binding is
        !! enough to route the former through the type-bound lowering and its
        !! explicit refusal diagnostics (runtime dispatch, generic, deferred,
        !! and ambiguous bindings).
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: base_idx
        type(fad_proc_t), intent(in) :: proc
        type(component_access_query_t) :: access
        type(derived_type_query_t) :: dtype
        type(type_binding_query_t) :: binding
        character(len=:), allocatable :: object_type, type_name
        integer :: i, j

        found = .false.
        access = query_component_access(arena, base_idx)
        if (.not. access%found) return
        if (access%base_node_index <= 0) return
        if (access%base_node_index > arena%size) return
        if (.not. arena%has_node_at(access%base_node_index)) return
        if (trim(arena%entries(access%base_node_index)%node_type) /= &
            "identifier") return
        call static_object_type(arena, access%base_node_index, proc, object_type)
        if (.not. allocated(object_type)) return
        type_name = canonical_type_name(object_type)
        if (.not. allocated(type_name)) then
            found = is_polymorphic_type(object_type)
            return
        end if
        if (len_trim(type_name) == 0) then
            found = is_polymorphic_type(object_type)
            return
        end if
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "derived_type") cycle
            dtype = query_derived_type(arena, i)
            if (.not. dtype%found .or. .not. same_name(dtype%name, type_name)) cycle
            found = type_has_binding(arena, type_name, access%component_name)
            return
        end do
    end function is_type_bound_reference

    recursive logical function type_has_binding(arena, type_name, method) &
            result(found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_name, method
        type(derived_type_query_t) :: dtype
        type(type_binding_query_t) :: binding
        character(len=:), allocatable :: parent
        integer :: i, j

        found = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "derived_type") cycle
            dtype = query_derived_type(arena, i)
            if (.not. dtype%found .or. .not. same_name(dtype%name, type_name)) cycle
            if (allocated(dtype%binding_indices)) then
                do j = 1, size(dtype%binding_indices)
                    binding = query_type_binding(arena, dtype%binding_indices(j))
                    if (binding%found .and. same_name(binding%binding_name, method)) then
                        found = .true.
                        return
                    end if
                end do
            end if
            if (allocated(dtype%extends_parent)) then
                parent = trim(dtype%extends_parent)
                if (len_trim(parent) > 0) then
                    found = type_has_binding(arena, parent, method)
                end if
            end if
            return
        end do
    end function type_has_binding

    subroutine refuse_type_bound(status, name, reason)
        type(lower_status_t), intent(inout) :: status
        character(len=*), intent(in) :: name, reason

        status%ok = .false.
        status%message = "unsupported type-bound call '"//trim(name)//"': "//trim(reason)
    end subroutine refuse_type_bound

    recursive subroutine static_object_type(arena, idx, proc, type_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(in) :: proc
        character(len=:), allocatable, intent(out) :: type_name
        integer :: i

        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        select type (id => arena%entries(idx)%node)
            type is (identifier_node)
            do i = 1, proc%n_decls
                if (.not. allocated(proc%decls(i)%name)) cycle
                if (.not. same_name(proc%decls(i)%name, id%name)) cycle
                if (allocated(proc%decls(i)%type_name)) then
                    type_name = proc%decls(i)%type_name
                end if
                return
            end do
        class default
        end select
    end subroutine static_object_type

    function canonical_type_name(raw) result(name)
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: name
        character(len=:), allocatable :: compact
        integer :: i

        compact = ""
        do i = 1, len_trim(raw)
            if (raw(i:i) == " " .or. raw(i:i) == achar(9)) cycle
            compact = compact//lower_char(raw(i:i))
        end do
        if (len(compact) < 7) return
        if (compact(:5) /= "type(") return
        if (compact(len(compact):len(compact)) /= ")") return
        if (len(compact) <= 6) return
        name = compact(6:len(compact) - 1)
    end function canonical_type_name

    logical function is_polymorphic_type(raw) result(found)
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: compact
        integer :: i

        compact = ""
        do i = 1, len_trim(raw)
            if (raw(i:i) == " " .or. raw(i:i) == achar(9)) cycle
            compact = compact//lower_char(raw(i:i))
        end do
        found = .false.
        if (len(compact) >= 6) then
            found = compact(:6) == "class("
        end if
    end function is_polymorphic_type

    logical function same_name(a, b) result(equal)
        character(len=*), intent(in) :: a, b
        integer :: i

        equal = len_trim(a) == len_trim(b)
        if (.not. equal) return
        do i = 1, len_trim(a)
            if (lower_char(a(i:i)) /= lower_char(b(i:i))) then
                equal = .false.
                return
            end if
        end do
    end function same_name

    character function lower_char(c)
        character, intent(in) :: c

        lower_char = c
        if (c >= "A" .and. c <= "Z") lower_char = achar(iachar(c) + 32)
    end function lower_char

    recursive function render_component_access(arena, idx, proc, status) &
            result(text)
        !! Render a component chain through FortFront's compiler-facing query.
        use fortad_emit, only: emit_expr
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        character(len=:), allocatable :: text
        type(component_access_query_t) :: query
        character(len=:), allocatable :: base
        integer :: base_index

        text = ""
        query = query_component_access(arena, idx)
        if (.not. query%found) then
            status%ok = .false.
            status%message = "unsupported component access at line "// &
                itoa(node_line(arena, idx))
            return
        end if
        if (query%base_node_index <= 0 .or. query%base_node_index > &
            arena%size) then
            status%ok = .false.
            status%message = "unsupported component access at line "// &
                itoa(node_line(arena, idx))
            return
        end if
        if (.not. arena%has_node_at(query%base_node_index)) then
            status%ok = .false.
            status%message = "unsupported component access at line "// &
                itoa(node_line(arena, idx))
            return
        end if
        if (trim(arena%entries(query%base_node_index)%node_type) == &
            "component_access") then
            base = render_component_access(arena, query%base_node_index, &
                proc, status)
        else
            base_index = lower_expr(arena, query%base_node_index, proc, status)
            if (.not. status%ok) return
            base = emit_expr(proc, base_index)
        end if
        if (.not. status%ok) return
        text = base//"%"//trim(query%component_name)
    end function render_component_access

    function component_reference_text(arena, base_idx, name, proc, status) &
            result(text)
        !! Render a component reference whose parser node may already include
        !! the final component.  FortFront represents both ``s%inner%q`` and
        !! ``s%values(i)`` with a component base plus a call/subscript node;
        !! appending the name unconditionally would produce ``values%values``.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: base_idx
        character(len=*), intent(in) :: name
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        character(len=:), allocatable :: text, base, tail

        base = render_component_access(arena, base_idx, proc, status)
        if (.not. status%ok) then
            text = ""
            return
        end if
        tail = "%"//trim(name)
        if (len_trim(base) >= len_trim(tail)) then
            if (same_name(base(len_trim(base) - len_trim(tail) + 1:len_trim(base)), &
                tail)) then
                text = base
                return
            end if
        end if
        text = base//tail
    end function component_reference_text

    subroutine validate_component_reference(arena, idx, status)
        !! Validate a component designator using FortFront's resolved path and
        !! storage facts.  The bounded allocatable-component slice accepts only
        !! one scalar REAL component element of a concrete, non-aliased object;
        !! its descriptor must already be allocated by the caller.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(lower_status_t), intent(inout) :: status
        type(component_path_query_t) :: path
        type(storage_query_t) :: storage, base_storage
        type(declaration_query_t) :: component_declaration
        integer :: path_idx, terminal_idx, component_rank
        logical :: is_component, whole

        call component_reference_parts(arena, idx, path_idx, whole, is_component)
        if (.not. is_component) return

        path = query_component_path(arena, path_idx)
        ! The bounded allocatable-component slice is opt-in: older or
        ! intentionally opaque component paths continue through the existing
        ! lowering rules, while a resolved path gets the stronger ownership
        ! checks below.
        if (.not. path%found) return
        storage = query_storage(arena, path_idx)
        if (.not. storage%found) return
        if (.not. storage%is_allocatable) return
        if (storage%is_polymorphic .or. storage%is_unlimited_polymorphic) return
        base_storage = query_storage(arena, path%base_node_index)
        if (base_storage%found .and. (base_storage%is_polymorphic .or. &
            base_storage%is_unlimited_polymorphic)) return
        if (.not. component_type_is_real(storage%type_name)) then
            call refuse_component(arena, idx, &
                "only concrete REAL allocatable components are supported", status)
            return
        end if
        ! The public component-path result carries the base storage class;
        ! the base AST node itself is not necessarily a storage-query node
        ! (for example, a resolved dummy designator).  Use detailed base
        ! facts when query_storage has them, and the resolved path class for
        ! the ownership boundary.
        if (path%base_storage_class == STORAGE_POINTER .or. &
            (base_storage%found .and. base_storage%is_pointer)) then
            call refuse_component(arena, idx, &
                "pointer base storage identity is not tracked", status)
            return
        end if
        if (base_storage%found .and. base_storage%is_target) then
            call refuse_component(arena, idx, &
                "TARGET alias storage identity is not tracked", status)
            return
        end if
        if (base_storage%found .and. (base_storage%is_polymorphic .or. &
            base_storage%is_unlimited_polymorphic)) then
            call refuse_component(arena, idx, &
                "polymorphic component bases are not supported", status)
            return
        end if
        if (path%base_storage_class == STORAGE_MODULE .or. &
            path%base_storage_class == STORAGE_SAVE .or. &
            path%base_storage_class == STORAGE_COMMON .or. &
            (base_storage%found .and. &
            (base_storage%is_module_state .or. base_storage%is_save_state .or. &
            base_storage%is_common_state))) then
            call refuse_component(arena, idx, &
                "global mutable component storage is not supported", status)
            return
        end if
        if (storage%is_pointer) then
            call refuse_component(arena, idx, &
                "pointer component storage identity is not tracked", status)
            return
        end if
        if (storage%is_target) then
            call refuse_component(arena, idx, &
                "TARGET component alias storage identity is not tracked", status)
            return
        end if
        if (storage%is_polymorphic .or. storage%is_unlimited_polymorphic) then
            call refuse_component(arena, idx, &
                "polymorphic components are not supported", status)
            return
        end if
        component_rank = storage%rank
        if (allocated(path%component_declaration_indices)) then
            if (size(path%component_declaration_indices) > 0) then
                terminal_idx = path%component_declaration_indices( &
                    size(path%component_declaration_indices))
                component_declaration = query_declaration(arena, terminal_idx)
                if (component_declaration%found) then
                    if (component_declaration%is_array) then
                        if (allocated(component_declaration%dimension_indices)) then
                            component_rank = size(component_declaration%dimension_indices)
                        end if
                    else
                        component_rank = 0
                    end if
                end if
            end if
        end if
        if (component_rank > 2) then
            call refuse_component(arena, idx, &
                "allocatable component rank greater than two is not supported", status)
            return
        end if
        if (whole) then
            call refuse_component(arena, idx, &
                "whole allocatable component assignment/read is not supported; use one element", &
                status)
            return
        end if
    end subroutine validate_component_reference

    subroutine component_reference_parts(arena, idx, path_idx, whole, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer, intent(out) :: path_idx
        logical, intent(out) :: whole, found

        path_idx = 0
        whole = .false.
        found = .false.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        if (trim(arena%entries(idx)%node_type) == "component_access") then
            path_idx = idx
            whole = .true.
            found = .true.
            return
        end if
        select type (node => arena%entries(idx)%node)
            type is (call_or_subscript_node)
            if (node%base_expr_index <= 0) return
            if (.not. is_component_base(arena, node%base_expr_index)) return
            path_idx = node%base_expr_index
            if (allocated(node%arg_indices)) then
                whole = size(node%arg_indices) == 0
            else
                whole = .true.
            end if
            found = .true.
        class default
        end select
    end subroutine component_reference_parts

    subroutine annotate_component_expr(arena, idx, expr)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_expr_t), intent(inout) :: expr
        type(component_path_query_t) :: path
        type(storage_query_t) :: storage, base_storage
        integer :: path_idx
        logical :: whole, found

        call component_reference_parts(arena, idx, path_idx, whole, found)
        if (.not. found) return
        path = query_component_path(arena, path_idx)
        if (.not. path%found) return
        storage = query_storage(arena, path_idx)
        if (.not. storage%found) return
        expr%is_component_path = .true.
        expr%component_is_allocatable = storage%is_allocatable
        expr%component_is_pointer = storage%is_pointer
        expr%component_is_target = storage%is_target
        expr%component_is_polymorphic = storage%is_polymorphic .or. &
            storage%is_unlimited_polymorphic
        expr%component_is_global = storage%is_module_state .or. &
            storage%is_save_state .or. storage%is_common_state
        expr%component_rank = storage%rank
        expr%component_type_name = storage%type_name
        base_storage = query_storage(arena, path%base_node_index)
        if (base_storage%found) then
            expr%component_is_global = expr%component_is_global .or. &
                base_storage%is_module_state .or. base_storage%is_save_state .or. &
                base_storage%is_common_state
        end if
        expr%component_is_real = component_type_is_real(storage%type_name)
    end subroutine annotate_component_expr

    logical function component_type_is_real(type_name) result(found)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: compact
        integer :: i

        compact = ""
        do i = 1, len_trim(type_name)
            if (type_name(i:i) == " " .or. type_name(i:i) == achar(9)) cycle
            compact = compact//lower_char(type_name(i:i))
        end do
        found = index(compact, "real") == 1
    end function component_type_is_real

    subroutine refuse_component(arena, idx, reason, status)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=*), intent(in) :: reason
        type(lower_status_t), intent(inout) :: status

        status%ok = .false.
        status%message = "unsupported component path at line "// &
            itoa(node_line(arena, idx))//": "//trim(reason)
    end subroutine refuse_component

    logical function is_component_base(arena, idx) result(yes)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        yes = .false.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        yes = trim(arena%entries(idx)%node_type) == "component_access"
    end function is_component_base

    logical function is_array_name(proc, name) result(yes)
        !! True when `name` was declared as an array in this procedure.
        type(fad_proc_t), intent(in) :: proc
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        i = proc%decl_index(name)
        if (i > 0) yes = proc%decls(i)%is_array
    end function is_array_name

    integer function node_line(arena, idx) result(line)
        !! Source line of an arena node, 0 when unknown.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        line = 0
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        line = arena%entries(idx)%node%line
    end function node_line

    function itoa(n) result(s)
        !! Integer to trimmed decimal text.
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=32) :: buf

        write (buf, '(i0)') n
        s = trim(buf)
    end function itoa

end module fortad_lower_statements
