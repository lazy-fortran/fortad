module fortad_lower_statements
    !! Lower statement and expression trees from fortfront into fortad IR.
    use fortfront, only: ast_arena_t, module_node, assignment_node, &
        binary_op_node, identifier_node, literal_node, call_or_subscript_node, &
        declaration_node, do_loop_node, if_node, parameter_declaration_node, &
        subroutine_call_node, use_statement_node, comment_node, &
        pointer_assignment_node, &
        get_select_type_info, get_type_guard_info, component_access_query_t, &
        query_component_access, query_derived_type, query_type_binding, &
        derived_type_query_t, type_binding_query_t, query_program_unit, &
        program_unit_query_t
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
        expr_const, expr_var, expr_binop, expr_call, fad_base_name, &
        FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, &
        FAD_END_IF, FAD_VAR, FAD_INDEX, FAD_CALL_STMT, FAD_INTENT_NONE, &
        FAD_INTENT_IN, FAD_INTENT_OUT, FAD_INTENT_INOUT, &
        FAD_SELECT_TYPE, FAD_TYPE_IS, FAD_CLASS_IS, FAD_CLASS_DEFAULT, &
        FAD_END_SELECT
    use fortad_lower_types, only: lower_status_t
    use fortad_use_store, only: ensure_use_capacity
    implicit none
    private

    public :: lower_body
    public :: inherit_module_uses

contains

    recursive subroutine lower_body(arena, body_indices, proc, status)
        !! Lower a statement list.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        integer :: i

        status%ok = .true.
        do i = 1, size(body_indices)
            call lower_stmt(arena, body_indices(i), proc, status)
            if (.not. status%ok) return
        end do
    end subroutine lower_body

    recursive subroutine lower_stmt(arena, idx, proc, status)
        !! Lower one statement, or refuse it by name.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        integer :: ignored, k

        status%ok = .true.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. arena%has_node_at(idx)) return
        if (trim(arena%entries(idx)%node_type) == "select_type") then
            call lower_select_type(arena, idx, proc, status)
            return
        end if
        select type (n => arena%entries(idx)%node)
            type is (comment_node)
            return

            type is (use_statement_node)
            call add_use(proc, n)

            type is (declaration_node)
            if (n%is_pointer .or. n%is_target) then
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
                    call fill_decl(n, trim(n%var_names(k)), arena, d)
                    ignored = proc%add_decl(d)
                end do
            else
                call fill_decl(n, n%var_name, arena, d)
                ignored = proc%add_decl(d)
            end if

            type is (assignment_node)
            s%kind = FAD_ASSIGN
            s%line = n%line
            call lower_target(arena, n%target_index, proc, s, status)
            if (.not. status%ok) return
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
            call lower_body(arena, n%then_body_indices, proc, status)
            if (.not. status%ok) return
            if (allocated(n%else_body_indices)) then
                if (size(n%else_body_indices) > 0) then
                    block
                        type(fad_stmt_t) :: e
                        e%kind = FAD_ELSE
                        ignored = proc%add_stmt(e)
                    end block
                    call lower_body(arena, n%else_body_indices, proc, status)
                    if (.not. status%ok) return
                end if
            end if
            block
                type(fad_stmt_t) :: e
                e%kind = FAD_END_IF
                ignored = proc%add_stmt(e)
            end block

            type is (subroutine_call_node)
            s%kind = FAD_CALL_STMT
            s%line = n%line
            s%target = n%name
            block
                integer, allocatable :: cargs(:)
                integer :: ci
                allocate (cargs(size(n%arg_indices)))
                do ci = 1, size(n%arg_indices)
                    cargs(ci) = lower_expr(arena, n%arg_indices(ci), proc, status)
                    if (.not. status%ok) return
                end do
                s%call_args = cargs
            end block
            ignored = proc%add_stmt(s)

            type is (pointer_assignment_node)
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
            call lower_body(arena, n%body_indices, proc, status)
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
        integer :: selector, default_index, type_index, i, ignored

        status%ok = .true.
        call get_select_type_info(arena, idx, selector, guards, default_index)
        if (selector <= 0) then
            status%ok = .false.
            status%message = "select type at line "//itoa(node_line(arena, idx))// &
                " has no selector"
            return
        end if

        s%kind = FAD_SELECT_TYPE
        s%value = lower_expr(arena, selector, proc, status)
        if (.not. status%ok) return
        ignored = proc%add_stmt(s)

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
            call lower_body(arena, body, proc, status)
            if (.not. status%ok) return
        end do

        if (default_index > 0) then
            call get_type_guard_info(arena, default_index, guard_kind, &
                type_index, body)
            s%kind = FAD_CLASS_DEFAULT
            s%value = 0
            if (allocated(s%target)) deallocate (s%target)
            ignored = proc%add_stmt(s)
            call lower_body(arena, body, proc, status)
            if (.not. status%ok) return
        end if

        s%kind = FAD_END_SELECT
        s%value = 0
        ignored = proc%add_stmt(s)
    end subroutine lower_select_type

    subroutine fill_decl(n, name, arena, d)
        !! Translate a fortfront declaration node into a fortad declaration.
        type(declaration_node), intent(in) :: n
        character(len=*), intent(in) :: name
        type(ast_arena_t), intent(in) :: arena
        type(fad_decl_t), intent(out) :: d

        d%name = name
        d%type_name = n%type_name
        d%is_value = n%is_value
        d%is_optional = n%is_optional
        if (n%has_kind .and. n%kind_value > 0) then
            d%type_name = n%type_name//"("//itoa(n%kind_value)//")"
        end if
        d%is_array = n%is_array
        d%is_contiguous = n%is_contiguous
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
        if (n%is_array .and. allocated(n%dimension_indices)) then
            d%dims = dims_text(arena, n%dimension_indices)
        end if
    end subroutine fill_decl

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

        if (idx <= 0 .or. idx > arena%size) then
            status%ok = .false.
            status%message = "empty assignment target"
            return
        end if
        if (is_section_node(arena, idx)) then
            call refuse_array_section(arena, idx, status)
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
        type(fad_expr_t) :: e
        integer :: i

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

        if (is_section_node(arena, idx)) then
            call refuse_array_section(arena, idx, status)
            return
        end if

        if (trim(arena%entries(idx)%node_type) == "component_access") then
            block
                character(len=:), allocatable :: component
                component = render_component_access(arena, idx, proc, status)
                if (.not. status%ok) return
                out = proc%add_expr(expr_var(component))
            end block
            return
        end if

        select type (n => arena%entries(idx)%node)
            type is (identifier_node)
            out = proc%add_expr(expr_var(n%name))
            type is (literal_node)
            out = proc%add_expr(expr_const(n%value))
            type is (binary_op_node)
            block
                integer :: l, r
                l = lower_expr(arena, n%left_index, proc, status)
                if (.not. status%ok) return
                r = lower_expr(arena, n%right_index, proc, status)
                if (.not. status%ok) return
                out = proc%add_expr(expr_binop(trim(n%operator), l, r))
            end block
            type is (call_or_subscript_node)
            allocate (args(size(n%arg_indices)))
            do i = 1, size(n%arg_indices)
                args(i) = lower_expr(arena, n%arg_indices(i), proc, status)
                if (.not. status%ok) return
            end do
            if (n%base_expr_index > 0) then
                if (is_component_base(arena, n%base_expr_index)) then
                    if (is_type_bound_reference(arena, n%base_expr_index, proc)) then
                        out = lower_type_bound_call(arena, n, proc, status)
                        if (.not. status%ok) return
                    else if (size(n%arg_indices) == 0) then
                        e%kind = FAD_VAR
                        e%text = component_reference_text(arena, &
                            n%base_expr_index, n%name, proc, status)
                        if (.not. status%ok) return
                        out = proc%add_expr(e)
                    else
                        e%kind = FAD_INDEX
                        e%text = component_reference_text(arena, &
                            n%base_expr_index, n%name, proc, status)
                        if (.not. status%ok) return
                        e%args = args
                        out = proc%add_expr(e)
                    end if
                else
                    out = lower_type_bound_call(arena, n, proc, status)
                    if (.not. status%ok) return
                end if
            else if (is_array_name(proc, n%name)) then
                e%kind = FAD_INDEX
                e%text = n%name
                e%args = args
                out = proc%add_expr(e)
            else
                out = proc%add_expr(expr_call(n%name, args))
            end if
        class default
            status%ok = .false.
            status%message = "unsupported expression at line "// &
                itoa(node_line(arena, idx))
        end select
    end function lower_expr

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

    subroutine refuse_array_section(arena, idx, status)
        !! Refuse non-element sections until storage identity is tracked.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(lower_status_t), intent(inout) :: status

        status%ok = .false.
        status%message = "unsupported array section at line "// &
            itoa(node_line(arena, idx))//": noncontiguous and overlapping "// &
            "storage identity is not tracked"
    end subroutine refuse_array_section

    recursive integer function lower_type_bound_call(arena, node, proc, status) &
            result(out)
        !! Lower one concrete same-file type-bound function call.
        !!
        !! The bounded contract is deliberately narrow: the receiver is a
        !! statically declared `type(t)` object, the binding uses the default
        !! implicit PASS argument, and the implementation is a local function.
        !! Runtime dispatch, inherited bindings, named PASS, NOPASS, generic,
        !! and deferred bindings remain explicit refusal cases.
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(inout) :: status
        type(component_access_query_t) :: access
        type(derived_type_query_t) :: dtype
        type(type_binding_query_t) :: binding
        type(program_unit_query_t) :: unit
        character(len=:), allocatable :: object_type, type_name, method, impl
        integer, allocatable :: args(:)
        integer :: receiver, i, j, dtype_index, binding_index
        logical :: found_type, found_binding, found_function

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
        call static_object_type(arena, access%base_node_index, proc, object_type)
        if (.not. allocated(object_type)) then
            call refuse_type_bound(status, node%name, &
                "the receiver has no statically declared type")
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
        dtype_index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "derived_type") cycle
            dtype = query_derived_type(arena, i)
            if (.not. dtype%found) cycle
            if (.not. same_name(dtype%name, type_name)) cycle
            found_type = .true.
            dtype_index = i
            exit
        end do
        if (.not. found_type) then
            call refuse_type_bound(status, method, &
                "the concrete type is not defined in this source")
            return
        end if
        if (allocated(dtype%extends_parent)) then
            if (len_trim(dtype%extends_parent) > 0) then
                call refuse_type_bound(status, method, "inherited bindings are unsupported")
                return
            end if
        end if
        found_binding = .false.
        binding_index = 0
        if (allocated(dtype%binding_indices)) then
            do i = 1, size(dtype%binding_indices)
                binding = query_type_binding(arena, dtype%binding_indices(i))
                if (.not. binding%found) cycle
                if (.not. same_name(binding%binding_name, method)) cycle
                found_binding = .true.
                binding_index = dtype%binding_indices(i)
                exit
            end do
        end if
        if (.not. found_binding) then
            call refuse_type_bound(status, method, "no local type-bound binding")
            return
        end if
        binding = query_type_binding(arena, binding_index)
        if (binding%is_generic) then
            call refuse_type_bound(status, method, "generic bindings are unsupported")
            return
        end if
        if (binding%is_deferred) then
            call refuse_type_bound(status, method, "deferred bindings are unsupported")
            return
        end if
        if (binding%pass_arg) then
            if (allocated(binding%pass_name)) then
                if (len_trim(binding%pass_name) > 0) then
                    call refuse_type_bound(status, method, &
                        "named PASS bindings are unsupported")
                    return
                end if
            end if
        end if
        impl = trim(binding%binding_name)
        if (allocated(binding%implementation)) then
            if (len_trim(binding%implementation) > 0) impl = trim(binding%implementation)
        end if
        found_function = .false.
        do j = 1, arena%size
            if (.not. arena%has_node_at(j)) cycle
            if (trim(arena%entries(j)%node_type) /= "function_def") cycle
            unit = query_program_unit(arena, j)
            if (unit%found .and. same_name(unit%name, impl)) then
                found_function = .true.
                exit
            end if
        end do
        if (.not. found_function) then
            call refuse_type_bound(status, method, &
                "the binding implementation is not a same-file function")
            return
        end if
        if (binding%pass_arg) then
            receiver = lower_expr(arena, access%base_node_index, proc, status)
            if (.not. status%ok) return
            allocate (args(size(node%arg_indices) + 1))
            args(1) = receiver
            do i = 1, size(node%arg_indices)
                args(i + 1) = lower_expr(arena, node%arg_indices(i), proc, status)
                if (.not. status%ok) return
            end do
        else
            ! NOPASS bindings do not receive the object expression.  Keeping
            ! the receiver out of the ordinary call is essential: the
            ! implementation's first dummy is the first explicit actual.
            allocate (args(size(node%arg_indices)))
            do i = 1, size(node%arg_indices)
                args(i) = lower_expr(arena, node%arg_indices(i), proc, status)
                if (.not. status%ok) return
            end do
        end if
        out = proc%add_expr(expr_call(impl, args))
    end function lower_type_bound_call

    logical function is_type_bound_reference(arena, base_idx, proc) result(found)
        !! Distinguish ``object%binding(args)`` from an array component
        !! ``object%values(i)`` before lowering either one.  A local binding is
        !! enough to route the former through the existing explicit refusal
        !! diagnostics (named PASS, NOPASS, generic, and deferred).
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
