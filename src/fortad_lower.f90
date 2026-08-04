module fortad_lower
    !! fortfront AST to fortad IR.
    !!
    !! Lowering is a refusal boundary as much as a translation. A construct
    !! fortad cannot differentiate correctly is reported by name and line, never
    !! silently approximated. A named refusal is a bug report; a silent fallback
    !! is a wrong derivative.
    use fortfront, only: compile_frontend_from_string, &
                        compiler_frontend_options_t, compiler_frontend_result_t, &
                        ast_arena_t, function_def_node, subroutine_def_node, &
                        assignment_node, binary_op_node, identifier_node, &
                        literal_node, call_or_subscript_node, declaration_node, &
                        do_loop_node, if_node, parameter_declaration_node, &
                        subroutine_call_node, use_statement_node, comment_node, &
                        INPUT_MODE_STANDARD
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                        expr_const, expr_var, expr_binop, expr_unop, expr_call, &
                        FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, &
                        FAD_END_IF, FAD_INDEX, FAD_CALL_STMT, FAD_INTENT_NONE, &
                        FAD_INTENT_IN, FAD_INTENT_OUT, FAD_INTENT_INOUT
    implicit none
    private

    public :: lower_source, lower_status_t

    type :: lower_status_t
        !! Outcome of a lowering attempt.
        logical :: ok = .false.
        character(len=:), allocatable :: message
    end type lower_status_t

contains

    subroutine lower_source(source, proc, status)
        !! Parse `source` through fortfront and lower its first procedure.
        character(len=*), intent(in) :: source
        type(fad_proc_t), intent(out) :: proc
        type(lower_status_t), intent(out) :: status
        type(compiler_frontend_options_t) :: opts
        type(compiler_frontend_result_t) :: res
        integer :: i

        opts%input_mode = INPUT_MODE_STANDARD
        opts%standardize = .false.
        call compile_frontend_from_string(source, res, opts)

        if (.not. res%parse_ok) then
            status%ok = .false.
            status%message = "parse failed"
            if (allocated(res%error_msg)) then
                if (len_trim(res%error_msg) > 0) then
                    status%message = "parse failed: "//trim(res%error_msg)
                end if
            end if
            return
        end if

        do i = 1, res%arena%size
            if (.not. allocated(res%arena%entries(i)%node)) cycle
            select type (n => res%arena%entries(i)%node)
            type is (function_def_node)
                call lower_function(res%arena, n, proc, status)
                return
            type is (subroutine_def_node)
                call lower_subroutine(res%arena, n, proc, status)
                return
            end select
        end do

        status%ok = .false.
        status%message = "no function or subroutine found in source"
    end subroutine lower_source

    subroutine infer_real_suffix(proc)
        !! Choose the kind suffix for real literals fortad emits, from the
        !! primal's own real declarations. Generated code must compile in the
        !! scope it is placed in, and that scope may not define `dp`.
        type(fad_proc_t), intent(inout) :: proc
        integer :: i, paren

        proc%real_suffix = "d0"
        do i = 1, proc%n_decls
            if (.not. allocated(proc%decls(i)%type_name)) cycle
            associate (tn => proc%decls(i)%type_name)
                if (index(tn, "real") /= 1 .and. index(tn, "REAL") /= 1) cycle
                paren = index(tn, "(")
                if (paren == 0) then
                    proc%real_suffix = "e0"
                    return
                end if
                select case (trim(tn(paren + 1:len(tn) - 1)))
                case ("4")
                    proc%real_suffix = "e0"
                case ("8", "kind(1.0d0)", "real64")
                    proc%real_suffix = "d0"
                case default
                    ! A named kind parameter is in scope wherever the primal
                    ! compiles, so reusing it is both correct and idiomatic.
                    proc%real_suffix = "_"//trim(tn(paren + 1:len(tn) - 1))
                end select
                return
            end associate
        end do
    end subroutine infer_real_suffix

    subroutine lower_function(arena, fn, proc, status)
        !! Lower a function definition.
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: fn
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        type(fad_decl_t) :: d

        proc%name = fn%name
        proc%is_function = .true.
        if (allocated(fn%result_variable)) then
            proc%result_name = fn%result_variable
        else
            proc%result_name = fn%name
        end if
        call collect_params(arena, fn%param_indices, proc)
        call lower_body(arena, fn%body_indices, proc, status)
        if (.not. status%ok) return
        call infer_real_suffix(proc)

        ! A function result needs a declaration even when the primal declared it
        ! implicitly through the return type.
        if (proc%decl_index(proc%result_name) == 0) then
            d%name = proc%result_name
            if (allocated(fn%return_type)) then
                d%type_name = fn%return_type
            else
                d%type_name = "real(dp)"
            end if
            d%is_result = .true.
            block
                integer :: ignored
                ignored = proc%add_decl(d)
            end block
        else
            proc%decls(proc%decl_index(proc%result_name))%is_result = .true.
        end if
    end subroutine lower_function

    subroutine lower_subroutine(arena, sub, proc, status)
        !! Lower a subroutine definition.
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status

        proc%name = sub%name
        proc%is_function = .false.
        call collect_params(arena, sub%param_indices, proc)
        call lower_body(arena, sub%body_indices, proc, status)
        if (.not. status%ok) return
        call infer_real_suffix(proc)
    end subroutine lower_subroutine

    subroutine collect_params(arena, param_indices, proc)
        !! Record the dummy argument names in declaration order.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        type(fad_proc_t), intent(inout) :: proc
        character(len=64), allocatable :: names(:)
        integer :: i, n

        n = 0
        allocate (names(max(1, size(param_indices))))
        do i = 1, size(param_indices)
            if (param_indices(i) <= 0) cycle
            if (param_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(param_indices(i))%node)) cycle
            select type (pn => arena%entries(param_indices(i))%node)
            type is (identifier_node)
                n = n + 1
                names(n) = pn%name
            type is (parameter_declaration_node)
                n = n + 1
                names(n) = pn%name
            type is (declaration_node)
                n = n + 1
                names(n) = pn%var_name
            end select
        end do
        if (n > 0) then
            proc%params = names(1:n)
        else
            allocate (character(len=64) :: proc%params(0))
        end if
    end subroutine collect_params

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
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
        type is (comment_node)
            ! A comment carries no computation. It is deliberately not copied
            ! into the derivative either: a sentence about the primal is not a
            ! sentence about its derivative, and reproducing it would put a
            ! wrong explanation next to generated code.
            return

        type is (use_statement_node)
            ! The derivative names the same kinds and calls the same helpers as
            ! the primal, so it needs the same imports. They are reproduced as
            ! text: resolving them is the consumer's compiler's job, and fortad
            ! guessing at module contents would be a second, worse answer.
            call add_use(proc, n)

        type is (declaration_node)
            if (n%is_multi_declaration .and. allocated(n%var_names)) then
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
            ! Only plain if/else is lowered. `else if` chains would need the
            ! IR to nest, and fortad refuses what it cannot represent exactly.
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
            ! A call is opaque: fortad differentiates it through a registered
            ! rule or not at all. Descending into the callee would need
            ! whole-program analysis fortad does not do.
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

    subroutine fill_decl(n, name, arena, d)
        !! Translate a fortfront declaration node into a fortad declaration.
        type(declaration_node), intent(in) :: n
        character(len=*), intent(in) :: name
        type(ast_arena_t), intent(in) :: arena
        type(fad_decl_t), intent(out) :: d

        d%name = name
        d%type_name = n%type_name
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
        if (.not. allocated(arena%entries(idx)%node)) return
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
        character(len=:), allocatable :: line, items
        character(len=256), allocatable :: grown(:)
        integer :: k

        if (.not. allocated(n%module_name)) return
        line = "use"
        if (n%is_intrinsic) line = line//", intrinsic"
        line = line//" :: "//trim(n%module_name)

        ! fortfront stores a rename flat, as consecutive local and remote
        ! entries - see `append_rename_pair` in its import parser - so the list
        ! is walked two at a time.
        items = ""
        if (allocated(n%rename_list)) then
            do k = 1, size(n%rename_list) - 1, 2
                if (.not. allocated(n%rename_list(k)%s)) cycle
                if (.not. allocated(n%rename_list(k + 1)%s)) cycle
                if (len(items) > 0) items = items//", "
                items = items//trim(n%rename_list(k)%s)//" => "// &
                        trim(n%rename_list(k + 1)%s)
            end do
        end if
        if (allocated(n%only_list)) then
            do k = 1, size(n%only_list)
                if (.not. allocated(n%only_list(k)%s)) cycle
                if (len(items) > 0) items = items//", "
                items = items//trim(n%only_list(k)%s)
            end do
        end if
        if (n%has_only .and. len(items) > 0) line = line//", only: "//items

        if (.not. allocated(proc%uses)) then
            allocate (character(len=256) :: proc%uses(8))
            proc%n_uses = 0
        else if (proc%n_uses >= size(proc%uses)) then
            allocate (character(len=256) :: grown(2*size(proc%uses)))
            grown(1:proc%n_uses) = proc%uses(1:proc%n_uses)
            call move_alloc(grown, proc%uses)
        end if
        proc%n_uses = proc%n_uses + 1
        proc%uses(proc%n_uses) = line
    end subroutine add_use

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
        select type (n => arena%entries(idx)%node)
        type is (identifier_node)
            s%target = n%name
        type is (call_or_subscript_node)
            allocate (subs(size(n%arg_indices)))
            do i = 1, size(n%arg_indices)
                subs(i) = lower_expr(arena, n%arg_indices(i), proc, status)
                if (.not. status%ok) return
            end do
            e%kind = FAD_INDEX
            e%text = n%name
            e%args = subs
            s%target = render_index(proc, e)
        class default
            status%ok = .false.
            status%message = "unsupported assignment target"
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
        if (.not. allocated(arena%entries(idx)%node)) then
            status%ok = .false.
            status%message = "empty expression node"
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
            if (is_array_name(proc, n%name)) then
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

    logical function is_array_name(proc, name) result(yes)
        !! True when `name` was declared as an array in this procedure, which is
        !! how a subscript is told apart from a function call.
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
        if (.not. allocated(arena%entries(idx)%node)) return
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

end module fortad_lower
