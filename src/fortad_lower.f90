module fortad_lower
    !! fortfront AST to fortad IR.
    !!
    !! Lowering is a refusal boundary as much as a translation. A construct
    !! fortad cannot differentiate correctly is reported by name and line, never
    !! silently approximated. A named refusal is a bug report; a silent fallback
    !! is a wrong derivative.
    use fortfront, only: compile_frontend_from_string, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        ast_arena_t, program_unit_query_t, query_program_unit, &
        query_declaration, declaration_query_t, binary_op_node, &
        defined_operator_query_t, query_defined_operator, INPUT_MODE_STANDARD
    use fortfront, only: generic_call_query_t, query_generic_call
    use fortad_call_boundaries, only: has_same_file_call, &
        validate_direct_call_boundaries
    use frontend_compiler_queries, only: global_reference_query_t, &
        query_active_global_references
    use fortad_ir, only: fad_proc_t
    use fortad_inline, only: inline_calls, inline_status_t, references
    use fortad_lower_body, only: lower_function, lower_subroutine, &
        inherit_module_uses
    use fortad_lower_statements, only: rewrite_associate_aliases
    use fortad_lower_types, only: lower_status_t
    implicit none
    private

    public :: lower_source, lower_status_t

contains

    logical function wanted(name, proc_name, chosen) result(yes)
        !! Whether this is the procedure to differentiate.
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: proc_name
        integer, intent(in) :: chosen

        yes = .false.
        if (chosen > 0) return
        if (.not. present(proc_name)) then
            yes = .true.
            return
        end if
        if (len_trim(proc_name) == 0) then
            yes = .true.
            return
        end if
        yes = matches(name, proc_name)
    end function wanted

    logical function needed(proc, others, n_others, name) result(yes)
        !! Whether the target, or something already pulled in, calls `name`.
        use fortad_inline, only: references
        type(fad_proc_t), intent(in) :: proc
        type(fad_proc_t), intent(in) :: others(:)
        integer, intent(in) :: n_others
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        do i = 1, n_others
            if (.not. allocated(others(i)%name)) cycle
            if (matches(others(i)%name, name)) return
        end do
        if (references(proc, name)) then
            yes = .true.
            return
        end if
        do i = 1, n_others
            if (references(others(i), name)) then
                yes = .true.
                return
            end if
        end do
    end function needed

    logical function matches(a, b) result(yes)
        !! Fortran names do not distinguish case.
        character(len=*), intent(in) :: a, b
        integer :: i
        character(len=:), allocatable :: la, lb

        la = trim(a)
        lb = trim(b)
        yes = .false.
        if (len(la) /= len(lb)) return
        do i = 1, len(la)
            if (down(la(i:i)) /= down(lb(i:i))) return
        end do
        yes = .true.
    end function matches

    character function down(c)
        character, intent(in) :: c

        down = c
        if (c >= "A" .and. c <= "Z") down = achar(iachar(c) + 32)
    end function down

    logical function has_module_allocatable_state(source) result(found)
        !! Keep mutable module-owned allocation state outside the supported
        !! ownership slice.  This lexical check is deliberately conservative:
        !! a module containing such state is refused before procedure lowering,
        !! rather than pretending that a derivative can replay its lifetime.
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: line, lower_line
        integer :: first, last, next, derived_depth
        logical :: module_scope

        found = .false.
        module_scope = .false.
        derived_depth = 0
        first = 1
        do while (first <= len(source))
            next = index(source(first:), new_line('a'))
            if (next == 0) then
                last = len(source)
            else
                last = first + next - 2
            end if
            if (last >= first) then
                line = adjustl(source(first:last))
                lower_line = lower_text(line)
                if (index(lower_line, "module ") == 1 .and. &
                    index(lower_line, "module procedure") /= 1) then
                    module_scope = .true.
                else if (index(lower_line, "contains") == 1) then
                    module_scope = .false.
                else if (index(lower_line, "end module") == 1) then
                    module_scope = .false.
                else if (index(lower_line, "type ") == 1 .and. &
                        index(lower_line, "::") > 0) then
                    derived_depth = derived_depth + 1
                else if (index(lower_line, "end type") == 1) then
                    derived_depth = max(0, derived_depth - 1)
                else if (module_scope .and. derived_depth == 0 .and. &
                        index(lower_line, "allocatable") > 0 .and. &
                        index(lower_line, "::") > 0) then
                    found = .true.
                    return
                end if
            end if
            if (next == 0) exit
            first = first + next
        end do
    end function has_module_allocatable_state

    function lower_text(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: j

        out = text
        do j = 1, len(text)
            out(j:j) = down(text(j:j))
        end do
    end function lower_text

    subroutine lower_source(source, proc, status, proc_name)
        !! Parse `source` through fortfront and lower one of its procedures.
        !!
        !! Without `proc_name` that is the first one, which is what a
        !! single-procedure file means. Naming one picks it out of a module and
        !! leaves the rest available to inline calls from.
        character(len=*), intent(in) :: source
        type(fad_proc_t), intent(out) :: proc
        type(lower_status_t), intent(out) :: status
        character(len=*), intent(in), optional :: proc_name
        type(compiler_frontend_options_t) :: opts
        type(compiler_frontend_result_t) :: res
        type(program_unit_query_t) :: unit
        type(declaration_query_t) :: global_decl
        logical :: has_callee
        character(len=4096) :: source_header, source_param_text, source_item
        character(len=:), allocatable :: allocation_construct
        type(global_reference_query_t), allocatable :: global_refs(:)
        integer :: i, chosen, source_pos, source_next, source_line
        integer :: source_open, source_close, source_first, source_last
        integer :: source_comma, source_depth, source_n_params
        opts%input_mode = INPUT_MODE_STANDARD
        ! FortAD lowers from the parsed/query AST and performs its own
        ! derivative checks.  Running FortFront's optional semantic pass here
        ! is redundant and trips an NVFORTRAN allocator bug for scalar
        ! external CALL statements; keep the frontend boundary parse-only.
        opts%run_semantics = .false.
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

        ! Generic exact matching needs FortFront's resolved type metadata.
        ! Keep the historical parse-only path for ordinary sources (it avoids
        ! a compiler-specific semantic pass issue for external scalar CALLs),
        ! then rerun only when the raw arena contains a same-file generic call.
        if (contains_generic_call(res%arena)) then
            opts%run_semantics = .true.
            call compile_frontend_from_string(source, res, opts)
            if (.not. res%parse_ok) then
                status%ok = .false.
                status%message = "parse failed while resolving a generic call"
                if (allocated(res%error_msg)) then
                    if (len_trim(res%error_msg) > 0) then
                        status%message = "parse failed while resolving a generic call: "// &
                            trim(res%error_msg)
                    end if
                end if
                return
            end if
        end if

        ! Defined-operator resolution is a semantic fact too.  The parse-only
        ! arena identifies the operator syntax, but only FortFront's semantic
        ! pass can select one exact same-file specific without guessing an
        ! implicit conversion or a dynamic target.
        if (contains_defined_operator(res%arena)) then
            opts%run_semantics = .true.
            call compile_frontend_from_string(source, res, opts)
            if (.not. res%parse_ok) then
                status%ok = .false.
                status%message = "parse failed while resolving a defined operator"
                if (allocated(res%error_msg)) then
                    if (len_trim(res%error_msg) > 0) then
                        status%message = "parse failed while resolving a defined operator: "// &
                            trim(res%error_msg)
                    end if
                end if
                return
            end if
        end if

        ! Direct same-file calls need FortFront's semantic actual/formal
        ! mapping. Keep the parse-only fast path for sources without one.
        if (has_same_file_call(res%arena)) then
            opts%run_semantics = .true.
            call compile_frontend_from_string(source, res, opts)
            if (.not. res%parse_ok) then
                status%ok = .false.
                status%message = "parse failed while resolving a procedure call boundary"
                if (allocated(res%error_msg)) then
                    if (len_trim(res%error_msg) > 0) then
                        status%message = "parse failed while resolving a procedure call boundary: "// &
                            trim(res%error_msg)
                    end if
                end if
                return
            end if
        end if

        ! Procedure-pointer target flow is a semantic fact.  Keep the
        ! parse-only fast path for ordinary sources, but give FortFront's
        ! bounded callback query the semantic arena whenever a procedure
        ! pointer declaration is present.
        if (contains_procedure_pointer_declaration(res%arena)) then
            opts%run_semantics = .true.
            call compile_frontend_from_string(source, res, opts)
            if (.not. res%parse_ok) then
                status%ok = .false.
                status%message = "parse failed while resolving a procedure-pointer callback"
                if (allocated(res%error_msg)) then
                    if (len_trim(res%error_msg) > 0) then
                        status%message = "parse failed while resolving a procedure-pointer callback: "// &
                            trim(res%error_msg)
                    end if
                end if
                return
            end if
        end if

        ! ASSOCIATE selector facts include resolved storage and type metadata.
        ! The ordinary parse-only path intentionally omits those facts, so
        ! rerun FortFront's semantic pass only for sources that contain the
        ! construct rather than paying that cost for every procedure.
        if (contains_associate_construct(res%arena)) then
            opts%run_semantics = .true.
            call compile_frontend_from_string(source, res, opts)
            if (.not. res%parse_ok) then
                status%ok = .false.
                status%message = "parse failed while resolving ASSOCIATE selector facts"
                if (allocated(res%error_msg)) then
                    if (len_trim(res%error_msg) > 0) then
                        status%message = "parse failed while resolving ASSOCIATE selector facts: "// &
                            trim(res%error_msg)
                    end if
                end if
                return
            end if
        end if

        if (has_module_allocatable_state(source)) then
            status%ok = .false.
            status%message = "module-level allocatable mutable state is not supported; use local or dummy ownership"
            return
        end if

        ! Lower the procedure asked for, then only those it calls.
        !
        ! Lowering everything in the file would drag in whatever else lives
        ! there - a benchmark driver, a command line parser - and fail or crash
        ! on constructs that have nothing to do with the kernel. Following the
        ! calls reaches exactly what inlining needs.
        chosen = 0
        do i = 1, res%arena%size
            if (.not. res%arena%has_node_at(i)) cycle
            if (trim(res%arena%entries(i)%node_type) /= "function_def" .and. &
                trim(res%arena%entries(i)%node_type) /= "subroutine_def") cycle
            unit = query_program_unit(res%arena, i)
            if (.not. unit%found) cycle
            if (wanted(unit%name, proc_name, chosen)) then
                source_header = ""
                source_pos = 1
                source_line = 1
                do while (source_line < unit%line)
                    source_next = index(source(source_pos:), new_line('a'))
                    if (source_next == 0) exit
                    source_pos = source_pos + source_next
                    source_line = source_line + 1
                end do
                call join_continued_statement(source, source_pos, source_header)
                source_open = procedure_arg_open(source_header)
                source_close = 0
                source_depth = 0
                if (source_open > 0) then
                    do source_comma = source_open, len_trim(source_header)
                        if (source_header(source_comma:source_comma) == "(") then
                            source_depth = source_depth + 1
                        else if (source_header(source_comma:source_comma) == ")") then
                            source_depth = source_depth - 1
                            if (source_depth == 0) then
                                source_close = source_comma
                                exit
                            end if
                        end if
                    end do
                end if
                source_n_params = 0
                if (source_close > source_open) then
                    source_param_text = source_header(source_open + 1:source_close - 1)
                    if (len_trim(source_param_text) > 0) then
                        source_n_params = 1
                        source_depth = 0
                        do source_comma = 1, len_trim(source_param_text)
                            if (source_param_text(source_comma:source_comma) == "(") then
                                source_depth = source_depth + 1
                            else if (source_param_text(source_comma:source_comma) == ")") then
                                source_depth = max(0, source_depth - 1)
                            else if (source_depth == 0 .and. &
                                    source_param_text(source_comma:source_comma) == ",") then
                                source_n_params = source_n_params + 1
                            end if
                        end do
                    end if
                end if
                if (allocated(proc%params)) deallocate (proc%params)
                allocate (character(len=64) :: proc%params(source_n_params))
                if (source_n_params > 0) then
                    source_first = 1
                    source_depth = 0
                    source_n_params = 0
                    do source_comma = 1, len_trim(source_param_text) + 1
                        if (source_comma <= len_trim(source_param_text)) then
                            if (source_param_text(source_comma:source_comma) == "(") then
                                source_depth = source_depth + 1
                            else if (source_param_text(source_comma:source_comma) == ")") then
                                source_depth = max(0, source_depth - 1)
                            end if
                        end if
                        if (source_comma > len_trim(source_param_text)) then
                            source_last = source_comma - 1
                            source_item = adjustl(source_param_text(source_first:source_last))
                            source_n_params = source_n_params + 1
                            proc%params(source_n_params) = trim(source_item)
                            source_first = source_comma + 1
                        else if (source_depth == 0 .and. &
                                source_param_text(source_comma:source_comma) == ",") then
                            source_last = source_comma - 1
                            source_item = adjustl(source_param_text(source_first:source_last))
                            source_n_params = source_n_params + 1
                            proc%params(source_n_params) = trim(source_item)
                            source_first = source_comma + 1
                        end if
                    end do
                end if
                if (trim(unit%unit_kind) == "function") then
                    call lower_function(res%arena, unit, source, proc, status)
                else
                    call lower_subroutine(res%arena, unit, source, proc, status)
                end if
                if (status%ok) call rewrite_associate_aliases(proc)
                proc%is_elemental = header_has_attribute(source_header, "elemental")
                proc%is_pure = header_has_attribute(source_header, "pure")
                if (proc%is_elemental) then
                    if (.not. header_has_attribute(source_header, "impure")) then
                        proc%is_pure = .true.
                    end if
                end if
                chosen = i
            end if
            if (chosen > 0) exit
        end do

        if (chosen == 0) then
            status%ok = .false.
            if (present(proc_name)) then
                if (len_trim(proc_name) > 0) then
                    status%message = "no procedure named '"//trim(proc_name)// &
                        "' in this source"
                    return
                end if
            end if
            status%message = "no function or subroutine found in source"
            return
        end if
        if (.not. status%ok) return

        if (has_same_file_call(res%arena)) then
            call validate_direct_call_boundaries(res%arena, chosen, status)
            if (.not. status%ok) return
        end if

        global_refs = query_active_global_references(res%arena, chosen)
        do i = 1, size(global_refs)
            global_decl = query_declaration(res%arena, &
                global_refs(i)%declaration_node_index)
            if (global_decl%is_parameter) cycle
            call refuse_global_state(res%arena, global_refs(i), status)
            return
        end do

        if (res%arena%entries(chosen)%parent_index > 0) then
            call inherit_module_uses(res%arena, chosen, proc)
        end if

        ! Only construct the deeply allocatable sibling buffer when this
        ! procedure actually references a sibling. This is also a compiler
        ! compatibility boundary for nvfortran's finalization of such arrays.
        has_callee = .false.
        do i = 1, res%arena%size
            if (i == chosen) cycle
            if (.not. res%arena%has_node_at(i)) cycle
            if (trim(res%arena%entries(i)%node_type) /= "function_def" .and. &
                trim(res%arena%entries(i)%node_type) /= "subroutine_def") cycle
            unit = query_program_unit(res%arena, i)
            if (unit%found) then
                if (references(proc, unit%name)) then
                    has_callee = .true.
                    exit
                end if
            end if
        end do
        if (has_callee) then
            call inline_reachable(res%arena, chosen, source, proc, status)
            if (.not. status%ok) return
        end if

        status%ok = .true.
        status%message = ""
    end subroutine lower_source

    logical function contains_generic_call(arena) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(generic_call_query_t) :: query
        integer :: i

        found = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            query = query_generic_call(arena, i)
            if (.not. query%found) cycle
            if (query%is_generic) then
                found = .true.
                return
            end if
        end do
    end function contains_generic_call

    logical function contains_defined_operator(arena) result(found)
        !! Whether semantic lowering is needed for a defined-operator query.
        type(ast_arena_t), intent(in) :: arena
        type(defined_operator_query_t) :: query
        integer :: i

        found = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (binary_op_node)
                query = query_defined_operator(arena, i)
                if (query%found .and. query%is_defined_operator) then
                    found = .true.
                    return
                end if
            class default
            end select
        end do
    end function contains_defined_operator

    logical function contains_procedure_pointer_declaration(arena) result(found)
        !! Whether semantic lowering is needed for a procedure-pointer fact.
        type(ast_arena_t), intent(in) :: arena
        type(declaration_query_t) :: query
        character(len=:), allocatable :: normalized
        integer :: i, j

        found = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            query = query_declaration(arena, i)
            if (.not. query%found) cycle
            if (.not. query%is_pointer) cycle
            if (.not. allocated(query%type_name)) cycle
            normalized = ""
            do j = 1, len_trim(query%type_name)
                if (query%type_name(j:j) == " ") cycle
                normalized = normalized//down(query%type_name(j:j))
            end do
            if (index(normalized, "procedure") == 1) then
                found = .true.
                return
            end if
        end do
    end function contains_procedure_pointer_declaration

    logical function contains_associate_construct(arena) result(found)
        !! Whether semantic ASSOCIATE selector facts are needed.
        type(ast_arena_t), intent(in) :: arena
        integer :: i

        found = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) == "associate") then
                found = .true.
                return
            end if
        end do
    end function contains_associate_construct

    subroutine refuse_global_state(arena, reference, status)
        !! Active mutable global state has no explicit ownership contract.
        !! Refuse it before lowering rather than treating it as an implicit
        !! constant or silently producing a derivative of the wrong function.
        use fortfront, only: ast_arena_t
        type(ast_arena_t), intent(in) :: arena
        type(global_reference_query_t), intent(in) :: reference
        type(lower_status_t), intent(out) :: status
        integer :: line
        character(len=:), allocatable :: kind

        line = arena%get_node_line(reference%reference_node_index)
        kind = "module"
        if (reference%is_common_state) then
            kind = "COMMON"
        else if (reference%is_save_state) then
            kind = "SAVE"
        end if
        status%ok = .false.
        status%message = "unsupported active global state '"//trim(reference%name)// &
            "' ("//trim(kind)//") at line "//line_text(line)// &
            "; global mutable state requires an explicit derivative rule"
    end subroutine refuse_global_state

    function line_text(line) result(text)
        !! Integer to trimmed decimal text for source-boundary diagnostics.
        integer, intent(in) :: line
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '(i0)') line
        text = trim(buffer)
    end function line_text

    logical function header_has_attribute(header, attribute) result(found)
        !! Return whether a procedure header contains a standalone prefix.
        !! FortFront exposes procedure names and bodies through its stable
        !! query, but not procedure prefixes. The source header is already
        !! assembled here for dummy-argument extraction.
        character(len=*), intent(in) :: header, attribute
        character(len=:), allocatable :: text, needle
        integer :: i, last, comment, keyword

        text = lower_ascii(header)
        comment = index(text, "!")
        if (comment == 1) then
            text = ""
        else if (comment > 1) then
            text = text(:comment - 1)
        end if
        keyword = index(text, "function")
        if (keyword == 0) keyword = index(text, "subroutine")
        if (keyword == 1) then
            text = ""
        else if (keyword > 1) then
            text = text(:keyword - 1)
        end if
        text = " "//trim(text)//" "
        needle = " "//trim(attribute)//" "
        found = .false.
        last = len_trim(text) - len(needle) + 1
        if (last < 1) return
        do i = 1, last
            if (text(i:i + len(needle) - 1) == needle) then
                found = .true.
                return
            end if
        end do
    end function header_has_attribute

    function lower_ascii(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i

        allocate (character(len=len(text)) :: out)
        out = text
        do i = 1, len(text)
            if (out(i:i) >= "A" .and. out(i:i) <= "Z") then
                out(i:i) = achar(iachar(out(i:i)) + 32)
            end if
        end do
    end function lower_ascii

    integer function procedure_arg_open(header) result(open)
        !! Find the argument list, skipping a kind selector in a return type.
        character(len=*), intent(in) :: header
        integer :: i, p

        open = 0
        do i = 1, len_trim(header) - 7
            if (.not. matches(header(i:i + 7), "function")) cycle
            p = index(header(i + 8:), "(")
            if (p > 0) then
                open = i + 7 + p
                return
            end if
        end do
        do i = 1, len_trim(header) - 10
            if (.not. matches(header(i:i + 9), "subroutine")) cycle
            p = index(header(i + 10:), "(")
            if (p > 0) then
                open = i + 9 + p
                return
            end if
        end do
    end function procedure_arg_open

    subroutine join_continued_statement(source, start_pos, statement)
        !! Return the whole logical statement starting at `start_pos`, joining
        !! free-form continuation lines.
        !!
        !! A dummy-argument list that fortad emitted itself is wrapped at the
        !! line limit, so reading only the first physical line finds no closing
        !! parenthesis and leaves the procedure with no parameters. That is how
        !! a forward-over-reverse pass over a wide adjoint used to lose every
        !! argument of the routine it was differentiating.
        character(len=*), intent(in) :: source
        integer, intent(in) :: start_pos
        character(len=*), intent(out) :: statement
        character(len=4096) :: piece
        integer :: pos, next, last
        logical :: continued

        statement = ""
        pos = start_pos
        do
            if (pos > len(source)) exit
            next = index(source(pos:), new_line('a'))
            piece = ""
            if (next == 0) then
                piece = source(pos:)
                pos = len(source) + 1
            else
                if (next > 1) piece = source(pos:pos + next - 2)
                pos = pos + next
            end if
            piece = adjustl(piece)
            if (len_trim(piece) > 0) then
                if (piece(1:1) == "&") piece = adjustl(piece(2:))
            end if
            last = len_trim(piece)
            continued = .false.
            if (last > 0) continued = piece(last:last) == "&"
            if (continued) then
                statement = trim(statement)//" "//piece(:last - 1)
                cycle
            end if
            statement = trim(statement)//" "//piece(:last)
            exit
        end do
        statement = adjustl(statement)
    end subroutine join_continued_statement

    subroutine inline_reachable(arena, chosen, source, proc, status)
        !! Lower and inline the sibling procedures actually reachable from proc.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: chosen
        character(len=*), intent(in) :: source
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        integer, parameter :: MAX_PROCS = 64
        integer, parameter :: MAX_ROUNDS = 8
        type(fad_proc_t) :: others(MAX_PROCS), one_proc
        type(inline_status_t) :: ist
        type(lower_status_t) :: one
        type(program_unit_query_t) :: unit
        integer :: round, added, i, n_others

        status%ok = .true.
        status%message = ""
        n_others = 0
        do round = 1, MAX_ROUNDS
            added = 0
            do i = 1, arena%size
                if (i == chosen) cycle
                if (n_others >= MAX_PROCS) exit
                if (.not. arena%has_node_at(i)) cycle
                call clear_proc(one_proc)
                one%ok = .false.
                if (allocated(one%message)) deallocate (one%message)
                if (trim(arena%entries(i)%node_type) /= "function_def" .and. &
                    trim(arena%entries(i)%node_type) /= "subroutine_def") cycle
                unit = query_program_unit(arena, i)
                if (.not. unit%found) cycle
                if (.not. needed(proc, others, n_others, unit%name)) cycle
                if (trim(unit%unit_kind) == "function") then
                    call lower_function(arena, unit, source, one_proc, one)
                else
                    call lower_subroutine(arena, unit, source, one_proc, one)
                end if
                if (.not. one%ok) cycle
                call copy_params(arena, unit, one_proc)
                n_others = n_others + 1
                others(n_others) = one_proc
                added = added + 1
            end do
            if (added == 0) exit
        end do

        if (n_others > 0) then
            call inline_calls(proc, others(:n_others), n_others, ist)
            if (.not. ist%ok) then
                status%ok = .false.
                status%message = ist%message
            end if
        end if
    end subroutine inline_reachable

    subroutine copy_params(arena, unit, proc)
        !! Preserve a sibling's declared dummy order for inlining.
        type(ast_arena_t), intent(in) :: arena
        type(program_unit_query_t), intent(in) :: unit
        type(fad_proc_t), intent(inout) :: proc
        type(declaration_query_t) :: decl
        integer :: i, n

        if (allocated(proc%params)) deallocate (proc%params)
        n = 0
        if (allocated(unit%parameter_indices)) n = size(unit%parameter_indices)
        allocate (character(len=64) :: proc%params(n))
        do i = 1, n
            decl = query_declaration(arena, unit%parameter_indices(i))
            if (decl%found) then
                if (allocated(decl%name)) then
                    proc%params(i) = trim(decl%name)
                else if (allocated(decl%names)) then
                    if (size(decl%names) > 0) proc%params(i) = trim(decl%names(1))
                end if
            end if
        end do
    end subroutine copy_params

    subroutine clear_proc(proc)
        !! Reset an internal lowering buffer without compiler-generated
        !! intrinsic assignment of a deeply allocatable IR value.
        type(fad_proc_t), intent(inout) :: proc

        if (allocated(proc%name)) deallocate (proc%name)
        if (allocated(proc%result_name)) deallocate (proc%result_name)
        if (allocated(proc%real_suffix)) deallocate (proc%real_suffix)
        if (allocated(proc%uses)) deallocate (proc%uses)
        if (allocated(proc%decls)) deallocate (proc%decls)
        if (allocated(proc%params)) deallocate (proc%params)
        if (allocated(proc%exprs)) deallocate (proc%exprs)
        if (allocated(proc%stmts)) deallocate (proc%stmts)
        if (allocated(proc%bucket_head)) deallocate (proc%bucket_head)
        if (allocated(proc%bucket_next)) deallocate (proc%bucket_next)
        proc%is_function = .false.
        proc%is_pure = .true.
        proc%is_elemental = .false.
        proc%n_uses = 0
        proc%n_exprs = 0
        proc%n_stmts = 0
        proc%n_decls = 0
    end subroutine clear_proc

end module fortad_lower
