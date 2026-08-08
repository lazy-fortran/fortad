module fortad_call_boundaries
    !! Semantic boundary for the bounded direct same-file call slice.
    use fortfront, only: ast_arena_t, program_unit_query_t, query_program_unit, &
        call_arguments_query_t, query_call_arguments, call_or_subscript_node, &
        subroutine_call_node, declaration_query_t, query_declaration
    use fortad_lower_types, only: lower_status_t
    implicit none
    private

    public :: has_same_file_call, validate_direct_call_boundaries

contains

    logical function has_same_file_call(arena) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer :: i, callee

        found = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (direct_same_file_call(arena, i, callee)) then
                found = .true.
                return
            end if
        end do
    end function has_same_file_call

    subroutine validate_direct_call_boundaries(arena, chosen, status)
        !! Follow direct calls reachable from CHOSEN and require FortFront's
        !! exact type, rank, and storage mapping for each one. Unrelated
        !! procedures in the same source do not affect the selected kernel.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: chosen
        type(lower_status_t), intent(out) :: status
        logical, allocatable :: reachable(:)
        logical :: changed
        logical :: is_subroutine
        integer :: i, caller, callee
        type(call_arguments_query_t) :: query

        status%ok = .true.
        status%message = ""
        if (chosen <= 0 .or. chosen > arena%size) return
        allocate (reachable(arena%size))
        reachable = .false.
        reachable(chosen) = .true.

        do
            changed = .false.
            do i = 1, arena%size
                if (.not. arena%has_node_at(i)) cycle
                if (.not. direct_same_file_call(arena, i, callee)) cycle
                caller = enclosing_procedure(arena, i)
                if (caller <= 0 .or. .not. reachable(caller)) cycle
                if (.not. reachable(callee)) then
                    reachable(callee) = .true.
                    changed = .true.
                end if
            end do
            if (.not. changed) exit
        end do

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. direct_same_file_call(arena, i, callee)) cycle
            is_subroutine = trim(arena%entries(i)%node_type) == "subroutine_call"
            caller = enclosing_procedure(arena, i)
            if (caller <= 0 .or. .not. reachable(caller)) cycle
            query = query_call_arguments(arena, i)
            if (.not. query%found) then
                ! Function expressions already have an established FortAD
                ! lowering path, including optional and keyword forwarding.
                ! Do not turn an unresolved function-expression query into a
                ! new refusal. A subroutine-call statement, however, must
                ! have an exact boundary contract.
                if (.not. is_subroutine) cycle
                call refuse(arena, i, "FortFront could not prove an exact "// &
                    "actual/formal mapping; ambiguous, unresolved, or non-storage actual", &
                    status)
                return
            end if
            if (query%is_refused) then
                if (.not. query%has_procedure_callback .or. &
                    has_non_callback_refusal(arena, query)) then
                    call refuse_query(arena, query, status)
                    return
                end if
            end if
            call validate_arguments(arena, query, status)
            if (.not. status%ok) return
        end do
    end subroutine validate_direct_call_boundaries

    logical function direct_same_file_call(arena, node_index, callee) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: callee
        character(len=:), allocatable :: name
        type(program_unit_query_t) :: unit
        integer :: i

        found = .false.
        callee = 0
        name = ""
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            if (node%base_expr_index /= 0) return
            if (allocated(node%name)) name = node%name
            type is (subroutine_call_node)
            if (allocated(node%name)) name = node%name
        class default
            return
        end select
        if (len_trim(name) == 0) return
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(arena%entries(i)%node_type) /= "function_def" .and. &
                trim(arena%entries(i)%node_type) /= "subroutine_def") cycle
            unit = query_program_unit(arena, i)
            if (.not. unit%found) cycle
            if (same_name(unit%name, name)) then
                callee = i
                found = .true.
                return
            end if
        end do
    end function direct_same_file_call

    integer function enclosing_procedure(arena, node_index) result(proc)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        proc = 0
        if (node_index <= 0 .or. node_index > arena%size) return
        current = arena%entries(node_index)%parent_index
        do while (current > 0)
            if (.not. arena%has_node_at(current)) return
            if (trim(arena%entries(current)%node_type) == "function_def" .or. &
                trim(arena%entries(current)%node_type) == "subroutine_def") then
                proc = current
                return
            end if
            current = arena%entries(current)%parent_index
        end do
    end function enclosing_procedure

    subroutine validate_arguments(arena, query, status)
        type(ast_arena_t), intent(in) :: arena
        type(call_arguments_query_t), intent(in) :: query
        type(lower_status_t), intent(out) :: status
        integer :: i
        character(len=:), allocatable :: intent

        status%ok = .true.
        status%message = ""
        do i = 1, size(query%arguments)
            if (.not. query%arguments(i)%is_supplied) cycle
            if (is_procedure_dummy(arena, query%arguments(i)%formal_node_index)) cycle
            if (query%arguments(i)%formal_is_pointer .or. &
                query%arguments(i)%actual_is_pointer) then
                call refuse(arena, query%call_node_index, &
                    "pointer storage is not part of this slice", status)
                return
            end if
            if (query%arguments(i)%formal_is_allocatable .or. &
                query%arguments(i)%actual_is_allocatable) then
                call refuse(arena, query%call_node_index, &
                    "allocatable storage is not part of this slice", status)
                return
            end if
            if (query%arguments(i)%formal_is_target .or. &
                query%arguments(i)%actual_is_target) then
                call refuse(arena, query%call_node_index, &
                    "TARGET alias storage is not part of this slice", status)
                return
            end if
            if (.not. query%arguments(i)%formal_type_known .or. &
                .not. query%arguments(i)%formal_rank_known .or. &
                .not. query%arguments(i)%actual_type_known .or. &
                .not. query%arguments(i)%actual_rank_known .or. &
                .not. query%arguments(i)%type_compatibility_known) then
                call refuse(arena, query%call_node_index, &
                    "FortFront did not expose complete type/kind/rank facts", status)
                return
            end if
            if (query%arguments(i)%has_type_mismatch) then
                call refuse(arena, query%call_node_index, &
                    "formal and actual type/kind/rank do not match exactly", status)
                return
            end if
            intent = lower(query%arguments(i)%formal_intent)
            if (.not. query%arguments(i)%formal_is_value .and. &
                trim(intent) /= "in") then
                if (query%arguments(i)%actual_value_node_index <= 0 .or. &
                    trim(arena%entries(query%arguments(i)%actual_value_node_index)%node_type) &
                    /= "identifier") then
                    call refuse(arena, query%call_node_index, &
                        "writable formal requires a plain scalar or whole-array actual", &
                        status)
                    return
                end if
            end if
        end do
    end subroutine validate_arguments

    logical function has_non_callback_refusal(arena, query) result(refused)
        !! A procedure-pointer actual is contextual metadata, not a data
        !! alias.  Preserve ordinary call-boundary refusals for every other
        !! actual while letting the P8.6 consumer inspect the callback facts.
        type(ast_arena_t), intent(in) :: arena
        type(call_arguments_query_t), intent(in) :: query
        integer :: i

        refused = query%has_global_mutable_state .or. &
            query%has_type_mismatch .or. query%has_unknown_argument_types
        if (refused) return
        do i = 1, size(query%arguments)
            if (.not. query%arguments(i)%is_supplied) cycle
            if (is_procedure_dummy(arena, query%arguments(i)%formal_node_index)) cycle
            if (query%arguments(i)%formal_is_pointer .or. &
                query%arguments(i)%formal_is_allocatable .or. &
                query%arguments(i)%formal_is_target .or. &
                query%arguments(i)%actual_is_pointer .or. &
                query%arguments(i)%actual_is_allocatable .or. &
                query%arguments(i)%actual_is_target) then
                refused = .true.
                return
            end if
        end do
    end function has_non_callback_refusal

    logical function is_procedure_dummy(arena, declaration_index) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: declaration_index
        type(declaration_query_t) :: declaration
        character(len=:), allocatable :: normalized
        integer :: i

        found = .false.
        declaration = query_declaration(arena, declaration_index)
        if (.not. declaration%found) return
        if (.not. allocated(declaration%type_name)) return
        normalized = ""
        do i = 1, len_trim(declaration%type_name)
            if (declaration%type_name(i:i) == " ") cycle
            normalized = normalized//lower(declaration%type_name(i:i))
        end do
        found = index(normalized, "procedure") == 1
    end function is_procedure_dummy

    subroutine refuse_query(arena, query, status)
        type(ast_arena_t), intent(in) :: arena
        type(call_arguments_query_t), intent(in) :: query
        type(lower_status_t), intent(out) :: status
        integer :: i
        character(len=:), allocatable :: reason

        reason = "unsupported call boundary"
        if (query%has_global_mutable_state) then
            reason = "callee reads active global mutable state"
        else if (query%has_procedure_callback) then
            reason = "procedure callback actual/formal is not part of this slice"
        else if (query%has_unresolved_alias) then
            reason = "pointer, allocatable, TARGET, or aliased storage is not part of this slice"
        else if (query%has_type_mismatch) then
            reason = "formal and actual type/kind/rank do not match exactly"
        else if (query%has_unknown_argument_types) then
            reason = "formal or actual type/kind/rank is unknown"
        end if
        do i = 1, size(query%arguments)
            if (.not. query%arguments(i)%is_supplied) cycle
            if (query%arguments(i)%formal_is_pointer .or. &
                query%arguments(i)%actual_is_pointer) then
                reason = "pointer storage is not part of this slice"
                exit
            end if
            if (query%arguments(i)%formal_is_allocatable .or. &
                query%arguments(i)%actual_is_allocatable) then
                reason = "allocatable storage is not part of this slice"
                exit
            end if
        end do
        call refuse(arena, query%call_node_index, reason, status)
    end subroutine refuse_query

    subroutine refuse(arena, node_index, reason, status)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: reason
        type(lower_status_t), intent(out) :: status
        character(len=32) :: line

        write (line, '(i0)') arena%get_node_line(node_index)
        status%ok = .false.
        status%message = "unsupported direct same-file procedure call at line "// &
            trim(line)//": "//trim(reason)
    end subroutine refuse

    logical function same_name(first, second) result(equal)
        character(len=*), intent(in) :: first, second
        integer :: i

        equal = len_trim(first) == len_trim(second)
        if (.not. equal) return
        do i = 1, len_trim(first)
            if (lower_char(first(i:i)) /= lower_char(second(i:i))) then
                equal = .false.
                return
            end if
        end do
    end function same_name

    character function lower_char(value)
        character, intent(in) :: value

        lower_char = value
        if (value >= "A" .and. value <= "Z") then
            lower_char = achar(iachar(value) + 32)
        end if
    end function lower_char

    function lower(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text
        integer :: i

        allocate (character(len=len(value)) :: text)
        text = value
        do i = 1, len(value)
            text(i:i) = lower_char(text(i:i))
        end do
    end function lower

end module fortad_call_boundaries
