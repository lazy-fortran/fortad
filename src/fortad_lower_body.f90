module fortad_lower_body
    !! Lower procedure metadata and delegate bodies to a separate unit.
    use fortfront, only: ast_arena_t, program_unit_query_t, identifier_node, &
        parameter_declaration_node, declaration_node
    use fortad_ir, only: fad_proc_t, fad_decl_t
    use fortad_lower_statements, only: lower_body, inherit_module_uses
    use fortad_lower_types, only: lower_status_t
    implicit none
    private

    public :: lower_function, lower_subroutine, inherit_module_uses

contains

    subroutine infer_real_suffix(proc)
        !! Choose the kind suffix for real literals fortad emits, from the
        !! primal's own real declarations.
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
                    proc%real_suffix = "_"//trim(tn(paren + 1:len(tn) - 1))
                end select
                return
            end associate
        end do
    end subroutine infer_real_suffix

    subroutine lower_function(arena, unit, source, proc, status)
        !! Lower a function definition.
        type(ast_arena_t), intent(in) :: arena
        type(program_unit_query_t), intent(in) :: unit
        character(len=*), intent(in) :: source
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status
        type(fad_decl_t) :: d

        proc%name = unit%name
        proc%is_function = .true.
        if (allocated(unit%result_name)) then
            proc%result_name = unit%result_name
        else
            proc%result_name = unit%name
        end if
        call collect_params(arena, unit%parameter_indices, proc)
        call lower_body(arena, unit%body_indices, proc, status)
        if (.not. status%ok) return
        call infer_real_suffix(proc)

        if (proc%decl_index(proc%result_name) == 0) then
            d%name = proc%result_name
            if (allocated(unit%return_type)) then
                d%type_name = unit%return_type
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

    subroutine lower_subroutine(arena, unit, source, proc, status)
        !! Lower a subroutine definition.
        type(ast_arena_t), intent(in) :: arena
        type(program_unit_query_t), intent(in) :: unit
        character(len=*), intent(in) :: source
        type(fad_proc_t), intent(inout) :: proc
        type(lower_status_t), intent(out) :: status

        proc%name = unit%name
        proc%is_function = .false.
        call collect_params(arena, unit%parameter_indices, proc)
        call lower_body(arena, unit%body_indices, proc, status)
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
            if (.not. arena%has_node_at(param_indices(i))) cycle
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

end module fortad_lower_body
