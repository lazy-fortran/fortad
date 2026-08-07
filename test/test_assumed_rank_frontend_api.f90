program test_assumed_rank_frontend_api
    !! The public FortAD facade must forward the current FortFront facts a
    !! transformer needs for an assumed-rank SELECT RANK boundary.  The
    !! expected facts are independent of the implementation's AST layout:
    !! they are the language-level declaration and guard properties.
    use fortad, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, declaration_query_t, &
        query_declaration, array_bounds_query_t, query_array_bounds, &
        storage_query_t, query_storage, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_RANK, CONTROL_RANK_BLOCK
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        'module assumed_rank_api'//nl// &
        'contains'//nl// &
        '  subroutine inspect(values, ordinary)'//nl// &
        '    real, intent(in) :: values(..)'//nl// &
        '    real, intent(in) :: ordinary(:)'//nl// &
        '    select rank (values)'//nl// &
        '    rank (1)'//nl// &
        '    rank (2)'//nl// &
        '    rank default'//nl// &
        '    end select'//nl// &
        '  end subroutine inspect'//nl// &
        'end module assumed_rank_api'//nl

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(declaration_query_t) :: declaration
    type(array_bounds_query_t) :: bounds
    type(storage_query_t) :: storage
    type(control_statement_query_t) :: control, arm
    integer :: i, j, assumed_rank_index, ordinary_index, select_rank_index
    integer :: rank_one_count, rank_two_count, default_count

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected assumed-rank source')

    assumed_rank_index = 0
    ordinary_index = 0
    select_rank_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        declaration = query_declaration(result%arena, i)
        if (declaration%found) then
            if (trim(declaration%name) == 'values') assumed_rank_index = i
            if (trim(declaration%name) == 'ordinary') ordinary_index = i
        end if
        if (trim(get_node_type_at(result%arena, i)) == 'select_rank') then
            select_rank_index = i
        end if
    end do
    call require(assumed_rank_index > 0, 'assumed-rank dummy declaration missing')
    call require(ordinary_index > 0, 'ordinary-rank dummy declaration missing')
    call require(select_rank_index > 0, 'SELECT RANK node missing')

    declaration = query_declaration(result%arena, assumed_rank_index)
    call require(declaration%is_array .and. size(declaration%dimension_indices) == 1, &
        'assumed-rank declaration shape fact is missing')
    bounds = query_array_bounds(result%arena, declaration%dimension_indices(1))
    call require(bounds%found .and. bounds%is_assumed_rank, &
        'assumed-rank bounds fact is missing')
    storage = query_storage(result%arena, assumed_rank_index)
    call require(storage%found .and. trim(storage%type_name) == 'real' .and. &
        .not. storage%is_allocatable, &
        'assumed-rank storage/type facts are incorrect')

    declaration = query_declaration(result%arena, ordinary_index)
    call require(declaration%is_array .and. size(declaration%dimension_indices) == 1, &
        'ordinary-rank declaration shape fact is missing')
    bounds = query_array_bounds(result%arena, declaration%dimension_indices(1))
    call require(.not. bounds%found, &
        'ordinary rank was incorrectly reported as an assumed-rank bounds node')
    storage = query_storage(result%arena, ordinary_index)
    call require(storage%found .and. trim(storage%type_name) == 'real', &
        'ordinary-rank storage/type facts are incorrect')

    control = query_control_statement(result%arena, select_rank_index)
    call require(control%found .and. control%statement_kind == CONTROL_SELECT_RANK .and. &
        control%has_selector .and. size(control%child_node_indices) == 2 .and. &
        control%has_default, 'SELECT RANK guard facts are incomplete')
    rank_one_count = 0
    rank_two_count = 0
    default_count = 0
    do j = 1, size(control%child_node_indices)
        arm = query_control_statement(result%arena, control%child_node_indices(j))
        call require(arm%found .and. arm%statement_kind == CONTROL_RANK_BLOCK, &
            'SELECT RANK arm kind is missing')
        if (arm%has_rank .and. arm%rank_value == 1) rank_one_count = rank_one_count + 1
        if (arm%has_rank .and. arm%rank_value == 2) rank_two_count = rank_two_count + 1
    end do
    arm = query_control_statement(result%arena, control%default_node_index)
    if (arm%found .and. arm%statement_kind == CONTROL_RANK_BLOCK .and. arm%is_default) &
        default_count = 1
    call require(rank_one_count == 1 .and. rank_two_count == 1 .and. &
        default_count == 1, 'SELECT RANK arm facts are incorrect')

    print *, 'PASS: assumed-rank frontend API facts'

contains

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_assumed_rank_frontend_api
