module fortad_pattern
    !! Conservative static dependency propagation for lowered procedures.
    !!
    !! The result is a structural pattern: an entry is present when the
    !! corresponding output may depend on the corresponding input. The pass
    !! deliberately unions assignments across control flow, so it may retain a
    !! harmless extra entry but never drops a possible derivative.
    use fortad_ir, only: fad_proc_t, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, &
                         FAD_END_IF, FAD_CALL_STMT, FAD_DIRECTIVE, FAD_CONST, &
                         FAD_VAR, FAD_INDEX
    use fortad_sparse, only: sparsity_t
    implicit none
    private

    public :: pattern_from_proc

contains

    subroutine pattern_from_proc(proc, independents, dependents, pattern, stat)
        !! Propagate structural dependencies through a lowered procedure.
        type(fad_proc_t), intent(in) :: proc
        character(len=*), intent(in) :: independents(:), dependents(:)
        type(sparsity_t), intent(out) :: pattern
        integer, intent(out) :: stat
        character(len=:), allocatable :: names(:)
        logical, allocatable :: dependencies(:, :), control(:), stack(:, :)
        logical, allocatable :: rhs(:), call_deps(:)
        integer :: n_names, width, n_inputs, n_outputs, i, j, k, top
        integer :: name_index, target_index, nnz
        character(len=:), allocatable :: target

        stat = 0
        n_inputs = size(independents)
        n_outputs = size(dependents)
        if (n_inputs < 1 .or. n_outputs < 1) then
            stat = 1
            call empty_pattern(pattern)
            return
        end if

        width = 1
        do i = 1, n_inputs
            width = max(width, len_trim(independents(i)))
        end do
        do i = 1, n_outputs
            width = max(width, len_trim(dependents(i)))
        end do
        do i = 1, proc%n_exprs
            if (proc%exprs(i)%kind == FAD_VAR .or. &
                proc%exprs(i)%kind == FAD_INDEX) then
                width = max(width, len_trim(proc%exprs(i)%text))
            end if
        end do
        do i = 1, proc%n_stmts
            if (proc%stmts(i)%kind == FAD_ASSIGN .or. &
                proc%stmts(i)%kind == FAD_DO) then
                width = max(width, len_trim(proc%stmts(i)%target))
            end if
        end do

        allocate (character(len=width) :: &
                  names(max(1, proc%n_decls + proc%n_exprs + &
                           2*proc%n_stmts + n_inputs + n_outputs)))
        n_names = 0
        do i = 1, n_inputs
            call add_name(names, n_names, independents(i))
        end do
        do i = 1, n_outputs
            call add_name(names, n_names, dependents(i))
        end do
        do i = 1, proc%n_exprs
            if (proc%exprs(i)%kind == FAD_VAR .or. &
                proc%exprs(i)%kind == FAD_INDEX) then
                call add_name(names, n_names, proc%exprs(i)%text)
            end if
        end do
        do i = 1, proc%n_stmts
            if (proc%stmts(i)%kind == FAD_ASSIGN) then
                target = base_name(proc%stmts(i)%target)
                call add_name(names, n_names, target)
            else if (proc%stmts(i)%kind == FAD_DO) then
                call add_name(names, n_names, proc%stmts(i)%target)
            end if
        end do

        allocate (dependencies(n_names, n_inputs), control(n_inputs))
        dependencies = .false.
        control = .false.
        do i = 1, n_inputs
            name_index = find_name(names, n_names, independents(i))
            if (name_index == 0) then
                stat = 2
                call empty_pattern(pattern)
                return
            end if
            dependencies(name_index, i) = .true.
        end do

        allocate (stack(max(1, proc%n_stmts), n_inputs))
        stack = .false.
        top = 0
        do i = 1, proc%n_stmts
            select case (proc%stmts(i)%kind)
            case (FAD_IF, FAD_DO)
                top = top + 1
                if (top > size(stack, 1)) then
                    stat = 3
                    call empty_pattern(pattern)
                    return
                end if
                stack(top, :) = control
                allocate (rhs(n_inputs))
                rhs = .false.
                if (proc%stmts(i)%value > 0) then
                    call expression_dependencies(proc, proc%stmts(i)%value, &
                                                  names, n_names, dependencies, rhs)
                end if
                if (proc%stmts(i)%kind == FAD_DO) then
                    call expression_dependencies(proc, proc%stmts(i)%lo, names, &
                                                  n_names, dependencies, rhs)
                    call expression_dependencies(proc, proc%stmts(i)%hi, names, &
                                                  n_names, dependencies, rhs)
                    call expression_dependencies(proc, proc%stmts(i)%step, names, &
                                                  n_names, dependencies, rhs)
                end if
                control = control .or. rhs
                deallocate (rhs)

            case (FAD_END_IF, FAD_END_DO)
                if (top > 0) then
                    control = stack(top, :)
                    top = top - 1
                end if

            case (FAD_ASSIGN)
                allocate (rhs(n_inputs))
                rhs = .false.
                call expression_dependencies(proc, proc%stmts(i)%value, names, &
                                              n_names, dependencies, rhs)
                rhs = rhs .or. control
                target = base_name(proc%stmts(i)%target)
                target_index = find_name(names, n_names, target)
                if (target_index > 0) then
                    dependencies(target_index, :) = dependencies(target_index, :) &
                        .or. rhs
                end if
                deallocate (rhs)

            case (FAD_CALL_STMT)
                ! Without a callee body, conservatively connect every actual to
                ! every other actual. This preserves possible in/out effects
                ! while retaining the explicit-call refusal boundary elsewhere.
                allocate (call_deps(n_inputs))
                call_deps = control
                do j = 1, size(proc%stmts(i)%call_args)
                    call expression_dependencies(proc, proc%stmts(i)%call_args(j), &
                                                  names, n_names, dependencies, &
                                                  call_deps)
                end do
                do j = 1, size(proc%stmts(i)%call_args)
                    target = expression_base(proc, proc%stmts(i)%call_args(j))
                    target_index = find_name(names, n_names, target)
                    if (target_index > 0) then
                        dependencies(target_index, :) = dependencies(target_index, :) &
                            .or. call_deps
                    end if
                end do
                deallocate (call_deps)

            case (FAD_DIRECTIVE)
                ! Directives carry no data-flow expression in the lowered IR.
            end select
        end do

        pattern%n_rows = n_outputs
        pattern%n_cols = n_inputs
        allocate (pattern%col_start(n_inputs + 1))
        nnz = 0
        do j = 1, n_inputs
            pattern%col_start(j) = nnz + 1
            do i = 1, n_outputs
                name_index = find_name(names, n_names, dependents(i))
                if (name_index > 0) then
                    if (dependencies(name_index, j)) nnz = nnz + 1
                end if
            end do
        end do
        pattern%col_start(n_inputs + 1) = nnz + 1
        allocate (pattern%rows(nnz))
        k = 0
        do j = 1, n_inputs
            do i = 1, n_outputs
                name_index = find_name(names, n_names, dependents(i))
                if (name_index > 0) then
                    if (dependencies(name_index, j)) then
                        k = k + 1
                        pattern%rows(k) = i
                    end if
                end if
            end do
        end do
    end subroutine pattern_from_proc

    recursive subroutine expression_dependencies(proc, index, names, n_names, &
                                                 dependencies, out)
        !! Collect input dependencies of one expression.
        type(fad_proc_t), intent(in) :: proc
        integer, intent(in) :: index
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        logical, intent(in) :: dependencies(:, :)
        logical, intent(inout) :: out(:)
        integer :: i, name_index

        if (index <= 0 .or. index > proc%n_exprs) return
        select case (proc%exprs(index)%kind)
        case (FAD_CONST)
            return
        case (FAD_VAR, FAD_INDEX)
            name_index = find_name(names, n_names, proc%exprs(index)%text)
            if (name_index > 0) out = out .or. dependencies(name_index, :)
        end select
        do i = 1, size(proc%exprs(index)%args)
            call expression_dependencies(proc, proc%exprs(index)%args(i), names, &
                                          n_names, dependencies, out)
        end do
    end subroutine expression_dependencies

    function expression_base(proc, index) result(name)
        !! Base variable of a call actual, or blank for a non-variable actual.
        type(fad_proc_t), intent(in) :: proc
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        name = ""
        if (index <= 0 .or. index > proc%n_exprs) return
        if (proc%exprs(index)%kind == FAD_VAR .or. &
            proc%exprs(index)%kind == FAD_INDEX) then
            name = trim(proc%exprs(index)%text)
        end if
    end function expression_base

    function base_name(text) result(name)
        !! Remove an array subscript from a rendered assignment target.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: name
        integer :: open

        open = index(text, "(")
        if (open > 1) then
            name = trim(text(:open - 1))
        else
            name = trim(text)
        end if
    end function base_name

    subroutine add_name(names, n_names, name)
        character(len=*), intent(inout) :: names(:)
        integer, intent(inout) :: n_names
        character(len=*), intent(in) :: name

        if (len_trim(name) == 0) return
        if (find_name(names, n_names, name) > 0) return
        n_names = n_names + 1
        names(n_names) = trim(name)
    end subroutine add_name

    integer function find_name(names, n_names, name) result(index)
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n_names
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, n_names
            if (same_name(names(i), name)) then
                index = i
                return
            end if
        end do
    end function find_name

    logical function same_name(a, b) result(equal)
        character(len=*), intent(in) :: a, b
        integer :: i

        equal = .false.
        if (len_trim(a) /= len_trim(b)) return
        do i = 1, len_trim(a)
            if (lower(a(i:i)) /= lower(b(i:i))) return
        end do
        equal = .true.
    end function same_name

    character function lower(c)
        character, intent(in) :: c

        lower = c
        if (c >= "A" .and. c <= "Z") lower = achar(iachar(c) + 32)
    end function lower

    subroutine empty_pattern(pattern)
        type(sparsity_t), intent(out) :: pattern

        pattern%n_rows = 0
        pattern%n_cols = 0
        allocate (pattern%col_start(1), pattern%rows(0))
        pattern%col_start = 1
    end subroutine empty_pattern

end module fortad_pattern
