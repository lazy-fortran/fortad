program test_fortad_ir_copy
    !! `fad_copy_stmt` must copy every component of `fad_stmt_t`.
    !!
    !! The helper exists to work around an nvfortran internal compiler error on
    !! intrinsic assignment of this type, and it carries the cost every manual
    !! copy does: a component added to the type without a matching line in the
    !! helper is silently dropped, and the failure would appear far away as a
    !! statement that lost its target or its call arguments.
    !!
    !! This sets every component to a value distinguishable from its default,
    !! round-trips it, and compares. It is deliberately exhaustive rather than
    !! representative: checking a few fields would pass while a forgotten one
    !! is exactly the defect being guarded against.

    use fortad_ir, only: fad_stmt_t, fad_copy_stmt, fad_expr_t, fad_proc_t, &
        fad_expr_equal, FAD_CALL, FAD_CALL_STMT
    implicit none

    type(fad_stmt_t) :: original, copy, arena_stmt
    type(fad_expr_t) :: arena_expr, filler_expr
    type(fad_proc_t) :: proc
    integer :: failures, expr_index, duplicate_index, stmt_index, call_index, i
    character(len=16) :: label

    failures = 0

    original%kind = 7
    original%target = "accumulator"
    original%callback_formal = "callback"
    original%callback_target = "scale"
    original%value = 42
    original%is_automatic_reallocation = .true.
    original%lo = 3
    original%hi = 11
    original%step = 2
    original%line = 199
    original%call_args = [5, 6, 7]
    original%call_arg_names = [character(len=6) :: "alpha", "beta", "gamma"]
    original%allocation_args = [9, 10]
    original%allocation_source = 13
    original%allocation_mold = 17
    original%allocation_target_polymorphic = .true.
    original%allocation_target_unlimited_polymorphic = .true.
    original%allocation_target_component = .true.

    call fad_copy_stmt(copy, original)

    call expect(copy%kind == original%kind, "kind", failures)
    call expect(allocated(copy%target), "target is allocated", failures)
    if (allocated(copy%target)) &
        call expect(copy%target == original%target, "target", failures)
    call expect(allocated(copy%callback_formal), &
        "callback_formal is allocated", failures)
    if (allocated(copy%callback_formal)) &
        call expect(copy%callback_formal == original%callback_formal, &
        "callback_formal", failures)
    call expect(allocated(copy%callback_target), &
        "callback_target is allocated", failures)
    if (allocated(copy%callback_target)) &
        call expect(copy%callback_target == original%callback_target, &
        "callback_target", failures)
    call expect(copy%value == original%value, "value", failures)
    call expect(copy%is_automatic_reallocation .eqv. &
        original%is_automatic_reallocation, "is_automatic_reallocation", failures)
    call expect(copy%lo == original%lo, "lo", failures)
    call expect(copy%hi == original%hi, "hi", failures)
    call expect(copy%step == original%step, "step", failures)
    call expect(copy%line == original%line, "line", failures)
    call expect(allocated(copy%call_args), "call_args is allocated", failures)
    if (allocated(copy%call_args)) &
        call expect(all(copy%call_args == original%call_args), "call_args", &
        failures)
    call expect(allocated(copy%call_arg_names), "call_arg_names is allocated", &
        failures)
    if (allocated(copy%call_arg_names)) &
        call expect(all(copy%call_arg_names == original%call_arg_names), &
        "call_arg_names", failures)
    call expect(allocated(copy%allocation_args), "allocation_args is allocated", &
        failures)
    if (allocated(copy%allocation_args)) &
        call expect(all(copy%allocation_args == original%allocation_args), &
        "allocation_args", failures)
    call expect(copy%allocation_source == original%allocation_source, &
        "allocation_source", failures)
    call expect(copy%allocation_mold == original%allocation_mold, &
        "allocation_mold", failures)
    call expect(copy%allocation_target_polymorphic .eqv. &
        original%allocation_target_polymorphic, &
        "allocation_target_polymorphic", failures)
    call expect(copy%allocation_target_unlimited_polymorphic .eqv. &
        original%allocation_target_unlimited_polymorphic, &
        "allocation_target_unlimited_polymorphic", failures)
    call expect(copy%allocation_target_component .eqv. &
        original%allocation_target_component, "allocation_target_component", failures)

    ! An unallocated component must stay unallocated rather than become an
    ! empty string, which would turn a "no target" statement into one whose
    ! target is the empty name.
    block
        type(fad_stmt_t) :: bare, bare_copy
        bare%kind = 1
        call fad_copy_stmt(bare_copy, bare)
        call expect(.not. allocated(bare_copy%target), &
            "an unallocated target stays unallocated", failures)
        call expect(.not. allocated(bare_copy%call_args), &
            "unallocated call_args stay unallocated", failures)
    end block

    ! Arena growth must preserve every expression and statement component.
    ! The initial arenas hold 64 entries; the loops cross that boundary and
    ! then check both a retained entry and hash-consing of a duplicate.
    arena_expr%kind = FAD_CALL
    arena_expr%text = "kernel"
    arena_expr%callback_formal = "callback"
    arena_expr%callback_target = "scale"
    arena_expr%args = [11, 13]
    arena_expr%call_arg_names = [character(len=6) :: "first", "second"]
    arena_expr%rank = 3
    arena_expr%is_component_path = .true.
    arena_expr%component_is_allocatable = .true.
    arena_expr%component_is_pointer = .true.
    arena_expr%component_is_target = .true.
    arena_expr%component_is_polymorphic = .true.
    arena_expr%component_is_global = .true.
    arena_expr%component_is_real = .true.
    arena_expr%component_rank = 2
    arena_expr%component_type_name = "child_t"
    arena_expr%component_original_path = "state%child"
    expr_index = proc%add_expr(arena_expr)

    do i = 1, 64
        write (label, '("filler_", i0)') i
        filler_expr%kind = 2
        filler_expr%text = trim(label)
        filler_expr%rank = i
        stmt_index = proc%add_expr(filler_expr)
    end do
    duplicate_index = proc%add_expr(arena_expr)
    call expect(duplicate_index == expr_index, &
        "expression hash-consing after arena growth", failures)
    call expect(fad_expr_equal(proc%exprs(expr_index), arena_expr), &
        "expression fields after arena growth", failures)

    ! The keyword-name path must construct the deferred-length array in place;
    ! nvfortran 26.5 overruns the heap when this is returned as a derived
    ! function value or assigned from an assumed-length character array.
    call_index = proc%add_expr_call("keyword_kernel", [21, 34], &
        [character(len=8) :: "first", "second"])
    call expect(proc%exprs(call_index)%call_arg_names(1) == "first", &
        "in-place call keyword name", failures)
    call expect(proc%exprs(call_index)%call_arg_names(2) == "second", &
        "in-place second keyword name", failures)

    do i = 1, 64
        arena_stmt%kind = FAD_CALL_STMT
        arena_stmt%target = "callee"
        arena_stmt%callback_formal = "callback"
        arena_stmt%callback_target = "scale"
        arena_stmt%value = i
        arena_stmt%call_args = [i, i + 1]
        arena_stmt%call_arg_names = [character(len=1) :: "x", "y"]
        stmt_index = proc%add_stmt(arena_stmt)
    end do
    call expect(proc%n_stmts == 64, "statement arena size", failures)
    call expect(proc%stmts(1)%value == 1, &
        "first statement after arena growth", failures)
    call expect(proc%stmts(64)%value == 64, &
        "last statement after arena growth", failures)
    call expect(proc%stmts(64)%callback_formal == "callback", &
        "statement callback formal after arena growth", failures)
    call expect(proc%stmts(64)%callback_target == "scale", &
        "statement callback target after arena growth", failures)
    call expect(all(proc%stmts(64)%call_args == [64, 65]), &
        "statement arguments after arena growth", failures)

    if (failures == 0) then
        print *, "test_fortad_ir_copy: PASS"
    else
        print *, "test_fortad_ir_copy: FAIL", failures
        error stop 1
    end if

contains

    subroutine expect(condition, description, failures)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        integer, intent(inout) :: failures

        if (.not. condition) then
            failures = failures + 1
            print *, "  FAIL: component not copied: ", description
        end if
    end subroutine expect

end program test_fortad_ir_copy
