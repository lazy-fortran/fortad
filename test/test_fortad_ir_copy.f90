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

    use fortad_ir, only: fad_stmt_t, fad_copy_stmt
    implicit none

    type(fad_stmt_t) :: original, copy
    integer :: failures

    failures = 0

    original%kind = 7
    original%target = "accumulator"
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

    call fad_copy_stmt(copy, original)

    call expect(copy%kind == original%kind, "kind", failures)
    call expect(allocated(copy%target), "target is allocated", failures)
    if (allocated(copy%target)) &
        call expect(copy%target == original%target, "target", failures)
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
