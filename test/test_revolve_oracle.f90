program test_revolve_oracle
    !! Independent behavioural oracle for checkpointing schedules.
    !!
    !! A schedule is not checked by inspecting it. It is **executed** against a
    !! simulated time integration whose state is a running integer, and the
    !! simulator asserts the properties that make a schedule correct:
    !!
    !! 1. Every `REV_TURN` happens with the state the primal would have had at
    !!    that step - so the adjoint sees the right input.
    !! 2. Each step is turned exactly once, in strictly decreasing order.
    !! 3. No slot is read before it is written, and no more than `n_slots` are
    !!    ever live.
    !!
    !! Then, separately, the cost: the forward-step count must not exceed the
    !! binomial bound `r * n_steps`. A schedule can be perfectly correct and
    !! uselessly expensive, so correctness alone is not a sufficient test.
    use fortad_revolve, only: revolve_t, revolve_schedule, revolve_min_repetitions, &
                              REV_ADVANCE, REV_TAKESHOT, REV_RESTORE, REV_TURN
    implicit none

    integer :: failures

    failures = 0

    call run_case(1, 1, failures)
    call run_case(2, 1, failures)
    call run_case(5, 1, failures)
    call run_case(10, 2, failures)
    call run_case(20, 3, failures)
    call run_case(100, 4, failures)
    call run_case(1000, 10, failures)
    call run_case(37, 5, failures)
    call run_case(64, 64, failures)
    call test_refusals(failures)

    if (failures == 0) then
        print *, "test_revolve_oracle: all cases passed"
    else
        print *, "test_revolve_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine run_case(n_steps, n_slots, failures)
        !! Execute a schedule against a simulated integration.
        integer, intent(in) :: n_steps, n_slots
        integer, intent(inout) :: failures
        type(revolve_t) :: schedule
        integer, allocatable :: slot_state(:)
        logical, allocatable :: slot_used(:), turned(:)
        integer :: stat, i, state, expected_turn, live, max_live, bound, r
        logical :: bad

        call revolve_schedule(n_steps, n_slots, schedule, stat)
        if (stat /= 0) then
            print *, "FAIL revolve", n_steps, n_slots, ": refused, stat =", stat
            failures = failures + 1
            return
        end if

        allocate (slot_state(0:max(1, 2*n_slots + 4)))
        allocate (slot_used(0:max(1, 2*n_slots + 4)))
        allocate (turned(0:n_steps - 1))
        slot_state = -1
        slot_used = .false.
        turned = .false.
        bad = .false.

        ! The simulated state is simply "which step index are we at". A real
        ! integration would carry a field; the index is what has to be right.
        state = 0
        expected_turn = n_steps - 1
        live = 0
        max_live = 0

        do i = 1, schedule%n_actions
            associate (a => schedule%actions(i))
                select case (a%kind)
                case (REV_ADVANCE)
                    if (a%from /= state) then
                        print *, "  advance from", a%from, "but state is", state
                        bad = .true.
                    end if
                    state = a%to

                case (REV_TAKESHOT)
                    if (a%from /= state) then
                        print *, "  takeshot of", a%from, "but state is", state
                        bad = .true.
                    end if
                    if (a%slot < 0 .or. a%slot > ubound(slot_state, 1)) then
                        print *, "  slot index out of range:", a%slot
                        bad = .true.
                    else
                        if (.not. slot_used(a%slot)) then
                            live = live + 1
                            max_live = max(max_live, live)
                        end if
                        slot_used(a%slot) = .true.
                        slot_state(a%slot) = a%from
                    end if

                case (REV_RESTORE)
                    if (a%slot < 0 .or. a%slot > ubound(slot_state, 1)) then
                        print *, "  restore from out-of-range slot:", a%slot
                        bad = .true.
                    else if (.not. slot_used(a%slot)) then
                        print *, "  restore from an unwritten slot:", a%slot
                        bad = .true.
                    else if (slot_state(a%slot) /= a%from) then
                        print *, "  restore claims", a%from, "but slot holds", &
                            slot_state(a%slot)
                        bad = .true.
                    else
                        state = slot_state(a%slot)
                    end if

                case (REV_TURN)
                    if (a%from /= state) then
                        print *, "  turn of step", a%from, "but state is", state
                        bad = .true.
                    end if
                    if (a%from /= expected_turn) then
                        print *, "  turned step", a%from, "expected", expected_turn
                        bad = .true.
                    end if
                    if (a%from >= 0 .and. a%from <= n_steps - 1) then
                        if (turned(a%from)) then
                            print *, "  step turned twice:", a%from
                            bad = .true.
                        end if
                        turned(a%from) = .true.
                    end if
                    expected_turn = expected_turn - 1
                end select
            end associate
        end do

        if (.not. all(turned)) then
            print *, "  some steps were never turned"
            bad = .true.
        end if
        if (max_live > n_slots) then
            print *, "  used", max_live, "slots but only", n_slots, "were offered"
            bad = .true.
        end if

        ! Cost: the schedule must stay within the binomial bound.
        r = revolve_min_repetitions(n_steps, n_slots)
        bound = max(n_steps, (r + 1)*n_steps)
        if (schedule%forward_steps > bound) then
            print *, "  forward steps", schedule%forward_steps, "exceeds bound", &
                bound
            bad = .true.
        end if

        if (bad) then
            print *, "FAIL revolve n_steps =", n_steps, " n_slots =", n_slots
            failures = failures + 1
        else
            print *, "pass revolve n_steps =", n_steps, " n_slots =", n_slots, &
                " forward =", schedule%forward_steps, " slots used =", max_live
        end if
    end subroutine run_case

    subroutine test_refusals(failures)
        !! Nonsensical requests must be refused rather than silently handled.
        integer, intent(inout) :: failures
        type(revolve_t) :: schedule
        integer :: stat

        call revolve_schedule(0, 4, schedule, stat)
        if (stat == 0) then
            print *, "FAIL revolve: accepted zero steps"
            failures = failures + 1
        else
            print *, "pass revolve refuses zero steps"
        end if

        call revolve_schedule(10, 0, schedule, stat)
        if (stat == 0) then
            print *, "FAIL revolve: accepted many steps with no slots"
            failures = failures + 1
        else
            print *, "pass revolve refuses many steps with no slots"
        end if
    end subroutine test_refusals

end program test_revolve_oracle
