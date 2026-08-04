module fortad_revolve
    !! Binomial checkpointing schedules for time-stepping adjoints.
    !!
    !! The adjoint of an `n`-step integration needs each step's input state
    !! again, in reverse order. Storing all `n` is often impossible; storing
    !! none means re-running from the start for every step, which is `O(n²)`
    !! work. Revolve (Griewank & Walther, ACM TOMS 26(1), 2000) is the optimal
    !! compromise: with `s` checkpoint slots it achieves the minimum possible
    !! recomputation, and that minimum grows only logarithmically.
    !!
    !! Concretely, `s` slots and `r` repetitions cover `binom(s+r, s)` steps.
    !! So 10 slots and 5 repetitions cover 3003 steps: 5x the forward work
    !! instead of 3003x, for 10 states in memory rather than 3003.
    !!
    !! What is produced here is a **schedule**, not a driver. fortad does not
    !! own your time loop, your state, or your storage, and a framework that
    !! demanded to would be useless in exactly the codes that need this most.
    !! Ask for the actions and execute them.
    implicit none
    private

    public :: revolve_t, revolve_action_t, revolve_schedule
    public :: REV_ADVANCE, REV_TAKESHOT, REV_RESTORE, REV_TURN
    public :: revolve_forward_steps, revolve_min_repetitions

    !! Run the primal forward from `from` to `to`.
    integer, parameter :: REV_ADVANCE = 1
    !! Save the current state into slot `slot`; it is the state at step `from`.
    integer, parameter :: REV_TAKESHOT = 2
    !! Restore the state in slot `slot`, which is the state at step `from`.
    integer, parameter :: REV_RESTORE = 3
    !! Take the adjoint of step `from`, whose input state is current.
    integer, parameter :: REV_TURN = 4

    type :: revolve_action_t
        integer :: kind = 0
        integer :: from = 0
        integer :: to = 0
        integer :: slot = 0
    end type revolve_action_t

    type :: revolve_t
        !! A complete schedule for `n_steps` steps in `n_slots` checkpoints.
        integer :: n_steps = 0
        integer :: n_slots = 0
        integer :: n_actions = 0
        !! Forward steps executed, the cost this whole construction minimises.
        integer :: forward_steps = 0
        type(revolve_action_t), allocatable :: actions(:)
    end type revolve_t

contains

    subroutine revolve_schedule(n_steps, n_slots, schedule, stat)
        !! Build the schedule for `n_steps` steps with `n_slots` checkpoints.
        integer, intent(in) :: n_steps, n_slots
        type(revolve_t), intent(out) :: schedule
        !! 0 on success; 1 for a nonsensical size; 2 when no slots are offered
        !! and more than one step is requested.
        integer, intent(out), optional :: stat

        if (present(stat)) stat = 0
        schedule%n_steps = n_steps
        schedule%n_slots = n_slots
        schedule%n_actions = 0
        schedule%forward_steps = 0
        allocate (schedule%actions(max(16, 8*n_steps)))

        if (n_steps < 1 .or. n_slots < 0) then
            if (present(stat)) stat = 1
            return
        end if
        if (n_slots < 1 .and. n_steps > 1) then
            ! With no storage at all every step would have to be reached by
            ! replaying from the start, and the caller has no state to replay
            ! from. Refusing is better than pretending.
            if (present(stat)) stat = 2
            return
        end if

        call plan(schedule, 0, n_steps, n_slots, 0)
    end subroutine revolve_schedule

    recursive subroutine plan(schedule, lo, hi, slots, next_slot)
        !! Schedule the adjoint of steps `lo` .. `hi-1`, given that the state at
        !! `lo` is current and `slots` checkpoint slots are free.
        !!
        !! The split point is chosen so the two halves cost the same number of
        !! repetitions, which is what makes the total optimal rather than merely
        !! logarithmic.
        type(revolve_t), intent(inout) :: schedule
        integer, intent(in) :: lo, hi, slots, next_slot
        integer :: mid

        if (hi <= lo) return

        if (hi - lo == 1) then
            call emit(schedule, REV_TURN, lo, lo + 1, 0)
            return
        end if

        if (slots < 1) then
            ! No storage left: replay from `lo` before each remaining step.
            call plan_without_slots(schedule, lo, hi)
            return
        end if

        mid = lo + split(hi - lo, slots)
        if (mid <= lo) mid = lo + 1
        if (mid >= hi) mid = hi - 1

        ! Keep the state at `lo`, run to `mid`, and adjoint the far half first.
        call emit(schedule, REV_TAKESHOT, lo, lo, next_slot)
        call emit_advance(schedule, lo, mid)
        call plan(schedule, mid, hi, slots - 1, next_slot + 1)
        ! Then come back to `lo` and adjoint the near half, reusing the slot.
        call emit(schedule, REV_RESTORE, lo, lo, next_slot)
        call plan(schedule, lo, mid, slots, next_slot)
    end subroutine plan

    recursive subroutine plan_without_slots(schedule, lo, hi)
        !! Fallback when no slot is free: the state at `lo` is current, so walk
        !! forward to each step in turn, adjointing from the last backwards.
        !! Quadratic, and only reached when the caller starved the schedule.
        type(revolve_t), intent(inout) :: schedule
        integer, intent(in) :: lo, hi
        integer :: k

        do k = hi - 1, lo, -1
            if (k > lo) call emit_advance(schedule, lo, k)
            call emit(schedule, REV_TURN, k, k + 1, 0)
        end do
    end subroutine plan_without_slots

    integer function split(length, slots) result(mid)
        !! Offset of the optimal split inside a run of `length` steps.
        !!
        !! With `r` the smallest repetition count that covers `length`, the
        !! balanced split puts `binom(slots+r-1, slots)` steps in the near half.
        !! That is the choice which makes both halves reach their bound at the
        !! same repetition number, and it is where optimality comes from.
        integer, intent(in) :: length, slots
        integer :: r

        r = revolve_min_repetitions(length, slots)
        mid = binomial(slots + r - 1, slots)
        if (mid < 1) mid = 1
        if (mid > length - 1) mid = length - 1
    end function split

    integer function revolve_min_repetitions(n_steps, n_slots) result(r)
        !! Smallest `r` with `binom(n_slots+r, n_slots) >= n_steps`.
        !!
        !! This is the number of times the schedule replays the average step,
        !! and the reason checkpointing is affordable: it grows logarithmically
        !! in the step count for fixed storage.
        integer, intent(in) :: n_steps, n_slots

        r = 0
        if (n_steps <= 1) return
        if (n_slots < 1) then
            r = n_steps
            return
        end if
        do while (binomial(n_slots + r, n_slots) < n_steps)
            r = r + 1
            if (r > n_steps) exit
        end do
    end function revolve_min_repetitions

    integer function binomial(n, k) result(c)
        !! `binom(n, k)`, saturating rather than overflowing.
        !!
        !! The schedule only ever compares this against a step count, so a
        !! saturated value is as good as the true one and cannot wrap negative
        !! and silently invert a comparison.
        integer, intent(in) :: n, k
        integer(kind=8) :: acc
        integer :: i, kk

        c = 0
        if (k < 0 .or. n < 0 .or. k > n) return
        kk = min(k, n - k)
        acc = 1
        do i = 1, kk
            acc = acc*(n - kk + i)/i
            if (acc > huge(c)) then
                c = huge(c)
                return
            end if
        end do
        c = int(acc)
    end function binomial

    integer function revolve_forward_steps(schedule) result(n)
        !! Total primal steps the schedule executes, including replays.
        type(revolve_t), intent(in) :: schedule

        n = schedule%forward_steps
    end function revolve_forward_steps

    subroutine emit_advance(schedule, from, to)
        !! One advance, skipped when it would cover no steps.
        type(revolve_t), intent(inout) :: schedule
        integer, intent(in) :: from, to

        if (to <= from) return
        call emit(schedule, REV_ADVANCE, from, to, 0)
        schedule%forward_steps = schedule%forward_steps + (to - from)
    end subroutine emit_advance

    subroutine emit(schedule, kind, from, to, slot)
        !! Append one action, growing the list geometrically.
        type(revolve_t), intent(inout) :: schedule
        integer, intent(in) :: kind, from, to, slot
        type(revolve_action_t), allocatable :: bigger(:)

        if (schedule%n_actions >= size(schedule%actions)) then
            allocate (bigger(2*size(schedule%actions)))
            bigger(1:schedule%n_actions) = schedule%actions(1:schedule%n_actions)
            call move_alloc(bigger, schedule%actions)
        end if
        schedule%n_actions = schedule%n_actions + 1
        schedule%actions(schedule%n_actions)%kind = kind
        schedule%actions(schedule%n_actions)%from = from
        schedule%actions(schedule%n_actions)%to = to
        schedule%actions(schedule%n_actions)%slot = slot
    end subroutine emit

end module fortad_revolve
