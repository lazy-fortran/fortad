module fortad_reverse_loop
    !! Reverse mode over reduction loops.
    !!
    !! The loop shape this handles is the one real gradient workloads are made
    !! of: a loop whose body computes per-iteration temporaries and accumulates
    !! them linearly into one or more running totals.
    !!
    !!     do i = lo, hi
    !!         t = g(a(i), b(i))
    !!         s = s + t
    !!     end do
    !!
    !! Two facts make this case both correct and fast without any tape.
    !!
    !! First, the adjoint of a linear accumulation does not need the
    !! accumulator's value. `s = s + e` contributes `e_b += s_b` and leaves
    !! `s_b` unchanged, so `s_b` is loop-invariant and nothing about `s` has to
    !! be saved.
    !!
    !! Second, every per-iteration temporary depends only on loop-invariant
    !! values and on arrays indexed by the loop variable, so the reverse sweep
    !! recomputes it instead of storing it. Recomputation costs flops; a tape
    !! costs memory bandwidth, and on current hardware bandwidth is the scarcer
    !! resource.
    !!
    !! The consequence worth stating: because `s_b` is loop-invariant and the
    !! per-iteration work is independent, **the emitted reverse loop carries no
    !! loop-carried dependence and is parallelisable**, which a taped adjoint of
    !! the same loop is not.
    !!
    !! A loop that does not fit this shape - a nonlinear loop-carried
    !! recurrence, for instance - is refused by name. That needs per-iteration
    !! storage and belongs to the next milestone.
    use fortad_ir, only: fad_proc_t, fad_base_name, &
        FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, &
        FAD_END_IF, FAD_VAR, FAD_INDEX, FAD_BINOP, FAD_CONST, &
        FAD_UNOP
    implicit none
    private

    integer, parameter, public :: LOOP_OK = 0
    integer, parameter, public :: LOOP_NOT_A_LOOP = 1
    integer, parameter, public :: LOOP_UNSUPPORTED = 2

    public :: loop_shape_t, analyse_loop, split_accumulation, target_base

    type :: loop_shape_t
        !! What `analyse_loop` found.
        integer :: status = LOOP_NOT_A_LOOP
        character(len=:), allocatable :: message
        !! Statement indices of the `do` and its matching `end do`.
        integer :: first = 0, last = 0
        !! Names accumulated into linearly, and the temporaries recomputed.
        character(len=64), allocatable :: accumulators(:)
        integer :: n_accumulators = 0
        character(len=64), allocatable :: temporaries(:)
        integer :: n_temporaries = 0
        !! Array-element targets, `c(i)`, written once per iteration.
        character(len=64), allocatable :: elements(:)
        integer :: n_elements = 0
        !! Loop-carried variables whose value must be stored per iteration.
        character(len=64), allocatable :: carried(:)
        integer :: n_carried = 0
        !! Carried variables the body is **linear** in. Their adjoint
        !! coefficient does not depend on their value, so nothing about them
        !! needs storing or recomputing.
        character(len=64), allocatable :: linear(:)
        integer :: n_linear = 0
        !! Statement indices of every `do` in the nest, outermost first.
        integer :: header_stmt(8) = 0
        integer :: n_headers = 0
    end type loop_shape_t

contains

    subroutine analyse_loop(p, first, shape, active)
        !! Classify the loop starting at statement `first`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first
        type(loop_shape_t), intent(out) :: shape
        logical, intent(in), optional :: active(:)
        character(len=:), allocatable :: target
        integer :: i, depth
        logical :: is_accum

        shape%status = LOOP_NOT_A_LOOP
        if (first < 1 .or. first > p%n_stmts) return
        if (p%stmts(first)%kind /= FAD_DO) return

        shape%first = first
        depth = 0
        shape%last = 0
        do i = first, p%n_stmts
            select case (p%stmts(i)%kind)
            case (FAD_DO)
                depth = depth + 1
            case (FAD_END_DO)
                depth = depth - 1
                if (depth == 0) then
                    shape%last = i
                    exit
                end if
            end select
        end do
        if (shape%last == 0) then
            shape%status = LOOP_UNSUPPORTED
            shape%message = "unterminated do loop"
            return
        end if

        ! Sized from the body, not from a constant. A body cannot have more
        ! distinct assignment targets than it has statements, so this cannot
        ! overflow - and overflowing was silent: a degree-eleven Bezier edge
        ! area has fifty-four temporaries, the thirty-fifth onwards were
        ! dropped from the shape, and the derivative came out referring to
        ! variables it never declared.
        block
            integer :: room
            room = max(16, shape%last - shape%first)
            allocate (shape%accumulators(room), shape%temporaries(room))
            allocate (shape%elements(room), shape%carried(room), &
                shape%linear(room))
        end block
        shape%n_headers = 1
        shape%header_stmt(1) = first
        shape%linear = ""
        shape%n_linear = 0
        shape%accumulators = ""
        shape%temporaries = ""
        shape%elements = ""
        shape%carried = ""
        shape%n_accumulators = 0
        shape%n_temporaries = 0
        shape%n_elements = 0
        shape%n_carried = 0

        do i = first + 1, shape%last - 1
            select case (p%stmts(i)%kind)
            case (FAD_ASSIGN)
                target = p%stmts(i)%target
                if (index(target, "(") > 0) then
                    ! `c(i) = expr` writes a distinct element per iteration, so
                    ! the value is still there after the loop and the adjoint is
                    ! a scatter into `c_b(i)`. Nothing needs saving. What is not
                    ! allowed is reading the same array back within the loop,
                    ! which would make the write order matter.
                    if (reads_name(p, p%stmts(i)%value, target_base(target))) then
                        shape%status = LOOP_UNSUPPORTED
                        shape%message = "reverse mode: '"//target_base(target)// &
                            "' is both read and written in the same loop; that "// &
                            "needs per-iteration storage"
                        return
                    end if
                    call add_name(shape%elements, shape%n_elements, target)
                    cycle
                end if
                is_accum = is_linear_accumulation(p, shape%first, i, target)
                if (is_accum) then
                    call add_name(shape%accumulators, shape%n_accumulators, target)
                else
                    if (is_known(shape%accumulators, shape%n_accumulators, target)) then
                        shape%status = LOOP_UNSUPPORTED
                        shape%message = "reverse mode: '"//target// &
                            "' is both accumulated and overwritten in the "// &
                            "same loop; that needs per-iteration storage"
                        return
                    end if
                    if (is_loop_carried(p, shape%first, shape%last, i, target)) then
                        ! Loop-carried: the reverse sweep needs this variable's
                        ! value at each iteration, and unlike a per-iteration
                        ! temporary it cannot be recomputed from loop-invariant
                        ! data. It is taped.
                        call add_name(shape%carried, shape%n_carried, target)
                        cycle
                    end if
                    ! A temporary written more than once is not ambiguous: the
                    ! emitter gives each write its own version, exactly as it
                    ! does outside a loop. It is recorded once here.
                    if (.not. is_known(shape%temporaries, shape%n_temporaries, &
                        target)) then
                        call add_name(shape%temporaries, shape%n_temporaries, target)
                    end if
                end if
            case (FAD_DO)
                ! A nested loop is part of the same nest: the accumulator's
                ! adjoint is invariant across every level, so the whole nest
                ! inverts as one unit with its headers reproduced in order.
                shape%n_headers = shape%n_headers + 1
                if (shape%n_headers > size(shape%header_stmt)) then
                    shape%status = LOOP_UNSUPPORTED
                    shape%message = "reverse mode: loop nest is deeper than "// &
                        "fortad supports"
                    return
                end if
                shape%header_stmt(shape%n_headers) = i
            case (FAD_END_DO)
                continue
            case (FAD_IF, FAD_ELSE, FAD_END_IF)
                shape%status = LOOP_UNSUPPORTED
                shape%message = "reverse mode: a branch inside a loop needs "// &
                    "control-flow reversal, which is the next milestone"
                return
            end select
        end do

        ! A carried variable the whole body is linear in needs no tape: every
        ! partial with respect to it is built from loop-invariant values, so the
        ! adjoint coefficient is the same whatever the variable held. This is
        ! what lets a linear recurrence collapse to a constant-coefficient
        ! update instead of a taped backward sweep.
        do i = 1, shape%n_carried
            if (.not. present(active)) exit
            if (body_linear_in(p, shape, trim(shape%carried(i)), active)) then
                call add_name(shape%linear, shape%n_linear, trim(shape%carried(i)))
            end if
        end do

        if (shape%n_carried > 0 .and. shape%n_headers > 1) then
            shape%status = LOOP_UNSUPPORTED
            shape%message = "reverse mode: a loop-carried recurrence inside a "// &
                "nest would need one tape per level; not supported yet"
            return
        end if

        if (shape%n_carried > 0 .and. p%stmts(first)%step /= 0) then
            shape%status = LOOP_UNSUPPORTED
            shape%message = "reverse mode: a taped loop must have unit stride, "// &
                "so that the iteration index maps directly onto the tape"
            return
        end if

        if (shape%n_accumulators == 0 .and. shape%n_elements == 0 .and. &
            shape%n_carried == 0) then
            shape%status = LOOP_UNSUPPORTED
            shape%message = "reverse mode: this loop accumulates nothing, "// &
                "writes no array element, and carries nothing across "// &
                "iterations, so its results do not leave the body"
            return
        end if

        shape%status = LOOP_OK
    end subroutine analyse_loop

    logical function body_linear_in(p, shape, name, active) result(yes)
        !! Whether every statement in the loop body is linear in `name`, with
        !! every coefficient built purely from inactive values.
        !!
        !! Linearity alone is not enough to drop the tape. `u = u*exp(a(i))` is
        !! linear in `u`, but the partial with respect to `a(i)` is `u*exp(a(i))`
        !! and needs `u` as it stood in that iteration. Only when the multiplying
        !! factor is inactive - a constant, a loop-invariant inactive scalar like
        !! a step size - does no partial anywhere reference the carried value,
        !! and only then is the tape genuinely dead. Getting this wrong produces
        !! a gradient that is quietly wrong rather than a compile error, so the
        !! test is on the strict side.
        !!
        !! Linear here means each occurrence of `name`, or of anything derived
        !! from it, appears only in additions, subtractions, negations, and
        !! products or quotients whose *other* operand does not depend on
        !! `name`. Under that condition every partial derivative with respect to
        !! `name` is built from values independent of `name`, so the reverse
        !! sweep never needs its value.
        !!
        !! Conservative by construction: a call, a power, or a product of two
        !! dependent operands makes the answer no. Being wrong in that direction
        !! costs a tape; being wrong in the other direction costs a wrong
        !! gradient.
        type(fad_proc_t), intent(in) :: p
        type(loop_shape_t), intent(in) :: shape
        character(len=*), intent(in) :: name
        logical, intent(in) :: active(:)
        character(len=64) :: tainted(64)
        integer :: n_tainted, i, k
        logical :: changed
        character(len=:), allocatable :: lhs

        n_tainted = 1
        tainted(1) = name

        ! Everything in the body derived from `name`.
        changed = .true.
        do while (changed)
            changed = .false.
            do k = shape%first + 1, shape%last - 1
                if (p%stmts(k)%kind /= FAD_ASSIGN) cycle
                lhs = target_base(p%stmts(k)%target)
                if (is_known(tainted, n_tainted, lhs)) cycle
                do i = 1, n_tainted
                    if (reads_name(p, p%stmts(k)%value, trim(tainted(i)))) then
                        call add_name(tainted, n_tainted, lhs)
                        changed = .true.
                        exit
                    end if
                end do
            end do
        end do

        yes = .true.
        do k = shape%first + 1, shape%last - 1
            if (p%stmts(k)%kind /= FAD_ASSIGN) cycle
            if (.not. expr_linear(p, p%stmts(k)%value, tainted, n_tainted, &
                active)) then
                yes = .false.
                return
            end if
        end do
    end function body_linear_in

    recursive logical function expr_linear(p, idx, tainted, n_tainted, active) &
            result(yes)
        !! Whether an expression is linear in the tainted set.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, n_tainted
        character(len=64), intent(in) :: tainted(:)
        logical, intent(in) :: active(:)
        logical :: l_dep, r_dep

        yes = .true.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (.not. depends(p, idx, tainted, n_tainted)) return

        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            return
        case (FAD_UNOP)
            yes = expr_linear(p, p%exprs(idx)%args(1), tainted, n_tainted, active)
        case (FAD_BINOP)
            l_dep = depends(p, p%exprs(idx)%args(1), tainted, n_tainted)
            r_dep = depends(p, p%exprs(idx)%args(2), tainted, n_tainted)
            select case (trim(p%exprs(idx)%text))
            case ("+", "-")
                yes = expr_linear(p, p%exprs(idx)%args(1), tainted, n_tainted, active) &
                    .and. expr_linear(p, p%exprs(idx)%args(2), tainted, n_tainted, active)
            case ("*")
                ! A product is linear only if one side is independent.
                if (l_dep .and. r_dep) then
                    yes = .false.
                else if (l_dep) then
                    yes = expr_linear(p, p%exprs(idx)%args(1), tainted, &
                        n_tainted, active) .and. &
                        .not. reads_active(p, p%exprs(idx)%args(2), active)
                else
                    yes = expr_linear(p, p%exprs(idx)%args(2), tainted, &
                        n_tainted, active) .and. &
                        .not. reads_active(p, p%exprs(idx)%args(1), active)
                end if
            case ("/")
                ! Dividing by something dependent is not linear.
                if (r_dep) then
                    yes = .false.
                else
                    yes = expr_linear(p, p%exprs(idx)%args(1), tainted, &
                        n_tainted, active) .and. &
                        .not. reads_active(p, p%exprs(idx)%args(2), active)
                end if
            case default
                yes = .false.
            end select
        case default
            yes = .false.
        end select
    end function expr_linear

    recursive logical function reads_active(p, idx, active) result(yes)
        !! Whether an expression reads any active variable.
        !!
        !! An adjoint coefficient built only from inactive values is the same
        !! in every iteration as far as differentiation is concerned, so it can
        !! be rebuilt in the reverse sweep from data that is still live.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        integer :: i, di

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            block
                character(len=:), allocatable :: base
                integer :: par
                base = trim(p%exprs(idx)%text)
                par = index(base, "(")
                if (par > 0) base = base(:par - 1)
                di = p%decl_index(base)
                if (di > 0) then
                    if (di <= size(active)) yes = active(di)
                else
                    ! An unresolvable name is treated as active: dropping the
                    ! tape on a guess is the one failure mode that is silent.
                    yes = .true.
                end if
            end block
            if (yes) return
        end select
        if (allocated(p%exprs(idx)%args)) then
            do i = 1, size(p%exprs(idx)%args)
                if (reads_active(p, p%exprs(idx)%args(i), active)) then
                    yes = .true.
                    return
                end if
            end do
        end if
    end function reads_active

    recursive logical function depends(p, idx, tainted, n_tainted) result(yes)
        !! Whether an expression reads anything in the tainted set.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, n_tainted
        character(len=64), intent(in) :: tainted(:)
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        do i = 1, n_tainted
            if (reads_name(p, idx, trim(tainted(i)))) then
                yes = .true.
                return
            end if
        end do
    end function depends

    logical function is_loop_carried(p, first, last, stmt, target) result(yes)
        !! Whether a variable assigned at `stmt` carries a value across
        !! iterations.
        !!
        !! The test is on the **first** event in the body that involves the
        !! variable. If that is a read, the value read came from the previous
        !! iteration and the variable is carried. If it is an assignment whose
        !! own right-hand side does not read it, the variable starts fresh each
        !! iteration and is not carried, however many times it is read after
        !! that.
        !!
        !! Reading the variable it assigns, `u = f(u)`, is the obvious carried
        !! case, but only when nothing earlier in the body assigned it:
        !!
        !!     value = sin(x)          ! starts fresh
        !!     value = value + w       ! reads this iteration's value
        !!
        !! is not a recurrence. Treating it as one taped a variable that needed
        !! no tape and then refused the loop outright.
        !!
        !! The other case is the variable being **read earlier in the body than
        !! it is assigned** - which means the read sees the previous iteration's
        !! value:
        !!
        !!     do i = 1, n
        !!         a = g(h)          ! reads last iteration's h
        !!         h = f(a)          ! assigns this iteration's h
        !!     end do
        !!
        !! Missing the second case is not a refusal, it is a wrong gradient:
        !! the reverse sweep would recompute `h` from loop-invariant data and
        !! silently use the wrong iteration's value. Found on the LSTM kernel
        !! from the Enzyme benchmark suite, where `hidden` has exactly this
        !! shape.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last, stmt
        character(len=*), intent(in) :: target
        integer :: k

        associate (unused => stmt)
        end associate
        yes = .true.
        do k = first + 1, last - 1
            if (p%stmts(k)%kind /= FAD_ASSIGN) cycle
            ! The right-hand side is evaluated before the assignment takes
            ! effect, so a read in the statement that assigns the variable still
            ! counts as happening first.
            if (reads_name(p, p%stmts(k)%value, target)) return
            if (.not. allocated(p%stmts(k)%target)) cycle
            if (base_name(p%stmts(k)%target) == target) then
                yes = .false.
                return
            end if
        end do
        yes = .false.
    end function is_loop_carried

    function base_name(target) result(base)
        !! An assignment target without its subscript.
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: base
        integer :: pos

        pos = index(target, "(")
        if (pos > 0) then
            base = trim(target(1:pos - 1))
        else
            base = trim(target)
        end if
    end function base_name

    logical function is_linear_accumulation(p, first, stmt, target) result(yes)
        !! True when the statement adds to `target` a sum of terms that do not
        !! depend on `target` **at all**, directly or through any temporary
        !! computed earlier in the same iteration.
        !!
        !! The transitive part is not a refinement, it is the whole test. In
        !!
        !!     k1 = f(state)
        !!     state = state + dt*k1
        !!
        !! the added term never mentions `state`, so a syntactic check calls
        !! this a linear accumulation and concludes the accumulator's adjoint is
        !! loop-invariant. It is not: the term depends on `state` through `k1`,
        !! the recurrence is nonlinear, and the reverse sweep must run backwards
        !! over a tape. Getting this wrong produced a gradient that was exactly
        !! reversed on the RK4 kernel from the Enzyme benchmark suite - not a
        !! crash, not a refusal, just the wrong answer.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, stmt
        character(len=*), intent(in) :: target
        integer :: terms(64), signs(64), n, i
        logical :: split_ok

        call split_accumulation(p, p%stmts(stmt)%value, target, terms, signs, n, &
            split_ok)
        yes = .false.
        if (.not. split_ok) return
        do i = 1, n
            if (depends_on(p, first, stmt, terms(i), target)) return
        end do
        yes = .true.
    end function is_linear_accumulation

    logical function depends_on(p, first, stmt, expr, target) result(yes)
        !! Whether `expr` reads `target`, or any name assigned earlier in this
        !! loop body that transitively depends on `target`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, stmt, expr
        character(len=*), intent(in) :: target
        character(len=64) :: tainted(64)
        integer :: n_tainted, k, i
        logical :: changed
        character(len=:), allocatable :: lhs

        n_tainted = 1
        tainted(1) = target

        ! Grow the tainted set to a fixed point over the statements that run
        ! before this one in the same iteration.
        changed = .true.
        do while (changed)
            changed = .false.
            do k = first + 1, stmt - 1
                if (p%stmts(k)%kind /= FAD_ASSIGN) cycle
                lhs = target_base(p%stmts(k)%target)
                if (is_known(tainted, n_tainted, lhs)) cycle
                do i = 1, n_tainted
                    if (reads_name(p, p%stmts(k)%value, trim(tainted(i)))) then
                        call add_name(tainted, n_tainted, lhs)
                        changed = .true.
                        exit
                    end if
                end do
            end do
        end do

        yes = .false.
        do i = 1, n_tainted
            if (reads_name(p, expr, trim(tainted(i)))) then
                yes = .true.
                return
            end if
        end do
    end function depends_on

    subroutine split_accumulation(p, root, target, terms, signs, n, ok)
        !! Walk the `+`/`-` spine of an accumulation.
        !!
        !! Returns the terms added to `target` with their signs, and whether the
        !! statement really is a linear accumulation: `target` must appear
        !! exactly once, as a bare variable, with a positive sign, and nowhere
        !! inside any term.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: root
        character(len=*), intent(in) :: target
        integer, intent(out) :: terms(:), signs(:), n
        logical, intent(out) :: ok
        integer :: n_found, i

        n = 0
        n_found = 0
        ok = .false.
        if (root <= 0 .or. root > p%n_exprs) return
        call walk_spine(p, root, target, 1, terms, signs, n, n_found)
        if (n_found /= 1) return
        if (n == 0) return
        do i = 1, n
            if (reads_name(p, terms(i), target)) return
        end do
        ok = .true.
    end subroutine split_accumulation

    recursive subroutine walk_spine(p, idx, target, sign, terms, signs, n, n_found)
        !! Collect additive terms, tracking the sign each inherits.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, sign
        character(len=*), intent(in) :: target
        integer, intent(inout) :: terms(:), signs(:), n, n_found

        if (idx <= 0 .or. idx > p%n_exprs) return

        if (p%exprs(idx)%kind == FAD_BINOP) then
            select case (trim(p%exprs(idx)%text))
            case ("+")
                call walk_spine(p, p%exprs(idx)%args(1), target, sign, terms, &
                    signs, n, n_found)
                call walk_spine(p, p%exprs(idx)%args(2), target, sign, terms, &
                    signs, n, n_found)
                return
            case ("-")
                call walk_spine(p, p%exprs(idx)%args(1), target, sign, terms, &
                    signs, n, n_found)
                call walk_spine(p, p%exprs(idx)%args(2), target, -sign, terms, &
                    signs, n, n_found)
                return
            end select
        end if

        if (p%exprs(idx)%kind == FAD_VAR) then
            if (p%exprs(idx)%text == target) then
                ! The accumulator itself must enter with a positive sign;
                ! `v = -v + e` is not an accumulation.
                if (sign > 0) n_found = n_found + 1
                return
            end if
        end if

        if (n >= size(terms)) return
        n = n + 1
        terms(n) = idx
        signs(n) = sign
    end subroutine walk_spine

    function target_base(target) result(base)
        !! The array name of an element target, without its subscript.
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: base
        base = fad_base_name(target)
    end function target_base

    recursive logical function reads_name(p, idx, name) result(yes)
        !! True when the expression reads `name`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            if (p%exprs(idx)%text == name) then
                yes = .true.
                return
            end if
        end select
        do i = 1, size(p%exprs(idx)%args)
            if (reads_name(p, p%exprs(idx)%args(i), name)) then
                yes = .true.
                return
            end if
        end do
    end function reads_name

    subroutine add_name(names, n, name)
        !! Append a name if it is not already present.
        character(len=64), intent(inout) :: names(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: name

        if (is_known(names, n, name)) return
        ! The caller sizes these from the body, which bounds the number of
        ! distinct names, so there is always room. Refusing silently is what
        ! this used to do and it produced a derivative that did not compile.
        if (n >= size(names)) error stop "fortad: loop shape array overflow"
        n = n + 1
        names(n) = name
    end subroutine add_name

    logical function is_known(names, n, name) result(yes)
        !! Membership test.
        character(len=64), intent(in) :: names(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        do i = 1, n
            if (trim(names(i)) == name) then
                yes = .true.
                return
            end if
        end do
    end function is_known

end module fortad_reverse_loop
