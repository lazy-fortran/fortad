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
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                        FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, &
                        FAD_END_IF, FAD_VAR, FAD_INDEX, FAD_BINOP, FAD_CONST
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
        !! Statement indices of every `do` in the nest, outermost first.
        integer :: header_stmt(8) = 0
        integer :: n_headers = 0
    end type loop_shape_t

contains

    subroutine analyse_loop(p, first, shape)
        !! Classify the loop starting at statement `first`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first
        type(loop_shape_t), intent(out) :: shape
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

        allocate (shape%accumulators(32), shape%temporaries(32))
        shape%n_headers = 1
        shape%header_stmt(1) = first
        allocate (shape%elements(32), shape%carried(32))
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
                is_accum = is_linear_accumulation(p, i, target)
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
                    if (reads_name(p, p%stmts(i)%value, target)) then
                        ! A nonlinear loop-carried recurrence: the reverse
                        ! sweep needs this variable's value at each iteration,
                        ! and unlike a per-iteration temporary it cannot be
                        ! recomputed from loop-invariant data. It is taped.
                        call add_name(shape%carried, shape%n_carried, target)
                        cycle
                    end if
                    if (is_known(shape%temporaries, shape%n_temporaries, target)) then
                        shape%status = LOOP_UNSUPPORTED
                        shape%message = "reverse mode: '"//target// &
                            "' is assigned more than once in the loop body; "// &
                            "the reverse sweep would need to know which "// &
                            "version each use saw"
                        return
                    end if
                    call add_name(shape%temporaries, shape%n_temporaries, target)
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

        if (shape%n_accumulators == 0 .and. shape%n_elements == 0) then
            shape%status = LOOP_UNSUPPORTED
            shape%message = "reverse mode: this loop neither accumulates nor "// &
                "writes array elements, so its results do not leave the body"
            return
        end if

        shape%status = LOOP_OK
    end subroutine analyse_loop

    logical function is_linear_accumulation(p, stmt, target) result(yes)
        !! True when the statement adds a sum of terms to `target`, in any
        !! association: `v = v + e`, `v = v - e`, `v = v + e1 + e2 - e3`.
        !!
        !! That is the shape whose adjoint leaves the accumulator's own adjoint
        !! untouched, which is what removes the loop-carried dependence from the
        !! reverse sweep. Requiring the literal two-operand form would reject
        !! most real reduction bodies, which chain several terms.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: stmt
        character(len=*), intent(in) :: target
        integer :: terms(64), signs(64), n

        call split_accumulation(p, p%stmts(stmt)%value, target, terms, signs, n, yes)
    end function is_linear_accumulation

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
        integer :: pos

        pos = index(target, "(")
        if (pos > 0) then
            base = target(1:pos - 1)
        else
            base = target
        end if
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
        if (n >= size(names)) return
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
