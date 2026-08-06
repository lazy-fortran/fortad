module fortad_affine
    !! Rewrite a loop body that is an affine map of its carried variable.
    !!
    !! The adjoint of a linear recurrence is a linear recurrence. A body whose
    !! every value is `a*u + b` in the carried variable `u`, with `a` and `b`
    !! free of `u`, can be written that way statement by statement - after
    !! which `a` and `b` are loop-invariant, hoisting lifts them out, and what
    !! is left is a multiply and an add. Four-stage Runge-Kutta is the case that
    !! matters: four stage adjoints, each accumulated onto twice, every one of
    !! them affine in the carried adjoint.
    !!
    !! It is decided by **analysis**, in one pass. Each name carries a form:
    !!
    !!     u          -> (1, 0)
    !!     a literal  -> (0, c)
    !!     x + y      -> (ax + ay, bx + by)
    !!     x * y      -> affine only if one side has a = 0
    !!     f(x)       -> affine only if x has a = 0
    !!
    !! A product of two `u`-dependent terms, or a transcendental of one, is not
    !! affine and the analysis says so rather than guessing. The cost is linear
    !! in the size of the body, so there is no budget to set and no threshold to
    !! tune.
    !!
    !! No statement is added, removed or reordered. Each keeps its target and
    !! gets a right-hand side stating the same value in terms of the incoming
    !! `u`; whatever that leaves unused is dead-store elimination's business.
    !! An earlier attempt kept only the carrier update and the array scatters,
    !! which loses anything read after the loop - seven oracle cases caught it.
    !!
    !! This replaces a search: rewriting each body two ways, one of them
    !! substituting and distributing everything, and keeping whichever measured
    !! smaller. That is exponential in the body - half a minute on a quartic
    !! Bezier edge area, non-terminating on a quintic Lagrange weight - and
    !! every bound put on it was a number chosen to fit the kernels tried.
    use fortad_kinds, only: dp
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                        expr_var, expr_const, &
                        expr_binop, FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, &
                        FAD_CALL, FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, &
                        FAD_INTENT_NONE
    implicit none
    private

    public :: collapse_affine_loops

    !! Names a body may track. A body larger than this is not one whose
    !! carried variable is a scalar recurrence.
    integer, parameter :: MAX_TRACKED = 512


    type :: form_t
        !! `a*u + b`, or not affine in `u` at all.
        logical :: affine = .false.
        integer :: a = 0
        integer :: b = 0
    end type form_t

    type :: table_t
        !! Every name's value, in terms of the incoming carried variable.
        character(len=64) :: names(MAX_TRACKED) = ""
        type(form_t) :: forms(MAX_TRACKED)
        integer :: n = 0
    end type table_t

contains

    subroutine collapse_affine_loops(p)
        !! Rewrite every loop body that is affine in its carried variable.
        type(fad_proc_t), intent(inout) :: p
        integer :: i, first, last

        i = 1
        do while (i <= p%n_stmts)
            if (p%stmts(i)%kind /= FAD_DO) then
                i = i + 1
                cycle
            end if
            call extent(p, i, first, last)
            if (last == 0) then
                i = i + 1
                cycle
            end if
            call collapse_one(p, first, last)
            call refresh_extent(p, first, last)
            i = last + 1
        end do
    end subroutine collapse_affine_loops

    subroutine collapse_one(p, first, last)
        !! Rewrite one loop body, if it is affine in exactly one carrier.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(inout) :: first, last
        character(len=:), allocatable :: carrier
        type(table_t) :: table
        integer, allocatable :: rewritten(:)
        integer :: j
        character(len=:), allocatable :: snapshot
        type(form_t) :: f
        logical :: ok

        if (.not. plain_body(p, first, last)) return
        call find_carrier(p, first, last, carrier, ok)
        if (.not. ok) return

        ! Every form is stated in terms of the value the carrier held on entry,
        ! named by a snapshot taken before anything in the body runs. Where the
        ! carrier is overwritten then stops mattering. Without the snapshot a
        ! statement scheduled after the update reads the new value and the
        ! adjoint comes out one iteration late - and checking for direct reads
        ! does not catch it, because the read is of an earlier snapshot whose
        ! own form mentions the carrier.


        allocate (rewritten(last - first))
        rewritten = 0
        table%n = 0
        call seed(p, table, carrier, ok)
        if (.not. ok) return

        snapshot = snapshot_name(p)
        do j = first + 1, last - 1
            call form_of(p, p%stmts(j)%value, table, carrier, f)
            if (.not. f%affine) return
            ! A form is stated in terms of the value the carrier held on entry.
            ! Past the statement that overwrites it, that value is no longer
            ! there to be read - and a snapshot taken before the update still
            ! has a form mentioning the carrier, so checking for direct reads
            ! is not enough. Anything after the update has to be free of it.
            rewritten(j - first) = combine(p, f, snapshot)
            call store(table, target_key(p, j), f, ok)
            if (.not. ok) return
        end do

        ! Nothing is committed until the whole body has been shown affine.
        do j = first + 1, last - 1
            if (rewritten(j - first) > 0) p%stmts(j)%value = rewritten(j - first)
        end do
        call take_snapshot(p, first, carrier, snapshot)
    end subroutine collapse_one

    subroutine seed(p, table, carrier, ok)
        !! The carrier is itself: `1*u + 0`.
        type(fad_proc_t), intent(inout) :: p
        type(table_t), intent(inout) :: table
        character(len=*), intent(in) :: carrier
        logical, intent(out) :: ok
        type(form_t) :: f

        f%affine = .true.
        f%a = p%add_expr(expr_const("1.0"//suffix_of(p)))
        f%b = p%add_expr(expr_const("0.0"//suffix_of(p)))
        call store(table, carrier, f, ok)
    end subroutine seed

    subroutine find_carrier(p, first, last, carrier, ok)
        !! The one scalar read in the body before anything assigns it.
        !!
        !! That is what loop-carried means, and unlike "the statement reads what
        !! it assigns" it survives single-assignment renaming, which turns
        !! `s = s + x` into `s__1 = s + x` and leaves the self-read nowhere to
        !! be seen.
        !!
        !! More than one carrier and this declines: two make the body an affine
        !! map of a vector, whose collapsed form is a matrix product and a
        !! different transformation from this one.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        character(len=:), allocatable, intent(out) :: carrier
        logical, intent(out) :: ok
        character(len=64) :: found(8), written(MAX_TRACKED)
        integer :: n_found, n_written, j, k
        logical :: seen

        carrier = ""
        ok = .false.
        n_found = 0
        n_written = 0
        do j = first + 1, last - 1
            if (.not. allocated(p%stmts(j)%target)) return
            call note_reads(p, p%stmts(j)%value, written, n_written, found, &
                            n_found, seen)
            if (.not. seen) return
            if (n_written >= size(written)) return
            n_written = n_written + 1
            written(n_written) = target_key(p, j)
        end do
        ! Read-before-written alone is every loop-invariant input as well.
        ! A carrier is also written: that is what makes it cross iterations.
        k = 0
        do j = 1, n_found
            if (.not. is_written(found(j), written, n_written)) cycle
            ! A carrier has to be a scalar. An array element reaches here as a
            ! name with its subscript in the text, and it is a place rather than
            ! a value a form can be stated in - `z_b(i)` names a different
            ! element every iteration, which is a scatter and not a recurrence.
            if (index(found(j), "(") > 0) cycle
            k = k + 1
            found(k) = found(j)
        end do
        n_found = k
        if (n_found /= 1) return
        carrier = trim(found(1))
        ok = .true.
    end subroutine find_carrier

    logical function is_written(name, written, n_written) result(yes)
        !! Whether the body assigns this name anywhere.
        character(len=*), intent(in) :: name
        character(len=64), intent(in) :: written(:)
        integer, intent(in) :: n_written
        integer :: i

        yes = .false.
        do i = 1, n_written
            if (trim(written(i)) == trim(name)) then
                yes = .true.
                return
            end if
        end do
    end function is_written

    subroutine note_reads(p, idx, written, n_written, found, n_found, ok)
        !! Record names an expression reads that nothing has written yet.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=64), intent(in) :: written(:)
        integer, intent(in) :: n_written
        character(len=64), intent(inout) :: found(:)
        integer, intent(inout) :: n_found
        logical, intent(out) :: ok
        character(len=64) :: names(MAX_TRACKED)
        integer :: n_names, i, k
        logical :: earlier, already

        ok = .false.
        n_names = 0
        call gather(p, idx, names, n_names)
        if (n_names > size(names)) return
        do i = 1, n_names
            earlier = .false.
            do k = 1, n_written
                if (trim(written(k)) == trim(names(i))) earlier = .true.
            end do
            if (earlier) cycle
            already = .false.
            do k = 1, n_found
                if (trim(found(k)) == trim(names(i))) already = .true.
            end do
            if (already) cycle
            if (n_found >= size(found)) return
            n_found = n_found + 1
            found(n_found) = names(i)
        end do
        ok = .true.
    end subroutine note_reads

    recursive subroutine gather(p, idx, names, n)
        !! Every plain variable an expression reads.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=64), intent(inout) :: names(:)
        integer, intent(inout) :: n
        integer :: i

        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_VAR) then
            if (allocated(p%exprs(idx)%text)) then
                if (n < size(names)) then
                    n = n + 1
                    names(n) = trim(p%exprs(idx)%text)
                else
                    n = size(names) + 1
                end if
            end if
            return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            call gather(p, p%exprs(idx)%args(i), names, n)
        end do
    end subroutine gather

    function snapshot_name(p) result(name)
        !! A local name for the carrier's value on entry to the iteration.
        type(fad_proc_t), intent(in) :: p
        character(len=:), allocatable :: name
        character(len=32) :: buffer
        integer :: k

        do k = 1, 999
            write (buffer, '(a,i0)') "fad_u", k
            if (p%decl_index(trim(buffer)) == 0) exit
        end do
        name = trim(buffer)
    end function snapshot_name

    subroutine take_snapshot(p, first, carrier, snapshot)
        !! Insert `snapshot = carrier` as the body's first statement.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: first
        character(len=*), intent(in) :: carrier, snapshot
        type(fad_stmt_t), allocatable :: out(:)
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        integer :: di, ignored, i, n

        di = p%decl_index(carrier)
        if (di == 0) return
        d = p%decls(di)
        d%name = snapshot
        d%intent = FAD_INTENT_NONE
        d%is_result = .false.
        d%is_array = .false.
        if (allocated(d%dims)) deallocate (d%dims)
        ignored = p%add_decl(d)

        s%kind = FAD_ASSIGN
        s%target = snapshot
        s%value = p%add_expr(expr_var(carrier))

        allocate (out(p%n_stmts + 1))
        n = 0
        do i = 1, p%n_stmts
            if (i == first + 1) then
                n = n + 1
                out(n) = s
            end if
            n = n + 1
            out(n) = p%stmts(i)
        end do
        if (size(p%stmts) < n) then
            block
                type(fad_stmt_t), allocatable :: grown(:)
                allocate (grown(2*n))
                grown(1:p%n_stmts) = p%stmts(1:p%n_stmts)
                call move_alloc(grown, p%stmts)
            end block
        end if
        p%stmts(1:n) = out(1:n)
        p%n_stmts = n
    end subroutine take_snapshot

    recursive subroutine form_of(p, idx, table, carrier, f)
        !! The affine form of an expression in the carried variable.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        type(table_t), intent(in) :: table
        character(len=*), intent(in) :: carrier
        type(form_t), intent(out) :: f
        type(form_t) :: l, r
        character(len=:), allocatable :: op

        f%affine = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return

        select case (p%exprs(idx)%kind)
        case (FAD_CONST, FAD_INDEX)
            call value_form(p, idx, f)
            return
        case (FAD_VAR)
            call lookup(table, trim(p%exprs(idx)%text), f)
            if (f%affine) return
            ! Not yet given a form: whatever it already holds on entry to the
            ! iteration, which does not mention the carrier.
            call value_form(p, idx, f)
            return
        case (FAD_CALL)
            if (reads(p, idx, carrier)) return
            call value_form(p, idx, f)
            return
        case (FAD_UNOP)
            call form_of(p, p%exprs(idx)%args(1), table, carrier, l)
            if (.not. l%affine) return
            if (trim(p%exprs(idx)%text) /= "-") then
                if (.not. reads(p, idx, carrier)) call value_form(p, idx, f)
                return
            end if
            f%affine = .true.
            f%a = negate(p, l%a)
            f%b = negate(p, l%b)
            return
        case (FAD_BINOP)
        case default
            return
        end select

        op = trim(p%exprs(idx)%text)
        call form_of(p, p%exprs(idx)%args(1), table, carrier, l)
        if (.not. l%affine) return
        call form_of(p, p%exprs(idx)%args(2), table, carrier, r)
        if (.not. r%affine) return

        select case (op)
        case ("+")
            f%affine = .true.
            f%a = add(p, l%a, r%a)
            f%b = add(p, l%b, r%b)
        case ("-")
            f%affine = .true.
            f%a = sub(p, l%a, r%a)
            f%b = sub(p, l%b, r%b)
        case ("*")
            ! One side free of the carrier, or the product is quadratic in it.
            if (is_zero(p, r%a)) then
                f%affine = .true.
                f%a = mul(p, l%a, r%b)
                f%b = mul(p, l%b, r%b)
            else if (is_zero(p, l%a)) then
                f%affine = .true.
                f%a = mul(p, l%b, r%a)
                f%b = mul(p, l%b, r%b)
            end if
        case ("/")
            if (is_zero(p, r%a)) then
                f%affine = .true.
                f%a = div(p, l%a, r%b)
                f%b = div(p, l%b, r%b)
            end if
        case default
            ! A power of the carrier is not affine in it.
            if (is_zero(p, l%a) .and. is_zero(p, r%a)) call value_form(p, idx, f)
        end select
    end subroutine form_of


    subroutine value_form(p, idx, f)
        !! A value free of the carrier: `0*u + value`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        type(form_t), intent(out) :: f

        f%affine = .true.
        f%a = p%add_expr(expr_const("0.0"//suffix_of(p)))
        f%b = idx
    end subroutine value_form

    integer function combine(p, f, carrier) result(idx)
        !! `a*u + b`, written out.
        type(fad_proc_t), intent(inout) :: p
        type(form_t), intent(in) :: f
        character(len=*), intent(in) :: carrier
        integer :: u, scaled

        if (is_zero(p, f%a)) then
            idx = f%b
            return
        end if
        u = p%add_expr(expr_var(carrier))
        scaled = p%add_expr(expr_binop("*", f%a, u))
        if (is_zero(p, f%b)) then
            idx = scaled
            return
        end if
        idx = p%add_expr(expr_binop("+", scaled, f%b))
    end function combine

    ! ------------------------------------------------------------------
    ! Bookkeeping
    ! ------------------------------------------------------------------

    function target_key(p, idx) result(key)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=:), allocatable :: key

        key = trim(p%stmts(idx)%target)
    end function target_key

    subroutine store(table, name, f, ok)
        type(table_t), intent(inout) :: table
        character(len=*), intent(in) :: name
        type(form_t), intent(in) :: f
        logical, intent(out) :: ok
        integer :: i

        ok = .true.
        do i = 1, table%n
            if (trim(table%names(i)) == name) then
                table%forms(i) = f
                return
            end if
        end do
        if (table%n >= MAX_TRACKED) then
            ok = .false.
            return
        end if
        table%n = table%n + 1
        table%names(table%n) = name
        table%forms(table%n) = f
    end subroutine store

    subroutine lookup(table, name, f)
        type(table_t), intent(in) :: table
        character(len=*), intent(in) :: name
        type(form_t), intent(out) :: f
        integer :: i

        f%affine = .false.
        do i = 1, table%n
            if (trim(table%names(i)) == name) then
                f = table%forms(i)
                return
            end if
        end do
    end subroutine lookup

    function suffix_of(p) result(text)
        type(fad_proc_t), intent(in) :: p
        character(len=:), allocatable :: text

        text = "d0"
        if (allocated(p%real_suffix)) text = p%real_suffix
    end function suffix_of

    logical function is_zero(p, idx) result(yes)
        !! Whether an expression is the literal zero, however it is spelled.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=:), allocatable :: text
        real(dp) :: value
        integer :: ios, pos

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind /= FAD_CONST) return
        if (.not. allocated(p%exprs(idx)%text)) return
        text = trim(adjustl(p%exprs(idx)%text))
        pos = index(text, "_")
        if (pos > 1) text = text(1:pos - 1)
        read (text, *, iostat=ios) value
        if (ios /= 0) return
        yes = value == 0.0_dp
    end function is_zero

    integer function add(p, x, y) result(idx)
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: x, y

        if (is_zero(p, x)) then
            idx = y
        else if (is_zero(p, y)) then
            idx = x
        else
            idx = p%add_expr(expr_binop("+", x, y))
        end if
    end function add

    integer function sub(p, x, y) result(idx)
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: x, y

        if (is_zero(p, y)) then
            idx = x
        else
            idx = p%add_expr(expr_binop("-", x, y))
        end if
    end function sub

    integer function mul(p, x, y) result(idx)
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: x, y

        if (is_zero(p, x) .or. is_zero(p, y)) then
            idx = p%add_expr(expr_const("0.0"//suffix_of(p)))
        else
            idx = p%add_expr(expr_binop("*", x, y))
        end if
    end function mul

    integer function div(p, x, y) result(idx)
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: x, y

        if (is_zero(p, x)) then
            idx = p%add_expr(expr_const("0.0"//suffix_of(p)))
        else
            idx = p%add_expr(expr_binop("/", x, y))
        end if
    end function div

    integer function negate(p, x) result(idx)
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: x
        type(fad_expr_t) :: e

        if (is_zero(p, x)) then
            idx = x
            return
        end if
        e%kind = FAD_UNOP
        e%text = "-"
        e%args = [x]
        idx = p%add_expr(e)
    end function negate

    logical function plain_body(p, first, last) result(yes)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        integer :: j

        yes = .false.
        if (last <= first + 1) return
        do j = first + 1, last - 1
            if (p%stmts(j)%kind /= FAD_ASSIGN) return
        end do
        yes = .true.
    end function plain_body

    subroutine extent(p, first, lo, hi)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first
        integer, intent(out) :: lo, hi
        integer :: j, depth

        lo = first
        hi = 0
        depth = 0
        do j = first, p%n_stmts
            select case (p%stmts(j)%kind)
            case (FAD_DO)
                depth = depth + 1
            case (FAD_END_DO)
                depth = depth - 1
                if (depth == 0) then
                    hi = j
                    return
                end if
            end select
        end do
    end subroutine extent

    subroutine refresh_extent(p, first, last)
        type(fad_proc_t), intent(in) :: p
        integer, intent(inout) :: first
        integer, intent(out) :: last
        integer :: lo

        call extent(p, first, lo, last)
        first = lo
    end subroutine refresh_extent

    recursive logical function reads(p, idx, name) result(yes)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_VAR) then
            if (allocated(p%exprs(idx)%text)) yes = trim(p%exprs(idx)%text) == name
            if (yes) return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            if (reads(p, p%exprs(idx)%args(i), name)) then
                yes = .true.
                return
            end if
        end do
    end function reads

end module fortad_affine
