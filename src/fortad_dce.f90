module fortad_dce
    !! Dead-store elimination over generated statements.
    !!
    !! The reverse sweep recomputes or restores a value before differentiating
    !! the statement that produced it. Often nothing needs that value: the
    !! adjoint of `state = state + dt*(a*state + b*z(i))` depends on `dt`, `a`
    !! and `b` but not on the new `state`, so recomputing it is pure loss.
    !!
    !! This is to-be-recorded analysis arriving from the other end. Rather than
    !! deciding in advance what the reverse sweep will need - which requires
    !! knowing what every rule will reference - the sweep is emitted and then
    !! the stores nothing reads are removed. Same result, and it cannot fall out
    !! of step with the rules the way a predictive analysis would.
    !!
    !! Only assignments to scalar locals are removed, and only when the value is
    !! read nowhere later. Inside a loop the test is stricter: the variable must
    !! not be read anywhere in that loop body at all, because a read earlier in
    !! the body is a read of the *next* iteration's value.
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
        copy_decl, &
        FAD_VAR, FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, &
        FAD_IF, FAD_ELSE, FAD_END_IF, FAD_CALL_STMT, &
        FAD_INTENT_NONE
    implicit none
    private

    public :: eliminate_dead_stores, fold_zero_accumulations, &
        eliminate_dead_arrays, eliminate_dead_loops

contains

    subroutine eliminate_dead_loops(p)
        !! Remove a loop nothing outside it reads.
        !!
        !! Dead-store elimination works one statement at a time and will not
        !! touch a loop: the body's assignments feed each other, so none of them
        !! looks individually dead. But a loop whose every written name is
        !! unread after it computes nothing, and that is exactly what is left of
        !! a primal sweep when the caller asked for the gradient alone.
        !!
        !! Removal is conservative. A loop is dropped only when it contains no
        !! call, writes no dummy argument, and no name it writes is read
        !! anywhere outside it - including by an earlier statement, since a
        !! later iteration of an enclosing construct could read it.
        type(fad_proc_t), intent(inout) :: p
        type(fad_stmt_t), allocatable :: out(:)
        logical, allocatable :: keep(:)
        integer :: i, j, depth, last, n_out, passes
        logical :: changed, dead

        if (p%n_stmts == 0) return
        allocate (keep(p%n_stmts), out(p%n_stmts))

        do passes = 1, 8
            keep = .true.
            changed = .false.
            i = 1
            do while (i <= p%n_stmts)
                if (p%stmts(i)%kind /= FAD_DO .or. .not. keep(i)) then
                    i = i + 1
                    cycle
                end if
                depth = 0
                last = 0
                do j = i, p%n_stmts
                    select case (p%stmts(j)%kind)
                    case (FAD_DO)
                        depth = depth + 1
                    case (FAD_END_DO)
                        depth = depth - 1
                        if (depth == 0) then
                            last = j
                            exit
                        end if
                    end select
                end do
                if (last == 0) then
                    i = i + 1
                    cycle
                end if
                if (loop_is_dead(p, i, last)) then
                    keep(i:last) = .false.
                    changed = .true.
                    i = last + 1
                else
                    i = i + 1
                end if
            end do
            if (.not. changed) exit
            n_out = 0
            do i = 1, p%n_stmts
                if (.not. keep(i)) cycle
                n_out = n_out + 1
                out(n_out) = p%stmts(i)
            end do
            p%stmts(1:n_out) = out(1:n_out)
            p%n_stmts = n_out
            if (n_out == 0) exit
        end do
    end subroutine eliminate_dead_loops

    logical function loop_is_dead(p, first, last) result(yes)
        !! Whether the loop spanning `first..last` can be removed outright.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        character(len=:), allocatable :: name
        integer :: i, j

        yes = .false.
        do i = first, last
            ! A call may do anything; a raw statement is text fortad cannot see
            ! into. Neither can be proved dead.
            if (p%stmts(i)%kind == FAD_CALL_STMT) return
            if (p%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (.not. allocated(p%stmts(i)%target)) return
            name = base_of(p%stmts(i)%target)
            ! Writing a dummy argument is visible to the caller.
            if (is_dummy_name(p, name)) return
            do j = 1, p%n_stmts
                if (j >= first .and. j <= last) cycle
                if (reads_outside(p, j, name)) return
            end do
        end do
        yes = .true.
    end function loop_is_dead

    logical function reads_outside(p, idx, name) result(yes)
        !! Whether statement `idx` reads `name`.
        !!
        !! `statement_reads` deliberately counts a mention inside an assignment
        !! target, so that `c(i) = ...` is seen to read `i`. Here that is too
        !! strong: a plain `state = ...` overwrites the name rather than reading
        !! it, and counting it would keep every loop alive.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: par

        yes = .false.
        if (idx < 1 .or. idx > p%n_stmts) return
        if (p%stmts(idx)%kind == FAD_ASSIGN) then
            if (allocated(p%stmts(idx)%target)) then
                par = index(p%stmts(idx)%target, "(")
                if (par == 0 .and. trim(p%stmts(idx)%target) == name) then
                    yes = expr_reads(p, p%stmts(idx)%value, name)
                    return
                end if
            end if
        end if
        yes = statement_reads(p, idx, name)
    end function reads_outside

    logical function is_dummy_name(p, name) result(yes)
        !! Whether a name is a dummy argument of the procedure.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        if (.not. allocated(p%params)) return
        do i = 1, size(p%params)
            if (trim(p%params(i)) == name) then
                yes = .true.
                return
            end if
        end do
    end function is_dummy_name

    subroutine eliminate_dead_stores(p)
        !! Remove assignments whose result is never read.
        type(fad_proc_t), intent(inout) :: p
        logical, allocatable :: keep(:)
        type(fad_stmt_t), allocatable :: out(:)
        integer :: i, n_out, passes
        logical :: changed

        if (p%n_stmts == 0) return
        allocate (keep(p%n_stmts), out(p%n_stmts))

        ! Iterate: removing one store can make the store that fed it dead too.
        do passes = 1, 8
            keep = .true.
            changed = .false.
            do i = 1, p%n_stmts
                if (.not. is_removable(p, i, keep)) cycle
                keep(i) = .false.
                changed = .true.
            end do
            if (.not. changed) exit

            n_out = 0
            do i = 1, p%n_stmts
                if (.not. keep(i)) cycle
                n_out = n_out + 1
                out(n_out) = p%stmts(i)
            end do
            p%stmts(1:n_out) = out(1:n_out)
            p%n_stmts = n_out
            if (n_out == 0) exit
        end do
    end subroutine eliminate_dead_stores

    subroutine eliminate_dead_arrays(p)
        !! Remove local arrays that are written and never read.
        !!
        !! A tape is decided on before the reverse sweep is built, and the
        !! sweep may then turn out not to need it: the adjoint of
        !! `state = state + dt*(a*state + b*z(i))` never reads the state it
        !! restored. What is left is a loop writing an array nobody looks at,
        !! which on the Euler kernel is 160 KB of memory traffic per call for
        !! nothing.
        !!
        !! Scalar dead stores are removed first, so this sees the sweep as it
        !! will actually be emitted rather than as it was built.
        type(fad_proc_t), intent(inout) :: p
        logical, allocatable :: keep(:)
        type(fad_stmt_t), allocatable :: out(:)
        type(fad_decl_t), allocatable :: decls(:)
        character(len=:), allocatable :: name
        integer :: d, i, n_out, n_decls

        if (p%n_decls == 0 .or. p%n_stmts == 0) return
        allocate (keep(p%n_stmts), out(p%n_stmts))

        do d = 1, p%n_decls
            if (.not. p%decls(d)%is_array) cycle
            if (.not. allocated(p%decls(d)%name)) cycle
            name = p%decls(d)%name
            if (is_dummy(p, name)) cycle
            if (array_is_read(p, name)) cycle

            keep = .true.
            do i = 1, p%n_stmts
                if (p%stmts(i)%kind /= FAD_ASSIGN) cycle
                if (.not. allocated(p%stmts(i)%target)) cycle
                if (base_of(p%stmts(i)%target) /= name) cycle
                keep(i) = .false.
            end do

            n_out = 0
            do i = 1, p%n_stmts
                if (.not. keep(i)) cycle
                n_out = n_out + 1
                out(n_out) = p%stmts(i)
            end do
            p%stmts(1:n_out) = out(1:n_out)
            p%n_stmts = n_out

            p%decls(d)%name = ""
        end do

        ! Drop the declarations that were blanked out.
        allocate (decls(p%n_decls))
        n_decls = 0
        do d = 1, p%n_decls
            if (.not. allocated(p%decls(d)%name)) cycle
            if (len_trim(p%decls(d)%name) == 0) cycle
            n_decls = n_decls + 1
            call copy_decl(decls(n_decls), p%decls(d))
        end do
        do d = 1, n_decls
            call copy_decl(p%decls(d), decls(d))
        end do
        p%n_decls = n_decls
    end subroutine eliminate_dead_arrays

    logical function array_is_read(p, name) result(yes)
        !! Whether an array is read anywhere: in an expression, or in the
        !! subscript of a write to it, or in a raw rule statement's text.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: name
        integer :: i

        yes = .true.
        do i = 1, p%n_stmts
            if (p%stmts(i)%kind == FAD_ASSIGN) then
                ! A write to this array is not a read of it, but a write to
                ! something else that mentions it is.
                if (allocated(p%stmts(i)%target)) then
                    if (base_of(p%stmts(i)%target) == name) then
                        if (expr_reads(p, p%stmts(i)%value, name)) return
                        cycle
                    end if
                end if
            end if
            if (statement_reads(p, i, name)) return
        end do
        yes = .false.
    end function array_is_read

    function base_of(target) result(base)
        !! The name in an assignment target, without any subscript.
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: base
        integer :: pos

        pos = index(target, "(")
        if (pos > 0) then
            base = target(1:pos - 1)
        else
            base = target
        end if
    end function base_of

    subroutine fold_zero_accumulations(p)
        !! Turn `x = 0` followed by `x = x + e` into `x = e`.
        !!
        !! The reverse sweep zeroes an adjoint and then accumulates into it,
        !! which is the right way to build it but two stores where one will do.
        !! The compiler will not fold them because between the two it cannot
        !! rule out an alias, and inside a hot loop that is a real cost: on the
        !! Euler kernel it is a fifth of the emitted body.
        type(fad_proc_t), intent(inout) :: p
        logical, allocatable :: keep(:)
        type(fad_stmt_t), allocatable :: out(:)
        integer :: i, j, n_out
        character(len=:), allocatable :: name

        if (p%n_stmts < 2) return
        allocate (keep(p%n_stmts), out(p%n_stmts))
        keep = .true.

        do i = 1, p%n_stmts - 1
            if (.not. keep(i)) cycle
            if (.not. is_zero_store(p, i)) cycle
            name = p%stmts(i)%target

            ! Find the next statement mentioning this variable at all.
            j = 0
            block
                integer :: k
                do k = i + 1, p%n_stmts
                    if (.not. keep(k)) cycle
                    if (statement_reads(p, k, name)) then
                        j = k
                        exit
                    end if
                    if (p%stmts(k)%kind == FAD_ASSIGN) then
                        if (allocated(p%stmts(k)%target)) then
                            if (p%stmts(k)%target == name) then
                                j = k
                                exit
                            end if
                        end if
                    end if
                    ! Control flow between the two makes the fold unsound.
                    select case (p%stmts(k)%kind)
                    case (FAD_DO, FAD_END_DO, FAD_IF, FAD_ELSE, FAD_END_IF)
                        j = 0
                        exit
                    end select
                end do
            end block
            if (j == 0) cycle

            ! It must be exactly `name = name + e`.
            if (p%stmts(j)%kind /= FAD_ASSIGN) cycle
            if (.not. allocated(p%stmts(j)%target)) cycle
            if (p%stmts(j)%target /= name) cycle
            if (.not. is_self_add(p, j, name)) cycle

            p%stmts(j)%value = p%exprs(p%stmts(j)%value)%args(2)
            keep(i) = .false.
        end do

        n_out = 0
        do i = 1, p%n_stmts
            if (.not. keep(i)) cycle
            n_out = n_out + 1
            out(n_out) = p%stmts(i)
        end do
        p%stmts(1:n_out) = out(1:n_out)
        p%n_stmts = n_out
    end subroutine fold_zero_accumulations

    logical function is_zero_store(p, idx) result(yes)
        !! Whether statement `idx` assigns a literal zero to a plain name.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: e

        yes = .false.
        if (p%stmts(idx)%kind /= FAD_ASSIGN) return
        if (.not. allocated(p%stmts(idx)%target)) return
        if (index(p%stmts(idx)%target, "(") > 0) return
        e = p%stmts(idx)%value
        if (e <= 0 .or. e > p%n_exprs) return
        if (.not. allocated(p%exprs(e)%text)) return
        select case (trim(p%exprs(e)%text))
        case ("0.0d0", "0.0e0", "0.0", "0")
            yes = .true.
        end select
    end function is_zero_store

    logical function is_self_add(p, idx, name) result(yes)
        !! Whether statement `idx` is `name = name + e`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: e, l

        yes = .false.
        e = p%stmts(idx)%value
        if (e <= 0 .or. e > p%n_exprs) return
        if (p%exprs(e)%kind /= 3) return ! FAD_BINOP
        if (trim(p%exprs(e)%text) /= "+") return
        l = p%exprs(e)%args(1)
        if (l <= 0 .or. l > p%n_exprs) return
        if (p%exprs(l)%kind /= FAD_VAR) return
        yes = p%exprs(l)%text == name
    end function is_self_add

    logical function is_removable(p, idx, keep) result(yes)
        !! Whether statement `idx` may be dropped.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        logical, intent(in) :: keep(:)
        character(len=:), allocatable :: name
        integer :: lo, hi, j

        yes = .false.
        if (p%stmts(idx)%kind /= FAD_ASSIGN) return
        if (.not. allocated(p%stmts(idx)%target)) return
        name = p%stmts(idx)%target

        ! Anything with a subscript or a marker target stays: an element write
        ! is a scatter with its own liveness, and a raw rule statement is
        ! opaque text.
        if (index(name, "(") > 0) return
        if (name(1:1) == "!") return

        ! Never remove a store to something the caller can see.
        if (is_dummy(p, name)) return

        ! Read later anywhere: live.
        do j = idx + 1, p%n_stmts
            if (.not. keep(j)) cycle
            if (statement_reads(p, j, name)) return
        end do

        ! Inside a loop, a read earlier in the body is a read of the value this
        ! iteration is about to overwrite, so it is live across iterations.
        call enclosing_loop(p, idx, lo, hi)
        if (lo > 0) then
            do j = lo, hi
                if (j == idx) cycle
                if (.not. keep(j)) cycle
                if (statement_reads(p, j, name)) return
            end do
        end if

        yes = .true.
    end function is_removable

    subroutine enclosing_loop(p, idx, lo, hi)
        !! Statement range of the innermost loop containing `idx`, or 0.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer, intent(out) :: lo, hi
        integer :: j, depth

        lo = 0
        hi = 0
        depth = 0
        do j = idx, 1, -1
            select case (p%stmts(j)%kind)
            case (FAD_END_DO)
                depth = depth + 1
            case (FAD_DO)
                if (depth == 0) then
                    lo = j
                    exit
                end if
                depth = depth - 1
            end select
        end do
        if (lo == 0) return

        depth = 0
        do j = lo, p%n_stmts
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
        hi = p%n_stmts
    end subroutine enclosing_loop

    logical function statement_reads(p, idx, name) result(yes)
        !! Whether statement `idx` reads `name`, counting a subscripted target
        !! as a read of the names in its subscripts.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: k

        yes = .false.
        select case (p%stmts(idx)%kind)
        case (FAD_ASSIGN)
            if (expr_reads(p, p%stmts(idx)%value, name)) then
                yes = .true.
                return
            end if
            ! `c(i) = ...` reads whatever appears in the subscript, and a raw
            ! rule statement is text fortad cannot see into, so treat its target
            ! as reading everything it mentions.
            if (allocated(p%stmts(idx)%target)) then
                if (mentions(p%stmts(idx)%target, name)) then
                    yes = .true.
                    return
                end if
            end if
        case (FAD_DO)
            if (expr_reads(p, p%stmts(idx)%lo, name)) yes = .true.
            if (expr_reads(p, p%stmts(idx)%hi, name)) yes = .true.
            if (expr_reads(p, p%stmts(idx)%step, name)) yes = .true.
            if (allocated(p%stmts(idx)%target)) then
                if (trim(p%stmts(idx)%target) == name) yes = .true.
            end if
        case (FAD_IF)
            yes = expr_reads(p, p%stmts(idx)%value, name)
        case (FAD_CALL_STMT)
            if (.not. allocated(p%stmts(idx)%call_args)) return
            do k = 1, size(p%stmts(idx)%call_args)
                if (expr_reads(p, p%stmts(idx)%call_args(k), name)) then
                    yes = .true.
                    return
                end if
            end do
        end select
    end function statement_reads

    recursive logical function expr_reads(p, idx, name) result(yes)
        !! Whether an expression reads `name`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        ! A variable node may carry a whole subscripted reference in its text -
        ! a tape restore is emitted as `x_tape(i - (1) + 1)` - so an exact
        ! comparison misses it. Missing a read here does not merely leave dead
        ! code behind, it deletes a store something needs.
        if (allocated(p%exprs(idx)%text)) then
            if (mentions(p%exprs(idx)%text, name)) then
                yes = .true.
                return
            end if
        end if
        do i = 1, size(p%exprs(idx)%args)
            if (expr_reads(p, p%exprs(idx)%args(i), name)) then
                yes = .true.
                return
            end if
        end do
    end function expr_reads

    logical function mentions(text, name) result(yes)
        !! Whether `name` appears in `text` as a whole identifier.
        character(len=*), intent(in) :: text, name
        integer :: pos, start, before, after

        yes = .false.
        start = 1
        do
            pos = index(text(start:), name)
            if (pos == 0) return
            pos = start + pos - 1
            before = pos - 1
            after = pos + len(name)
            if (.not. is_ident_char(text, before) .and. &
                .not. is_ident_char(text, after)) then
                yes = .true.
                return
            end if
            start = pos + 1
            if (start > len(text)) return
        end do
    end function mentions

    logical function is_ident_char(text, pos) result(yes)
        !! Whether the character at `pos` could continue an identifier.
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos
        character(len=1) :: c

        yes = .false.
        if (pos < 1 .or. pos > len(text)) return
        c = text(pos:pos)
        yes = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") .or. &
            (c >= "0" .and. c <= "9") .or. c == "_"
    end function is_ident_char

    logical function is_dummy(p, name) result(yes)
        !! Whether `name` is a dummy argument.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        if (.not. allocated(p%params)) return
        do i = 1, size(p%params)
            if (trim(p%params(i)) == name) then
                yes = .true.
                return
            end if
        end do
    end function is_dummy

end module fortad_dce
