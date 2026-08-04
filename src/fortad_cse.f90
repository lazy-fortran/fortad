module fortad_cse
    !! Common-subexpression elimination over generated statements.
    !!
    !! **Not currently wired into the pipeline.** It produces wrong Hessians:
    !! `fad_hvp` runs forward mode over a generated adjoint, and the temporaries
    !! this pass introduces interact with the ones already in that adjoint in a
    !! way that corrupts the second-order result. Skipping names already in
    !! scope was not sufficient, and the failure is silent - a plausible but
    !! non-symmetric Hessian - so the pass stays disabled until the interaction
    !! is understood rather than patched around.
    !!
    !! Measurement also did not support enabling it: on the recurrence kernel
    !! it removed two of three `exp` evaluations from the emitted text and moved
    !! runtime by less than the noise, because gfortran had already shared them
    !! within the loop body. The gap it was meant to close is elsewhere.
    !!
    !! Derivative code repeats itself by construction. The tangent of
    !! `u*exp(k*a)` needs `exp(k*a)` for the value and again for the partial;
    !! a reverse sweep that recomputes a statement then differentiates it needs
    !! it a third time. Each of those is a transcendental, and the compiler will
    !! not share them across statements because it cannot prove the arguments
    !! unchanged.
    !!
    !! fortad can, because it built them. Expressions are hash-consed, so two
    !! occurrences of the same subtree carry the same index and counting uses is
    !! counting integers.
    !!
    !! What gets hoisted is deliberately narrow: only subexpressions that
    !! contain a function call, and only within a run of consecutive assignments
    !! with no control flow between them. Hoisting cheap arithmetic would trade
    !! a register for a multiply and lose, and hoisting across a branch or a
    !! loop boundary would change what is evaluated.
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                        expr_var, FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, &
                        FAD_CALL, FAD_INDEX, FAD_ASSIGN, FAD_INTENT_NONE
    implicit none
    private

    public :: eliminate_common_subexpressions

contains

    subroutine eliminate_common_subexpressions(p, prefix)
        !! Bind repeated call-bearing subexpressions to temporaries.
        type(fad_proc_t), intent(inout) :: p
        !! Prefix for the temporaries this pass introduces.
        character(len=*), intent(in) :: prefix
        integer, allocatable :: uses(:), bound(:)
        type(fad_stmt_t), allocatable :: out(:)
        integer :: run_start, i, n_out, n_tmp

        if (p%n_stmts == 0 .or. p%n_exprs == 0) return

        allocate (uses(p%n_exprs), bound(p%n_exprs))
        allocate (out(4*p%n_stmts + 16))
        bound = 0
        n_out = 0
        n_tmp = 0

        run_start = 1
        i = 1
        do while (i <= p%n_stmts + 1)
            if (i > p%n_stmts) then
                call flush_run(p, run_start, i - 1, uses, bound, out, n_out, &
                               n_tmp, prefix)
                exit
            end if
            if (p%stmts(i)%kind /= FAD_ASSIGN) then
                call flush_run(p, run_start, i - 1, uses, bound, out, n_out, &
                               n_tmp, prefix)
                n_out = n_out + 1
                out(n_out) = p%stmts(i)
                run_start = i + 1
            end if
            i = i + 1
        end do

        if (n_out > 0) then
            if (size(p%stmts) < n_out) then
                deallocate (p%stmts)
                allocate (p%stmts(n_out + 16))
            end if
            p%stmts(1:n_out) = out(1:n_out)
            p%n_stmts = n_out
        end if
    end subroutine eliminate_common_subexpressions

    subroutine flush_run(p, lo, hi, uses, bound, out, n_out, n_tmp, prefix)
        !! Rewrite one run of consecutive assignments.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: lo, hi
        integer, intent(inout) :: uses(:), bound(:), n_out, n_tmp
        type(fad_stmt_t), intent(inout) :: out(:)
        character(len=*), intent(in) :: prefix
        type(fad_stmt_t) :: s
        type(fad_decl_t) :: d
        character(len=32) :: buf
        character(len=:), allocatable :: name
        integer :: i, e, ignored, tmp_expr

        if (hi < lo) return

        ! Count how often each expression index is reached from this run.
        uses = 0
        do i = lo, hi
            call count_uses(p, p%stmts(i)%value, uses)
        end do

        ! Emit the run, hoisting a shared subexpression the first time its
        ! defining statement is reached.
        do i = lo, hi
            do e = 1, p%n_exprs
                if (uses(e) < 2) cycle
                if (bound(e) /= 0) cycle
                if (.not. worth_binding(p, e)) cycle
                if (.not. reaches(p, p%stmts(i)%value, e)) cycle

                ! Skip any name already in scope. fad_hvp runs forward mode
                ! over a generated adjoint, so the inner pass's temporaries are
                ! already declared; reusing one would silently alias two
                ! different values and produce a plausible wrong Hessian.
                do
                    n_tmp = n_tmp + 1
                    write (buf, '(i0)') n_tmp
                    name = prefix//trim(buf)
                    if (p%decl_index(name) == 0) exit
                end do

                d%name = name
                d%type_name = real_type_of(p)
                d%intent = FAD_INTENT_NONE
                d%is_array = .false.
                ignored = p%add_decl(d)

                s%kind = FAD_ASSIGN
                s%target = name
                s%value = e
                n_out = n_out + 1
                out(n_out) = s
                ! Recording the binding *after* emitting means the definition
                ! still prints the original expression; every later reference
                ! becomes the variable.
                bound(e) = p%add_expr(expr_var(name))
            end do

            s = p%stmts(i)
            s%value = substitute(p, s%value, bound)
            n_out = n_out + 1
            out(n_out) = s
        end do
    end subroutine flush_run

    recursive subroutine count_uses(p, idx, uses)
        !! Count references to each expression index, once per occurrence.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer, intent(inout) :: uses(:)
        integer :: i

        if (idx <= 0 .or. idx > p%n_exprs) return
        uses(idx) = uses(idx) + 1
        ! A shared node is visited once per reference, which is exactly the
        ! count wanted; descending again is redundant but harmless and keeps
        ! nested sharing counted at every level.
        if (uses(idx) > 1) return
        do i = 1, size(p%exprs(idx)%args)
            call count_uses(p, p%exprs(idx)%args(i), uses)
        end do
    end subroutine count_uses

    recursive logical function reaches(p, root, target) result(yes)
        !! Whether `target` occurs anywhere under `root`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: root, target
        integer :: i

        yes = .false.
        if (root <= 0 .or. root > p%n_exprs) return
        if (root == target) then
            yes = .true.
            return
        end if
        do i = 1, size(p%exprs(root)%args)
            if (reaches(p, p%exprs(root)%args(i), target)) then
                yes = .true.
                return
            end if
        end do
    end function reaches

    logical function worth_binding(p, idx) result(yes)
        !! Only subexpressions containing a call are worth a temporary.
        !!
        !! A transcendental is tens of cycles and a load is a few; a multiply is
        !! one, so hoisting arithmetic would cost more than it saves.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx

        yes = contains_call(p, idx)
    end function worth_binding

    recursive logical function contains_call(p, idx) result(yes)
        !! Whether the subtree contains a function call.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: i

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_CALL) then
            yes = .true.
            return
        end if
        do i = 1, size(p%exprs(idx)%args)
            if (contains_call(p, p%exprs(idx)%args(i))) then
                yes = .true.
                return
            end if
        end do
    end function contains_call

    recursive integer function substitute(p, idx, bound) result(out)
        !! Replace bound subexpressions with their temporaries.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx
        integer, intent(in) :: bound(:)
        type(fad_expr_t) :: e
        integer, allocatable :: args(:)
        integer :: i
        logical :: changed

        out = idx
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (bound(idx) /= 0) then
            out = bound(idx)
            return
        end if
        if (size(p%exprs(idx)%args) == 0) return

        allocate (args(size(p%exprs(idx)%args)))
        changed = .false.
        do i = 1, size(args)
            args(i) = substitute(p, p%exprs(idx)%args(i), bound)
            if (args(i) /= p%exprs(idx)%args(i)) changed = .true.
        end do
        if (.not. changed) return

        e%kind = p%exprs(idx)%kind
        e%text = p%exprs(idx)%text
        e%args = args
        out = p%add_expr(e)
    end function substitute

    function real_type_of(p) result(type_name)
        !! The real type this procedure works in.
        type(fad_proc_t), intent(in) :: p
        character(len=:), allocatable :: type_name
        integer :: i

        type_name = "real(8)"
        do i = 1, p%n_decls
            if (.not. allocated(p%decls(i)%type_name)) cycle
            if (p%decls(i)%is_array) cycle
            if (index(p%decls(i)%type_name, "real") == 1) then
                type_name = p%decls(i)%type_name
                return
            end if
        end do
    end function real_type_of

end module fortad_cse
