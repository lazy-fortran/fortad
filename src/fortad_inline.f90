module fortad_inline
    !! Replace a call to a procedure defined alongside it with that
    !! procedure's body.
    !!
    !! fortad refuses to differentiate a call it has no rule for, because
    !! treating an unknown call as inactive would silently drop whatever
    !! derivative flows through it. That refusal is right for a kernel behind a
    !! C binding, where a rule is the only honest answer. It is needless when
    !! the callee is a plain Fortran procedure sitting in the same file: its
    !! body is right there, and differentiating the body is exact where a rule
    !! is only as good as whoever wrote it.
    !!
    !! Enzyme differentiates such calls by inlining at the IR level and never
    !! asks the user for anything. Most of fortnum's Enzyme fixtures are shaped
    !! this way - a residual next to the Newton solve that calls it, an
    !! integrand next to its quadrature - so this is what stands between fortad
    !! and that corpus.
    !!
    !! Inlining happens before differentiation, so every later pass sees one
    !! flat procedure and needs no notion of calls at all.
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                         FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, FAD_CALL, &
                         FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_CALL_STMT, &
                         FAD_IF, FAD_ELSE, FAD_END_IF, &
                         FAD_INTENT_NONE, expr_var
    use fortad_registry, only: registry_has, call_rule_has
    implicit none
    private

    public :: inline_calls, inline_status_t, references

    integer, parameter :: MAX_BINDINGS = 64

    type :: inline_status_t
        logical :: ok = .true.
        character(len=:), allocatable :: message
    end type inline_status_t

    type :: binding_t
        !! What one name inside the callee stands for at this call site.
        character(len=:), allocatable :: name
        !! The caller expression a dummy argument was bound to, or 0 when the
        !! name is a local that was merely renamed.
        integer :: expr = 0
        character(len=:), allocatable :: renamed
        !! An optional dummy the call site did not supply. `present` of it is
        !! false, which is known here and nowhere later.
        logical :: absent = .false.
    end type binding_t

contains

    logical function references(p, name) result(yes)
        !! Whether this procedure calls `name` anywhere.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        do i = 1, p%n_exprs
            if (p%exprs(i)%kind /= FAD_CALL) cycle
            if (same_name(p%exprs(i)%text, name)) then
                yes = .true.
                return
            end if
        end do
        do i = 1, p%n_stmts
            if (.not. allocated(p%stmts(i)%target)) cycle
            if (p%stmts(i)%kind /= FAD_CALL_STMT) cycle
            if (same_name(p%stmts(i)%target, name)) then
                yes = .true.
                return
            end if
        end do
    end function references

    subroutine inline_calls(target, others, n_others, status)
        !! Inline every call in `target` to one of `others`, repeatedly.
        !!
        !! Repeated because a callee may itself call a sibling. A callee that
        !! calls back into the target, directly or in a cycle, would not
        !! terminate, so the number of rounds is bounded and exceeding it is
        !! reported rather than spun on.
        type(fad_proc_t), intent(inout) :: target
        type(fad_proc_t), intent(in) :: others(:)
        integer, intent(in) :: n_others
        type(inline_status_t), intent(out) :: status
        integer, parameter :: MAX_ROUNDS = 8
        !! Total splices before giving up. A self-recursive procedure has no
        !! finite inlining, and a deep chain can grow faster than it is worth
        !! following; either way, stopping with a message beats spinning.
        integer, parameter :: MAX_SPLICES = 256
        integer :: round, n_inlined, tag

        status%ok = .true.
        status%message = ""
        tag = 0
        do round = 1, MAX_ROUNDS
            n_inlined = 0
            call inline_round(target, others, n_others, tag, n_inlined, status)
            if (.not. status%ok) return
            if (n_inlined == 0) return
            if (tag > MAX_SPLICES) exit
        end do
        status%ok = .false.
        status%message = "a call chain in this file did not settle; it is "// &
                         "probably recursive, which has no finite inlining"
    end subroutine inline_calls

    subroutine inline_round(target, others, n_others, tag, n_inlined, status)
        !! One pass over the statement list, inlining what it finds.
        type(fad_proc_t), intent(inout) :: target
        type(fad_proc_t), intent(in) :: others(:)
        integer, intent(in) :: n_others
        integer, intent(inout) :: tag, n_inlined
        type(inline_status_t), intent(inout) :: status
        integer :: i, callee

        i = 1
        do while (i <= target%n_stmts)
            if (tag > 256) then
                status%ok = .false.
                status%message = "a call chain in this file did not settle; "// &
                                 "it is probably recursive"
                return
            end if
            if (target%stmts(i)%kind == FAD_CALL_STMT) then
                callee = named_callee(target%stmts(i)%target, others, n_others)
                if (callee > 0) then
                    tag = tag + 1
                    call inline_sub(target, others(callee), i, tag, status)
                    if (.not. status%ok) return
                    n_inlined = n_inlined + 1
                    cycle
                end if
            end if
            if (target%stmts(i)%value > 0) then
                callee = callee_in(target, target%stmts(i)%value, others, n_others)
                if (callee > 0) then
                    tag = tag + 1
                    call inline_one(target, others(callee), i, tag, status)
                    if (.not. status%ok) return
                    n_inlined = n_inlined + 1
                    cycle
                end if
            end if
            i = i + 1
        end do
    end subroutine inline_round

    integer function named_callee(name, others, n_others) result(which)
        !! Which of `others` this call statement names, if any.
        character(len=*), intent(in) :: name
        type(fad_proc_t), intent(in) :: others(:)
        integer, intent(in) :: n_others
        integer :: k

        which = 0
        if (call_rule_has(name)) return
        do k = 1, n_others
            if (.not. allocated(others(k)%name)) cycle
            if (same_name(others(k)%name, name)) then
                which = k
                return
            end if
        end do
    end function named_callee

    subroutine inline_sub(target, callee, at, tag, status)
        !! Splice a subroutine call's body in place of the call.
        !!
        !! Each dummy is bound to the actual's *name*, not to an expression,
        !! so a dummy the callee writes to writes to the caller's variable -
        !! which is what passing it meant. That only works when the actual is
        !! a plain variable; anything else is refused rather than guessed at.
        type(fad_proc_t), intent(inout) :: target
        type(fad_proc_t), intent(in) :: callee
        integer, intent(in) :: at, tag
        type(inline_status_t), intent(inout) :: status
        type(binding_t) :: binds(MAX_BINDINGS)
        character(len=32) :: suffix
        integer :: n_binds, i, n_actual, a

        n_actual = 0
        if (allocated(target%stmts(at)%call_args)) &
            n_actual = size(target%stmts(at)%call_args)
        n_binds = 0
        if (allocated(callee%params)) then
            ! Fewer actuals than dummies means the trailing dummies are
            ! optional and were left out. They are bound as absent, and
            ! `present` of them folds to false when the body is spliced.
            if (n_actual > size(callee%params)) then
                status%ok = .false.
                status%message = "call to "//trim(callee%name)// &
                                 " does not match its argument list"
                return
            end if
            do i = 1, size(callee%params)
                if (i > n_actual) then
                    n_binds = n_binds + 1
                    binds(n_binds)%name = trim(callee%params(i))
                    binds(n_binds)%expr = 0
                    binds(n_binds)%renamed = ""
                    binds(n_binds)%absent = .true.
                    cycle
                end if
                a = target%stmts(at)%call_args(i)
                if (a <= 0 .or. a > target%n_exprs) then
                    status%ok = .false.
                    status%message = "call to "//trim(callee%name)// &
                                     " has an argument fortad cannot follow"
                    return
                end if
                if (target%exprs(a)%kind /= FAD_VAR) then
                    status%ok = .false.
                    status%message = "inlining "//trim(callee%name)// &
                        " needs plain variables as arguments, because it may "// &
                        "write to them"
                    return
                end if
                n_binds = n_binds + 1
                binds(n_binds)%name = trim(callee%params(i))
                binds(n_binds)%expr = 0
                binds(n_binds)%renamed = trim(target%exprs(a)%text)
            end do
        end if

        write (suffix, '(a,i0,a)') "_", tag, "_"
        do i = 1, callee%n_decls
            if (bound(binds, n_binds, callee%decls(i)%name)) cycle
            if (n_binds >= MAX_BINDINGS) then
                status%ok = .false.
                status%message = trim(callee%name)//" has more names than "// &
                                 "inlining can rename"
                return
            end if
            n_binds = n_binds + 1
            binds(n_binds)%name = trim(callee%decls(i)%name)
            binds(n_binds)%expr = 0
            binds(n_binds)%renamed = trim(callee%name)//trim(suffix)// &
                                     trim(callee%decls(i)%name)
        end do

        call declare_locals(target, callee, binds, n_binds)
        call splice_body(target, callee, binds, n_binds, at, status)
        if (.not. status%ok) return
        ! The body now stands where the call did.
        call drop_stmt(target, at + callee%n_stmts)
    end subroutine inline_sub

    subroutine drop_stmt(p, at)
        !! Remove one statement.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: at
        integer :: i

        do i = at, p%n_stmts - 1
            p%stmts(i) = p%stmts(i + 1)
        end do
        p%n_stmts = p%n_stmts - 1
    end subroutine drop_stmt

    recursive integer function callee_in(p, idx, others, n_others) result(which)
        !! The first call in this expression to one of `others`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        type(fad_proc_t), intent(in) :: others(:)
        integer, intent(in) :: n_others
        integer :: i, k

        which = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        ! A registered rule is an explicit instruction about this name, so it
        ! wins over the body sitting next door. Someone who writes a rule for a
        ! procedure fortad could have inlined means to override it - a
        ! stabilised derivative, a cheaper closed form - and inlining anyway
        ! would quietly discard that.
        if (p%exprs(idx)%kind == FAD_CALL) then
            if (.not. registry_has(p%exprs(idx)%text) .and. &
                .not. call_rule_has(p%exprs(idx)%text)) then
                do k = 1, n_others
                    if (.not. allocated(others(k)%name)) cycle
                    if (same_name(others(k)%name, p%exprs(idx)%text)) then
                        which = k
                        return
                    end if
                end do
            end if
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            which = callee_in(p, p%exprs(idx)%args(i), others, n_others)
            if (which > 0) return
        end do
    end function callee_in

    logical function same_name(a, b) result(yes)
        !! Fortran does not distinguish case in names.
        character(len=*), intent(in) :: a, b
        integer :: i
        character(len=:), allocatable :: la, lb

        la = trim(a)
        lb = trim(b)
        yes = .false.
        if (len(la) /= len(lb)) return
        do i = 1, len(la)
            if (lower(la(i:i)) /= lower(lb(i:i))) return
        end do
        yes = .true.
    end function same_name

    character function lower(c)
        character, intent(in) :: c

        lower = c
        if (c >= "A" .and. c <= "Z") lower = achar(iachar(c) + 32)
    end function lower

    subroutine inline_one(target, callee, at, tag, status)
        !! Splice one call to `callee` into `target` ahead of statement `at`.
        type(fad_proc_t), intent(inout) :: target
        type(fad_proc_t), intent(in) :: callee
        integer, intent(in) :: at, tag
        type(inline_status_t), intent(inout) :: status
        type(binding_t) :: binds(MAX_BINDINGS)
        integer :: n_binds, site, result_expr, i

        site = find_call(target, target%stmts(at)%value, callee)
        if (site <= 0) then
            status%ok = .false.
            status%message = "lost the call to "//trim(callee%name)// &
                             " while inlining it"
            return
        end if

        call bind_arguments(target, callee, site, binds, n_binds, tag, status)
        if (.not. status%ok) return
        call declare_locals(target, callee, binds, n_binds)
        call splice_body(target, callee, binds, n_binds, at, status)
        if (.not. status%ok) return

        ! The call now stands for whatever the callee left in its result.
        result_expr = 0
        do i = 1, n_binds
            if (same_name(binds(i)%name, callee%result_name)) then
                result_expr = target%add_expr(expr_var(binds(i)%renamed))
                exit
            end if
        end do
        if (result_expr == 0) then
            status%ok = .false.
            status%message = trim(callee%name)//" never assigns its result"
            return
        end if
        call replace_expr(target, target%stmts(target%n_stmts)%value, site, &
                          result_expr)
        do i = 1, target%n_stmts
            if (target%stmts(i)%value <= 0) cycle
            call replace_expr(target, target%stmts(i)%value, site, result_expr)
            if (target%stmts(i)%value == site) target%stmts(i)%value = result_expr
        end do
    end subroutine inline_one

    recursive integer function find_call(p, idx, callee) result(site)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        type(fad_proc_t), intent(in) :: callee
        integer :: i

        site = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (p%exprs(idx)%kind == FAD_CALL) then
            if (same_name(callee%name, p%exprs(idx)%text)) then
                site = idx
                return
            end if
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            site = find_call(p, p%exprs(idx)%args(i), callee)
            if (site > 0) return
        end do
    end function find_call

    subroutine bind_arguments(target, callee, site, binds, n_binds, tag, status)
        !! Bind each dummy to its actual, and give every other name of the
        !! callee a fresh name so two inlinings of the same procedure cannot
        !! collide.
        type(fad_proc_t), intent(in) :: target
        type(fad_proc_t), intent(in) :: callee
        integer, intent(in) :: site, tag
        type(binding_t), intent(out) :: binds(:)
        integer, intent(out) :: n_binds
        type(inline_status_t), intent(inout) :: status
        character(len=32) :: suffix
        integer :: i, n_actual

        n_binds = 0
        n_actual = 0
        if (allocated(target%exprs(site)%args)) n_actual = size(target%exprs(site)%args)
        if (allocated(callee%params)) then
            if (n_actual /= size(callee%params)) then
                status%ok = .false.
                status%message = "call to "//trim(callee%name)// &
                                 " does not match its argument list"
                return
            end if
            do i = 1, size(callee%params)
                n_binds = n_binds + 1
                binds(n_binds)%name = trim(callee%params(i))
                binds(n_binds)%expr = target%exprs(site)%args(i)
                binds(n_binds)%renamed = ""
            end do
        end if

        write (suffix, '(a,i0,a)') "_", tag, "_"
        do i = 1, callee%n_decls
            if (bound(binds, n_binds, callee%decls(i)%name)) cycle
            if (n_binds >= MAX_BINDINGS) then
                status%ok = .false.
                status%message = trim(callee%name)//" has more names than "// &
                                 "inlining can rename"
                return
            end if
            n_binds = n_binds + 1
            binds(n_binds)%name = trim(callee%decls(i)%name)
            binds(n_binds)%expr = 0
            binds(n_binds)%renamed = trim(callee%name)//trim(suffix)// &
                                     trim(callee%decls(i)%name)
        end do
    end subroutine bind_arguments

    logical function bound(binds, n_binds, name) result(yes)
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds
        character(len=*), intent(in) :: name
        integer :: i

        yes = .false.
        do i = 1, n_binds
            if (same_name(binds(i)%name, name)) then
                yes = .true.
                return
            end if
        end do
    end function bound

    subroutine declare_locals(target, callee, binds, n_binds)
        !! Carry the callee's locals across under their new names.
        type(fad_proc_t), intent(inout) :: target
        type(fad_proc_t), intent(in) :: callee
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds
        type(fad_decl_t) :: d
        integer :: i, k, ignored, actual_index

        do i = 1, callee%n_decls
            do k = 1, n_binds
                if (.not. same_name(binds(k)%name, callee%decls(i)%name)) cycle
                if (binds(k)%expr /= 0) exit
                d = callee%decls(i)
                d%name = binds(k)%renamed
                d%intent = FAD_INTENT_NONE
                d%is_result = .false.
                actual_index = target%decl_index(binds(k)%renamed)
                if (actual_index > 0) then
                    ! The actual belongs to the caller. Preserve interface
                    ! properties such as VALUE when the callee dummy is
                    ! copied over that declaration during inlining.
                    d%is_value = target%decls(actual_index)%is_value
                    d%is_optional = target%decls(actual_index)%is_optional
                else
                    d%is_optional = .false.
                end if
                ignored = target%add_decl(d)
                exit
            end do
        end do
    end subroutine declare_locals

    subroutine splice_body(target, callee, binds, n_binds, at, status)
        !! Copy the callee's statements in ahead of the call site.
        type(fad_proc_t), intent(inout) :: target
        type(fad_proc_t), intent(in) :: callee
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds, at
        type(inline_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        integer :: i, where_at

        where_at = at
        i = 0
        do while (i < callee%n_stmts)
            i = i + 1
            ! `present(dummy)` is decided here: the call site either supplied
            ! that argument or it did not. When it did not, the guarded branch
            ! is dead and is dropped rather than emitted against a name that
            ! does not exist.
            if (callee%stmts(i)%kind == FAD_IF) then
                if (guards_absent(callee, callee%stmts(i)%value, binds, n_binds)) then
                    i = end_of_if(callee, i)
                    cycle
                end if
            end if
            select case (callee%stmts(i)%kind)
            case (FAD_ASSIGN)
                s%kind = FAD_ASSIGN
                s%target = renamed_target(binds, n_binds, callee%stmts(i)%target)
                s%value = import(target, callee, callee%stmts(i)%value, binds, &
                                 n_binds)
                s%line = callee%stmts(i)%line
            case (FAD_IF, FAD_ELSE, FAD_END_IF)
                s = callee%stmts(i)
                if (callee%stmts(i)%kind == FAD_IF) &
                    s%value = import(target, callee, callee%stmts(i)%value, &
                                     binds, n_binds)
            case (FAD_DO, FAD_END_DO)
                s = callee%stmts(i)
                if (callee%stmts(i)%kind == FAD_DO) then
                    s%target = renamed_target(binds, n_binds, callee%stmts(i)%target)
                    s%lo = import(target, callee, callee%stmts(i)%lo, binds, n_binds)
                    s%hi = import(target, callee, callee%stmts(i)%hi, binds, n_binds)
                    if (callee%stmts(i)%step > 0) &
                        s%step = import(target, callee, callee%stmts(i)%step, &
                                        binds, n_binds)
                end if
            case default
                status%ok = .false.
                status%message = "inlining "//trim(callee%name)// &
                                 " would need a statement form it does not have"
                return
            end select
            call insert_stmt(target, where_at, s)
            where_at = where_at + 1
        end do
    end subroutine splice_body

    logical function guards_absent(callee, cond, binds, n_binds) result(yes)
        !! Whether this condition is `present(x)` for an x that was not passed.
        type(fad_proc_t), intent(in) :: callee
        integer, intent(in) :: cond
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds
        integer :: arg, k

        yes = .false.
        if (cond <= 0 .or. cond > callee%n_exprs) return
        if (callee%exprs(cond)%kind /= FAD_CALL) return
        if (.not. same_name(callee%exprs(cond)%text, "present")) return
        if (.not. allocated(callee%exprs(cond)%args)) return
        if (size(callee%exprs(cond)%args) /= 1) return
        arg = callee%exprs(cond)%args(1)
        if (callee%exprs(arg)%kind /= FAD_VAR) return
        do k = 1, n_binds
            if (same_name(binds(k)%name, callee%exprs(arg)%text)) then
                yes = binds(k)%absent
                return
            end if
        end do
    end function guards_absent

    integer function end_of_if(callee, at) result(out)
        !! The index of the FAD_END_IF closing the FAD_IF at `at`.
        type(fad_proc_t), intent(in) :: callee
        integer, intent(in) :: at
        integer :: i, depth

        depth = 0
        do i = at, callee%n_stmts
            if (callee%stmts(i)%kind == FAD_IF) depth = depth + 1
            if (callee%stmts(i)%kind == FAD_END_IF) then
                depth = depth - 1
                if (depth == 0) then
                    out = i
                    return
                end if
            end if
        end do
        out = callee%n_stmts
    end function end_of_if

    function renamed_target(binds, n_binds, text) result(out)
        !! Rename an assignment target, subscript and all.
        !!
        !! A target is text, not an expression tree, so `output(i)` needs both
        !! its array and every name in its subscript rewritten. Renaming only
        !! whole targets left `output(i)` untouched while the right-hand side
        !! moved on to the new names, which is wrong code rather than a
        !! refusal.
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out, base, subs
        integer :: paren

        paren = index(text, "(")
        if (paren == 0) then
            out = renamed_of(binds, n_binds, text)
            return
        end if
        base = trim(text(:paren - 1))
        subs = text(paren + 1:len_trim(text) - 1)
        out = renamed_of(binds, n_binds, base)//"("// &
              rename_in_text(binds, n_binds, subs)//")"
    end function renamed_target

    function rename_in_text(binds, n_binds, text) result(out)
        !! Rewrite every identifier in a fragment of Fortran text.
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i, from

        out = ""
        i = 1
        do while (i <= len(text))
            if (starts_name(text, i)) then
                from = i
                do while (i <= len(text))
                    if (.not. name_char(text(i:i))) exit
                    i = i + 1
                end do
                out = out//renamed_of(binds, n_binds, text(from:i - 1))
            else
                out = out//text(i:i)
                i = i + 1
            end if
        end do
    end function rename_in_text

    logical function starts_name(text, at) result(yes)
        !! Whether a name begins here and is not the tail of an earlier one.
        character(len=*), intent(in) :: text
        integer, intent(in) :: at

        yes = .false.
        if (.not. name_char(text(at:at))) return
        if (text(at:at) >= "0" .and. text(at:at) <= "9") return
        if (at > 1) then
            if (name_char(text(at - 1:at - 1))) return
        end if
        yes = .true.
    end function starts_name

    logical function name_char(c) result(yes)
        character, intent(in) :: c

        yes = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") .or. &
              (c >= "0" .and. c <= "9") .or. c == "_"
    end function name_char

    function renamed_of(binds, n_binds, name) result(out)
        !! What this name is called in the caller.
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: out
        integer :: i

        out = trim(name)
        do i = 1, n_binds
            if (.not. same_name(binds(i)%name, name)) cycle
            if (len_trim(binds(i)%renamed) > 0) out = trim(binds(i)%renamed)
            return
        end do
    end function renamed_of

    recursive integer function import(target, callee, idx, binds, n_binds) &
        result(out)
        !! Rebuild one of the callee's expressions in the caller's arena.
        type(fad_proc_t), intent(inout) :: target
        type(fad_proc_t), intent(in) :: callee
        integer, intent(in) :: idx
        type(binding_t), intent(in) :: binds(:)
        integer, intent(in) :: n_binds
        type(fad_expr_t) :: e
        integer :: i, k

        out = 0
        if (idx <= 0 .or. idx > callee%n_exprs) return
        if (callee%exprs(idx)%kind == FAD_VAR) then
            do k = 1, n_binds
                if (.not. same_name(binds(k)%name, callee%exprs(idx)%text)) cycle
                ! A dummy stands for the caller's actual expression; a local
                ! stands for itself under a new name.
                if (binds(k)%expr /= 0) then
                    out = binds(k)%expr
                else
                    out = target%add_expr(expr_var(binds(k)%renamed))
                end if
                return
            end do
        end if

        ! An array reference carries its name as text, not as a child, so it
        ! needs the same substitution a bare variable gets. Missing this left
        ! the callee's own name for the caller's array in the emitted code.
        e%kind = callee%exprs(idx)%kind
        e%text = callee%exprs(idx)%text
        if (callee%exprs(idx)%kind == FAD_INDEX) then
            do k = 1, n_binds
                if (.not. same_name(binds(k)%name, callee%exprs(idx)%text)) cycle
                if (len_trim(binds(k)%renamed) > 0) then
                    e%text = trim(binds(k)%renamed)
                else if (binds(k)%expr > 0) then
                    ! Bound to a caller expression: only a plain name can serve
                    ! as an array reference here.
                    if (target%exprs(binds(k)%expr)%kind == FAD_VAR) then
                        e%text = trim(target%exprs(binds(k)%expr)%text)
                    end if
                end if
                exit
            end do
        end if
        if (allocated(callee%exprs(idx)%args)) then
            allocate (e%args(size(callee%exprs(idx)%args)))
            do i = 1, size(callee%exprs(idx)%args)
                e%args(i) = import(target, callee, callee%exprs(idx)%args(i), &
                                   binds, n_binds)
            end do
        end if
        out = target%add_expr(e)
    end function import

    subroutine insert_stmt(p, at, s)
        !! Put one statement at `at`, moving the rest down.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: at
        type(fad_stmt_t), intent(in) :: s
        type(fad_stmt_t), allocatable :: grown(:)
        integer :: i

        if (p%n_stmts + 1 > size(p%stmts)) then
            allocate (grown(max(16, 2*size(p%stmts))))
            grown(:p%n_stmts) = p%stmts(:p%n_stmts)
            call move_alloc(grown, p%stmts)
        end if
        do i = p%n_stmts, at, -1
            p%stmts(i + 1) = p%stmts(i)
        end do
        p%stmts(at) = s
        p%n_stmts = p%n_stmts + 1
    end subroutine insert_stmt

    recursive subroutine replace_expr(p, idx, old, new)
        !! Point every reference to `old` at `new`.
        type(fad_proc_t), intent(inout) :: p
        integer, intent(in) :: idx, old, new
        integer :: i

        if (idx <= 0 .or. idx > p%n_exprs) return
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            if (p%exprs(idx)%args(i) == old) then
                p%exprs(idx)%args(i) = new
            else
                call replace_expr(p, p%exprs(idx)%args(i), old, new)
            end if
        end do
    end subroutine replace_expr

end module fortad_inline
