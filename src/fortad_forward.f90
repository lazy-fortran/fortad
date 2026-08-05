module fortad_forward
    !! Forward (tangent) mode: build the JVP procedure from the primal IR.
    !!
    !! For each primal statement `v = e` the tangent statement `v_d = D(e)` is
    !! emitted immediately before it, so `v_d` still sees the old `v` where the
    !! rule needs it and no value has to be saved. Statements whose tangent is a
    !! structural zero produce no code at all: that is activity analysis falling
    !! out of the zero-aware rule builders rather than being a separate pass.
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                        expr_const, expr_var, expr_binop, expr_unop, expr_call, &
                        FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, FAD_CALL, &
                        FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, &
                        FAD_ELSE, FAD_END_IF, FAD_CALL_STMT, FAD_INTENT_IN, &
                        FAD_INTENT_OUT, FAD_INTENT_INOUT, FAD_INTENT_NONE
    use fortad_rules, only: jvp_binop, jvp_unop, jvp_call, has_rule
    use fortad_registry, only: call_rule_has, call_rule_lines, &
                               call_rule_substitute
    use fortad_emit, only: emit_expr
    implicit none
    private

    public :: differentiate_forward, forward_spec_t, forward_status_t

    type :: forward_spec_t
        !! What to differentiate, and with respect to what.
        character(len=:), allocatable :: independents(:)
        character(len=:), allocatable :: dependents(:)
        !! Suffix for tangent names. `x` becomes `x_d` by default.
        character(len=:), allocatable :: suffix
        !! Name of the generated procedure. Defaults to `<primal>_jvp`.
        character(len=:), allocatable :: name
        !! Vector mode: carry `n_dir` tangent directions through one primal
        !! sweep instead of one. Every tangent gains a leading direction
        !! dimension, which is the contiguous axis in Fortran, so the emitted
        !! array expressions vectorise across directions. One primal traversal
        !! then serves k directions at cost `primal + k*active`, rather than
        !! `k*(primal + active)`.
        logical :: vector = .false.
        !! Name of the direction-count dummy argument in vector mode.
        character(len=:), allocatable :: ndir_name
        !! Whether the generated routine also returns the primal value.
        !!
        !! A consumer that already has the value, or that wants a routine
        !! matching a tangent-only contract, does not want it back - and asking
        !! for it keeps the whole primal computation live.
        logical :: with_primal = .true.
    end type forward_spec_t

    type :: forward_status_t
        logical :: ok = .false.
        character(len=:), allocatable :: message
    end type forward_status_t

contains

    subroutine differentiate_forward(primal, spec, tangent, status)
        !! Build the tangent procedure.
        type(fad_proc_t), intent(in) :: primal
        type(forward_spec_t), intent(in) :: spec
        type(fad_proc_t), intent(out) :: tangent
        type(forward_status_t), intent(out) :: status
        character(len=:), allocatable :: suffix, ndir
        logical, allocatable :: active(:)
        integer :: i, ignored

        status%ok = .true.
        suffix = "_d"
        if (allocated(spec%suffix)) suffix = spec%suffix
        ndir = "n_dir"
        if (allocated(spec%ndir_name)) ndir = spec%ndir_name

        if (.not. allocated(spec%independents)) then
            status%ok = .false.
            status%message = "no independent variables given"
            return
        end if

        call seed_activity(primal, spec, active, status)
        if (.not. status%ok) return

        tangent%name = primal%name//"_jvp"
        if (allocated(spec%name)) tangent%name = spec%name
        tangent%is_function = .false.
        tangent%real_suffix = "d0"
        if (allocated(primal%real_suffix)) tangent%real_suffix = primal%real_suffix
        ! The derivative names the same kinds as the primal, so it needs the
        ! same imports.
        if (allocated(primal%uses)) then
            tangent%uses = primal%uses
            tangent%n_uses = primal%n_uses
        end if
        tangent%is_pure = .not. has_calls(primal)

        call build_signature(primal, tangent, active, suffix, spec%vector, ndir, &
                             spec%with_primal)
        call build_body(primal, tangent, active, suffix, spec%vector, status)
        if (.not. status%ok) return

        ! Every local the primal declared is still a local of the tangent
        ! procedure, active or not: an inactive local still holds a primal
        ! value the active statements read.
        do i = 1, primal%n_decls
            if (is_dummy(primal, primal%decls(i)%name)) cycle
            if (primal%decls(i)%is_result) cycle
            ignored = tangent%add_decl(strip_intent(primal%decls(i)))
            if (active(i)) then
                call add_tangent_decl(tangent, primal%decls(i), suffix, &
                                      FAD_INTENT_NONE, spec%vector, ndir)
            end if
        end do
    end subroutine differentiate_forward

    subroutine seed_activity(primal, spec, active, status)
        !! Mark declarations reachable from an independent.
        !!
        !! Forward "varied" dataflow: a variable is active if it is an
        !! independent, or if it is assigned from an expression that reads an
        !! active variable. Iterated to a fixed point so loop-carried
        !! dependencies converge.
        type(fad_proc_t), intent(in) :: primal
        type(forward_spec_t), intent(in) :: spec
        logical, allocatable, intent(out) :: active(:)
        type(forward_status_t), intent(inout) :: status
        integer :: i, j, di
        logical :: changed
        character(len=:), allocatable :: base

        allocate (active(max(1, primal%n_decls)))
        active = .false.

        do i = 1, size(spec%independents)
            di = primal%decl_index(trim(spec%independents(i)))
            if (di == 0) then
                status%ok = .false.
                status%message = "independent '"//trim(spec%independents(i))// &
                                 "' is not declared in "//primal%name
                return
            end if
            active(di) = .true.
        end do

        changed = .true.
        do while (changed)
            changed = .false.
            do j = 1, primal%n_stmts
                if (primal%stmts(j)%kind == FAD_CALL_STMT) then
                    ! A call is opaque, so which arguments it writes is unknown.
                    ! If any argument is active, every argument is treated as
                    ! active: the alternative is guessing, and guessing wrong
                    ! drops a derivative silently.
                    if (.not. call_reads_active(primal, primal%stmts(j), active)) cycle
                    do i = 1, size(primal%stmts(j)%call_args)
                        di = arg_decl_index(primal, primal%stmts(j)%call_args(i))
                        if (di <= 0) cycle
                        if (.not. is_real_decl(primal, di)) cycle
                        if (.not. active(di)) then
                            active(di) = .true.
                            changed = .true.
                        end if
                    end do
                    cycle
                end if
                if (primal%stmts(j)%kind /= FAD_ASSIGN) cycle
                if (.not. expr_reads_active(primal, primal%stmts(j)%value, active)) cycle
                base = target_base(primal%stmts(j)%target)
                di = primal%decl_index(base)
                if (di > 0) then
                    if (.not. active(di)) then
                        active(di) = .true.
                        changed = .true.
                    end if
                end if
            end do
        end do
    end subroutine seed_activity

    logical function has_calls(p) result(yes)
        !! True when the procedure calls something fortad cannot see into.
        type(fad_proc_t), intent(in) :: p
        integer :: i

        yes = .false.
        do i = 1, p%n_stmts
            if (p%stmts(i)%kind == FAD_CALL_STMT) then
                yes = .true.
                return
            end if
        end do
    end function has_calls

    logical function call_reads_active(p, s, active) result(yes)
        !! True when any actual argument of a call reads an active variable.
        type(fad_proc_t), intent(in) :: p
        type(fad_stmt_t), intent(in) :: s
        logical, intent(in) :: active(:)
        integer :: i

        yes = .false.
        if (.not. allocated(s%call_args)) return
        do i = 1, size(s%call_args)
            if (expr_reads_active(p, s%call_args(i), active)) then
                yes = .true.
                return
            end if
        end do
    end function call_reads_active

    integer function arg_decl_index(p, idx) result(di)
        !! Declaration index of an actual argument that is a plain variable.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx

        di = 0
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_VAR, FAD_INDEX)
            di = p%decl_index(p%exprs(idx)%text)
        end select
    end function arg_decl_index

    logical function is_real_decl(p, di) result(yes)
        !! Only real arguments carry derivatives; an integer dimension does not.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: di

        yes = .false.
        if (di <= 0 .or. di > p%n_decls) return
        if (.not. allocated(p%decls(di)%type_name)) return
        yes = index(p%decls(di)%type_name, "real") == 1 .or. &
              index(p%decls(di)%type_name, "REAL") == 1
    end function is_real_decl

    recursive logical function expr_reads_active(p, idx, active) result(yes)
        !! True when the expression reads any active variable.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        integer :: i, di

        yes = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        associate (e => p%exprs(idx))
            select case (e%kind)
            case (FAD_VAR, FAD_INDEX)
                di = p%decl_index(e%text)
                if (di > 0) yes = active(di)
                if (yes) return
            end select
            do i = 1, size(e%args)
                if (expr_reads_active(p, e%args(i), active)) then
                    yes = .true.
                    return
                end if
            end do
        end associate
    end function expr_reads_active

    subroutine build_signature(primal, tangent, active, suffix, vector, ndir, &
                               with_primal)
        !! Dummy arguments: every primal argument, each active one followed by
        !! its tangent, then the result and its tangent for a function. In
        !! vector mode the direction count leads the list.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        character(len=*), intent(in) :: ndir
        logical, intent(in) :: with_primal
        character(len=64), allocatable :: names(:)
        integer :: i, n, di, ignored
        type(fad_decl_t) :: d

        allocate (names(2*(size(primal%params) + 3)))
        n = 0
        if (vector) then
            n = n + 1
            names(n) = ndir
            d%name = ndir
            d%type_name = "integer"
            d%intent = FAD_INTENT_IN
            ignored = tangent%add_decl(d)
            d = fad_decl_t()
        end if
        do i = 1, size(primal%params)
            di = primal%decl_index(trim(primal%params(i)))
            ! An active `intent(out)` dummy is a primal value the caller asked
            ! not to be given back. Inactive ones stay: a status flag is not a
            ! derivative output and dropping it would change the contract.
            if (di > 0 .and. .not. with_primal) then
                if (active(di) .and. primal%decls(di)%intent == FAD_INTENT_OUT) then
                    n = n + 1
                    names(n) = trim(primal%params(i))//suffix
                    call add_tangent_decl(tangent, primal%decls(di), suffix, &
                                          FAD_INTENT_OUT, vector, ndir)
                    ! Dropped from the signature but still written by the primal
                    ! statements, so it stays as a local. Whether those writes
                    ! survive is dead-store elimination's decision, not this
                    ! routine's - and an undeclared name would not compile.
                    d = primal%decls(di)
                    d%intent = FAD_INTENT_NONE
                    d%is_result = .false.
                    ignored = tangent%add_decl(d)
                    d = fad_decl_t()
                    cycle
                end if
            end if
            n = n + 1
            names(n) = trim(primal%params(i))
            if (di == 0) cycle
            ignored = tangent%add_decl(primal%decls(di))
            if (.not. active(di)) cycle
            n = n + 1
            names(n) = trim(primal%params(i))//suffix
            call add_tangent_decl(tangent, primal%decls(di), suffix, &
                                  tangent_intent(primal%decls(di)%intent), &
                                  vector, ndir)
        end do

        if (primal%is_function) then
            di = primal%decl_index(primal%result_name)
            if (with_primal) then
                n = n + 1
                names(n) = primal%result_name
            end if
            if (di > 0) then
                d = primal%decls(di)
                d%intent = FAD_INTENT_OUT
                if (with_primal) ignored = tangent%add_decl(d)
                n = n + 1
                names(n) = primal%result_name//suffix
                call add_tangent_decl(tangent, d, suffix, FAD_INTENT_OUT, &
                                      vector, ndir)
            end if
        end if

        tangent%params = names(1:n)
    end subroutine build_signature

    integer function tangent_intent(primal_intent) result(out)
        !! A tangent argument carries the intent of its primal, except that an
        !! `intent(in)` primal still needs its tangent read in.
        integer, intent(in) :: primal_intent

        select case (primal_intent)
        case (FAD_INTENT_IN)
            out = FAD_INTENT_IN
        case (FAD_INTENT_OUT)
            out = FAD_INTENT_OUT
        case default
            out = FAD_INTENT_INOUT
        end select
    end function tangent_intent

    subroutine add_tangent_decl(tangent, primal_decl, suffix, intent_code, &
                                vector, ndir)
        !! Declare the tangent counterpart of a primal entity.
        !!
        !! In vector mode the direction axis goes **first**, because Fortran
        !! stores the leftmost index contiguously and the direction axis is the
        !! one every tangent expression sweeps. `a(n)` becomes `a_d(n_dir, n)`,
        !! and `a_d(:, i)` is then a contiguous vector the compiler can load
        !! and fuse as a unit.
        type(fad_proc_t), intent(inout) :: tangent
        type(fad_decl_t), intent(in) :: primal_decl
        character(len=*), intent(in) :: suffix
        integer, intent(in) :: intent_code
        logical, intent(in), optional :: vector
        character(len=*), intent(in), optional :: ndir
        type(fad_decl_t) :: d
        integer :: ignored
        logical :: vec

        vec = .false.
        if (present(vector)) vec = vector

        d = primal_decl
        d%name = primal_decl%name//suffix
        ! VALUE belongs to the primal dummy, not to its tangent. A tangent is
        ! written by the generated routine, so VALUE would conflict with the
        ! required INTENT(INOUT) contract.
        d%is_value = .false.
        d%intent = intent_code
        d%is_result = .false.
        if (vec) then
            if (d%is_array .and. allocated(d%dims)) then
                d%dims = ndir//", "//d%dims
            else
                d%dims = ndir
            end if
            d%is_array = .true.
            ! Contiguity of the primal says nothing about the tangent block,
            ! and a wrong `contiguous` is a promise the caller may not keep.
            d%is_contiguous = .false.
        end if
        ignored = tangent%add_decl(d)
    end subroutine add_tangent_decl

    type(fad_decl_t) function strip_intent(d) result(out)
        !! A local copy of a declaration with no intent, for use as a local.
        type(fad_decl_t), intent(in) :: d

        out = d
        out%intent = FAD_INTENT_NONE
        out%is_result = .false.
    end function strip_intent

    subroutine build_body(primal, tangent, active, suffix, vector, status)
        !! Walk the primal statements, emitting tangent then primal.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        type(fad_stmt_t) :: s
        integer :: i, dexpr, ignored, di

        do i = 1, primal%n_stmts
            associate (ps => primal%stmts(i))
                select case (ps%kind)
                case (FAD_ASSIGN)
                    di = primal%decl_index(target_base(ps%target))
                    if (di > 0) then
                        if (active(di)) then
                            dexpr = tangent_of(primal, tangent, ps%value, active, &
                                               suffix, vector, status)
                            if (.not. status%ok) return
                            s%kind = FAD_ASSIGN
                            s%target = tangent_name(ps%target, suffix, vector)
                            if (dexpr == 0) then
                                s%value = tangent%add_expr( &
                                    expr_const("0.0"//tangent%real_suffix))
                            else
                                s%value = dexpr
                            end if
                            ignored = tangent%add_stmt(s)
                        end if
                    end if
                    s%kind = FAD_ASSIGN
                    s%target = ps%target
                    s%value = copy_expr(primal, tangent, ps%value)
                    ignored = tangent%add_stmt(s)

                case (FAD_DO)
                    s%kind = FAD_DO
                    s%target = ps%target
                    s%lo = copy_expr(primal, tangent, ps%lo)
                    s%hi = copy_expr(primal, tangent, ps%hi)
                    s%step = 0
                    if (ps%step /= 0) s%step = copy_expr(primal, tangent, ps%step)
                    ignored = tangent%add_stmt(s)

                case (FAD_END_DO, FAD_END_IF, FAD_ELSE)
                    s%kind = ps%kind
                    s%value = 0
                    ignored = tangent%add_stmt(s)

                case (FAD_IF)
                    s%kind = FAD_IF
                    s%value = copy_expr(primal, tangent, ps%value)
                    ignored = tangent%add_stmt(s)

                case (FAD_CALL_STMT)
                    call emit_call_tangent(primal, tangent, ps, active, suffix, &
                                           vector, status)
                    if (.not. status%ok) return

                case default
                    status%ok = .false.
                    status%message = "forward mode: unsupported statement kind"
                    return
                end select
            end associate
        end do
    end subroutine build_body

    subroutine emit_call_tangent(primal, tangent, ps, active, suffix, vector, &
                                 status)
        !! Apply a registered statement rule to a subroutine call.
        !!
        !! The call is opaque to fortad: it emits the registered tangent
        !! statements and then the call itself. Without a rule it refuses,
        !! because assuming a call is inactive would silently drop whatever
        !! derivative flows through it.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        type(fad_stmt_t), intent(in) :: ps
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        character(len=512), allocatable :: args(:), tangents(:), adjoints(:)
        type(fad_stmt_t) :: s
        integer :: i, di, ignored, n_args

        if (.not. call_rule_has(ps%target)) then
            status%ok = .false.
            status%message = "no derivative rule for the call to '"//ps%target// &
                "'; register one with fad_add_call_rule, or keep it out of "// &
                "the active path"
            return
        end if

        n_args = size(ps%call_args)
        allocate (args(n_args), tangents(n_args), adjoints(n_args))
        do i = 1, n_args
            args(i) = emit_expr(primal, ps%call_args(i))
            tangents(i) = trim(args(i))//suffix
            adjoints(i) = trim(args(i))//"_b"
            di = primal%decl_index(trim(args(i)))
            if (di > 0) then
                if (.not. active(di)) tangents(i) = "<inactive>"
            end if
        end do

        ! The primal call goes first, unlike an assignment. A rule generally
        ! needs the call's *outputs*: the tangent of a linear solve is
        ! `A x_d = b_d - A_d x`, which reads the solution `x`. A rule needing a
        ! pre-call value must save it itself, and this is documented rather
        ! than inferred, because fortad cannot see which arguments a call
        ! writes.
        s%kind = FAD_CALL_STMT
        s%target = ps%target
        block
            integer, allocatable :: cargs(:)
            allocate (cargs(n_args))
            do i = 1, n_args
                cargs(i) = copy_expr(primal, tangent, ps%call_args(i))
            end do
            s%call_args = cargs
        end block
        ignored = tangent%add_stmt(s)

        do i = 1, call_rule_lines(ps%target, "tangent")
            s%kind = FAD_ASSIGN
            s%target = "!fad_raw"
            s%value = tangent%add_expr(expr_const( &
                call_rule_substitute(ps%target, "tangent", i, args, tangents, &
                                     adjoints)))
            ignored = tangent%add_stmt(s)
        end do
    end subroutine emit_call_tangent

    recursive integer function tangent_of(primal, tangent, idx, active, suffix, &
                                          vector, status) result(out)
        !! Tangent of a primal expression, as an expression in `tangent`.
        !!
        !! In vector mode a tangent leaf carries the whole direction block:
        !! `x_d` becomes `x_d(:)` and `a(i)` becomes `a_d(:, i)`. Every rule
        !! above then combines array tangents with scalar primal factors, which
        !! is exactly the shape Fortran vectorises.
        type(fad_proc_t), intent(in) :: primal
        type(fad_proc_t), intent(inout) :: tangent
        integer, intent(in) :: idx
        logical, intent(in) :: active(:)
        character(len=*), intent(in) :: suffix
        logical, intent(in) :: vector
        type(forward_status_t), intent(inout) :: status
        integer, allocatable :: args(:), dargs(:)
        integer :: i, di, a, b, da, db
        type(fad_expr_t) :: e

        out = 0
        if (idx <= 0 .or. idx > primal%n_exprs) return

        associate (pe => primal%exprs(idx))
            select case (pe%kind)
            case (FAD_CONST)
                out = 0

            case (FAD_VAR)
                di = primal%decl_index(pe%text)
                if (di > 0) then
                    if (active(di)) then
                        if (vector) then
                            allocate (args(1))
                            args(1) = tangent%add_expr(expr_const(":"))
                            e%kind = FAD_INDEX
                            e%text = pe%text//suffix
                            e%args = args
                            out = tangent%add_expr(e)
                        else
                            out = tangent%add_expr(expr_var(pe%text//suffix))
                        end if
                    end if
                end if

            case (FAD_INDEX)
                di = primal%decl_index(pe%text)
                if (di > 0) then
                    if (active(di)) then
                        if (vector) then
                            allocate (args(size(pe%args) + 1))
                            args(1) = tangent%add_expr(expr_const(":"))
                            do i = 1, size(pe%args)
                                args(i + 1) = copy_expr(primal, tangent, pe%args(i))
                            end do
                        else
                            allocate (args(size(pe%args)))
                            do i = 1, size(pe%args)
                                args(i) = copy_expr(primal, tangent, pe%args(i))
                            end do
                        end if
                        e%kind = FAD_INDEX
                        e%text = pe%text//suffix
                        e%args = args
                        out = tangent%add_expr(e)
                    end if
                end if

            case (FAD_BINOP)
                a = copy_expr(primal, tangent, pe%args(1))
                b = copy_expr(primal, tangent, pe%args(2))
                da = tangent_of(primal, tangent, pe%args(1), active, suffix, vector, status)
                if (.not. status%ok) return
                db = tangent_of(primal, tangent, pe%args(2), active, suffix, vector, status)
                if (.not. status%ok) return
                out = jvp_binop(tangent, pe%text, a, b, da, db)

            case (FAD_UNOP)
                a = copy_expr(primal, tangent, pe%args(1))
                da = tangent_of(primal, tangent, pe%args(1), active, suffix, vector, status)
                if (.not. status%ok) return
                out = jvp_unop(tangent, pe%text, a, da)

            case (FAD_CALL)
                allocate (args(size(pe%args)), dargs(size(pe%args)))
                do i = 1, size(pe%args)
                    args(i) = copy_expr(primal, tangent, pe%args(i))
                    dargs(i) = tangent_of(primal, tangent, pe%args(i), active, &
                                          suffix, vector, status)
                    if (.not. status%ok) return
                end do
                if (all(dargs == 0)) then
                    out = 0
                else if (has_rule(pe%text)) then
                    out = jvp_call(tangent, pe%text, args, dargs)
                else
                    status%ok = .false.
                    status%message = "no derivative rule for '"//pe%text// &
                        "'; register one with fad_add_rule, or keep it out of "// &
                        "the active path"
                    return
                end if
            end select
        end associate
    end function tangent_of

    recursive integer function copy_expr(src, dst, idx) result(out)
        !! Copy a primal expression into the tangent procedure's arena.
        type(fad_proc_t), intent(in) :: src
        type(fad_proc_t), intent(inout) :: dst
        integer, intent(in) :: idx
        type(fad_expr_t) :: e
        integer, allocatable :: args(:)
        integer :: i

        out = 0
        if (idx <= 0 .or. idx > src%n_exprs) return
        e%kind = src%exprs(idx)%kind
        e%text = src%exprs(idx)%text
        allocate (args(size(src%exprs(idx)%args)))
        do i = 1, size(args)
            args(i) = copy_expr(src, dst, src%exprs(idx)%args(i))
        end do
        e%args = args
        out = dst%add_expr(e)
    end function copy_expr

    logical function is_dummy(p, name) result(yes)
        !! True when `name` is a dummy argument of `p`.
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

    function target_base(target) result(base)
        !! The variable name of an assignment target, without any subscript.
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

    function tangent_name(target, suffix, vector) result(name)
        !! Tangent counterpart of an assignment target, keeping any subscript
        !! and, in vector mode, prefixing the direction axis.
        character(len=*), intent(in) :: target, suffix
        logical, intent(in) :: vector
        character(len=:), allocatable :: name
        integer :: pos

        pos = index(target, "(")
        if (pos > 0) then
            if (vector) then
                name = target(1:pos - 1)//suffix//"(:, "//target(pos + 1:)
            else
                name = target(1:pos - 1)//suffix//target(pos:)
            end if
        else
            if (vector) then
                name = target//suffix//"(:)"
            else
                name = target//suffix
            end if
        end if
    end function tangent_name

end module fortad_forward
