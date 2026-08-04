module fortad
    !! fortad's public API.
    !!
    !! One call differentiates Fortran source and returns Fortran source:
    !!
    !! ```fortran
    !! use fortad, only: fad_jvp, fad_result_t
    !!
    !! res = fad_jvp(source, independents=["x", "y"])
    !! if (res%ok) write (unit, '(a)') res%code
    !! ```
    !!
    !! Nothing else is required: no annotations in the primal, no plugin, no
    !! build-system change. The returned text is standard Fortran.
    use fortad_ir, only: fad_proc_t
    use fortad_lower, only: lower_source, lower_status_t
    use fortad_forward, only: differentiate_forward, forward_spec_t, &
                              forward_status_t
    use fortad_reverse, only: differentiate_reverse, reverse_spec_t, &
                              reverse_status_t
    use fortad_taylor_gen, only: differentiate_taylor, taylor_spec_t, &
                                 taylor_status_t
    use fortad_emit, only: emit_proc, emit_module
    use fortad_dce, only: eliminate_dead_stores, fold_zero_accumulations, &
                          eliminate_dead_arrays, eliminate_dead_loops
    use fortad_opt, only: optimise
    use fortad_registry, only: fad_add_rule, fad_add_call_rule, &
                               fad_clear_rules
    use fortad_sparse, only: sparsity_t, colour_columns, seed_matrix, &
                             recover_entries, star_colour_columns, &
                             recover_symmetric
    use fortad_revolve, only: revolve_t, revolve_action_t, revolve_schedule, &
                              REV_ADVANCE, REV_TAKESHOT, REV_RESTORE, REV_TURN
    use fortad_taylor, only: tay_const, tay_var, tay_add, tay_sub, tay_scale, &
                             tay_mul, tay_div, tay_exp, tay_log, tay_sqrt, &
                             tay_sin_cos, tay_pow_int, tay_derivative
    implicit none
    private

    public :: fad_jvp, fad_vjp, fad_hvp, fad_taylor, fad_roundtrip, &
              fad_result_t, &
              fad_version, fad_add_rule, fad_add_call_rule, fad_clear_rules
    public :: sparsity_t, colour_columns, seed_matrix, recover_entries
    public :: star_colour_columns, recover_symmetric
    public :: revolve_t, revolve_action_t, revolve_schedule
    public :: REV_ADVANCE, REV_TAKESHOT, REV_RESTORE, REV_TURN
    public :: tay_const, tay_var, tay_add, tay_sub, tay_scale, tay_mul, tay_div
    public :: tay_exp, tay_log, tay_sqrt, tay_sin_cos, tay_pow_int, tay_derivative

    character(len=*), parameter :: FORTAD_VERSION = "0.1.0"

    type :: fad_result_t
        !! The outcome of a differentiation.
        logical :: ok = .false.
        !! Generated Fortran, when `ok`.
        character(len=:), allocatable :: code
        !! Why not, when not `ok`. Always names the construct and, where the
        !! frontend knows it, the line.
        character(len=:), allocatable :: message
    end type fad_result_t

contains

    function fad_version() result(v)
        !! The fortad version string.
        character(len=:), allocatable :: v

        v = FORTAD_VERSION
    end function fad_version

    function fad_jvp(source, independents, name, suffix, n_directions, &
                     module_name) result(res)
        !! Forward mode. Returns a subroutine computing the primal and its
        !! Jacobian-vector product in one sweep.
        !!
        !! `independents` names the variables the derivative is taken with
        !! respect to. Everything reachable from them is active; everything else
        !! is left alone, which is why the generated code contains no dead
        !! tangent statements.
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: independents(:)
        !! Name of the generated procedure. Defaults to `<primal>_jvp`.
        character(len=*), intent(in), optional :: name
        !! Suffix for tangent variables. Defaults to `_d`.
        character(len=*), intent(in), optional :: suffix
        !! Name of a direction-count dummy argument. Supplying it selects
        !! **vector mode**: the generated routine carries that many tangent
        !! directions through a single primal sweep, with the direction axis
        !! leading every tangent array so it is the contiguous one.
        character(len=*), intent(in), optional :: n_directions
        !! Wrap the result in a module of this name. Strongly recommended: the
        !! consumer then gets a compiler-checked interface instead of an
        !! external declaration nobody verifies.
        character(len=*), intent(in), optional :: module_name
        type(fad_result_t) :: res
        type(fad_proc_t) :: primal, tangent
        type(lower_status_t) :: lstat
        type(forward_status_t) :: fstat
        type(forward_spec_t) :: spec
        integer :: i, width

        call lower_source(source, primal, lstat)
        if (.not. lstat%ok) then
            res%ok = .false.
            res%message = lstat%message
            return
        end if

        width = 1
        do i = 1, size(independents)
            width = max(width, len_trim(independents(i)))
        end do
        allocate (character(len=width) :: spec%independents(size(independents)))
        do i = 1, size(independents)
            spec%independents(i) = trim(independents(i))
        end do
        if (present(name)) spec%name = name
        if (present(suffix)) spec%suffix = suffix
        if (present(n_directions)) then
            spec%vector = .true.
            spec%ndir_name = n_directions
        end if

        call differentiate_forward(primal, spec, tangent, fstat)
        if (.not. fstat%ok) then
            res%ok = .false.
            res%message = fstat%message
            return
        end if

        call fold_zero_accumulations(tangent)
        call eliminate_dead_stores(tangent)
        ! The same passes pay off in forward mode: a tangent loop carries
        ! invariant coefficients just as an adjoint one does.
        call optimise(tangent)
        call eliminate_dead_stores(tangent)
        call eliminate_dead_arrays(tangent)

        res%ok = .true.
        if (present(module_name)) then
            res%code = emit_module(tangent, module_name, "fortad "//FORTAD_VERSION)
        else
            res%code = emit_proc(tangent)
        end if
    end function fad_jvp

    function fad_vjp(source, independents, dependent, name, suffix, &
                     module_name, with_primal) result(res)
        !! Reverse mode. Returns a subroutine computing the primal and the
        !! vector-Jacobian product: one sweep yields the gradient with respect
        !! to every independent at once, which is the cheap-gradient principle.
        !!
        !! The generated routine takes the dependent's adjoint as an incoming
        !! seed and returns one adjoint per independent.
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: independents(:)
        !! The dependent to differentiate. Defaults to the function result, or
        !! to the sole `intent(out)` argument.
        character(len=*), intent(in), optional :: dependent
        !! Name of the generated procedure. Defaults to `<primal>_vjp`.
        character(len=*), intent(in), optional :: name
        !! Suffix for adjoint variables. Defaults to `_b`.
        character(len=*), intent(in), optional :: suffix
        !! Wrap the result in a module of this name.
        character(len=*), intent(in), optional :: module_name
        !! Also return the primal value. Default `.true.`.
        !!
        !! Set this `.false.` when only the gradient is wanted. Everything the
        !! primal value alone kept alive is then dead, and is removed - which
        !! for a recurrence whose adjoint needs no primal value is the entire
        !! forward loop.
        logical, intent(in), optional :: with_primal
        type(fad_result_t) :: res
        type(fad_proc_t) :: primal, adjoint
        type(lower_status_t) :: lstat
        type(reverse_status_t) :: rstat
        type(reverse_spec_t) :: spec
        integer :: i, width

        call lower_source(source, primal, lstat)
        if (.not. lstat%ok) then
            res%ok = .false.
            res%message = lstat%message
            return
        end if

        width = 1
        do i = 1, size(independents)
            width = max(width, len_trim(independents(i)))
        end do
        allocate (character(len=width) :: spec%independents(size(independents)))
        do i = 1, size(independents)
            spec%independents(i) = trim(independents(i))
        end do
        if (present(dependent)) spec%dependent = dependent
        if (present(name)) spec%name = name
        if (present(suffix)) spec%suffix = suffix
        if (present(with_primal)) spec%with_primal = with_primal

        call differentiate_reverse(primal, spec, adjoint, rstat)
        if (.not. rstat%ok) then
            res%ok = .false.
            res%message = rstat%message
            return
        end if

        call fold_zero_accumulations(adjoint)
        call eliminate_dead_stores(adjoint)
        call eliminate_dead_loops(adjoint)
        call eliminate_dead_stores(adjoint)
        ! Substitution and factoring leave their inputs behind, so dead-store
        ! elimination runs again after them.
        call optimise(adjoint)
        call eliminate_dead_stores(adjoint)
        call eliminate_dead_arrays(adjoint)

        res%ok = .true.
        if (present(module_name)) then
            res%code = emit_module(adjoint, module_name, "fortad "//FORTAD_VERSION)
        else
            res%code = emit_proc(adjoint)
        end if
    end function fad_vjp

    function fad_hvp(source, independents, dependent, name, module_name) result(res)
        !! Second order: forward mode applied to the generated adjoint.
        !!
        !! This is **forward-over-reverse**, the standard route to a
        !! Hessian-vector product, and fortad obtains it by composing with
        !! itself: it differentiates its own emitted adjoint. Nothing in the
        !! second pass knows it is running on generated code, which is a useful
        !! property - if the emitter ever produced Fortran fortad could not read
        !! back, this would be the first thing to fail.
        !!
        !! Seed the tangents with the direction `v` and the dependent's adjoint
        !! with one; the tangents of the returned adjoints are `H v`.
        !!
        !! Cost is `O(primal)` per product, independent of the number of
        !! inputs, which is what makes Newton-Krylov practical.
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: independents(:)
        !! The dependent. Defaults as for `fad_vjp`.
        character(len=*), intent(in), optional :: dependent
        !! Name of the generated procedure. Defaults to `<primal>_hvp`.
        character(len=*), intent(in), optional :: name
        !! Wrap the result in a module of this name.
        character(len=*), intent(in), optional :: module_name
        type(fad_result_t) :: res
        type(fad_result_t) :: adjoint
        character(len=:), allocatable :: inner_name, outer_name

        inner_name = "fad_inner_vjp"
        if (present(dependent)) then
            adjoint = fad_vjp(source, independents, dependent=dependent, &
                              name=inner_name)
        else
            adjoint = fad_vjp(source, independents, name=inner_name)
        end if
        if (.not. adjoint%ok) then
            res%ok = .false.
            res%message = "reverse pass failed: "//adjoint%message
            return
        end if

        outer_name = "fad_hvp"
        if (present(name)) outer_name = name

        if (present(module_name)) then
            res = fad_jvp(adjoint%code, independents, name=outer_name, &
                          module_name=module_name)
        else
            res = fad_jvp(adjoint%code, independents, name=outer_name)
        end if
        if (.not. res%ok) then
            res%message = "forward pass over the generated adjoint failed: "// &
                          res%message
        end if
    end function fad_hvp

    function fad_taylor(source, independents, order_name, name, module_name) &
        result(res)
        !! Taylor mode: every derivative up to order `d` in one sweep.
        !!
        !! The generated routine takes the order as an argument, so the caller
        !! chooses it at the call site. Each variable becomes a coefficient
        !! array `(0:order)`, and each operation a call into `fortad_taylor`.
        !!
        !! Cost is `O(d^2)` per operation, against the `O(2^d)` of nesting a
        !! first-order tool `d` times. Straight-line scalar kernels only;
        !! anything else is refused by name.
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: independents(:)
        !! Name of the order dummy argument. Defaults to `order`.
        character(len=*), intent(in), optional :: order_name
        !! Name of the generated procedure. Defaults to `<primal>_taylor`.
        character(len=*), intent(in), optional :: name
        !! Wrap the result in a module of this name.
        character(len=*), intent(in), optional :: module_name
        type(fad_result_t) :: res
        type(fad_proc_t) :: primal, taylor
        type(lower_status_t) :: lstat
        type(taylor_status_t) :: tstat
        type(taylor_spec_t) :: spec
        integer :: i, width

        call lower_source(source, primal, lstat)
        if (.not. lstat%ok) then
            res%ok = .false.
            res%message = lstat%message
            return
        end if

        width = 1
        do i = 1, size(independents)
            width = max(width, len_trim(independents(i)))
        end do
        allocate (character(len=width) :: spec%independents(size(independents)))
        do i = 1, size(independents)
            spec%independents(i) = trim(independents(i))
        end do
        if (present(order_name)) spec%order_name = order_name
        if (present(name)) spec%name = name

        call differentiate_taylor(primal, spec, taylor, tstat)
        if (.not. tstat%ok) then
            res%ok = .false.
            res%message = tstat%message
            return
        end if

        res%ok = .true.
        if (present(module_name)) then
            res%code = emit_module(taylor, module_name, "fortad "//FORTAD_VERSION)
        else
            res%code = emit_proc(taylor)
        end if
    end function fad_taylor

    function fad_roundtrip(source) result(res)
        !! Parse and re-emit without differentiating.
        !!
        !! This is the semantics check that gates everything else: if the
        !! round-tripped primal does not compute what the original computed,
        !! no derivative of it can be trusted.
        character(len=*), intent(in) :: source
        type(fad_result_t) :: res
        type(fad_proc_t) :: primal
        type(lower_status_t) :: lstat

        call lower_source(source, primal, lstat)
        if (.not. lstat%ok) then
            res%ok = .false.
            res%message = lstat%message
            return
        end if
        res%ok = .true.
        res%code = emit_proc(primal)
    end function fad_roundtrip

end module fortad
