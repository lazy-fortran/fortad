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
    use fortad_emit, only: emit_proc
    implicit none
    private

    public :: fad_jvp, fad_roundtrip, fad_result_t, fad_version

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

    function fad_jvp(source, independents, name, suffix) result(res)
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

        call differentiate_forward(primal, spec, tangent, fstat)
        if (.not. fstat%ok) then
            res%ok = .false.
            res%message = fstat%message
            return
        end if

        res%ok = .true.
        res%code = emit_proc(tangent)
    end function fad_jvp

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
