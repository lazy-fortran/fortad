module fortad_use_store
    !! Capacity management for rendered USE statements.
    use fortad_ir, only: fad_proc_t
    implicit none
    private

    public :: ensure_use_capacity

contains

    subroutine ensure_use_capacity(proc)
        type(fad_proc_t), intent(inout) :: proc
        character(len=256), allocatable :: grown(:)

        if (.not. allocated(proc%uses)) then
            allocate (character(len=256) :: proc%uses(8))
            proc%n_uses = 0
        else if (proc%n_uses >= size(proc%uses)) then
            allocate (character(len=256) :: grown(2*size(proc%uses)))
            grown(1:proc%n_uses) = proc%uses(1:proc%n_uses)
            call move_alloc(grown, proc%uses)
        end if
    end subroutine ensure_use_capacity

end module fortad_use_store
