module fortad_lower_types
    !! Shared status type for the lowering units.
    implicit none
    private

    public :: lower_status_t

    type :: lower_status_t
        !! Outcome of a lowering attempt.
        logical :: ok = .false.
        character(len=:), allocatable :: message
    end type lower_status_t

end module fortad_lower_types
