program test_global_state_oracle
    !! Independent refusal oracle for active mutable module state.
    !!
    !! The state is intentionally reachable from the differentiated procedure.
    !! Both public derivative modes must reject it by name instead of treating
    !! the module variable as an implicit constant.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module global_state_case"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: factor = 2.0d0"//nl// &
        "contains"//nl// &
        "    pure subroutine scale(x, y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: y"//nl// &
        "        y = factor * x"//nl// &
        "    end subroutine scale"//nl// &
        "end module global_state_case"//nl

    type(fad_result_t) :: jvp, vjp
    integer :: failures

    failures = 0
    jvp = fad_jvp(source, ["x"], from="scale")
    call check_refusal(jvp, "JVP", failures)
    vjp = fad_vjp(source, ["x"], dependent="y", from="scale")
    call check_refusal(vjp, "VJP", failures)
    if (failures /= 0) error stop 1
    print *, "test_global_state_oracle: all cases passed"

contains

    subroutine check_refusal(result, label, failures)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        integer, intent(inout) :: failures

        if (result%ok) then
            print *, "FAIL ", trim(label), ": active global state was accepted"
            failures = failures + 1
        else if (.not. allocated(result%message)) then
            print *, "FAIL ", trim(label), ": refusal has no diagnostic"
            failures = failures + 1
        else if (index(result%message, "factor") == 0 .or. &
                index(result%message, "global mutable state") == 0) then
            print *, "FAIL ", trim(label), ": unexpected diagnostic: ", &
                trim(result%message)
            failures = failures + 1
        else
            print *, "pass ", trim(label), ": ", trim(result%message)
        end if
    end subroutine check_refusal

end program test_global_state_oracle
