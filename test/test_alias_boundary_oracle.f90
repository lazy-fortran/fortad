program test_alias_boundary_oracle
    !! Executable P7.3 boundary for storage aliases and array sections.
    !!
    !! These are deliberate refusal cases: the IR tracks values and names, not
    !! storage locations. The test is independent of the implementation's
    !! internal declarations because it checks the public result and diagnostic
    !! returned by both derivative modes.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0
    call check_case("target declaration JVP", target_source(), &
        "TARGET alias storage identity", .false., failures)
    call check_case("target declaration VJP", target_source(), &
        "TARGET alias storage identity", .true., failures)
    call check_case("pointer declaration JVP", pointer_source(), &
        "pointer association storage identity", .false., failures)
    call check_case("pointer declaration VJP", pointer_source(), &
        "pointer association storage identity", .true., failures)
    call check_case("strided section JVP", section_source(), &
        "unsupported array section", .false., failures)
    call check_case("strided section VJP", section_source(), &
        "unsupported array section", .true., failures)

    if (failures /= 0) then
        print *, "test_alias_boundary_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if
    print *, "test_alias_boundary_oracle: all cases passed"

contains

    subroutine check_case(label, source, needle, reverse, failures)
        character(len=*), intent(in) :: label, source, needle
        logical, intent(in) :: reverse
        integer, intent(inout) :: failures
        type(fad_result_t) :: result

        if (reverse) then
            result = fad_vjp(source, ["x"], dependent="y")
        else
            result = fad_jvp(source, ["x"])
        end if
        if (result%ok) then
            print *, "FAIL ", trim(label), ": unsupported alias was accepted"
            failures = failures + 1
        else if (.not. allocated(result%message)) then
            print *, "FAIL ", trim(label), ": diagnostic was empty"
            failures = failures + 1
        else if (index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), ": unexpected diagnostic: ", &
                trim(result%message)
            failures = failures + 1
        else
            print *, "pass ", trim(label)
        end if
    end subroutine check_case

    function target_source() result(source)
        character(len=:), allocatable :: source

        source = "subroutine k(x, y)"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    implicit none"//nl// &
            "    real(dp), intent(in), target :: x"//nl// &
            "    real(dp), intent(out) :: y"//nl// &
            "    y = x*x"//nl// &
            "end subroutine k"//nl
    end function target_source

    function pointer_source() result(source)
        character(len=:), allocatable :: source

        source = "subroutine k(x, y)"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    implicit none"//nl// &
            "    real(dp), intent(in) :: x"//nl// &
            "    real(dp), pointer :: p"//nl// &
            "    real(dp), intent(out) :: y"//nl// &
            "    y = x*x"//nl// &
            "end subroutine k"//nl
    end function pointer_source

    function section_source() result(source)
        character(len=:), allocatable :: source

        source = "subroutine k(x, y)"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    implicit none"//nl// &
            "    real(dp), intent(in) :: x(:)"//nl// &
            "    real(dp), intent(out) :: y"//nl// &
            "    y = sum(x(1:size(x):2))"//nl// &
            "end subroutine k"//nl
    end function section_source

end program test_alias_boundary_oracle
