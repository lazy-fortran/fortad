program test_local_element_adjoint_oracle
    !! Reverse mode must declare one adjoint array for a local array whose
    !! elements are assigned outside a loop.  The oracle is an independent
    !! central-difference gradient of the original scalar map.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortad, only: fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir
    type(fad_result_t) :: vjp
    integer :: stat, unit

    source = "subroutine k(a, b)"//nl// &
        "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
        "    implicit none"//nl// &
        "    real(dp), intent(in) :: a"//nl// &
        "    real(dp), intent(out) :: b"//nl// &
        "    real(dp) :: x(2)"//nl// &
        "    x(1) = 0.0_dp"//nl// &
        "    x(2) = a"//nl// &
        "    x(1) = x(1) + 2.0_dp*a"//nl// &
        "    b = x(1) + x(2)"//nl// &
        "end subroutine k"//nl

    vjp = fad_vjp(source, ["a"], dependent="b", name="k_vjp", &
        with_primal=.true.)
    if (.not. vjp%ok) then
        print *, "FAIL local element adjoint generation: ", vjp%message
        error stop 1
    end if

    dir = "build/oracle_local_element_adjoint"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create local element oracle directory"
    open (newunit=unit, file=dir//"/gen.f90", status="replace", action="write")
    write (unit, '(a)') "module gen"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module gen"
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line("cd "//dir//" && gfortran -std=f2018 -pedantic-errors "// &
        "-ffree-line-length-none -O2 -o run gen.f90 driver.f90", &
        exitstat=stat)
    if (stat /= 0) error stop "local element reverse code did not compile"
    call execute_command_line("cd "//dir//" && ./run", exitstat=stat)
    if (stat /= 0) error stop "local element reverse oracle failed"
    print *, "test_local_element_adjoint_oracle: all cases passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    use gen, only: k, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(dp) :: a, b, seed, grad, plus, minus, h"//nl// &
            "    a = 0.73_dp"//nl// &
            "    seed = 0.41_dp"//nl// &
            "    h = 1.0e-6_dp"//nl// &
            "    call k_vjp(a, b, seed, grad)"//nl// &
            "    call k(a + h, plus)"//nl// &
            "    call k(a - h, minus)"//nl// &
            "    if (abs(grad - seed*(plus-minus)/(2.0_dp*h)) > 1.0e-7_dp) &"//nl// &
            "        error stop 'local element gradient mismatch'"//nl// &
            "end program driver"//nl
    end function driver_text
end program test_local_element_adjoint_oracle
