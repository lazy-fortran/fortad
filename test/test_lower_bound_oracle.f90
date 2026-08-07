program test_lower_bound_oracle
    !! Independent directional finite-difference oracle for explicit lower bounds.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "subroutine k(x, y)"//nl// &
        "    real(8), intent(in) :: x(0:2)"//nl// &
        "    real(8), intent(out) :: y"//nl// &
        "    y = x(0)*x(1) + x(2)"//nl// &
        "end subroutine k"//nl
    type(fad_result_t) :: result
    integer :: stat, unit

    result = fad_jvp(source, ["x"], name="k_jvp")
    if (.not. result%ok) then
        print *, "FAIL lower-bound generation: ", result%message
        error stop 1
    end if

    call execute_command_line("mkdir -p build/oracle_lower_bound", exitstat=stat)
    if (stat /= 0) error stop "could not create lower-bound oracle directory"
    open (newunit=unit, file="build/oracle_lower_bound/kernel.f90", &
        status="replace", action="write")
    write (unit, '(a)') "module lower_bound_kernel"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') result%code
    write (unit, '(a)') "end module lower_bound_kernel"
    close (unit)
    open (newunit=unit, file="build/oracle_lower_bound/driver.f90", &
        status="replace", action="write")
    write (unit, '(a)') driver_source()
    close (unit)

    call execute_command_line("cd build/oracle_lower_bound && gfortran -O2 -fcheck=bounds -o run "// &
        "kernel.f90 driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) error stop "lower-bound generated code did not compile"
    call execute_command_line("cd build/oracle_lower_bound && ./run", exitstat=stat)
    if (stat /= 0) error stop "lower-bound finite-difference oracle failed"
    print *, "test_lower_bound_oracle: all cases passed"

contains

    function driver_source() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use lower_bound_kernel, only: k, k_jvp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x(0:2), x_d(0:2), y, y_d, yp, ym, h, fd"//nl// &
            "    x = [1.5d0, -0.4d0, 2.0d0]"//nl// &
            "    x_d = [-0.7d0, 0.3d0, 1.1d0]"//nl// &
            "    call k_jvp(x, x_d, y, y_d)"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call k(x + h*x_d, yp)"//nl// &
            "    call k(x - h*x_d, ym)"//nl// &
            "    fd = (yp - ym)/(2.0d0*h)"//nl// &
            "    if (abs(y_d - fd) > 1.0d-8) error stop 1"//nl// &
            "    print *, 'lower-bound oracle pass'"//nl// &
            "end program driver"//nl
    end function driver_source

end program test_lower_bound_oracle
