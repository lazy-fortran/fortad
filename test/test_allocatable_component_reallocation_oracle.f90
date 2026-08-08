program test_allocatable_component_reallocation_oracle
    !! The reference compiler owns whole-component descriptor assignment.  The
    !! generated JVP must therefore reallocate the derived tangent component
    !! in lockstep; reverse mode keeps the explicit replay boundary.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module component_reallocation_case"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: value"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    subroutine kernel(box, x, out)"//nl// &
        "        type(box_t), intent(inout) :: box"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        box%value = 3.0d0*x"//nl// &
        "        out = box%value"//nl// &
        "    end subroutine kernel"//nl// &
        "end module component_reallocation_case"//nl
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, derivatives, driver
    integer :: unit, stat

    jvp = fad_jvp(source, [character(len=12) :: "box%value", "x"], &
        from="kernel", name="kernel_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL allocatable component reallocation JVP: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, [character(len=12) :: "box%value", "x"], &
        dependent="out", from="kernel")
    if (vjp%ok .or. .not. allocated(vjp%message) .or. &
        index(vjp%message, "component lifetime replay") == 0) then
        print *, "FAIL component lifetime reverse boundary: ", vjp%message
        error stop 2
    end if

    dir = "build/oracle/allocatable_component_reallocation"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 3
    call write_file(dir//"/primal.f90", source)
    derivatives = "module component_reallocation_derivatives"//nl// &
        "    use component_reallocation_case, only: box_t"//nl// &
        "contains"//nl//jvp%code//nl// &
        "end module component_reallocation_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = "program driver"//nl// &
        "    use component_reallocation_case, only: box_t, kernel"//nl// &
        "    use component_reallocation_derivatives, only: kernel_jvp"//nl// &
        "    type(box_t) :: box, box_d"//nl// &
        "    real(8) :: x, x_d, out, out_d"//nl// &
        "    x = 1.5d0"//nl// &
        "    x_d = 0.7d0"//nl// &
        "    call kernel_jvp(box, box_d, x, x_d, out, out_d)"//nl// &
        "    if (abs(out - 4.5d0) > 1.0d-13) error stop 4"//nl// &
        "    if (abs(out_d - 2.1d0) > 1.0d-13) error stop 5"//nl// &
        "    if (.not. allocated(box%value)) error stop 6"//nl// &
        "    if (.not. allocated(box_d%value)) error stop 7"//nl// &
        "    print *, 'allocatable component reallocation oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90", &
        exitstat=stat)
    if (stat /= 0) error stop 8
    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop 9
contains
    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, '(a)') text
        close (unit)
    end subroutine write_file
end program test_allocatable_component_reallocation_oracle
