program test_allocatable_component_reallocation_oracle
    !! The reference compiler owns whole-component descriptor assignment.  The
    !! generated JVP and VJP must therefore replay the derived component
    !! descriptor transition in lockstep.  The scalar concrete case has no
    !! alias, pointer, polymorphic, or repeated-lifetime path.
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
    if (.not. vjp%ok) then
        print *, "FAIL allocatable component reallocation VJP: ", vjp%message
        error stop 2
    end if

    dir = "build/oracle/allocatable_component_reallocation"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 3
    call write_file(dir//"/primal.f90", source)
    derivatives = "module component_reallocation_derivatives"//nl// &
        "    use component_reallocation_case, only: box_t"//nl// &
        "contains"//nl//jvp%code//nl// &
        vjp%code//nl// &
        "end module component_reallocation_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = "program driver"//nl// &
        "    use component_reallocation_case, only: box_t, kernel"//nl// &
        "    use component_reallocation_derivatives, only: kernel_jvp, kernel_vjp"//nl// &
        "    type(box_t) :: box, box_d, box_b, initial, plus, minus"//nl// &
        "    real(8) :: x, x_d, out, out_d, out_b, x_b"//nl// &
        "    real(8) :: h, fp, fm, fd, dot_forward, dot_reverse"//nl// &
        "    allocate(box%value, initial%value, plus%value, minus%value)"//nl// &
        "    box%value = -2.0d0"//nl// &
        "    initial = box"//nl// &
        "    x = 1.5d0"//nl// &
        "    x_d = 0.7d0"//nl// &
        "    call kernel_jvp(box, box_d, x, x_d, out, out_d)"//nl// &
        "    if (abs(out - 4.5d0) > 1.0d-13) error stop 4"//nl// &
        "    if (abs(out_d - 2.1d0) > 1.0d-13) error stop 5"//nl// &
        "    if (.not. allocated(box%value)) error stop 6"//nl// &
        "    if (.not. allocated(box_d%value)) error stop 7"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = initial"//nl// &
        "    minus = initial"//nl// &
        "    call kernel(plus, x + h*x_d, fp)"//nl// &
        "    call kernel(minus, x - h*x_d, fm)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(fd - out_d) > 1.0d-8) error stop 8"//nl// &
        "    out_b = 1.7d0"//nl// &
        "    call kernel_vjp(initial, x, out, out_b, box_b, x_b)"//nl// &
        "    if (.not. allocated(box_b%value)) error stop 9"//nl// &
        "    if (abs(box_b%value) > 1.0d-13) error stop 10"//nl// &
        "    if (abs(x_b - 5.1d0) > 1.0d-13) error stop 11"//nl// &
        "    dot_forward = out_b*out_d"//nl// &
        "    dot_reverse = box_b%value*0.4d0 + x_b*x_d"//nl// &
        "    if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 12"//nl// &
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
