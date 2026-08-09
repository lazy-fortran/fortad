program test_active_allocatable_component_oracle
    !! Independent behavioral gate for an already allocated REAL component.
    !! The differentiated procedure neither allocates nor deallocates the
    !! component: its caller owns the descriptor and supplies an allocated
    !! tangent/adjoint shadow with the same concrete type.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module active_allocatable_component_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: values(:)"//nl// &
        "        real(8) :: bias"//nl// &
        "        integer :: tag"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure subroutine kernel(box, scale, out)"//nl// &
        "        type(box_t), intent(inout) :: box"//nl// &
        "        real(8), intent(in) :: scale"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        box%values(1) = box%values(1)*scale + box%bias"//nl// &
        "        out = box%values(1)*box%values(2) + scale*box%values(1)"//nl// &
        "    end subroutine kernel"//nl// &
        "end module active_allocatable_component_case"//nl
    character(len=*), parameter :: whole_source = &
        "module whole_allocatable_component_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: values(:)"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure subroutine whole_read(box, out)"//nl// &
        "        type(box_t), intent(in) :: box"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        out = sum(box%values)"//nl// &
        "    end subroutine whole_read"//nl// &
        "    pure subroutine whole_write(box, value)"//nl// &
        "        type(box_t), intent(inout) :: box"//nl// &
        "        real(8), intent(in) :: value(:)"//nl// &
        "        box%values = value"//nl// &
        "    end subroutine whole_write"//nl// &
        "end module whole_allocatable_component_case"//nl

    type(fad_result_t) :: jvp, vjp, refusal
    character(len=32) :: independent_paths(3)
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    independent_paths = [character(len=32) :: "box%values(1)", "box%values(2)", &
        "scale"]
    jvp = fad_jvp(source, independent_paths, from="kernel", name="kernel_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL active allocatable component JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, independent_paths, dependent="out", from="kernel", &
        name="kernel_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL active allocatable component VJP generation: ", vjp%message
        error stop 1
    end if

    dir = "build/oracle/active_allocatable_component"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create active allocatable oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", action="write")
    write (unit, '(a)') "module active_allocatable_component_derivatives"
    write (unit, '(a)') "    use active_allocatable_component_case, only: box_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module active_allocatable_component_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use active_allocatable_component_case, only: box_t, kernel"//nl// &
        "    use active_allocatable_component_derivatives, only: kernel_jvp, kernel_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(box_t) :: box, initial, box_d, box_b, plus, minus"//nl// &
        "    real(8) :: scale, scale_d, out, out_d, out_b, scale_b"//nl// &
        "    real(8) :: h, fp, fm, fd, dot_forward, dot_reverse"//nl// &
        "    allocate(box%values(2), initial%values(2), box_d%values(2), "// &
        "box_b%values(2))"//nl// &
        "    box%values = [2.0d0, 3.0d0]"//nl// &
        "    box%bias = 0.5d0"//nl// &
        "    box%tag = 17"//nl// &
        "    initial = box"//nl// &
        "    box_d%values = [0.4d0, 0.2d0]"//nl// &
        "    box_d%bias = 99.0d0"//nl// &
        "    box_d%tag = 0"//nl// &
        "    scale = 1.5d0"//nl// &
        "    scale_d = -0.3d0"//nl// &
        "    call kernel_jvp(box, box_d, scale, scale_d, out, out_d)"//nl// &
        "    if (abs(out - 15.75d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(out_d + 0.35d0) > 1.0d-13) error stop 3"//nl// &
        "    box_d%values = [0.4d0, 0.2d0]"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = initial"//nl// &
        "    plus%values(1) = plus%values(1) + h*box_d%values(1)"//nl// &
        "    plus%values(2) = plus%values(2) + h*box_d%values(2)"//nl// &
        "    minus = initial"//nl// &
        "    minus%values(1) = minus%values(1) - h*box_d%values(1)"//nl// &
        "    minus%values(2) = minus%values(2) - h*box_d%values(2)"//nl// &
        "    call kernel(plus, scale + h*scale_d, fp)"//nl// &
        "    call kernel(minus, scale - h*scale_d, fm)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(out_d - fd) > 1.0d-6) then"//nl// &
        "        print *, 'JVP central mismatch', out_d, fd, fp, fm"//nl// &
        "        error stop 4"//nl// &
        "    end if"//nl// &
        "    out_b = 1.7d0"//nl// &
        "    call kernel_vjp(initial, scale, out, out_b, box_b, scale_b)"//nl// &
        "    if (abs(box_b%values(1) - 11.475d0) > 1.0d-13) then"//nl// &
        "        print *, 'VJP value1 mismatch', box_b%values(1)"//nl// &
        "        error stop 5"//nl// &
        "    end if"//nl// &
        "    if (abs(box_b%values(2) - 5.95d0) > 1.0d-13) error stop 6"//nl// &
        "    if (abs(scale_b - 21.25d0) > 1.0d-13) error stop 7"//nl// &
        "    dot_forward = out_b*out_d"//nl// &
        "    dot_reverse = box_b%values(1)*box_d%values(1) + "// &
        "box_b%values(2)*box_d%values(2) + scale_b*scale_d"//nl// &
        "    if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 8"//nl// &
        "    print *, 'active allocatable component oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL active allocatable component: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL active allocatable component: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if

    refusal = fad_jvp(whole_source, [character(len=32) :: "box%values(1)"], &
        from="whole_read")
    if (refusal%ok .or. .not. allocated(refusal%message) .or. &
        index(refusal%message, "whole allocatable component") == 0) then
        print *, "FAIL whole allocatable component read was not refused: ", &
            refusal%message
        error stop 1
    end if
    refusal = fad_jvp(whole_source, [character(len=32) :: "value(1)"], &
        from="whole_write")
    if (refusal%ok .or. .not. allocated(refusal%message) .or. &
        index(refusal%message, "whole allocatable component") == 0) then
        print *, "FAIL whole allocatable component assignment was not refused: ", &
            refusal%message
        error stop 1
    end if
    print *, "test_active_allocatable_component_oracle: all cases passed"

contains

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: ios, file_unit

        open (newunit=file_unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_active_allocatable_component_oracle
