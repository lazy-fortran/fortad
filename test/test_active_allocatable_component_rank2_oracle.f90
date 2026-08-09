program test_active_allocatable_component_rank2_oracle
    !! Independent behavioral gate for scalar accesses to an allocated
    !! concrete REAL rank-two allocatable component.
    !!
    !! The hand derivative, central finite difference, and reverse adjoint
    !! identity are all checked after compiling and running the generated
    !! routines.  The same gate also checks the product boundary's rank and
    !! whole-component refusals.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module active_allocatable_component_rank2_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: values(:,:)"//nl// &
        "        real(8) :: bias"//nl// &
        "        integer :: tag"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure subroutine kernel(box, scale, out)"//nl// &
        "        type(box_t), intent(inout) :: box"//nl// &
        "        real(8), intent(in) :: scale"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        box%values(1,2) = box%values(1,2)*scale + box%bias"//nl// &
        "        out = box%values(1,2)*box%values(2,1) + "// &
        "            scale*box%values(2,2)"//nl// &
        "    end subroutine kernel"//nl// &
        "end module active_allocatable_component_rank2_case"//nl
    character(len=*), parameter :: whole_source = &
        "module whole_allocatable_component_rank2_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: values(:,:)"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure subroutine whole_read(box, out)"//nl// &
        "        type(box_t), intent(in) :: box"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        out = sum(box%values)"//nl// &
        "    end subroutine whole_read"//nl// &
        "end module whole_allocatable_component_rank2_case"//nl
    character(len=*), parameter :: rank5_source = &
        "module rank5_allocatable_component_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: values(:,:,:,:,:)"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure subroutine kernel(box, out)"//nl// &
        "        type(box_t), intent(in) :: box"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        out = box%values(1,1,1,1,1)"//nl// &
        "    end subroutine kernel"//nl// &
        "end module rank5_allocatable_component_case"//nl
    character(len=*), parameter :: integer_source = &
        "module integer_allocatable_component_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        integer, allocatable :: values(:,:)"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure subroutine kernel(box, out)"//nl// &
        "        type(box_t), intent(in) :: box"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        out = real(box%values(1,1), 8)"//nl// &
        "    end subroutine kernel"//nl// &
        "end module integer_allocatable_component_case"//nl

    type(fad_result_t) :: jvp, vjp, refused
    character(len=32) :: independent_paths(4)
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    independent_paths = [character(len=32) :: "box%values(1,2)", &
        "box%values(2,1)", "box%values(2,2)", "scale"]
    jvp = fad_jvp(source, independent_paths, from="kernel", name="kernel_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL rank-two allocatable component JVP generation: ", &
            jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, independent_paths, dependent="out", from="kernel", &
        name="kernel_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL rank-two allocatable component VJP generation: ", &
            vjp%message
        error stop 1
    end if

    dir = "build/oracle/active_allocatable_component_rank2"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create rank-two oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module active_allocatable_component_rank2_derivatives"
    write (unit, '(a)') "    use active_allocatable_component_rank2_case, only: box_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module active_allocatable_component_rank2_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use active_allocatable_component_rank2_case, only: box_t, kernel"//nl// &
        "    use active_allocatable_component_rank2_derivatives, only: "// &
        "kernel_jvp, kernel_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(box_t) :: box, initial, box_d, box_b, plus, minus"//nl// &
        "    real(8) :: scale, scale_d, out, out_d, out_b, scale_b"//nl// &
        "    real(8) :: h, fp, fm, fd, dot_forward, dot_reverse"//nl// &
        "    allocate(box%values(2,2), initial%values(2,2), "// &
        "box_d%values(2,2), box_b%values(2,2))"//nl// &
        "    box%values = 0.0d0"//nl// &
        "    box%values(1,2) = 2.0d0"//nl// &
        "    box%values(2,1) = 3.0d0"//nl// &
        "    box%values(2,2) = 4.0d0"//nl// &
        "    box%bias = 0.5d0"//nl// &
        "    box%tag = 17"//nl// &
        "    initial = box"//nl// &
        "    box_d%values = 0.0d0"//nl// &
        "    box_d%values(1,2) = 0.4d0"//nl// &
        "    box_d%values(2,1) = 0.2d0"//nl// &
        "    box_d%values(2,2) = 0.1d0"//nl// &
        "    box_d%bias = 99.0d0"//nl// &
        "    box_d%tag = 0"//nl// &
        "    scale = 1.5d0"//nl// &
        "    scale_d = -0.3d0"//nl// &
        "    call kernel_jvp(box, box_d, scale, scale_d, out, out_d)"//nl// &
        "    if (abs(out - 16.5d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(out_d + 0.35d0) > 1.0d-13) error stop 3"//nl// &
        "    box_d%values(1,2) = 0.4d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = initial"//nl// &
        "    plus%values(1,2) = plus%values(1,2) + h*box_d%values(1,2)"//nl// &
        "    plus%values(2,1) = plus%values(2,1) + h*box_d%values(2,1)"//nl// &
        "    plus%values(2,2) = plus%values(2,2) + h*box_d%values(2,2)"//nl// &
        "    minus = initial"//nl// &
        "    minus%values(1,2) = minus%values(1,2) - h*box_d%values(1,2)"//nl// &
        "    minus%values(2,1) = minus%values(2,1) - h*box_d%values(2,1)"//nl// &
        "    minus%values(2,2) = minus%values(2,2) - h*box_d%values(2,2)"//nl// &
        "    call kernel(plus, scale + h*scale_d, fp)"//nl// &
        "    call kernel(minus, scale - h*scale_d, fm)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(out_d - fd) > 1.0d-6) then"//nl// &
        "        print *, 'JVP central mismatch', out_d, fd"//nl// &
        "        error stop 4"//nl// &
        "    end if"//nl// &
        "    out_b = 1.7d0"//nl// &
        "    call kernel_vjp(initial, scale, out, out_b, box_b, scale_b)"//nl// &
        "    if (abs(box_b%values(1,2) - 7.65d0) > 1.0d-13) error stop 5"//nl// &
        "    if (abs(box_b%values(2,1) - 5.95d0) > 1.0d-13) error stop 6"//nl// &
        "    if (abs(box_b%values(2,2) - 2.55d0) > 1.0d-13) error stop 7"//nl// &
        "    if (abs(scale_b - 17.0d0) > 1.0d-13) error stop 8"//nl// &
        "    dot_forward = out_b*out_d"//nl// &
        "    dot_reverse = box_b%values(1,2)*box_d%values(1,2) + &"//nl// &
        "        box_b%values(2,1)*box_d%values(2,1) + &"//nl// &
        "        box_b%values(2,2)*box_d%values(2,2) + scale_b*scale_d"//nl// &
        "    if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 9"//nl// &
        "    print *, 'active rank-two allocatable component oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL rank-two allocatable component: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL rank-two allocatable component: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if

    refused = fad_jvp(whole_source, [character(len=32) :: "box%values(1,1)"], &
        from="whole_read")
    call require_refusal(refused, "whole allocatable component")
    refused = fad_jvp(rank5_source, [character(len=32) :: "box%values(1,1,1,1,1)"], &
        from="kernel")
    call require_refusal(refused, "rank greater than four")
    refused = fad_jvp(integer_source, [character(len=32) :: "box%values(1,1)"], &
        from="kernel")
    call require_refusal(refused, "only concrete REAL allocatable components")
    print *, "test_active_allocatable_component_rank2_oracle: all cases passed"

contains

    subroutine require_refusal(result, needle)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: needle

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL expected precise refusal containing: ", trim(needle)
            if (allocated(result%message)) print *, trim(result%message)
            error stop 1
        end if
    end subroutine require_refusal

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

end program test_active_allocatable_component_rank2_oracle
