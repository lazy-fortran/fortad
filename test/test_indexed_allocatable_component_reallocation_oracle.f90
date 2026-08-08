program test_indexed_allocatable_component_reallocation_oracle
    !! Independent oracle for one fixed element of a concrete derived array.
    !! The scalar allocatable component is assigned once and may be
    !! automatically allocated by the reference compiler.  A literal owner
    !! index is part of the fixed storage path; dynamic indexing is a
    !! deliberate boundary.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module indexed_component_reallocation_case"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: value"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    subroutine kernel(boxes, x, out)"//nl// &
        "        type(box_t), intent(inout) :: boxes(:)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        boxes(2)%value = 3.0d0*x"//nl// &
        "        out = boxes(2)%value"//nl// &
        "    end subroutine kernel"//nl// &
        "end module indexed_component_reallocation_case"//nl
    character(len=*), parameter :: dynamic_source = &
        "module dynamic_indexed_component_case"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: value"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    subroutine kernel(boxes, index, x, out)"//nl// &
        "        type(box_t), intent(inout) :: boxes(:)"//nl// &
        "        integer, intent(in) :: index"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: out"//nl// &
        "        boxes(index)%value = 3.0d0*x"//nl// &
        "        out = boxes(index)%value"//nl// &
        "    end subroutine kernel"//nl// &
        "end module dynamic_indexed_component_case"//nl
    type(fad_result_t) :: jvp, vjp, refused
    character(len=:), allocatable :: dir, derivatives, driver
    integer :: unit, stat

    jvp = fad_jvp(source, [character(len=16) :: "boxes(2)%value", "x"], &
        from="kernel", name="kernel_jvp")
    call require_ok(jvp, "fixed-index JVP")
    vjp = fad_vjp(source, [character(len=16) :: "boxes(2)%value", "x"], &
        dependent="out", from="kernel", name="kernel_vjp")
    call require_ok(vjp, "fixed-index VJP")

    refused = fad_jvp(dynamic_source, [character(len=16) :: "boxes(index)%value", "x"], &
        from="kernel")
    call require_refusal(refused, "dynamic component index")
    refused = fad_vjp(dynamic_source, [character(len=16) :: "boxes(index)%value", "x"], &
        dependent="out", from="kernel")
    call require_refusal(refused, "dynamic component index")

    dir = "build/oracle/indexed_allocatable_component_reallocation"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 1
    call write_file(dir//"/primal.f90", source)
    derivatives = "module indexed_component_reallocation_derivatives"//nl// &
        "    use indexed_component_reallocation_case, only: box_t"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module indexed_component_reallocation_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = "program driver"//nl// &
        "    use indexed_component_reallocation_case, only: box_t, kernel"//nl// &
        "    use indexed_component_reallocation_derivatives, only: kernel_jvp, kernel_vjp"//nl// &
        "    type(box_t) :: boxes(2), boxes_d(2), boxes_b(2), initial(2), plus(2), minus(2)"//nl// &
        "    real(8) :: x, x_d, out, out_d, out_b, x_b"//nl// &
        "    real(8) :: h, fp, fm, fd, dot_forward, dot_reverse"//nl// &
        "    boxes(2)%value = -2.0d0"//nl// &
        "    initial = boxes"//nl// &
        "    x = 1.5d0"//nl// &
        "    x_d = 0.7d0"//nl// &
        "    call kernel_jvp(boxes, boxes_d, x, x_d, out, out_d)"//nl// &
        "    if (abs(out - 4.5d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(out_d - 2.1d0) > 1.0d-13) error stop 3"//nl// &
        "    if (.not. allocated(boxes(2)%value)) error stop 4"//nl// &
        "    if (.not. allocated(boxes_d(2)%value)) error stop 5"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = initial"//nl// &
        "    minus = initial"//nl// &
        "    call kernel(plus, x + h*x_d, fp)"//nl// &
        "    call kernel(minus, x - h*x_d, fm)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(fd - out_d) > 1.0d-8) error stop 6"//nl// &
        "    out_b = 1.7d0"//nl// &
        "    call kernel_vjp(initial, x, out, out_b, boxes_b, x_b)"//nl// &
        "    if (.not. allocated(boxes_b(2)%value)) error stop 7"//nl// &
        "    if (abs(boxes_b(2)%value) > 1.0d-13) error stop 8"//nl// &
        "    if (abs(x_b - 5.1d0) > 1.0d-13) error stop 9"//nl// &
        "    dot_forward = out_b*out_d"//nl// &
        "    dot_reverse = boxes_b(2)%value*0.4d0 + x_b*x_d"//nl// &
        "    if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 10"//nl// &
        "    print *, 'indexed allocatable component reallocation oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop 11
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop 12
    end if
    print *, "test_indexed_allocatable_component_reallocation_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 20
        end if
    end subroutine require_ok

    subroutine require_refusal(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, "static component index") == 0) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 21
        end if
    end subroutine require_refusal

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: file_unit

        open (newunit=file_unit, file=path, status="replace", action="write")
        write (file_unit, '(a)') text
        close (file_unit)
    end subroutine write_file

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: file_unit, ios

        open (newunit=file_unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_indexed_allocatable_component_reallocation_oracle
