program test_allocatable_component_move_oracle
    !! Independent numerical and refusal oracle for one concrete component
    !! MOVE_ALLOC lifetime.  The reference compiler performs the descriptor
    !! transfer; finite differences and the VJP dot product check replay.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module component_move_case"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: payload"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    function component_move(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        type(box_t) :: box, moved"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(box%payload)"//nl// &
        "        box%payload = 3.0d0*x"//nl// &
        "        call move_alloc(box%payload, moved%payload)"//nl// &
        "        out = moved%payload + x"//nl// &
        "        deallocate(moved%payload)"//nl// &
        "    end function component_move"//nl// &
        "end module component_move_case"//nl
    character(len=*), parameter :: array_source = &
        "module component_array_move_case"//nl// &
        "    type :: box_array_t"//nl// &
        "        real(8), allocatable :: payload(:)"//nl// &
        "    end type box_array_t"//nl// &
        "contains"//nl// &
        "    function component_array_move(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        type(box_array_t) :: box, moved"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(box%payload(2))"//nl// &
        "        box%payload(1) = 3.0d0*x"//nl// &
        "        box%payload(2) = x*x"//nl// &
        "        call move_alloc(box%payload, moved%payload)"//nl// &
        "        out = moved%payload(1) + 2.0d0*moved%payload(2) + x"//nl// &
        "        deallocate(moved%payload)"//nl// &
        "    end function component_array_move"//nl// &
        "end module component_array_move_case"//nl
    character(len=*), parameter :: polymorphic_source = &
        "module polymorphic_component_move_case"//nl// &
        "    type :: box_t"//nl// &
        "        class(*), allocatable :: payload"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    function polymorphic_move(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        type(box_t) :: box, moved"//nl// &
        "        real(8) :: seed, out"//nl// &
        "        seed = x"//nl// &
        "        allocate(box%payload, source=seed)"//nl// &
        "        call move_alloc(box%payload, moved%payload)"//nl// &
        "        out = x"//nl// &
        "        deallocate(moved%payload)"//nl// &
        "    end function polymorphic_move"//nl// &
        "end module polymorphic_component_move_case"//nl
    character(len=*), parameter :: indexed_source = &
        "module indexed_component_move_case"//nl// &
        "    type :: indexed_box_t"//nl// &
        "        real(8), allocatable :: payload(:,:)"//nl// &
        "    end type indexed_box_t"//nl// &
        "contains"//nl// &
        "    function indexed_move(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        type(indexed_box_t) :: box, moved"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(box%payload(2,2))"//nl// &
        "        box%payload(1,1) = x"//nl// &
        "        box%payload(2,1) = 2.0d0*x"//nl// &
        "        box%payload(1,2) = x*x"//nl// &
        "        box%payload(2,2) = 0.5d0*x"//nl// &
        "        call move_alloc(box%payload, moved%payload)"//nl// &
        "        out = moved%payload(1,1) + 2.0d0*moved%payload(2,1) + &"//nl// &
        "            3.0d0*moved%payload(1,2) + &"//nl// &
        "            4.0d0*moved%payload(2,2) + x"//nl// &
        "        deallocate(moved%payload)"//nl// &
        "    end function indexed_move"//nl// &
        "end module indexed_component_move_case"//nl
    character(len=*), parameter :: higher_rank_source = &
        "module higher_rank_component_move_case"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: payload(:,:,:)"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    function higher_rank_move(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        type(box_t) :: box, moved"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(box%payload(2,2,2))"//nl// &
        "        box%payload(1,1,1) = x"//nl// &
        "        call move_alloc(box%payload, moved%payload)"//nl// &
        "        out = moved%payload(1,1,1)"//nl// &
        "        deallocate(moved%payload)"//nl// &
        "    end function higher_rank_move"//nl// &
        "end module higher_rank_component_move_case"//nl
    character(len=*), parameter :: target_source = &
        "module target_component_move_case"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: payload"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    function target_move(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        type(box_t), target :: box, moved"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(box%payload)"//nl// &
        "        box%payload = x"//nl// &
        "        call move_alloc(box%payload, moved%payload)"//nl// &
        "        out = moved%payload"//nl// &
        "        deallocate(moved%payload)"//nl// &
        "    end function target_move"//nl// &
        "end module target_component_move_case"//nl
    character(len=*), parameter :: dynamic_source = &
        "module dynamic_component_move_case"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: payload(:)"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    function dynamic_move(x, n) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        integer, intent(in) :: n"//nl// &
        "        type(box_t) :: box, moved"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(box%payload(n))"//nl// &
        "        box%payload(1) = x"//nl// &
        "        call move_alloc(box%payload, moved%payload)"//nl// &
        "        out = moved%payload(1)"//nl// &
        "        deallocate(moved%payload)"//nl// &
        "    end function dynamic_move"//nl// &
        "end module dynamic_component_move_case"//nl

    type(fad_result_t) :: jvp, vjp, array_jvp, array_vjp
    type(fad_result_t) :: indexed_jvp, indexed_vjp, refused
    character(len=:), allocatable :: dir, derivatives, driver
    integer :: unit, stat

    jvp = fad_jvp(source, [character(len=1) :: "x"], from="component_move", &
        name="component_move_jvp")
    call require_ok(jvp, "component MOVE_ALLOC JVP")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="out", &
        from="component_move", name="component_move_vjp")
    call require_ok(vjp, "component MOVE_ALLOC VJP")
    array_jvp = fad_jvp(array_source, [character(len=1) :: "x"], &
        from="component_array_move", name="component_array_move_jvp")
    call require_ok(array_jvp, "rank-one component MOVE_ALLOC JVP")
    array_vjp = fad_vjp(array_source, [character(len=1) :: "x"], dependent="out", &
        from="component_array_move", name="component_array_move_vjp")
    call require_ok(array_vjp, "rank-one component MOVE_ALLOC VJP")
    indexed_jvp = fad_jvp(indexed_source, [character(len=1) :: "x"], &
        from="indexed_move", name="indexed_move_jvp")
    call require_ok(indexed_jvp, "rank-two component MOVE_ALLOC JVP")
    indexed_vjp = fad_vjp(indexed_source, [character(len=1) :: "x"], &
        dependent="out", from="indexed_move", name="indexed_move_vjp")
    call require_ok(indexed_vjp, "rank-two component MOVE_ALLOC VJP")

    refused = fad_vjp(polymorphic_source, [character(len=1) :: "x"], &
        dependent="out", from="polymorphic_move")
    call require_refusal(refused, "polymorphic component lifetime", &
        "polymorphic component ownership")
    refused = fad_vjp(higher_rank_source, [character(len=1) :: "x"], &
        dependent="out", from="higher_rank_move")
    call require_refusal(refused, "higher-rank component lifetime", &
        "scalar through rank-two")
    refused = fad_vjp(target_source, [character(len=1) :: "x"], &
        dependent="out", from="target_move")
    call require_refusal(refused, "TARGET component lifetime", "TARGET alias")
    refused = fad_vjp(dynamic_source, [character(len=1) :: "x"], dependent="out", &
        from="dynamic_move")
    call require_refusal(refused, "dynamic component shape", "literal allocation shape")

    dir = "build/oracle/allocatable_component_move"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create component MOVE_ALLOC oracle directory"
    call write_file(dir//"/primal.f90", source//array_source//indexed_source)
    derivatives = "module component_move_derivatives"//nl// &
        "    use component_move_case, only: box_t"//nl// &
        "    use component_array_move_case, only: box_array_t"//nl// &
        "    use indexed_component_move_case, only: indexed_box_t"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code//nl// &
        array_jvp%code//nl//array_vjp%code//nl// &
        indexed_jvp%code//nl//indexed_vjp%code//nl// &
        "end module component_move_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = "program driver"//nl// &
        "    use component_move_case, only: component_move"//nl// &
        "    use component_array_move_case, only: component_array_move"//nl// &
        "    use indexed_component_move_case, only: indexed_move"//nl// &
        "    use component_move_derivatives, only: component_move_jvp, &"//nl// &
        "        component_move_vjp, component_array_move_jvp, &"//nl// &
        "        component_array_move_vjp, indexed_move_jvp, &"//nl// &
        "        indexed_move_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, out, outd, outb, xb, h, fp, fm, fd"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.7d0"//nl// &
        "    call component_move_jvp(x, xd, out, outd)"//nl// &
        "    if (abs(out - 4.0d0*x) > 1.0d-12) error stop 1"//nl// &
        "    if (abs(outd - 4.0d0*xd) > 1.0d-12) error stop 2"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = component_move(x + h*xd)"//nl// &
        "    fm = component_move(x - h*xd)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(outd - fd) > 1.0d-7) error stop 3"//nl// &
        "    outb = -1.3d0"//nl// &
        "    call component_move_vjp(x, out, outb, xb)"//nl// &
        "    if (abs(xb - 4.0d0*outb) > 1.0d-12) error stop 4"//nl// &
        "    if (abs(xb*xd - outb*outd) > 1.0d-12) error stop 5"//nl// &
        "    call component_array_move_jvp(x, xd, out, outd)"//nl// &
        "    if (abs(out - 8.125d0) > 1.0d-12) error stop 6"//nl// &
        "    if (abs(outd + 6.3d0) > 1.0d-12) error stop 7"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = component_array_move(x + h*xd)"//nl// &
        "    fm = component_array_move(x - h*xd)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(outd - fd) > 1.0d-7) error stop 8"//nl// &
        "    outb = -1.3d0"//nl// &
        "    call component_array_move_vjp(x, out, outb, xb)"//nl// &
        "    if (abs(xb + 11.7d0) > 1.0d-12) error stop 9"//nl// &
        "    if (abs(xb*xd - outb*outd) > 1.0d-12) error stop 10"//nl// &
        "    call indexed_move_jvp(x, xd, out, outd)"//nl// &
        "    if (abs(out - 14.6875d0) > 1.0d-12) error stop 11"//nl// &
        "    if (abs(outd + 10.85d0) > 1.0d-12) error stop 12"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = indexed_move(x + h*xd)"//nl// &
        "    fm = indexed_move(x - h*xd)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(outd - fd) > 1.0d-7) error stop 13"//nl// &
        "    outb = -1.3d0"//nl// &
        "    call indexed_move_vjp(x, out, outb, xb)"//nl// &
        "    if (abs(xb + 20.15d0) > 1.0d-12) error stop 14"//nl// &
        "    if (abs(xb*xd - outb*outd) > 1.0d-12) error stop 15"//nl// &
        "    print *, 'allocatable component MOVE_ALLOC oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "component MOVE_ALLOC generated code did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "component MOVE_ALLOC numerical oracle failed"
    end if
    print *, "test_allocatable_component_move_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 10
        end if
    end subroutine require_ok

    subroutine require_refusal(result, label, reason)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label, reason

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, reason) == 0) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 11
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
            print *, "    ", trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_allocatable_component_move_oracle
