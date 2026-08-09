program test_polymorphic_component_move_oracle
    !! Independent numerical and exact-refusal oracle for one fixed-path
    !! polymorphic allocatable-component MOVE_ALLOC lifetime.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, generated, driver, dir
    type(fad_result_t) :: jvp, vjp, refused
    integer :: stat

    source = supported_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], from="move_component", &
        name="move_component_jvp")
    call require_ok(jvp, "polymorphic component MOVE_ALLOC JVP")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="out", &
        from="move_component", name="move_component_vjp")
    call require_ok(vjp, "polymorphic component MOVE_ALLOC VJP")

    refused = fad_vjp(refused_source(), [character(len=1) :: "x"], &
        dependent="out", from="move_component")
    call require_refusal(refused, "unresolved polymorphic component lifetime", &
        "polymorphic")

    dir = "build/oracle/polymorphic_component_move"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create polymorphic component oracle directory"
    call write_file(dir//"/primal.f90", source)
    generated = "module polymorphic_component_move_generated"//nl// &
        "    use polymorphic_component_move_case, only: base_t, child_t, box_t"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module polymorphic_component_move_generated"//nl
    call write_file(dir//"/generated.f90", generated)
    driver = &
        "program driver"//nl// &
        "    use polymorphic_component_move_case, only: move_component"//nl// &
        "    use polymorphic_component_move_generated, only: "// &
        "move_component_jvp, move_component_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, out, outd, outb, xb, h, fd"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.7d0"//nl// &
        "    call move_component_jvp(x, xd, out, outd)"//nl// &
        "    if (abs(out - 2.0d0*x*x) > 1.0d-12) error stop 1"//nl// &
        "    if (abs(outd - 4.0d0*x*xd) > 1.0d-12) error stop 2"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fd = (move_component(x+h*xd)-move_component(x-h*xd))/(2.0d0*h)"//nl// &
        "    if (abs(outd - fd) > 1.0d-7) error stop 3"//nl// &
        "    outb = -1.3d0"//nl// &
        "    call move_component_vjp(x, out, outb, xb)"//nl// &
        "    if (abs(xb - 4.0d0*x*outb) > 1.0d-12) error stop 4"//nl// &
        "    if (abs(xb*xd - outb*outd) > 1.0d-12) error stop 5"//nl// &
        "    print *, 'polymorphic component MOVE_ALLOC oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/generated.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated polymorphic component derivative did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "polymorphic component numerical oracle failed"
    end if
    print *, "test_polymorphic_component_move_oracle: all cases passed"

contains

    function supported_source() result(text)
        character(len=:), allocatable :: text

        text = "module polymorphic_component_move_case"//nl// &
            "    implicit none"//nl// &
            "    type :: base_t"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    end type child_t"//nl// &
            "    type :: box_t"//nl// &
            "        class(base_t), allocatable :: payload"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure function move_component(x) result(out)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(child_t) :: child"//nl// &
            "        type(box_t) :: box, moved"//nl// &
            "        real(8) :: out"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "        allocate(box%payload, source=child)"//nl// &
            "        call move_alloc(box%payload, moved%payload)"//nl// &
            "        select type (owner => moved%payload)"//nl// &
            "        type is (child_t)"//nl// &
            "            out = owner%scale*x"//nl// &
            "        class default"//nl// &
            "            out = x"//nl// &
            "        end select"//nl// &
            "        deallocate(moved%payload)"//nl// &
            "    end function move_component"//nl// &
            "end module polymorphic_component_move_case"//nl
    end function supported_source

    function refused_source() result(text)
        character(len=:), allocatable :: text

        text = replace_text(supported_source(), &
            "        select type (owner => moved%payload)"//nl// &
            "        type is (child_t)"//nl// &
            "            out = owner%scale*x"//nl// &
            "        class default"//nl// &
            "            out = x"//nl// &
            "        end select"//nl, &
            "        out = x"//nl)
    end function refused_source

    function replace_text(input, old, new) result(output)
        character(len=*), intent(in) :: input, old, new
        character(len=:), allocatable :: output
        integer :: at

        at = index(input, old)
        if (at <= 0) then
            output = input
        else
            output = input(:at - 1)//new//input(at + len(old):)
        end if
    end function replace_text

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
        integer :: unit

        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, '(a)') text
        close (unit)
    end subroutine write_file

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: ios, unit

        open (newunit=unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print '(a)', trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_polymorphic_component_move_oracle
