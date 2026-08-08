program test_polymorphic_nested_component_oracle
    !! Independent analytic, finite-difference, and adjoint oracle for a
    !! borrowed polymorphic allocatable component in a concrete holder.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module polymorphic_nested_component_case"//nl// &
        "    implicit none"//nl// &
        "    type, abstract :: base_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    contains"//nl// &
        "        procedure(value_iface), deferred :: value"//nl// &
        "    end type base_t"//nl// &
        "    abstract interface"//nl// &
        "        pure function value_iface(self, x) result(y)"//nl// &
        "            import base_t"//nl// &
        "            class(base_t), intent(in) :: self"//nl// &
        "            real(8), intent(in) :: x"//nl// &
        "            real(8) :: y"//nl// &
        "        end function value_iface"//nl// &
        "    end interface"//nl// &
        "    type, extends(base_t) :: child_t"//nl// &
        "        real(8) :: bias"//nl// &
        "    contains"//nl// &
        "        procedure :: value => child_value"//nl// &
        "    end type child_t"//nl// &
        "    type :: holder_t"//nl// &
        "        class(base_t), allocatable :: payload"//nl// &
        "    end type holder_t"//nl// &
        "contains"//nl// &
        "    pure function child_value(self, x) result(y)"//nl// &
        "        class(child_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale*x + self%bias"//nl// &
        "    end function child_value"//nl// &
        "    pure function top(box, x) result(y)"//nl// &
        "        type(holder_t), intent(in) :: box"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        select type (item => box%payload)"//nl// &
        "        type is (child_t)"//nl// &
        "            y = item%scale*x + item%bias"//nl// &
        "        class default"//nl// &
        "            y = x"//nl// &
        "        end select"//nl// &
        "    end function top"//nl// &
        "end module polymorphic_nested_component_case"//nl

    character(len=32) :: independents(3)
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    independents = [character(len=32) :: "box%payload%scale", &
        "box%payload%bias", "x"]
    jvp = fad_jvp(source, independents, from="top", name="top_jvp")
    call require_ok(jvp, "JVP")
    vjp = fad_vjp(source, independents, dependent="y", from="top", &
        name="top_vjp")
    call require_ok(vjp, "VJP")

    dir = "build/oracle/polymorphic_nested_component"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", &
        "module polymorphic_nested_component_derivatives"//nl// &
        "    use polymorphic_nested_component_case, only: holder_t, child_t"//nl// &
        "contains"//nl//jvp%code//vjp%code// &
        "end module polymorphic_nested_component_derivatives"//nl)
    driver = &
        "program driver"//nl// &
        "    use polymorphic_nested_component_case, only: holder_t, child_t, top"//nl// &
        "    use polymorphic_nested_component_derivatives, only: top_jvp, top_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(holder_t) :: box, box_d, box_b, plus, minus"//nl// &
        "    real(8) :: x, x_d, y, y_d, y_b, x_b, h, fp, fm, fd"//nl// &
        "    real(8) :: dot_forward, dot_reverse"//nl// &
        "    allocate(box%payload, source=child_t(3.0d0, 0.5d0))"//nl// &
        "    allocate(box_d%payload, source=child_t(0.7d0, -0.2d0))"//nl// &
        "    allocate(box_b%payload, source=child_t(0.0d0, 0.0d0))"//nl// &
        "    x = 2.0d0; x_d = 0.4d0; y_b = 1.3d0"//nl// &
        "    call top_jvp(box, box_d, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 6.5d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - 2.4d0) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    allocate(plus%payload, source=child_t(3.0d0 + h*0.7d0, &"//nl// &
        "        0.5d0 - h*0.2d0))"//nl// &
        "    allocate(minus%payload, source=child_t(3.0d0 - h*0.7d0, &"//nl// &
        "        0.5d0 + h*0.2d0))"//nl// &
        "    fp = top(plus, x + h*x_d); fm = top(minus, x - h*x_d)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(y_d - fd) > 1.0d-7) error stop 4"//nl// &
        "    call top_vjp(box, x, y, y_b, box_b, x_b)"//nl// &
        "    select type (box_b_item => box_b%payload)"//nl// &
        "    type is (child_t)"//nl// &
        "        select type (box_d_item => box_d%payload)"//nl// &
        "        type is (child_t)"//nl// &
        "            if (abs(box_b_item%scale - 2.6d0) > 1.0d-13) error stop 5"//nl// &
        "            if (abs(box_b_item%bias - 1.3d0) > 1.0d-13) error stop 6"//nl// &
        "            if (abs(x_b - 3.9d0) > 1.0d-13) error stop 7"//nl// &
        "            dot_forward = y_b*y_d"//nl// &
        "            dot_reverse = box_b_item%scale*box_d_item%scale + &"//nl// &
        "                box_b_item%bias*box_d_item%bias + x_b*x_d"//nl// &
        "            if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 8"//nl// &
        "        end select"//nl// &
        "    end select"//nl// &
        "    print *, 'polymorphic nested component oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated nested component derivative did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "nested component behavioral oracle failed"
    end if

    call expect_refusal(replace_text(source, "class(base_t), allocatable :: payload", &
        "class(base_t), pointer :: payload"), "pointer", "pointer or TARGET")
    call expect_refusal(replace_text( &
        replace_text(source, "select type (item => box%payload)", &
        "associate (alias => box%payload)"//nl// &
        "        select type (item => alias)"), &
        "        end select", "        end select"//nl// &
        "        end associate"), "alias", "alias")
    call expect_refusal(multi_dispatch_source(), "unresolved dispatch", &
        "unresolved dispatch")
    call expect_lifetime_refusal()
    call expect_dynamic_refusal()
    print *, "test_polymorphic_nested_component_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

    subroutine expect_refusal(case_source, label, needle)
        character(len=*), intent(in) :: case_source, label, needle
        call expect_refusal_with(case_source, independents, label, needle)
    end subroutine expect_refusal

    subroutine expect_refusal_with(case_source, case_independents, label, needle)
        character(len=*), intent(in) :: case_source, label, needle
        character(len=*), intent(in) :: case_independents(:)
        type(fad_result_t) :: result
        result = fad_jvp(case_source, case_independents, from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), " JVP: ", result%message
            error stop 1
        end if
        result = fad_vjp(case_source, case_independents, dependent="y", from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), " VJP: ", result%message
            error stop 1
        end if
    end subroutine expect_refusal_with

    subroutine expect_dynamic_refusal()
        character(len=64) :: case_independents(2)
        type(fad_result_t) :: result
        logical :: precise
        case_independents = [character(len=64) :: "box%payload(1)%scale", "k"]
        result = fad_jvp(dynamic_source(), case_independents, from="top")
        precise = allocated(result%message)
        if (precise) precise = index(result%message, "unsupported array section") > 0 .or. &
            index(result%message, "dynamic bounds or indexing") > 0
        if (result%ok .or. .not. precise) then
            print *, "FAIL dynamic bounds JVP: ", result%message
            error stop 1
        end if
        result = fad_vjp(dynamic_source(), case_independents, dependent="y", &
            from="top")
        precise = allocated(result%message)
        if (precise) precise = index(result%message, "unsupported array section") > 0 .or. &
            index(result%message, "dynamic bounds or indexing") > 0
        if (result%ok .or. .not. precise) then
            print *, "FAIL dynamic bounds VJP: ", result%message
            error stop 1
        end if
    end subroutine expect_dynamic_refusal

    subroutine expect_lifetime_refusal()
        type(fad_result_t) :: result
        result = fad_jvp(lifetime_source(), independents, from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, "ownership/lifetime") == 0) then
            print *, "FAIL ownership/lifetime JVP: ", result%message
            error stop 1
        end if
        result = fad_vjp(lifetime_source(), independents, dependent="y", &
            from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, "nested polymorphic component") == 0 .or. &
            index(result%message, "SOURCE= ownership") == 0) then
            print *, "FAIL ownership/lifetime VJP: ", result%message
            error stop 1
        end if
    end subroutine expect_lifetime_refusal

    function multi_dispatch_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "    type, extends(base_t) :: child_t"//nl, &
            "    type, extends(base_t) :: other_t"//nl// &
            "        real(8) :: bias"//nl// &
            "    contains"//nl// &
            "        procedure :: value => other_value"//nl// &
            "    end type other_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl)
        text = replace_text(text, "    pure function child_value(self, x) result(y)", &
            "    pure function other_value(self, x) result(y)"//nl// &
            "        class(other_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = self%scale*x - self%bias"//nl// &
            "    end function other_value"//nl// &
            "    pure function child_value(self, x) result(y)")
        text = replace_text(text, "        class default"//nl// &
            "            y = x", "        type is (other_t)"//nl// &
            "            y = item%scale*x - item%bias"//nl// &
            "        class default"//nl// &
            "            y = x")
    end function multi_dispatch_source

    function lifetime_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "type(holder_t), intent(in) :: box", &
            "type(holder_t), intent(inout) :: box")
        text = replace_text(text, "        select type (item => box%payload)", &
            "        type(child_t) :: seed"//nl// &
            "        seed = child_t(2.0d0*x, 0.5d0)"//nl// &
            "        allocate(box%payload, source=seed)"//nl// &
            "        select type (item => box%payload)")
        text = replace_text(text, "        end select"//nl// &
            "    end function top", "        end select"//nl// &
            "        deallocate(box%payload)"//nl// &
            "    end function top")
    end function lifetime_source

    function dynamic_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "class(base_t), allocatable :: payload", &
            "class(base_t), allocatable :: payload(:)")
        text = replace_text(text, "pure function top(box, x) result(y)", &
            "pure function top(box, x, k) result(y)")
        text = replace_text(text, "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y", "        real(8), intent(in) :: x"//nl// &
            "        integer, intent(in) :: k"//nl// &
            "        real(8) :: y")
        text = replace_text(text, "select type (item => box%payload)", &
            "select type (item => box%payload(k:2))")
        text = replace_text(text, "y = item%scale*x + item%bias", &
            "y = item(1)%scale*x + item(1)%bias")
    end function dynamic_source

    function replace_text(base, old, new) result(text)
        character(len=*), intent(in) :: base, old, new
        character(len=:), allocatable :: text
        integer :: position
        text = base
        position = index(text, old)
        if (position > 0) text = text(:position - 1)//new// &
            text(position + len(old):)
    end function replace_text

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
        integer :: ios, file_unit
        open (newunit=file_unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_polymorphic_nested_component_oracle
