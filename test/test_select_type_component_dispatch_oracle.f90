program test_select_type_component_dispatch_oracle
    !! Independent oracle for one fixed SELECT TYPE component dispatch path.
    !! The concrete leaf inherits a named-PASS implementation from an
    !! abstract intermediate type.  The numerical checks are independent of
    !! FortAD: hand derivative, central finite difference, and adjoint dot
    !! product.  Boundary cases must remain named refusals.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=32) :: independent(2)
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    independent = [character(len=32) :: "object%leaf%scale", "amount"]
    jvp = fad_jvp(supported_source(), independent, from="top", &
        name="top_component_jvp")
    vjp = fad_vjp(supported_source(), independent, dependent="output", from="top", &
        name="top_component_vjp")
    call require_ok(jvp, "component JVP")
    call require_ok(vjp, "component VJP")

    dir = "build/oracle/select_type_component_dispatch"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create component dispatch oracle directory"
    call write_file(dir//"/primal.f90", supported_source())
    call write_file(dir//"/derivatives.f90", &
        "module select_type_component_dispatch_derivatives"//nl// &
        "    use select_type_component_dispatch_case, only: dispatch_base_t, "// &
        "component_mid_t, container_t"//nl// &
        "contains"//nl//jvp%code//vjp%code// &
        "end module select_type_component_dispatch_derivatives"//nl)
    driver = &
        "program driver"//nl// &
        "    use select_type_component_dispatch_case, only: container_t, top"//nl// &
        "    use select_type_component_dispatch_derivatives, only: "// &
        "top_component_jvp, top_component_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(container_t) :: object, object_d, object_b, plus, minus"//nl// &
        "    real(8) :: amount, amount_d, amount_b, y, y_d, y_b, h, fp, fm"//nl// &
        "    real(8) :: forward_dot, reverse_dot"//nl// &
        "    object%leaf%scale = 2.0d0"//nl// &
        "    object_d%leaf%scale = -0.7d0"//nl// &
        "    amount = 1.5d0"//nl// &
        "    amount_d = 0.4d0"//nl// &
        "    y_b = 1.3d0"//nl// &
        "    call top_component_jvp(object, object_d, amount, amount_d, y, y_d)"//nl// &
        "    if (abs(y - 7.0d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - (-3.05d0)) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = object; plus%leaf%scale = object%leaf%scale + h*object_d%leaf%scale"//nl// &
        "    minus = object; minus%leaf%scale = object%leaf%scale - h*object_d%leaf%scale"//nl// &
        "    call top(plus, amount + h*amount_d, fp)"//nl// &
        "    call top(minus, amount - h*amount_d, fm)"//nl// &
        "    if (abs(y_d - (fp - fm)/(2.0d0*h)) > 1.0d-7) error stop 4"//nl// &
        "    call top_component_vjp(object, amount, y, y_b, object_b, amount_b)"//nl// &
        "    if (abs(y - 7.0d0) > 1.0d-13) error stop 5"//nl// &
        "    if (abs(object_b%leaf%scale - 7.15d0) > 1.0d-13) error stop 6"//nl// &
        "    if (abs(amount_b - 2.6d0) > 1.0d-13) error stop 7"//nl// &
        "    forward_dot = y_b*y_d"//nl// &
        "    reverse_dot = object_b%leaf%scale*object_d%leaf%scale + amount_b*amount_d"//nl// &
        "    if (abs(forward_dot - reverse_dot) > 1.0d-13) error stop 8"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated component dispatch derivative did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "component dispatch numerical oracle failed"
    end if

    call expect_refusal(generic_source(), "generic component", "generic")
    call expect_refusal(pointer_source(), "pointer component", "pointer")
    call expect_refusal(allocatable_source(), "allocatable component", &
        "ownership")
    call expect_refusal(array_source(), "array component", "array")
    call expect_refusal(alias_source(), "selector alias", "alias")
    call expect_refusal(nested_source(), "nested call", "single direct")
    call expect_refusal(missing_source(), "unresolved binding", "unresolved")
    call expect_refusal(global_source(), "global mutable state", "global selector")
    print *, "test_select_type_component_dispatch_oracle: all cases passed"

contains

    function supported_source() result(source)
        character(len=:), allocatable :: source
        source = &
            "module select_type_component_dispatch_case"//nl// &
            "    implicit none"//nl// &
            "    type, abstract :: component_base_t"//nl// &
            "    contains"//nl// &
            "        procedure(run_iface), deferred, pass(self) :: run"//nl// &
            "    end type component_base_t"//nl// &
            "    type, abstract, extends(component_base_t) :: component_mid_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    contains"//nl// &
            "        procedure, pass(self) :: run => mid_run"//nl// &
            "    end type component_mid_t"//nl// &
            "    type, extends(component_mid_t) :: component_leaf_t"//nl// &
            "    end type component_leaf_t"//nl// &
            "    type, abstract :: dispatch_base_t"//nl// &
            "    end type dispatch_base_t"//nl// &
            "    type, extends(dispatch_base_t) :: container_t"//nl// &
            "        type(component_leaf_t) :: leaf"//nl// &
            "    end type container_t"//nl// &
            "    abstract interface"//nl// &
            "        subroutine run_iface(self, amount, output)"//nl// &
            "            import component_base_t"//nl// &
            "            class(component_base_t), intent(in) :: self"//nl// &
            "            real(8), intent(in) :: amount"//nl// &
            "            real(8), intent(out) :: output"//nl// &
            "        end subroutine run_iface"//nl// &
            "    end interface"//nl// &
            "contains"//nl// &
            "    pure subroutine mid_run(self, amount, output)"//nl// &
            "        class(component_mid_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: amount"//nl// &
            "        real(8), intent(out) :: output"//nl// &
            "        output = self%scale*amount + self%scale*self%scale"//nl// &
            "    end subroutine mid_run"//nl// &
            "    pure subroutine top(object, amount, output)"//nl// &
            "        class(dispatch_base_t), intent(in) :: object"//nl// &
            "        real(8), intent(in) :: amount"//nl// &
            "        real(8), intent(out) :: output"//nl// &
            "        select type (typed => object)"//nl// &
            "        type is (container_t)"//nl// &
            "            call typed%leaf%run(amount, output)"//nl// &
            "        class default"//nl// &
            "            output = amount"//nl// &
            "        end select"//nl// &
            "    end subroutine top"//nl// &
            "end module select_type_component_dispatch_case"//nl
    end function supported_source

    subroutine expect_refusal(source, label, needle)
        character(len=*), intent(in) :: source, label, needle
        type(fad_result_t) :: result

        result = fad_jvp(source, independent, from="top")
        call require_refusal(result, label//" JVP", needle)
        result = fad_vjp(source, independent, dependent="output", from="top")
        call require_refusal(result, label//" VJP", needle)
    end subroutine expect_refusal

    subroutine require_refusal(result, label, needle)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label, needle
        if (result%ok .or. .not. allocated(result%message) .or. &
                index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine require_refusal

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

    function replace_text(base, old, new) result(text)
        character(len=*), intent(in) :: base, old, new
        character(len=:), allocatable :: text
        integer :: position

        text = base
        position = index(text, old)
        if (position > 0) text = text(:position - 1)//new// &
            text(position + len(old):)
    end function replace_text

    function generic_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), &
            "        type(component_leaf_t) :: leaf"//nl, &
            "        type(component_leaf_t) :: leaf"//nl// &
            "        type(generic_t) :: generic"//nl)
        text = replace_text(text, "    type, abstract :: dispatch_base_t"//nl, &
            "    type :: generic_t"//nl// &
            "    contains"//nl// &
            "        generic :: choose => choose_real"//nl// &
            "    end type generic_t"//nl// &
            "    type, abstract :: dispatch_base_t"//nl)
        text = replace_text(text, "call typed%leaf%run(amount, output)", &
            "call typed%generic%choose(amount)")
    end function generic_source

    function pointer_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), &
            "        type(component_leaf_t) :: leaf", &
            "        type(component_leaf_t), pointer :: leaf")
    end function pointer_source

    function allocatable_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), &
            "        type(component_leaf_t) :: leaf", &
            "        type(component_leaf_t), allocatable :: leaf")
    end function allocatable_source

    function array_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), &
            "        type(component_leaf_t) :: leaf", &
            "        type(component_leaf_t) :: leaf(2)")
        text = replace_text(text, "typed%leaf%run(amount, output)", &
            "typed%leaf(1)%run(amount, output)")
    end function array_source

    function alias_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), &
            "        select type (typed => object)"//nl// &
            "        type is (container_t)"//nl// &
            "            call typed%leaf%run(amount, output)"//nl// &
            "        class default"//nl// &
            "            output = amount"//nl// &
            "        end select", &
            "        associate (alias => object)"//nl// &
            "            select type (typed => alias)"//nl// &
            "            type is (container_t)"//nl// &
            "                call typed%leaf%run(amount, output)"//nl// &
            "            class default"//nl// &
            "                output = amount"//nl// &
            "            end select"//nl// &
            "        end associate")
    end function alias_source

    function nested_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), &
            "            call typed%leaf%run(amount, output)", &
            "            if (amount > 0.0d0) call typed%leaf%run(amount, output)")
    end function nested_source

    function missing_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), "typed%leaf%run", &
            "typed%leaf%missing")
    end function missing_source

    function global_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(supported_source(), "    end type container_t"//nl, &
            "    end type container_t"//nl// &
            "    class(dispatch_base_t), pointer, save :: shared_object"//nl)
        text = replace_text(text, "pure subroutine top", "subroutine top")
        text = replace_text(text, "select type (typed => object)", &
            "select type (typed => shared_object)")
    end function global_source

    subroutine write_file(path, contents)
        character(len=*), intent(in) :: path, contents
        integer :: file_unit
        open (newunit=file_unit, file=path, status="replace", action="write")
        write (file_unit, '(a)') contents
        close (file_unit)
    end subroutine write_file

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

end program test_select_type_component_dispatch_oracle
