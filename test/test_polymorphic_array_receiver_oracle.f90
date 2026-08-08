program test_polymorphic_array_receiver_oracle
    !! Independent P8.3e oracle: one active literal-index receiver on a
    !! borrowed, one-dimensional CLASS array with one concrete dispatch arm.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module polymorphic_array_receiver_case"//nl// &
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
        "contains"//nl// &
        "    pure function child_value(self, x) result(y)"//nl// &
        "        class(child_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale*x + self%bias"//nl// &
        "    end function child_value"//nl// &
        "    pure function top(a, x) result(y)"//nl// &
        "        class(base_t), intent(in) :: a(:)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = a(2)%value(x)"//nl// &
        "    end function top"//nl// &
        "end module polymorphic_array_receiver_case"//nl

    character(len=32) :: independents(3)
    type(fad_result_t) :: jvp, vjp, active_vjp
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    independents = [character(len=32) :: "a(2)%scale", "a(2)%bias", "x"]
    jvp = fad_jvp(source, independents, from="top", name="top_jvp")
    call require_ok(jvp, "JVP")
    vjp = fad_vjp(source, ["x"], dependent="y", from="top", &
        name="top_vjp")
    call require_ok(vjp, "VJP")
    active_vjp = fad_vjp(source, [character(len=32) :: "a(2)%scale", &
        "a(2)%bias", "x"], &
        dependent="y", from="top", name="top_active_vjp")
    call require_ok(active_vjp, "active receiver VJP")

    dir = "build/oracle/polymorphic_array_receiver"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create polymorphic receiver oracle directory"
    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module polymorphic_array_receiver_derivatives"
    write (unit, '(a)') "    use polymorphic_array_receiver_case, only: base_t, child_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') active_vjp%code
    write (unit, '(a)') "end module polymorphic_array_receiver_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use polymorphic_array_receiver_case, only: child_t, top"//nl// &
        "    use polymorphic_array_receiver_derivatives, only: top_jvp, top_vjp, "// &
        "top_active_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(child_t) :: a(2), a_d(2), a_b(2), plus(2), minus(2)"//nl// &
        "    real(8) :: x, x_d, y, y_d, y_b, x_b, h, fp, fm, fd"//nl// &
        "    real(8) :: dot_forward, dot_reverse"//nl// &
        "    a = child_t(0.0d0, 0.0d0)"//nl// &
        "    a(2)%scale = 3.0d0"//nl// &
        "    a(2)%bias = 0.5d0"//nl// &
        "    a_d = child_t(0.0d0, 0.0d0)"//nl// &
        "    a_d(2)%scale = 0.7d0"//nl// &
        "    a_d(2)%bias = -0.2d0"//nl// &
        "    x = 2.0d0; x_d = 0.4d0; y_b = 1.3d0"//nl// &
        "    call top_jvp(a, a_d, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 6.5d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - 2.4d0) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = a; plus(2)%scale = a(2)%scale + h*a_d(2)%scale; "// &
        "plus(2)%bias = a(2)%bias + h*a_d(2)%bias"//nl// &
        "    minus = a; minus(2)%scale = a(2)%scale - h*a_d(2)%scale; "// &
        "minus(2)%bias = a(2)%bias - h*a_d(2)%bias"//nl// &
        "    fp = top(plus, x + h*x_d); fm = top(minus, x - h*x_d)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(y_d - fd) > 1.0d-7) error stop 4"//nl// &
        "    call top_vjp(a, x, y, y_b, x_b)"//nl// &
        "    if (abs(x_b - 3.9d0) > 1.0d-13) error stop 5"//nl// &
        "    a_b = child_t(0.0d0, 0.0d0)"//nl// &
        "    call top_active_vjp(a, x, y, y_b, a_b, x_b)"//nl// &
        "    if (abs(a_b(2)%scale - 2.6d0) > 1.0d-13) error stop 7"//nl// &
        "    if (abs(a_b(2)%bias - 1.3d0) > 1.0d-13) error stop 8"//nl// &
        "    if (abs(x_b - 3.9d0) > 1.0d-13) error stop 9"//nl// &
        "    dot_forward = y_b*y_d"//nl// &
        "    dot_reverse = a_b(2)%scale*a_d(2)%scale + "// &
        "a_b(2)%bias*a_d(2)%bias + x_b*x_d"//nl// &
        "    if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 10"//nl// &
        "    print *, 'polymorphic array receiver oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL generated polymorphic receiver did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL polymorphic receiver behavioral oracle"
        call show_file(dir//"/out.txt")
        error stop 1
    end if

    call expect_refusal(dynamic_source(), "dynamic indices", &
        "dynamic receiver indices")
    call expect_refusal(section_source(), "sections", "array sections")
    call expect_refusal(pointer_source(), "pointers", "pointer")
    call expect_refusal(alias_source(), "aliases", "alias")
    call expect_refusal(ownership_source(), "ownership", "ownership-changing")
    call expect_refusal(unresolved_source(), "runtime dispatch", &
        "fixed concrete runtime path")
    call expect_dynamic_type_refusal()
    print *, "test_polymorphic_array_receiver_oracle: all cases passed"

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
        type(fad_result_t) :: result
        result = fad_jvp(case_source, independents, from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
        result = fad_vjp(case_source, independents, dependent="y", from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL active VJP ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine expect_refusal

    subroutine expect_dynamic_type_refusal()
        type(fad_result_t) :: result
        result = fad_jvp(source, [character(len=32) :: "a"], from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, "dynamic type perturbations") == 0) then
            print *, "FAIL dynamic-type active receiver refusal: ", result%message
            error stop 1
        end if
        result = fad_vjp(source, [character(len=32) :: "a"], dependent="y", &
            from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, "dynamic type perturbations") == 0) then
            print *, "FAIL dynamic-type active receiver VJP refusal: ", &
                result%message
            error stop 1
        end if
    end subroutine expect_dynamic_type_refusal

    function replace_text(base, old, new) result(text)
        character(len=*), intent(in) :: base, old, new
        character(len=:), allocatable :: text
        integer :: position
        text = base
        position = index(text, old)
        if (position > 0) text = text(:position - 1)//new//text(position + len(old):)
    end function replace_text

    function dynamic_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "y = a(2)%value(x)", "integer :: i"//nl// &
            "        y = a(i)%value(x)")
        text = replace_text(text, "        class(base_t), intent(in) :: a(:)"//nl, &
            "        class(base_t), intent(in) :: a(:)"//nl// &
            "        integer, intent(in) :: i"//nl)
    end function dynamic_source

    function section_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "a(2)%value(x)", "a(:)%value(x)")
    end function section_source

    function pointer_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "class(base_t), intent(in) :: a(:)", &
            "class(base_t), pointer, intent(in) :: a(:)")
    end function pointer_source

    function ownership_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "class(base_t), intent(in) :: a(:)", &
            "class(base_t), allocatable, intent(in) :: a(:)")
    end function ownership_source

    function alias_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "        y = a(2)%value(x)"//nl, &
            "        associate (alias => a)"//nl// &
            "            y = alias(2)%value(x)"//nl// &
            "        end associate"//nl)
    end function alias_source

    function unresolved_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(source, "    type, extends(base_t) :: child_t", &
            "    type, extends(base_t) :: other_t"//nl// &
            "    contains"//nl// &
            "        procedure :: value => other_value"//nl// &
            "    end type other_t"//nl// &
            "    type, extends(base_t) :: child_t")
        text = replace_text(text, "    pure function child_value(self, x) result(y)", &
            "    pure function other_value(self, x) result(y)"//nl// &
            "        class(other_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = self%scale*x"//nl// &
            "    end function other_value"//nl// &
            "    pure function child_value(self, x) result(y)")
    end function unresolved_source

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

end program test_polymorphic_array_receiver_oracle
