program test_abstract_hierarchy_oracle
    !! Independent oracle for a bounded abstract/deferred hierarchy slice.
    !! A statically declared concrete child may override a deferred binding
    !! through one intermediate level.  The derivative follows that fixed
    !! override path.  Direct CLASS dispatch is covered by the separate
    !! two-child runtime oracle; an unresolved deferred call remains refused.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module abstract_hierarchy_case"//nl// &
        "    implicit none"//nl// &
        "    type, abstract :: base_t"//nl// &
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
        "    type, extends(base_t) :: mid_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    contains"//nl// &
        "        procedure :: value => mid_value"//nl// &
        "    end type mid_t"//nl// &
        "    type, extends(mid_t) :: leaf_t"//nl// &
        "        real(8) :: bias"//nl// &
        "    contains"//nl// &
        "        procedure :: value => leaf_value"//nl// &
        "    end type leaf_t"//nl// &
        "contains"//nl// &
        "    pure function mid_value(self, x) result(y)"//nl// &
        "        class(mid_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale * x"//nl// &
        "    end function mid_value"//nl// &
        "    pure function leaf_value(self, x) result(y)"//nl// &
        "        class(leaf_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale * x + self%bias"//nl// &
        "    end function leaf_value"//nl// &
        "    pure function evaluate_mid(model, x) result(y)"//nl// &
        "        type(mid_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = model%value(x)"//nl// &
        "    end function evaluate_mid"//nl// &
        "    pure function evaluate_leaf(model, x) result(y)"//nl// &
        "        type(leaf_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = model%value(x)"//nl// &
        "    end function evaluate_leaf"//nl// &
        "end module abstract_hierarchy_case"//nl

    type(fad_result_t) :: mid_jvp, mid_vjp, leaf_jvp, leaf_vjp
    character(len=1) :: indep(1)
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    indep = "x"
    mid_jvp = fad_jvp(source, indep, from="evaluate_mid", &
        name="evaluate_mid_jvp")
    mid_vjp = fad_vjp(source, indep, dependent="y", from="evaluate_mid", &
        name="evaluate_mid_vjp")
    leaf_jvp = fad_jvp(source, indep, from="evaluate_leaf", &
        name="evaluate_leaf_jvp")
    leaf_vjp = fad_vjp(source, indep, dependent="y", from="evaluate_leaf", &
        name="evaluate_leaf_vjp")
    call require_ok(mid_jvp, "mid JVP")
    call require_ok(mid_vjp, "mid VJP")
    call require_ok(leaf_jvp, "leaf JVP")
    call require_ok(leaf_vjp, "leaf VJP")

    call expect_supported(runtime_source(), "runtime CLASS dispatch")
    call expect_refusal(deferred_source(), "deferred binding", "deferred")

    dir = "build/oracle/abstract_hierarchy"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module abstract_hierarchy_derivatives"
    write (unit, '(a)') "    use abstract_hierarchy_case, only: mid_t, leaf_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') mid_jvp%code
    write (unit, '(a)') mid_vjp%code
    write (unit, '(a)') leaf_jvp%code
    write (unit, '(a)') leaf_vjp%code
    write (unit, '(a)') "end module abstract_hierarchy_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use abstract_hierarchy_case, only: mid_t, leaf_t, "// &
        "evaluate_mid, evaluate_leaf"//nl// &
        "    use abstract_hierarchy_derivatives, only: "// &
        "evaluate_mid_jvp, evaluate_mid_vjp, evaluate_leaf_jvp, "// &
        "evaluate_leaf_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(mid_t) :: mid"//nl// &
        "    type(leaf_t) :: leaf"//nl// &
        "    real(8) :: x, x_d, y, y_d, x_b, y_b, h, fp, fm"//nl// &
        "    mid%scale = 2.0d0"//nl// &
        "    leaf%scale = 2.0d0"//nl// &
        "    leaf%bias = 3.0d0"//nl// &
        "    x = 1.5d0"//nl// &
        "    x_d = -0.75d0"//nl// &
        "    y_b = 1.3d0"//nl// &
        "    call check_mid()"//nl// &
        "    call check_leaf()"//nl// &
        "contains"//nl// &
        "    subroutine check_mid()"//nl// &
        "        call evaluate_mid_jvp(mid, x, x_d, y, y_d)"//nl// &
        "        if (abs(y - 3.0d0) > 1.0d-13) error stop 2"//nl// &
        "        if (abs(y_d - 2.0d0*x_d) > 1.0d-13) error stop 3"//nl// &
        "        call evaluate_mid_vjp(mid, x, y, y_b, x_b)"//nl// &
        "        if (abs(x_b - 2.0d0*y_b) > 1.0d-13) error stop 4"//nl// &
        "        if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 10"//nl// &
        "        h = 1.0d-6"//nl// &
        "        fp = evaluate_mid(mid, x + h)"//nl// &
        "        fm = evaluate_mid(mid, x - h)"//nl// &
        "        if (abs((fp - fm)/(2.0d0*h) - 2.0d0) > 1.0d-7) "// &
        "error stop 8"//nl// &
        "    end subroutine check_mid"//nl// &
        "    subroutine check_leaf()"//nl// &
        "        call evaluate_leaf_jvp(leaf, x, x_d, y, y_d)"//nl// &
        "        if (abs(y - 6.0d0) > 1.0d-13) error stop 5"//nl// &
        "        if (abs(y_d - 2.0d0*x_d) > 1.0d-13) error stop 6"//nl// &
        "        call evaluate_leaf_vjp(leaf, x, y, y_b, x_b)"//nl// &
        "        if (abs(x_b - 2.0d0*y_b) > 1.0d-13) error stop 7"//nl// &
        "        if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 11"//nl// &
        "        h = 1.0d-6"//nl// &
        "        fp = evaluate_leaf(leaf, x + h)"//nl// &
        "        fm = evaluate_leaf(leaf, x - h)"//nl// &
        "        if (abs((fp - fm)/(2.0d0*h) - 2.0d0) > 1.0d-7) "// &
        "error stop 9"//nl// &
        "    end subroutine check_leaf"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "generated hierarchy code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "abstract hierarchy behavioral oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_abstract_hierarchy_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, "FAIL ", label, ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

    subroutine expect_refusal(case_source, label, needle)
        character(len=*), intent(in) :: case_source, label, needle
        type(fad_result_t) :: result

        result = fad_jvp(case_source, indep, from="top")
        if (result%ok) then
            print *, "FAIL ", label, ": JVP was accepted"
            error stop 1
        end if
        if (.not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", label, ": unexpected JVP message: ", &
                result%message
            error stop 1
        end if

        result = fad_vjp(case_source, indep, dependent="y", from="top")
        if (result%ok) then
            print *, "FAIL ", label, ": VJP was accepted"
            error stop 1
        end if
        if (.not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", label, ": unexpected VJP message: ", &
                result%message
            error stop 1
        end if
    end subroutine expect_refusal

    subroutine expect_supported(case_source, label)
        character(len=*), intent(in) :: case_source, label
        type(fad_result_t) :: result

        result = fad_jvp(case_source, indep, from="top")
        if (.not. result%ok) then
            print *, "FAIL ", label, ": unexpected JVP refusal: ", result%message
            error stop 1
        end if
        result = fad_vjp(case_source, indep, dependent="y", from="top")
        if (.not. result%ok) then
            print *, "FAIL ", label, ": unexpected VJP refusal: ", result%message
            error stop 1
        end if
    end subroutine expect_supported

    function runtime_source() result(text)
        character(len=:), allocatable :: text
        text = "module runtime_abstract_case"//nl// &
            "    type, abstract :: base_t"//nl// &
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
            "    contains"//nl// &
            "        procedure :: value => child_value"//nl// &
            "    end type child_t"//nl// &
            "contains"//nl// &
            "    pure function child_value(self, x) result(y)"//nl// &
            "        class(child_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = x"//nl// &
            "    end function child_value"//nl// &
            "    pure function top(model, x) result(y)"//nl// &
            "        class(base_t), intent(in) :: model"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function top"//nl// &
            "end module runtime_abstract_case"//nl
    end function runtime_source

    function deferred_source() result(text)
        character(len=:), allocatable :: text
        text = "module deferred_abstract_case"//nl// &
            "    type, abstract :: base_t"//nl// &
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
            "contains"//nl// &
            "    pure function top(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(base_t) :: model"//nl// &
            "        real(8) :: y"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function top"//nl// &
            "end module deferred_abstract_case"//nl
    end function deferred_source

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

end program test_abstract_hierarchy_oracle
