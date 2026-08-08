program test_direct_polymorphic_oracle
    !! Independent behavioral oracle for direct CLASS dispatch through a
    !! deferred binding.  The source has no SELECT TYPE: FortAD must preserve
    !! the runtime child choice in generated JVP and VJP code.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module direct_polymorphic_case"//nl// &
        "    implicit none"//nl// &
        "    type, abstract :: model_t"//nl// &
        "    contains"//nl// &
        "        procedure(value_iface), deferred :: value"//nl// &
        "        procedure(update_iface), deferred :: update"//nl// &
        "    end type model_t"//nl// &
        "    abstract interface"//nl// &
        "        pure function value_iface(self, x) result(y)"//nl// &
        "            import model_t"//nl// &
        "            class(model_t), intent(in) :: self"//nl// &
        "            real(8), intent(in) :: x"//nl// &
        "            real(8) :: y"//nl// &
        "        end function value_iface"//nl// &
        "        pure subroutine update_iface(self, x, y)"//nl// &
        "            import model_t"//nl// &
        "            class(model_t), intent(in) :: self"//nl// &
        "            real(8), intent(in) :: x"//nl// &
        "            real(8), intent(out) :: y"//nl// &
        "        end subroutine update_iface"//nl// &
        "    end interface"//nl// &
        "    type, extends(model_t) :: linear_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    contains"//nl// &
        "        procedure :: value => linear_value"//nl// &
        "        procedure :: update => linear_update"//nl// &
        "    end type linear_t"//nl// &
        "    type, extends(model_t) :: quadratic_t"//nl// &
        "        real(8) :: scale, bias"//nl// &
        "    contains"//nl// &
        "        procedure :: value => quadratic_value"//nl// &
        "        procedure :: update => quadratic_update"//nl// &
        "    end type quadratic_t"//nl// &
        "contains"//nl// &
        "    pure function linear_value(self, x) result(y)"//nl// &
        "        class(linear_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale*x"//nl// &
        "    end function linear_value"//nl// &
        "    pure subroutine linear_update(self, x, y)"//nl// &
        "        class(linear_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: y"//nl// &
        "        y = self%scale*x"//nl// &
        "    end subroutine linear_update"//nl// &
        "    pure function quadratic_value(self, x) result(y)"//nl// &
        "        class(quadratic_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale*x*x + self%bias"//nl// &
        "    end function quadratic_value"//nl// &
        "    pure subroutine quadratic_update(self, x, y)"//nl// &
        "        class(quadratic_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: y"//nl// &
        "        y = self%scale*x*x + self%bias"//nl// &
        "    end subroutine quadratic_update"//nl// &
        "    pure function evaluate(model, x) result(y)"//nl// &
        "        class(model_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = model%value(x)"//nl// &
        "    end function evaluate"//nl// &
        "    pure subroutine apply(model, x, y)"//nl// &
        "        class(model_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: y"//nl// &
        "        call model%update(x, y)"//nl// &
        "    end subroutine apply"//nl// &
        "end module direct_polymorphic_case"//nl

    type(fad_result_t) :: eval_jvp, eval_vjp, apply_jvp, apply_vjp
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    eval_jvp = fad_jvp(source, ["x"], from="evaluate", name="evaluate_jvp")
    eval_vjp = fad_vjp(source, ["x"], dependent="y", from="evaluate", &
        name="evaluate_vjp")
    apply_jvp = fad_jvp(source, ["x"], from="apply", name="apply_jvp")
    apply_vjp = fad_vjp(source, ["x"], dependent="y", from="apply", &
        name="apply_vjp")
    call require_ok(eval_jvp, "evaluate JVP")
    call require_ok(eval_vjp, "evaluate VJP")
    call require_ok(apply_jvp, "apply JVP")
    call require_ok(apply_vjp, "apply VJP")
    call expect_dynamic_type_refusal()

    dir = "build/oracle/direct_polymorphic"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create oracle directory"
    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", action="write")
    write (unit, '(a)') "module direct_polymorphic_derivatives"
    write (unit, '(a)') "    use direct_polymorphic_case, only: model_t, linear_t, quadratic_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') eval_jvp%code
    write (unit, '(a)') eval_vjp%code
    write (unit, '(a)') apply_jvp%code
    write (unit, '(a)') apply_vjp%code
    write (unit, '(a)') "end module direct_polymorphic_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use direct_polymorphic_case, only: model_t, linear_t, quadratic_t, evaluate, apply"//nl// &
        "    use direct_polymorphic_derivatives, only: evaluate_jvp, evaluate_vjp, apply_jvp, apply_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(linear_t) :: linear"//nl// &
        "    type(quadratic_t) :: quadratic"//nl// &
        "    linear%scale = 3.0d0"//nl// &
        "    quadratic%scale = 2.0d0"//nl// &
        "    quadratic%bias = 1.5d0"//nl// &
        "    call check(linear, 6.0d0, 3.0d0, 'linear')"//nl// &
        "    call check(quadratic, 9.5d0, 8.0d0, 'quadratic')"//nl// &
        "contains"//nl// &
        "    subroutine check(model, expected_y, expected_grad, label)"//nl// &
        "        class(model_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: expected_y, expected_grad"//nl// &
        "        character(len=*), intent(in) :: label"//nl// &
        "        real(8) :: x, x_d, y, y_d, y_b, x_b, h, yp, ym, fd"//nl// &
        "        x = 2.0d0"//nl// &
        "        x_d = -0.4d0"//nl// &
        "        y_b = 1.7d0"//nl// &
        "        h = 1.0d-5"//nl// &
        "        call evaluate_jvp(model, x, x_d, y, y_d)"//nl// &
        "        if (abs(y - expected_y) > 1.0d-13 .or. abs(y_d - expected_grad*x_d) > 1.0d-13) error stop 2"//nl// &
        "        call evaluate_vjp(model, x, y, y_b, x_b)"//nl// &
        "        if (abs(y - expected_y) > 1.0d-13 .or. abs(x_b - y_b*expected_grad) > 1.0d-13) error stop 3"//nl// &
        "        yp = evaluate(model, x+h)"//nl// &
        "        ym = evaluate(model, x-h)"//nl// &
        "        fd = (yp-ym)/(2.0d0*h)"//nl// &
        "        if (abs(fd - expected_grad) > 1.0d-7) error stop 4"//nl// &
        "        if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 5"//nl// &
        "        call apply_jvp(model, x, x_d, y, y_d)"//nl// &
        "        if (abs(y - expected_y) > 1.0d-13 .or. abs(y_d - expected_grad*x_d) > 1.0d-13) error stop 6"//nl// &
        "        call apply_vjp(model, x, y, y_b, x_b)"//nl// &
        "        if (abs(y - expected_y) > 1.0d-13 .or. abs(x_b - y_b*expected_grad) > 1.0d-13) error stop 7"//nl// &
        "        call apply(model, x+h, yp)"//nl// &
        "        call apply(model, x-h, ym)"//nl// &
        "        fd = (yp-ym)/(2.0d0*h)"//nl// &
        "        if (abs(fd - expected_grad) > 1.0d-7) error stop 8"//nl// &
        "        if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 9"//nl// &
        "    end subroutine check"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)
    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "generated source did not compile; see ", dir//"/build.log"
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "direct polymorphic derivative mismatch"
    print *, "test_direct_polymorphic_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, "FAIL ", label, ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

    subroutine expect_dynamic_type_refusal()
        type(fad_result_t) :: result

        result = fad_jvp(source, ["model"], from="evaluate")
        if (result%ok) then
            print *, "FAIL dynamic type perturbation was not refused: ", result%message
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL dynamic type refusal had no message"
            error stop 1
        end if
        if (index(result%message, "dynamic type") == 0) then
            print *, "FAIL dynamic type refusal was not named: ", result%message
            error stop 1
        end if
        result = fad_vjp(source, ["model"], dependent="y", from="evaluate")
        if (result%ok) then
            print *, "FAIL reverse dynamic type perturbation was not refused: ", result%message
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL reverse dynamic type refusal had no message"
            error stop 1
        end if
        if (index(result%message, "dynamic type") == 0) then
            print *, "FAIL reverse dynamic type refusal was not named: ", result%message
            error stop 1
        end if
    end subroutine expect_dynamic_type_refusal

end program test_direct_polymorphic_oracle
