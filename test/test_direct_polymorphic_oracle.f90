program test_direct_polymorphic_oracle
    !! Independent refusal oracle for multiple-target direct CLASS dispatch through
    !! a deferred binding.  With two runtime children, FortAD must not choose
    !! one vtable arm without a fixed FortFront proof.
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

    eval_jvp = fad_jvp(source, ["x"], from="evaluate", name="evaluate_jvp")
    eval_vjp = fad_vjp(source, ["x"], dependent="y", from="evaluate", &
        name="evaluate_vjp")
    apply_jvp = fad_jvp(source, ["x"], from="apply", name="apply_jvp")
    apply_vjp = fad_vjp(source, ["x"], dependent="y", from="apply", &
        name="apply_vjp")
    call require_refusal(eval_jvp, "evaluate JVP")
    call require_refusal(eval_vjp, "evaluate VJP")
    call require_refusal(apply_jvp, "apply JVP")
    call require_refusal(apply_vjp, "apply VJP")
    call expect_multiple_target_refusal()
    print *, "test_direct_polymorphic_oracle: all cases passed"

contains

    subroutine require_refusal(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, "multiple runtime targets") == 0) then
            print *, "FAIL ", label, ": ", result%message
            error stop 1
        end if
    end subroutine require_refusal

    subroutine expect_multiple_target_refusal()
        type(fad_result_t) :: result

        result = fad_jvp(source, ["model"], from="evaluate")
        if (result%ok) then
            print *, "FAIL dynamic type perturbation was not refused: ", result%message
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL multiple-target refusal had no message"
            error stop 1
        end if
        if (index(result%message, "multiple runtime targets") == 0) then
            print *, "FAIL multiple-target refusal was not named: ", result%message
            error stop 1
        end if
        result = fad_vjp(source, ["model"], dependent="y", from="evaluate")
        if (result%ok) then
            print *, "FAIL reverse dynamic type perturbation was not refused: ", result%message
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL reverse multiple-target refusal had no message"
            error stop 1
        end if
        if (index(result%message, "multiple runtime targets") == 0) then
            print *, "FAIL reverse multiple-target refusal was not named: ", result%message
            error stop 1
        end if
    end subroutine expect_multiple_target_refusal

end program test_direct_polymorphic_oracle
