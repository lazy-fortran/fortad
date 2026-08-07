program test_runtime_select_type_oracle
    !! A runtime type choice must select matching primal, tangent, and adjoint
    !! arms through an abstract deferred binding. The selector and its dynamic
    !! type stay passive: only `x` is an independent. Hand gradients, central
    !! differences of the untouched primal at two step sizes, and the adjoint
    !! identity cover three named child types and the class-default path;
    !! source inspection would not test runtime dispatch.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module runtime_models"//nl// &
        "    implicit none"//nl// &
        "    type, abstract :: model_t"//nl// &
        "    contains"//nl// &
        "        procedure(value_iface), deferred :: value"//nl// &
        "    end type model_t"//nl// &
        "    abstract interface"//nl// &
        "        pure function value_iface(self, x) result(y)"//nl// &
        "            import model_t"//nl// &
        "            class(model_t), intent(in) :: self"//nl// &
        "            real(8), intent(in) :: x"//nl// &
        "            real(8) :: y"//nl// &
        "        end function value_iface"//nl// &
        "    end interface"//nl// &
        "    type, extends(model_t) :: linear_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    contains"//nl// &
        "        procedure :: value => linear_value"//nl// &
        "    end type linear_t"//nl// &
        "    type, extends(model_t) :: quadratic_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    contains"//nl// &
        "        procedure :: value => quadratic_value"//nl// &
        "    end type quadratic_t"//nl// &
        "    type, extends(model_t) :: cubic_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    contains"//nl// &
        "        procedure :: value => cubic_value"//nl// &
        "    end type cubic_t"//nl// &
        "    type, extends(model_t) :: fallback_t"//nl// &
        "    contains"//nl// &
        "        procedure :: value => fallback_value"//nl// &
        "    end type fallback_t"//nl// &
        "contains"//nl// &
        "    pure function linear_value(self, x) result(y)"//nl// &
        "        class(linear_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale*x"//nl// &
        "    end function linear_value"//nl// &
        "    pure function quadratic_value(self, x) result(y)"//nl// &
        "        class(quadratic_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale*x*x"//nl// &
        "    end function quadratic_value"//nl// &
        "    pure function cubic_value(self, x) result(y)"//nl// &
        "        class(cubic_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = self%scale*x*x*x"//nl// &
        "    end function cubic_value"//nl// &
        "    pure function fallback_value(self, x) result(y)"//nl// &
        "        class(fallback_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = -2.0d0*x"//nl// &
        "    end function fallback_value"//nl// &
        "end module runtime_models"//nl// &
        "module runtime_kernel"//nl// &
        "    use runtime_models, only: model_t, linear_t, quadratic_t, cubic_t"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    function evaluate(model, x) result(y)"//nl// &
        "        class(model_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        select type (model)"//nl// &
        "        type is (linear_t)"//nl// &
        "            y = model%value(x)"//nl// &
        "        class is (quadratic_t)"//nl// &
        "            y = model%value(x)"//nl// &
        "        class is (cubic_t)"//nl// &
        "            y = model%value(x)"//nl// &
        "        class default"//nl// &
        "            y = -2.0d0*x"//nl// &
        "        end select"//nl// &
        "    end function evaluate"//nl// &
        "end module runtime_kernel"//nl

    type(fad_result_t) :: tangent, adjoint
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    tangent = fad_jvp(source, ["x"], from="evaluate")
    if (.not. tangent%ok) then
        print *, "generation failed: ", tangent%message
        error stop 1
    end if

    adjoint = fad_vjp(source, ["x"], dependent="y", from="evaluate")
    if (.not. adjoint%ok) then
        print *, "reverse generation failed: ", adjoint%message
        error stop 1
    end if

    dir = "build/oracle/runtime_select_type"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)

    open (newunit=unit, file=dir//"/tangent.f90", status="replace", action="write")
    write (unit, '(a)') "module runtime_generated"
    write (unit, '(a)') "contains"
    write (unit, '(a)') tangent%code
    write (unit, '(a)') "end module runtime_generated"
    close (unit)

    open (newunit=unit, file=dir//"/adjoint.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module runtime_adjoint_generated"
    write (unit, '(a)') "contains"
    write (unit, '(a)') adjoint%code
    write (unit, '(a)') "end module runtime_adjoint_generated"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use runtime_models, only: model_t, linear_t, quadratic_t, cubic_t, fallback_t"//nl// &
        "    use runtime_kernel, only: evaluate"//nl// &
        "    use runtime_generated, only: evaluate_jvp"//nl// &
        "    use runtime_adjoint_generated, only: evaluate_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(linear_t) :: linear"//nl// &
        "    type(quadratic_t) :: quadratic"//nl// &
        "    type(cubic_t) :: cubic"//nl// &
        "    type(fallback_t) :: fallback"//nl// &
        "    linear%scale = 3.0d0"//nl// &
        "    quadratic%scale = 3.0d0"//nl// &
        "    cubic%scale = 3.0d0"//nl// &
        "    call check(linear, 6.0d0, 3.0d0, 'linear')"//nl// &
        "    call check(quadratic, 12.0d0, 12.0d0, 'quadratic')"//nl// &
        "    call check(cubic, 24.0d0, 36.0d0, 'cubic')"//nl// &
        "    call check(fallback, -4.0d0, -2.0d0, 'class default')"//nl// &
        "contains"//nl// &
        "    subroutine check(model, expected_y, expected_grad, label)"//nl// &
        "        class(model_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: expected_y, expected_grad"//nl// &
        "        character(len=*), intent(in) :: label"//nl// &
        "        real(8) :: x, x_d, y, y_d, y_b, x_b"//nl// &
        "        real(8) :: h, y_plus, y_minus, fd_grad"//nl// &
        "        real(8) :: fd_half"//nl// &
        "        x = 2.0d0"//nl// &
        "        x_d = -0.4d0"//nl// &
        "        y_b = 1.7d0"//nl// &
        "        h = 1.0d-4"//nl// &
        "        call evaluate_jvp(model, x, x_d, y, y_d)"//nl// &
        "        if (abs(y - expected_y) > 1.0d-13) then"//nl// &
        "            print *, label, ' primal', y, expected_y"//nl// &
        "            error stop 1"//nl// &
        "        end if"//nl// &
        "        call evaluate_vjp(model, x, y, y_b, x_b)"//nl// &
        "        if (abs(y - expected_y) > 1.0d-13) then"//nl// &
        "            print *, label, ' vjp primal', y, expected_y"//nl// &
        "            error stop 2"//nl// &
        "        end if"//nl// &
        "        if (abs(x_b - y_b*expected_grad) > 1.0d-13) then"//nl// &
        "            print *, label, ' gradient', x_b, y_b*expected_grad"//nl// &
        "            error stop 3"//nl// &
        "        end if"//nl// &
        "        y_plus = evaluate(model, x + h)"//nl// &
        "        y_minus = evaluate(model, x - h)"//nl// &
        "        fd_grad = (y_plus - y_minus)/(2.0d0*h)"//nl// &
        "        y_plus = evaluate(model, x + h/2.0d0)"//nl// &
        "        y_minus = evaluate(model, x - h/2.0d0)"//nl// &
        "        fd_half = (y_plus - y_minus)/h"//nl// &
        "        if (abs(fd_grad - expected_grad) > 1.0d-7) then"//nl// &
        "            print *, label, ' finite-difference gradient', fd_grad, expected_grad"//nl// &
        "            error stop 5"//nl// &
        "        end if"//nl// &
        "        if (abs(x_b/y_b - fd_grad) > 1.0d-7) then"//nl// &
        "            print *, label, ' VJP versus finite difference', x_b/y_b, fd_grad"//nl// &
        "            error stop 6"//nl// &
        "        end if"//nl// &
        "        if (label == 'cubic') then"//nl// &
        "            if (abs(fd_half - expected_grad) >= &"//nl// &
        "                0.5d0*abs(fd_grad - expected_grad)) then"//nl// &
        "                print *, label, ' finite-difference convergence', fd_grad, fd_half"//nl// &
        "                error stop 7"//nl// &
        "            end if"//nl// &
        "        end if"//nl// &
        "        if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) then"//nl// &
        "            print *, label, ' adjoint identity', y_b*y_d, x_b*x_d"//nl// &
        "            error stop 4"//nl// &
        "        end if"//nl// &
        "    end subroutine check"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "// &
        dir//"/tangent.f90 "//dir//"/adjoint.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "generated source did not compile; see ", dir//"/build.log"
        error stop 1
    end if

    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "runtime type dispatch derivative mismatch"
    print *, "test_runtime_select_type_oracle: all cases passed"
end program test_runtime_select_type_oracle
