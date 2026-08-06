program test_runtime_select_type_oracle
    !! A runtime type choice must select matching primal, tangent, and adjoint
    !! arms. The selector and its dynamic type stay passive: only `x` is an
    !! independent. Hand gradients and the adjoint identity cover two named
    !! child types and the class-default path; source inspection would not test
    !! the runtime dispatch.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module runtime_models"//nl// &
        "    implicit none"//nl// &
        "    type, abstract :: model_t"//nl// &
        "    end type model_t"//nl// &
        "    type, extends(model_t) :: linear_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    end type linear_t"//nl// &
        "    type, extends(model_t) :: quadratic_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    end type quadratic_t"//nl// &
        "    type, extends(model_t) :: fallback_t"//nl// &
        "    end type fallback_t"//nl// &
        "end module runtime_models"//nl// &
        "module runtime_kernel"//nl// &
        "    use runtime_models, only: model_t, linear_t, quadratic_t"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    function evaluate(model, x) result(y)"//nl// &
        "        class(model_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        select type (model)"//nl// &
        "        type is (linear_t)"//nl// &
        "            y = model%scale*x"//nl// &
        "        class is (quadratic_t)"//nl// &
        "            y = model%scale*x*x"//nl// &
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
        "    use runtime_models, only: model_t, linear_t, quadratic_t, fallback_t"//nl// &
        "    use runtime_generated, only: evaluate_jvp"//nl// &
        "    use runtime_adjoint_generated, only: evaluate_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(linear_t) :: linear"//nl// &
        "    type(quadratic_t) :: quadratic"//nl// &
        "    type(fallback_t) :: fallback"//nl// &
        "    linear%scale = 3.0d0"//nl// &
        "    quadratic%scale = 3.0d0"//nl// &
        "    call check(linear, 6.0d0, 3.0d0, 'linear')"//nl// &
        "    call check(quadratic, 12.0d0, 12.0d0, 'quadratic')"//nl// &
        "    call check(fallback, -4.0d0, -2.0d0, 'class default')"//nl// &
        "contains"//nl// &
        "    subroutine check(model, expected_y, expected_grad, label)"//nl// &
        "        class(model_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: expected_y, expected_grad"//nl// &
        "        character(len=*), intent(in) :: label"//nl// &
        "        real(8) :: x, x_d, y, y_d, y_b, x_b"//nl// &
        "        x = 2.0d0"//nl// &
        "        x_d = -0.4d0"//nl// &
        "        y_b = 1.7d0"//nl// &
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
