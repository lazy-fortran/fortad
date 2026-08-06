program test_runtime_select_type_oracle
    !! A runtime type choice must select the matching primal and tangent arm.
    !! The expected derivatives below are hand-derived from two different
    !! concrete models; inspecting generated source would not test dispatch.
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
        "        type is (quadratic_t)"//nl// &
        "            y = model%scale*x*x"//nl// &
        "        class default"//nl// &
        "            y = 0.0d0"//nl// &
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
    if (adjoint%ok .or. index(adjoint%message, "select type") == 0) then
        error stop "reverse mode must name its unsupported dispatch boundary"
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

    driver = &
        "program driver"//nl// &
        "    use runtime_models, only: linear_t, quadratic_t"//nl// &
        "    use runtime_generated, only: evaluate_jvp"//nl// &
        "    implicit none"//nl// &
        "    type(linear_t) :: linear"//nl// &
        "    type(quadratic_t) :: quadratic"//nl// &
        "    real(8) :: x, x_d, y, y_d"//nl// &
        "    x = 2.0d0"//nl// &
        "    x_d = 1.0d0"//nl// &
        "    linear%scale = 3.0d0"//nl// &
        "    call evaluate_jvp(linear, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 6.0d0) > 1.0d-13) error stop 1"//nl// &
        "    if (abs(y_d - 3.0d0) > 1.0d-13) error stop 2"//nl// &
        "    quadratic%scale = 3.0d0"//nl// &
        "    call evaluate_jvp(quadratic, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 12.0d0) > 1.0d-13) error stop 3"//nl// &
        "    if (abs(y_d - 12.0d0) > 1.0d-13) error stop 4"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "// &
        dir//"/tangent.f90 "//dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "generated source did not compile; see ", dir//"/build.log"
        error stop 1
    end if

    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "runtime type dispatch derivative mismatch"
    print *, "test_runtime_select_type_oracle: all cases passed"
end program test_runtime_select_type_oracle
