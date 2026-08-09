program test_abstract_deferred_dispatch_oracle
    !! Independent oracle for the bounded direct abstract/deferred dispatch path.
    !! A single FortFront-proven child is lowered to one SELECT TYPE arm and
    !! differentiated through JVP and VJP.  A second child or no known child
    !! must remain a named refusal rather than an arbitrary vtable choice.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: source, dir, derivatives, driver
    integer :: stat

    source = single_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], &
        from="evaluate_deferred", name="evaluate_deferred_jvp")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        from="evaluate_deferred", name="evaluate_deferred_vjp")
    call require_ok(jvp, "single-target JVP")
    call require_ok(vjp, "single-target VJP")

    dir = "build/oracle/abstract_deferred_dispatch"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create abstract dispatch oracle directory"
    call write_file(dir//"/primal.f90", source)
    derivatives = "module abstract_deferred_single_derivatives"//nl// &
        "    use abstract_deferred_single_case, only: model_t, affine_model_t"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module abstract_deferred_single_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = &
        "program driver"//nl// &
        "    use abstract_deferred_single_case, only: affine_model_t, "// &
        "evaluate_deferred"//nl// &
        "    use abstract_deferred_single_derivatives, only: "// &
        "evaluate_deferred_jvp, evaluate_deferred_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(affine_model_t) :: model"//nl// &
        "    real(8) :: x, x_d, y, y_d, y_b, x_b, h, fp, fm"//nl// &
        "    model%bias = 0.75d0"//nl// &
        "    model%slope = 2.5d0"//nl// &
        "    x = 1.4d0"//nl// &
        "    x_d = -0.6d0"//nl// &
        "    y_b = 1.7d0"//nl// &
        "    call evaluate_deferred_jvp(model, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - (model%slope*x + model%bias)) > 1.0d-13) "// &
        "error stop 1"//nl// &
        "    if (abs(y_d - model%slope*x_d) > 1.0d-13) error stop 2"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = evaluate_deferred(model, x + h*x_d)"//nl// &
        "    fm = evaluate_deferred(model, x - h*x_d)"//nl// &
        "    if (abs(y_d - (fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
        "    call evaluate_deferred_vjp(model, x, y, y_b, x_b)"//nl// &
        "    if (abs(x_b - y_b*model%slope) > 1.0d-13) error stop 4"//nl// &
        "    if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 5"//nl// &
        "    print *, 'abstract deferred single-target oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "single-target abstract dispatch did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "single-target abstract dispatch oracle failed"
    end if

    call expect_refusal(multiple_source(), "multiple runtime targets", &
        "multiple runtime targets")
    call expect_refusal(unresolved_source(), "unresolved runtime target", &
        "dispatch target set")
    print *, "test_abstract_deferred_dispatch_oracle: all cases passed"

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

        result = fad_jvp(case_source, [character(len=1) :: "x"], &
            from="evaluate_deferred")
        call check_refusal(result, label//" JVP", needle)
        result = fad_vjp(case_source, [character(len=1) :: "x"], dependent="y", &
            from="evaluate_deferred")
        call check_refusal(result, label//" VJP", needle)
    end subroutine expect_refusal

    subroutine check_refusal(result, label, needle)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label, needle

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine check_refusal

    function single_source() result(text)
        character(len=:), allocatable :: text

        text = "module abstract_deferred_single_case"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    implicit none"//nl// &
            "    type, abstract :: model_t"//nl// &
            "        real(dp) :: bias"//nl// &
            "    contains"//nl// &
            "        procedure(value_iface), deferred, pass(self) :: value"//nl// &
            "    end type model_t"//nl// &
            "    abstract interface"//nl// &
            "        pure function value_iface(self, x) result(y)"//nl// &
            "            import :: dp, model_t"//nl// &
            "            class(model_t), intent(in) :: self"//nl// &
            "            real(dp), intent(in) :: x"//nl// &
            "            real(dp) :: y"//nl// &
            "        end function value_iface"//nl// &
            "    end interface"//nl// &
            "    type, extends(model_t) :: affine_model_t"//nl// &
            "        real(dp) :: slope"//nl// &
            "    contains"//nl// &
            "        procedure, pass(self) :: value => affine_value"//nl// &
            "    end type affine_model_t"//nl// &
            "contains"//nl// &
            "    pure function affine_value(self, x) result(y)"//nl// &
            "        class(affine_model_t), intent(in) :: self"//nl// &
            "        real(dp), intent(in) :: x"//nl// &
            "        real(dp) :: y"//nl// &
            "        y = self%slope*x + self%bias"//nl// &
            "    end function affine_value"//nl// &
            "    function evaluate_deferred(model, x) result(y)"//nl// &
            "        class(model_t), intent(in) :: model"//nl// &
            "        real(dp), intent(in) :: x"//nl// &
            "        real(dp) :: y"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function evaluate_deferred"//nl// &
            "end module abstract_deferred_single_case"//nl
    end function single_source

    function multiple_source() result(text)
        character(len=:), allocatable :: text

        text = single_source()
        text = replace_text(text, "    end type affine_model_t"//nl// &
            "contains"//nl, "    end type affine_model_t"//nl// &
            "    type, extends(affine_model_t) :: square_model_t"//nl// &
            "        real(dp) :: curvature"//nl// &
            "    contains"//nl// &
            "        procedure, pass(self) :: value => square_value"//nl// &
            "    end type square_model_t"//nl// &
            "contains"//nl)
        text = replace_text(text, "    function evaluate_deferred(model, x) result(y)"//nl, &
            "    pure function square_value(self, x) result(y)"//nl// &
            "        class(square_model_t), intent(in) :: self"//nl// &
            "        real(dp), intent(in) :: x"//nl// &
            "        real(dp) :: y"//nl// &
            "        y = self%curvature*x*x + self%slope*x + self%bias"//nl// &
            "    end function square_value"//nl// &
            "    function evaluate_deferred(model, x) result(y)"//nl)
    end function multiple_source

    function unresolved_source() result(text)
        character(len=:), allocatable :: text

        text = "module abstract_deferred_unresolved_case"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    implicit none"//nl// &
            "    type, abstract :: model_t"//nl// &
            "    contains"//nl// &
            "        procedure(value_iface), deferred :: value"//nl// &
            "    end type model_t"//nl// &
            "    abstract interface"//nl// &
            "        pure function value_iface(self, x) result(y)"//nl// &
            "            import :: dp, model_t"//nl// &
            "            class(model_t), intent(in) :: self"//nl// &
            "            real(dp), intent(in) :: x"//nl// &
            "            real(dp) :: y"//nl// &
            "        end function value_iface"//nl// &
            "    end interface"//nl// &
            "contains"//nl// &
            "    function evaluate_deferred(model, x) result(y)"//nl// &
            "        class(model_t), intent(in) :: model"//nl// &
            "        real(dp), intent(in) :: x"//nl// &
            "        real(dp) :: y"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function evaluate_deferred"//nl// &
            "end module abstract_deferred_unresolved_case"//nl
    end function unresolved_source

    function replace_text(base, old, new) result(text)
        character(len=*), intent(in) :: base, old, new
        character(len=:), allocatable :: text
        integer :: position

        text = base
        position = index(text, old)
        if (position <= 0) error stop "test source replacement failed"
        text = text(:position - 1)//new//text(position + len(old):)
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
        integer :: file_unit, ios

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

end program test_abstract_deferred_dispatch_oracle
