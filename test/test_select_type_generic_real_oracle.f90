program test_select_type_generic_real_oracle
    !! Independent compiled JVP/VJP, finite-difference, adjoint, and refusal
    !! oracle for one fixed scalar REAL(8) type-bound generic function.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none
    character(len=1), parameter :: nl = achar(10)
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: source, dir, driver
    integer :: stat

    source = positive_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], from="evaluate", &
        name="evaluate_jvp")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        from="evaluate", name="evaluate_vjp")
    call require_ok(jvp, "JVP")
    call require_ok(vjp, "VJP")
    dir = "build/oracle/select_type_generic_real"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", &
        "module select_type_generic_real_derivatives"//nl// &
        "    use select_type_generic_real_case, only: base_t, child_t"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module select_type_generic_real_derivatives"//nl)
    driver = &
        "program driver"//nl// &
        "    use select_type_generic_real_case, only: child_t, evaluate"//nl// &
        "    use select_type_generic_real_derivatives, only: evaluate_jvp, evaluate_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(child_t) :: model"//nl// &
        "    real(8) :: x, xd, y, yd, yb, xb, h, fp, fm"//nl// &
        "    model%scale = 2.75d0; model%bias = -0.4d0"//nl// &
        "    x = 1.6d0; xd = -0.35d0; yb = 1.9d0"//nl// &
        "    call evaluate_jvp(model, x, xd, y, yd)"//nl// &
        "    if (abs(y-model%scale*x) > 1.0d-13) error stop 1"//nl// &
        "    if (abs(yd-model%scale*xd) > 1.0d-13) error stop 2"//nl// &
        "    h = 1.0d-6; fp = evaluate(model,x+h*xd); fm = evaluate(model,x-h*xd)"//nl// &
        "    if (abs(yd-(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
        "    call evaluate_vjp(model, x, y, yb, xb)"//nl// &
        "    if (abs(xb-model%scale*yb) > 1.0d-13) error stop 4"//nl// &
        "    if (abs(yb*yd-xb*xd) > 1.0d-13) error stop 5"//nl// &
        "    print *, 'select type generic real oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated generic derivatives did not compile"
    end if
    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "generic numerical oracle failed"

    call expect_refusal(nonreal_source(), "non-REAL generic", "scalar REAL(8)")
    call expect_refusal(alias_source(), "alias", "alias")
    call expect_refusal(pointer_source(), "pointer", "pointer")
    call expect_refusal(dynamic_source(), "dynamic", "dispatch targets")
    call expect_refusal(global_source(), "global", "active global mutable state")
    print *, "test_select_type_generic_real_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

    subroutine expect_refusal(text, label, needle)
        character(len=*), intent(in) :: text, label, needle
        type(fad_result_t) :: result
        result = fad_jvp(text, [character(len=1) :: "x"], from="evaluate")
        call check_refusal(result, label//" JVP", needle)
        result = fad_vjp(text, [character(len=1) :: "x"], dependent="y", from="evaluate")
        call check_refusal(result, label//" VJP", needle)
    end subroutine expect_refusal

    subroutine check_refusal(result, label, needle)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label, needle
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine check_refusal

    function positive_source() result(text)
        character(len=:), allocatable :: text
        text = "module select_type_generic_real_case"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: real64"//nl// &
            "    implicit none"//nl// &
            "    type, abstract :: base_t"//nl// &
            "        real(real64) :: scale"//nl// &
            "    contains"//nl// &
            "        procedure, pass(self) :: eval_real"//nl// &
            "        generic :: evaluate => eval_real"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl// &
            "        real(8) :: bias"//nl// &
            "    end type child_t"//nl// &
            "contains"//nl// &
            "    pure function eval_real(value, self) result(y)"//nl// &
            "        real(real64), intent(in) :: value"//nl// &
            "        class(base_t), intent(in) :: self"//nl// &
            "        real(real64) :: y"//nl// &
            "        y = self%scale*value"//nl// &
            "    end function eval_real"//nl// &
            "    function evaluate(model, x) result(y)"//nl// &
            "        class(base_t), intent(in) :: model"//nl// &
            "        real(real64), intent(in) :: x"//nl// &
            "        real(real64) :: y"//nl// &
            "        select type (model)"//nl// &
            "        type is (child_t)"//nl// &
            "            y = model%evaluate(x)"//nl// &
            "        class default"//nl// &
            "            y = 0.0d0"//nl// &
            "        end select"//nl// &
            "    end function evaluate"//nl// &
            "end module select_type_generic_real_case"//nl
    end function positive_source

    function nonreal_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(positive_source(), "real(real64), intent(in) :: x", &
            "integer, intent(in) :: x")
    end function nonreal_source

    function alias_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(positive_source(), "            y = model%evaluate(x)"//nl, &
            "            associate (alias => model)"//nl// &
            "                y = alias%evaluate(x)"//nl// &
            "            end associate"//nl)
    end function alias_source

    function pointer_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(positive_source(), "class(base_t), intent(in) :: model", &
            "class(base_t), pointer, intent(in) :: model")
    end function pointer_source

    function dynamic_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(positive_source(), &
            "        select type (model)"//nl// &
            "        type is (child_t)"//nl// &
            "            y = model%evaluate(x)"//nl// &
            "        class default"//nl// &
            "            y = 0.0d0"//nl// &
            "        end select"//nl, &
            "        y = model%evaluate(x)"//nl)
    end function dynamic_source

    function global_source() result(text)
        character(len=:), allocatable :: text
        text = replace_text(positive_source(), "    implicit none"//nl, &
            "    implicit none"//nl//"    real(8) :: mutable_global"//nl)
        text = replace_text(text, "    pure function eval_real(value, self) result(y)"//nl, &
            "    function eval_real(value, self) result(y)"//nl)
        text = replace_text(text, "        y = self%scale*value"//nl, &
            "        y = self%scale*value+mutable_global"//nl)
    end function global_source

    function replace_text(base, old, new) result(text)
        character(len=*), intent(in) :: base, old, new
        character(len=:), allocatable :: text
        integer :: position
        text = base
        position = index(text, old)
        if (position <= 0) error stop "test source replacement failed"
        text = text(:position-1)//new//text(position+len(old):)
    end function replace_text

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit
        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, '(a)') text
        close (unit)
    end subroutine write_file

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: unit, ios
        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_select_type_generic_real_oracle
