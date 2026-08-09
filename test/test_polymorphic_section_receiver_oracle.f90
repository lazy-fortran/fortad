program test_polymorphic_section_receiver_oracle
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module polymorphic_section_receiver_case"//nl// &
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
        "    pure function top(model, x) result(y)"//nl// &
        "        class(base_t), contiguous, intent(in) :: model(:)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        select type (item => model(2:3))"//nl// &
        "        type is (child_t)"//nl// &
        "            y = item(1)%scale*x + item(2)%bias"//nl// &
        "        class default"//nl// &
        "            y = x"//nl// &
        "        end select"//nl// &
        "    end function top"//nl// &
        "end module polymorphic_section_receiver_case"//nl

    character(len=32) :: independent_paths(5)
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    independent_paths = [character(len=32) :: "model(2)%scale", "model(2)%bias", &
        "model(3)%scale", "model(3)%bias", "x"]
    jvp = fad_jvp(source, independent_paths, from="top", name="top_jvp")
    call require_ok(jvp, "JVP")
    vjp = fad_vjp(source, independent_paths, dependent="y", from="top", name="top_vjp")
    call require_ok(vjp, "VJP")

    dir = "build/oracle/polymorphic_section_receiver"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", &
        "module polymorphic_section_receiver_derivatives"//nl// &
        "    use polymorphic_section_receiver_case, only: base_t, child_t"//nl// &
        "contains"//nl//jvp%code//vjp%code// &
        "end module polymorphic_section_receiver_derivatives"//nl)
    driver = &
        "program driver"//nl// &
        "    use polymorphic_section_receiver_case, only: child_t, top"//nl// &
        "    use polymorphic_section_receiver_derivatives, only: top_jvp, top_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(child_t) :: model(3), model_d(3), model_b(3), plus(3), minus(3)"//nl// &
        "    real(8) :: x, x_d, y, y_d, y_b, x_b, h, fp, fm, fd"//nl// &
        "    real(8) :: dot_forward, dot_reverse"//nl// &
        "    model(1)%scale = 1.0d0; model(1)%bias = 0.2d0"//nl// &
        "    model(2)%scale = 3.0d0; model(2)%bias = 0.5d0"//nl// &
        "    model(3)%scale = -2.0d0; model(3)%bias = 1.0d0"//nl// &
        "    model_d(1)%scale = 0.1d0; model_d(1)%bias = 0.3d0"//nl// &
        "    model_d(2)%scale = 0.7d0; model_d(2)%bias = -0.2d0"//nl// &
        "    model_d(3)%scale = -0.4d0; model_d(3)%bias = 0.6d0"//nl// &
        "    x = 2.0d0; x_d = 0.4d0; y_b = 1.3d0"//nl// &
        "    call top_jvp(model, model_d, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 7.0d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - 3.2d0) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = model; plus(2)%scale = model(2)%scale + h*model_d(2)%scale"//nl// &
        "    plus(2)%bias = model(2)%bias + h*model_d(2)%bias"//nl// &
        "    plus(3)%scale = model(3)%scale + h*model_d(3)%scale"//nl// &
        "    plus(3)%bias = model(3)%bias + h*model_d(3)%bias"//nl// &
        "    minus = model; minus(2)%scale = model(2)%scale - h*model_d(2)%scale"//nl// &
        "    minus(2)%bias = model(2)%bias - h*model_d(2)%bias"//nl// &
        "    minus(3)%scale = model(3)%scale - h*model_d(3)%scale"//nl// &
        "    minus(3)%bias = model(3)%bias - h*model_d(3)%bias"//nl// &
        "    fp = top(plus, x + h*x_d); fm = top(minus, x - h*x_d)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(y_d - fd) > 1.0d-7) error stop 4"//nl// &
        "    model_b = child_t(0.0d0, 0.0d0)"//nl// &
        "    call top_vjp(model, x, y, y_b, model_b, x_b)"//nl// &
        "    if (abs(model_b(2)%scale - 2.6d0) > 1.0d-13) error stop 5"//nl// &
        "    if (abs(model_b(2)%bias) > 1.0d-13) error stop 6"//nl// &
        "    if (abs(model_b(3)%scale) > 1.0d-13) error stop 7"//nl// &
        "    if (abs(model_b(3)%bias - 1.3d0) > 1.0d-13) error stop 8"//nl// &
        "    if (abs(x_b - 3.9d0) > 1.0d-13) error stop 9"//nl// &
        "    dot_forward = y_b*y_d"//nl// &
        "    dot_reverse = model_b(2)%scale*model_d(2)%scale + model_b(2)%bias*model_d(2)%bias + &"//nl// &
        "        model_b(3)%scale*model_d(3)%scale + model_b(3)%bias*model_d(3)%bias + x_b*x_d"//nl// &
        "    if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 10"//nl// &
        "    print *, 'polymorphic section receiver oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated section receiver did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "section receiver behavioral oracle failed"
    end if
    call expect_refusal(replace_text(source, ", contiguous", ""), &
        "non-contiguous borrowed section", "not declared contiguous or owning")
    print *, "test_polymorphic_section_receiver_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

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
        integer :: ios, file_unit
        open (newunit=file_unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (file_unit)
    end subroutine show_file

    subroutine expect_refusal(case_source, label, needle)
        character(len=*), intent(in) :: case_source, label, needle
        type(fad_result_t) :: result

        result = fad_jvp(case_source, independent_paths, from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), " JVP: ", result%message
            error stop 1
        end if
        result = fad_vjp(case_source, independent_paths, dependent="y", from="top")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL ", trim(label), " VJP: ", result%message
            error stop 1
        end if
    end subroutine expect_refusal

    function replace_text(base, old, new) result(text)
        character(len=*), intent(in) :: base, old, new
        character(len=:), allocatable :: text
        integer :: position

        text = base
        position = index(text, old)
        if (position > 0) text = text(:position - 1)//new//text(position + len(old):)
    end function replace_text

end program test_polymorphic_section_receiver_oracle
