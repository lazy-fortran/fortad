program test_type_bound_array_receiver_oracle
    !! Independent oracle for one fixed-shape array-element receiver.
    !! The same concrete type-bound function and subroutine are reached at
    !! models(2).  Hand values, central differences, and the adjoint identity
    !! check both generated derivative products.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module type_bound_array_receiver_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8) :: scale"//nl// &
        "        real(8) :: bias"//nl// &
        "    contains"//nl// &
        "        procedure :: value"//nl// &
        "        procedure :: apply"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure real(8) function value(self, x) result(y)"//nl// &
        "        class(box_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = self%scale*x + self%bias*x*x"//nl// &
        "    end function value"//nl// &
        "    pure subroutine apply(self, x, y)"//nl// &
        "        class(box_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: y"//nl// &
        "        y = self%scale*x + self%bias*x*x"//nl// &
        "    end subroutine apply"//nl// &
        "    pure real(8) function top(models, x) result(y)"//nl// &
        "        type(box_t), intent(in) :: models(2)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = models(2)%value(x)"//nl// &
        "    end function top"//nl// &
        "    pure subroutine top_sub(models, x, y)"//nl// &
        "        type(box_t), intent(in) :: models(2)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: y"//nl// &
        "        call models(2)%apply(x, y)"//nl// &
        "    end subroutine top_sub"//nl// &
        "end module type_bound_array_receiver_case"//nl

    character(len=32) :: independents(3)
    type(fad_result_t) :: jvp, vjp, sub_jvp, sub_vjp
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    independents = [character(len=32) :: "models(2)%scale", &
        "models(2)%bias", "x"]
    jvp = fad_jvp(source, independents, from="top", name="top_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL array receiver function JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, independents, dependent="y", from="top", &
        name="top_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL array receiver function VJP generation: ", vjp%message
        error stop 1
    end if
    sub_jvp = fad_jvp(source, independents, from="top_sub", name="top_sub_jvp")
    if (.not. sub_jvp%ok) then
        print *, "FAIL array receiver subroutine JVP generation: ", &
            sub_jvp%message
        error stop 1
    end if
    sub_vjp = fad_vjp(source, independents, dependent="y", from="top_sub", &
        name="top_sub_vjp")
    if (.not. sub_vjp%ok) then
        print *, "FAIL array receiver subroutine VJP generation: ", &
            sub_vjp%message
        error stop 1
    end if

    dir = "build/oracle/type_bound_array_receiver"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create array receiver oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module type_bound_array_receiver_derivatives"
    write (unit, '(a)') "    use type_bound_array_receiver_case, only: box_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') sub_jvp%code
    write (unit, '(a)') sub_vjp%code
    write (unit, '(a)') "end module type_bound_array_receiver_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use type_bound_array_receiver_case, only: box_t, top, top_sub"//nl// &
        "    use type_bound_array_receiver_derivatives, only: top_jvp, top_vjp, "// &
        "top_sub_jvp, top_sub_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(box_t) :: models(2), models_d(2), models_b(2), plus(2), minus(2)"//nl// &
        "    real(8) :: x, x_d, y, y_d, y_b, x_b, h, fp, fm, fd"//nl// &
        "    real(8) :: sub_y, sub_y_d, sub_fp, sub_fm, sub_fd"//nl// &
        "    real(8) :: dot_forward, dot_reverse"//nl// &
        "    models = box_t(0.0d0, 0.0d0)"//nl// &
        "    models(2)%scale = 3.0d0"//nl// &
        "    models(2)%bias = 0.5d0"//nl// &
        "    models_d = box_t(0.0d0, 0.0d0)"//nl// &
        "    models_d(2)%scale = 0.7d0"//nl// &
        "    models_d(2)%bias = -0.2d0"//nl// &
        "    x = 2.0d0"//nl// &
        "    x_d = 0.4d0"//nl// &
        "    y_b = 1.3d0"//nl// &
        "    call top_jvp(models, models_d, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 8.0d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - 2.6d0) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = models"//nl// &
        "    plus(2)%scale = models(2)%scale + h*models_d(2)%scale"//nl// &
        "    plus(2)%bias = models(2)%bias + h*models_d(2)%bias"//nl// &
        "    minus = models"//nl// &
        "    minus(2)%scale = models(2)%scale - h*models_d(2)%scale"//nl// &
        "    minus(2)%bias = models(2)%bias - h*models_d(2)%bias"//nl// &
        "    fp = top(plus, x + h*x_d)"//nl// &
        "    fm = top(minus, x - h*x_d)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(y_d - fd) > 1.0d-7) error stop 4"//nl// &
        "    call top_vjp(models, x, y, y_b, models_b, x_b)"//nl// &
        "    if (abs(models_b(2)%scale - 2.6d0) > 1.0d-13) error stop 5"//nl// &
        "    if (abs(models_b(2)%bias - 5.2d0) > 1.0d-13) error stop 6"//nl// &
        "    if (abs(x_b - 6.5d0) > 1.0d-13) error stop 7"//nl// &
        "    dot_forward = y_b*y_d"//nl// &
        "    dot_reverse = models_b(2)%scale*models_d(2)%scale + &"//nl// &
        "        models_b(2)%bias*models_d(2)%bias + x_b*x_d"//nl// &
        "    if (abs(dot_forward - dot_reverse) > 1.0d-13) error stop 8"//nl// &
        "    call top_sub_jvp(models, models_d, x, x_d, sub_y, sub_y_d)"//nl// &
        "    if (abs(sub_y - y) > 1.0d-13) error stop 9"//nl// &
        "    if (abs(sub_y_d - y_d) > 1.0d-13) error stop 10"//nl// &
        "    sub_fp = 0.0d0"//nl// &
        "    sub_fm = 0.0d0"//nl// &
        "    call top_sub(plus, x + h*x_d, sub_fp)"//nl// &
        "    call top_sub(minus, x - h*x_d, sub_fm)"//nl// &
        "    sub_fd = (sub_fp - sub_fm)/(2.0d0*h)"//nl// &
        "    if (abs(sub_y_d - sub_fd) > 1.0d-7) error stop 11"//nl// &
        "    call top_sub_vjp(models, x, sub_y, y_b, models_b, x_b)"//nl// &
        "    if (abs(models_b(2)%scale - 2.6d0) > 1.0d-13) error stop 12"//nl// &
        "    if (abs(models_b(2)%bias - 5.2d0) > 1.0d-13) error stop 13"//nl// &
        "    if (abs(x_b - 6.5d0) > 1.0d-13) error stop 14"//nl// &
        "    print *, 'type-bound array receiver oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL array receiver: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL array receiver: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if

    call expect_refusal(dynamic_source(), "dynamic receiver indices", &
        "dynamic receiver indices")
    call expect_refusal(section_source(), "array sections", "array sections")
    call expect_refusal(allocatable_source(), "allocatable receivers", &
        "allocatable receivers")
    call expect_refusal(polymorphic_source(), "polymorphic receivers", &
        "polymorphic receivers")
    call expect_refusal(alias_source(), "aliasing", "alias")
    print *, "test_type_bound_array_receiver_oracle: all cases passed"

contains

    subroutine expect_refusal(case_source, label, needle)
        character(len=*), intent(in) :: case_source, label, needle
        type(fad_result_t) :: result

        result = fad_jvp(case_source, [character(len=32) :: "models(2)%scale", "x"], &
            from="top")
        if (result%ok) then
            print *, "FAIL ", label, ": unsupported case was accepted"
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL ", label, ": refusal was unnamed"
            error stop 1
        end if
        if (index(result%message, needle) == 0) then
            print *, "FAIL ", label, ": wrong refusal: ", result%message
            error stop 1
        end if

        result = fad_vjp(case_source, [character(len=32) :: "models(2)%scale", "x"], &
            dependent="y", from="top")
        if (result%ok) then
            print *, "FAIL ", label, ": unsupported VJP case was accepted"
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL ", label, ": VJP refusal was unnamed"
            error stop 1
        end if
        if (index(result%message, needle) == 0) then
            print *, "FAIL ", label, ": wrong VJP refusal: ", result%message
            error stop 1
        end if
    end subroutine expect_refusal

    function dynamic_source() result(text)
        character(len=:), allocatable :: text
        text = replace_top(source, "models(2)%value(x)", "models(i)%value(x)", &
            "        integer, intent(in) :: i"//nl)
    end function dynamic_source

    function section_source() result(text)
        character(len=:), allocatable :: text
        text = replace_top(source, "models(2)%value(x)", "models(:)%value(x)", &
            "")
    end function section_source

    function allocatable_source() result(text)
        character(len=:), allocatable :: text
        text = replace_top(source, "type(box_t), intent(in) :: models(2)", &
            "type(box_t), allocatable, intent(in) :: models(:)", "")
    end function allocatable_source

    function polymorphic_source() result(text)
        character(len=:), allocatable :: text
        text = replace_top(source, "type(box_t), intent(in) :: models(2)", &
            "class(box_t), intent(in) :: models(2)", "")
    end function polymorphic_source

    function alias_source() result(text)
        character(len=:), allocatable :: text
        text = replace_top(source, "type(box_t), intent(in) :: models(2)", &
            "type(box_t), pointer :: models(:)", "")
    end function alias_source

    function replace_top(base, old, new, extra) result(text)
        character(len=*), intent(in) :: base, old, new, extra
        character(len=:), allocatable :: text
        integer :: position

        text = base
        position = index(text, old)
        if (position > 0) text = text(:position - 1)//new//text(position + len(old):)
        if (len_trim(extra) > 0) then
            position = index(text, "        type(box_t), intent(in) :: models(2)"//nl)
            if (position > 0) then
                position = position + len("        type(box_t), intent(in) :: models(2)"//nl)
                text = text(:position - 1)//extra//text(position:)
            end if
        end if
    end function replace_top

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

end program test_type_bound_array_receiver_oracle
