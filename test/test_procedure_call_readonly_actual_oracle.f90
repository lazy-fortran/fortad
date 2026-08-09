program test_procedure_call_readonly_actual_oracle
    !! Independent oracle for scalar INTENT(IN) computed actual mapping.
    !! The positive case checks generated JVP/VJP code against a hand formula,
    !! central differences, and the adjoint identity.  Every unsafe boundary
    !! is checked in both modes and must retain a named refusal.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: source, dir, driver
    integer :: stat

    source = positive_source()
    jvp = fad_jvp(source, ["x"], from="kernel", name="kernel_jvp")
    vjp = fad_vjp(source, ["x"], dependent="y", from="kernel", &
        name="kernel_vjp")
    if (.not. jvp%ok) error stop "scalar computed actual JVP was refused"
    if (.not. vjp%ok) error stop "scalar computed actual VJP was refused"

    dir = "build/oracle_procedure_call_readonly_actual"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create read-only actual oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", "module derivative_mod"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code//"end module derivative_mod"//nl)
    driver = positive_driver()
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -pedantic-errors "// &
        "-Wall -Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated scalar computed-actual source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "scalar computed-actual numerical oracle failed"
    end if

    call expect_refusal(alias_source(), "alias", "aliased")
    call expect_refusal(callback_source(), "callback", "callback")
    call expect_refusal(global_source(), "global", "global mutable")
    call expect_refusal(pointer_source(), "pointer", "alias")
    call expect_refusal(allocatable_source(), "allocatable", "allocatable")
    call expect_refusal(ambiguous_source(), "ambiguous", "ambiguous generic call")

    print *, "test_procedure_call_readonly_actual_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "module readonly_actual_case"//nl// &
            "    implicit none"//nl//"contains"//nl// &
            "    subroutine square_shift(input, output)"//nl// &
            "        real(8), intent(in) :: input"//nl// &
            "        real(8), intent(out) :: output"//nl// &
            "        output = input*input + 3.0d0*input"//nl// &
            "    end subroutine square_shift"//nl// &
            "    subroutine kernel(x, shift, y)"//nl// &
            "        real(8), intent(in) :: x, shift"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call square_shift(x + shift, y)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module readonly_actual_case"//nl
    end function positive_source

    function positive_driver() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use readonly_actual_case, only: kernel"//nl// &
            "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, x_d, shift, y, y_d, x_b, y_b"//nl// &
            "    real(8) :: h, yp, ym, expected, finite_difference"//nl// &
            "    x = 2.0d0"//nl// &
            "    x_d = -0.4d0"//nl// &
            "    shift = 0.5d0"//nl// &
            "    y_b = 1.3d0"//nl// &
            "    call kernel_jvp(x=x, x_d=x_d, shift=shift, y=y, y_d=y_d)"//nl// &
            "    expected = (x+shift)*(x+shift) + 3.0d0*(x+shift)"//nl// &
            "    if (abs(y-expected) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(y_d-x_d*(2.0d0*(x+shift)+3.0d0)) > 1.0d-12) error stop 2"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call kernel(x+h, shift, yp)"//nl// &
            "    call kernel(x-h, shift, ym)"//nl// &
            "    finite_difference = x_d*(yp-ym)/(2.0d0*h)"//nl// &
            "    if (abs(y_d-finite_difference) > 1.0d-7) error stop 3"//nl// &
            "    call kernel_vjp(x=x, shift=shift, y=y, y_b=y_b, x_b=x_b)"//nl// &
            "    if (abs(x_b-y_b*(2.0d0*(x+shift)+3.0d0)) > 1.0d-12) error stop 4"//nl// &
            "    if (abs(y_d*y_b-x_d*x_b) > 1.0d-12) error stop 5"//nl// &
            "    print *, 'scalar computed actual oracle pass'"//nl// &
            "end program driver"//nl
    end function positive_driver

    function alias_source() result(text)
        character(len=:), allocatable :: text

        text = "module readonly_actual_refusal"//nl//"contains"//nl// &
            "    subroutine update(left, right)"//nl// &
            "        real(8), intent(inout) :: left, right"//nl// &
            "        left = left + right"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), intent(inout) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x, x)"//nl//"        y = x"//nl// &
            "    end subroutine kernel"//nl//"end module readonly_actual_refusal"//nl
    end function alias_source

    function callback_source() result(text)
        character(len=:), allocatable :: text

        text = "module readonly_actual_refusal"//nl// &
            "    abstract interface"//nl// &
            "        subroutine action(x, y)"//nl// &
            "            real(8), intent(in) :: x"//nl// &
            "            real(8), intent(inout) :: y"//nl// &
            "        end subroutine action"//nl// &
            "    end interface"//nl//"contains"//nl// &
            "    subroutine apply(callback, x, y)"//nl// &
            "        procedure(action) :: callback"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(inout) :: y"//nl// &
            "        call callback(x, y)"//nl// &
            "    end subroutine apply"//nl// &
            "    subroutine kernel(callback, x, y)"//nl// &
            "        procedure(action) :: callback"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call apply(callback, x, y)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module readonly_actual_refusal"//nl
    end function callback_source

    function global_source() result(text)
        character(len=:), allocatable :: text

        text = "module readonly_actual_refusal"//nl// &
            "    real(8) :: shared"//nl//"contains"//nl// &
            "    subroutine update(value)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        shared = shared + value"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x)"//nl//"        y = shared"//nl// &
            "    end subroutine kernel"//nl// &
            "end module readonly_actual_refusal"//nl
    end function global_source

    function pointer_source() result(text)
        character(len=:), allocatable :: text

        text = "module readonly_actual_refusal"//nl//"contains"//nl// &
            "    subroutine update(value)"//nl// &
            "        real(8), pointer, intent(in) :: value"//nl// &
            "        print *, value"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), target, intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x)"//nl//"        y = x"//nl// &
            "    end subroutine kernel"//nl// &
            "end module readonly_actual_refusal"//nl
    end function pointer_source

    function allocatable_source() result(text)
        character(len=:), allocatable :: text

        text = "module readonly_actual_refusal"//nl//"contains"//nl// &
            "    subroutine update(value)"//nl// &
            "        real(8), allocatable, intent(in) :: value"//nl// &
            "        print *, value"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), allocatable, intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x)"//nl//"        y = x"//nl// &
            "    end subroutine kernel"//nl// &
            "end module readonly_actual_refusal"//nl
    end function allocatable_source

    function ambiguous_source() result(text)
        character(len=:), allocatable :: text

        text = "module readonly_actual_refusal"//nl// &
            "    interface choose"//nl// &
            "        module procedure left, right"//nl// &
            "    end interface choose"//nl//"contains"//nl// &
            "    real(8) function left(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = value"//nl//"    end function left"//nl// &
            "    real(8) function right(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = 2.0d0*value"//nl//"    end function right"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = choose(x)"//nl//"    end function kernel"//nl// &
            "end module readonly_actual_refusal"//nl
    end function ambiguous_source

    subroutine expect_refusal(text, label, reason)
        character(len=*), intent(in) :: text, label, reason
        type(fad_result_t) :: result

        print *, "checking read-only-boundary refusal: ", trim(label)
        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok) error stop "unsafe JVP call boundary was accepted"
        if (.not. allocated(result%message)) error stop "JVP refusal was unnamed"
        if (index(result%message, reason) == 0) then
            print *, trim(label), " JVP diagnostic: ", trim(result%message)
            error stop "JVP refusal lost reason"
        end if
        if (allocated(result%code)) error stop "JVP refusal emitted code"

        result = fad_vjp(text, ["x"], dependent="y", from="kernel", &
            name="kernel_vjp")
        if (result%ok) error stop "unsafe VJP call boundary was accepted"
        if (.not. allocated(result%message)) error stop "VJP refusal was unnamed"
        if (index(result%message, reason) == 0) then
            print *, trim(label), " VJP diagnostic: ", trim(result%message)
            error stop "VJP refusal lost reason"
        end if
        if (allocated(result%code)) error stop "VJP refusal emitted code"
    end subroutine expect_refusal

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
        integer :: ios, unit

        open (newunit=unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_procedure_call_readonly_actual_oracle
