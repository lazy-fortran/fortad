program test_procedure_call_boundary_oracle
    !! Independent numerical/refusal oracle for the bounded direct-call slice.
    !! The positive case exercises scalar and whole-array mappings, then
    !! compiles and runs both generated modes.  Each negative case checks the
    !! boundary message rather than accepting an unsafe approximation.
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
    if (.not. jvp%ok) then
        if (allocated(jvp%message)) print *, trim(jvp%message)
        error stop "direct-call JVP was refused"
    end if
    if (.not. vjp%ok) then
        if (allocated(vjp%message)) print *, trim(vjp%message)
        error stop "direct-call VJP was refused"
    end if

    dir = "build/oracle_procedure_call_boundary"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create procedure-call oracle directory"
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
        error stop "direct-call generated source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "direct-call numerical oracle failed"
    end if

    call expect_refusal(alias_source(), "alias", "aliased")
    call expect_refusal(pointer_source(), "pointer", "alias")
    call expect_refusal(allocatable_source(), "allocatable", "allocatable")
    call expect_refusal(global_source(), "global", "global mutable")
    call expect_refusal(callback_source(), "callback", "callback")
    call expect_refusal(mismatch_source(), "mismatch", "match exactly")
    call expect_refusal(ambiguous_source(), "ambiguous", "ambiguous generic call")

    print *, "test_procedure_call_boundary_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "module direct_call_boundary_case"//nl// &
            "    implicit none"//nl//"contains"//nl// &
            "    pure real(8) function scale_value(x, scale) result(y)"//nl// &
            "        real(8), intent(in) :: x, scale"//nl// &
            "        y = x*scale"//nl// &
            "    end function scale_value"//nl// &
            "    subroutine scale_array(values, scale)"//nl// &
            "        real(8), intent(inout) :: values(:)"//nl// &
            "        real(8), intent(in) :: scale"//nl// &
            "        values = values*scale"//nl// &
            "    end subroutine scale_array"//nl// &
            "    subroutine kernel(x, values, scale, y)"//nl// &
            "        real(8), intent(inout) :: x"//nl// &
            "        real(8), intent(inout) :: values(:)"//nl// &
            "        real(8), intent(in) :: scale"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call scale_array(values, scale)"//nl// &
            "        x = scale_value(x, scale)"//nl// &
            "        y = x + sum(values)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module direct_call_boundary_case"//nl
    end function positive_source

    function positive_driver() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use direct_call_boundary_case, only: kernel"//nl// &
            "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, x_d, x_direction, values(3), scale, y, y_d"//nl// &
            "    real(8) :: y_b, x_b, h, fp, fm"//nl// &
            "    x = 2.0d0"//nl// &
            "    x_d = -0.25d0"//nl// &
            "    x_direction = x_d"//nl// &
            "    values = [1.0d0, 2.0d0, 3.0d0]"//nl// &
            "    scale = 0.5d0"//nl// &
            "    call kernel_jvp(x=x, x_d=x_d, values=values, scale=scale, "// &
            "y=y, y_d=y_d)"//nl// &
            "    if (abs(x-1.0d0) > 1.0d-12) error stop 1"//nl// &
            "    if (maxval(abs(values-[0.5d0,1.0d0,1.5d0])) > 1.0d-12) "// &
            "error stop 2"//nl// &
            "    if (abs(y-4.0d0) > 1.0d-12) error stop 3"//nl// &
            "    if (abs(y_d+0.125d0) > 1.0d-12) error stop 4"//nl// &
            "    h = 1.0d-6"//nl// &
            "    fp = (2.0d0+h)*scale + sum([1.0d0,2.0d0,3.0d0])*scale"//nl// &
            "    fm = (2.0d0-h)*scale + sum([1.0d0,2.0d0,3.0d0])*scale"//nl// &
            "    if (abs(y_d-x_direction*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 5"//nl// &
            "    x = 2.0d0"//nl// &
            "    values = [1.0d0, 2.0d0, 3.0d0]"//nl// &
            "    y_b = 1.0d0"//nl// &
            "    call kernel_vjp(x=x, values=values, scale=scale, y=y, "// &
            "y_b=y_b, x_b=x_b)"//nl// &
            "    if (abs(x_b-0.5d0) > 1.0d-12) error stop 6"//nl// &
            "    if (abs(y_d*y_b-x_direction*x_b) > 1.0d-12) error stop 7"//nl// &
            "    print *, 'procedure-call boundary numerical oracle pass'"//nl// &
            "end program driver"//nl
    end function positive_driver

    function alias_source() result(text)
        character(len=:), allocatable :: text

        text = "module call_refusal_case"//nl//"contains"//nl// &
            "    subroutine update(left, right)"//nl// &
            "        real(8), intent(inout) :: left, right"//nl// &
            "        left = left + right"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), intent(inout) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x, x)"//nl//"        y = x"//nl// &
            "    end subroutine kernel"//nl//"end module call_refusal_case"//nl
    end function alias_source

    function pointer_source() result(text)
        character(len=:), allocatable :: text

        text = "module call_refusal_case"//nl//"contains"//nl// &
            "    subroutine update(value)"//nl// &
            "        real(8), pointer, intent(inout) :: value"//nl// &
            "        value = value + 1.0d0"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), target, intent(inout) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x)"//nl//"        y = x"//nl// &
            "    end subroutine kernel"//nl//"end module call_refusal_case"//nl
    end function pointer_source

    function allocatable_source() result(text)
        character(len=:), allocatable :: text

        text = "module call_refusal_case"//nl//"contains"//nl// &
            "    subroutine update(value)"//nl// &
            "        real(8), allocatable, intent(inout) :: value"//nl// &
            "        value = value + 1.0d0"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), allocatable, intent(inout) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x)"//nl//"        y = x"//nl// &
            "    end subroutine kernel"//nl//"end module call_refusal_case"//nl
    end function allocatable_source

    function global_source() result(text)
        character(len=:), allocatable :: text

        text = "module call_refusal_case"//nl// &
            "    real(8) :: shared"//nl//"contains"//nl// &
            "    subroutine update(value)"//nl// &
            "        real(8), intent(inout) :: value"//nl// &
            "        value = value + shared"//nl// &
            "    end subroutine update"//nl// &
            "    subroutine kernel(x, y)"//nl// &
            "        real(8), intent(inout) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call update(x)"//nl//"        y = x"//nl// &
            "    end subroutine kernel"//nl//"end module call_refusal_case"//nl
    end function global_source

    function callback_source() result(text)
        character(len=:), allocatable :: text

        text = "module call_refusal_case"//nl// &
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
            "    end subroutine kernel"//nl//"end module call_refusal_case"//nl
    end function callback_source

    function mismatch_source() result(text)
        character(len=:), allocatable :: text

        text = "module call_refusal_case"//nl//"contains"//nl// &
            "    real(8) function update(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = value"//nl//"    end function update"//nl// &
            "    real(8) function kernel(value) result(out)"//nl// &
            "        integer, intent(in) :: value"//nl// &
            "        out = update(value)"//nl//"    end function kernel"//nl// &
            "end module call_refusal_case"//nl
    end function mismatch_source

    function ambiguous_source() result(text)
        character(len=:), allocatable :: text

        text = "module call_refusal_case"//nl// &
            "    interface choose"//nl// &
            "        module procedure left, right"//nl// &
            "    end interface choose"//nl//"contains"//nl// &
            "    real(8) function left(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = value"//nl//"    end function left"//nl// &
            "    real(8) function right(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = 2.0d0*value"//nl//"    end function right"//nl// &
            "    real(8) function kernel(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = choose(value)"//nl//"    end function kernel"//nl// &
            "end module call_refusal_case"//nl
    end function ambiguous_source

    subroutine expect_refusal(text, label, reason)
        character(len=*), intent(in) :: text, label, reason
        type(fad_result_t) :: result

        print *, "checking refusal: ", trim(label)
        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (label == "mismatch") then
            result = fad_jvp(text, ["value"], from="kernel", name="kernel_jvp")
        end if
        if (result%ok) then
            print *, "accepted unsafe case: ", trim(label)
            error stop "unsafe procedure-call boundary was accepted"
        end if
        if (.not. allocated(result%message)) error stop "refusal was unnamed"
        if (index(result%message, reason) == 0) then
            print *, trim(label), ": ", trim(result%message)
            error stop "refusal lost its reason"
        end if
        if (allocated(result%code)) error stop "refusal emitted derivative code"
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

end program test_procedure_call_boundary_oracle
