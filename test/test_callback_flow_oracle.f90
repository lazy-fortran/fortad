program test_callback_flow_oracle
    !! Independent oracle for one branch-merged procedure-pointer callback.
    !! The positive source is compiled with gfortran and checked against a
    !! hand derivative, central-difference convergence, and the adjoint
    !! identity.  Refusal cases require a line-local diagnostic.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    character(len=:), allocatable :: source, derivative, driver, dir
    type(fad_result_t) :: jvp, vjp
    integer :: stat

    source = positive_source()
    jvp = fad_jvp(source, ["x"], from="kernel", name="kernel_jvp")
    if (.not. jvp%ok) then
        if (allocated(jvp%message)) print *, trim(jvp%message)
        error stop "branch callback JVP generation was refused"
    end if
    vjp = fad_vjp(source, ["x"], dependent="y", from="kernel", &
        name="kernel_vjp")
    if (.not. vjp%ok) then
        if (allocated(vjp%message)) print *, trim(vjp%message)
        error stop "branch callback VJP generation was refused"
    end if
    if (index(jvp%code, "procedure") > 0 .or. &
            index(vjp%code, "procedure") > 0 .or. &
            index(jvp%code, "call callback") > 0 .or. &
            index(vjp%code, "call callback") > 0 .or. &
            index(jvp%code, "=>") > 0 .or. index(vjp%code, "=>") > 0) then
        error stop "procedure pointer survived callback-flow lowering"
    end if

    dir = "build/oracle_callback_flow"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create callback-flow oracle directory"
    call write_file(dir//"/primal.f90", source)
    derivative = "module callback_flow_derivatives"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module callback_flow_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivative)
    driver = positive_driver()
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -pedantic-errors "// &
        "-Wall -Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "branch callback generated source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "branch callback behavioral oracle failed"
    end if

    ! Loop rejection is an upstream query-contract gap: FortFront currently
    ! omits loop facts from this callback-flow query, so it is not asserted
    ! here as FortAD-tested behavior.
    call expect_refusal(source_for("nested"), "nested", "nested")
    call expect_refusal(source_for("missing"), "missing", "unresolved")
    call expect_refusal(source_for("reassigned"), "reassigned", "reassignment")
    call expect_refusal(source_for("null"), "null", "NULL()")
    call expect_refusal(source_for("nullify"), "nullify", "NULLIFY")
    call expect_refusal(source_for("inside_call"), "inside-call", "arm")
    call expect_refusal(source_for("generic"), "generic", "generic")
    call expect_refusal(source_for("incompatible"), "incompatible", "scalar REAL(8)")
    call expect_refusal(source_for("alias"), "alias", "unresolved")
    call expect_global_refusal()

    print *, "test_callback_flow_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = source_prefix()// &
            "    subroutine kernel(x, flag, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        logical, intent(in) :: flag"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        procedure(left), pointer :: callback"//nl// &
            "        if (flag) then"//nl// &
            "            callback => left"//nl// &
            "        else"//nl// &
            "            callback => right"//nl// &
            "        end if"//nl// &
            "        call callback(x, y)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module callback_flow_case"//nl
    end function positive_source

    function source_prefix() result(text)
        character(len=:), allocatable :: text

        text = "module callback_flow_case"//nl// &
            "    implicit none"//nl//"contains"//nl// &
            "    subroutine left(x, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        y = 2.0d0*x + x*x*x"//nl// &
            "    end subroutine left"//nl// &
            "    subroutine right(x, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        y = 3.0d0*x - 0.5d0*x*x*x"//nl// &
            "    end subroutine right"//nl
    end function source_prefix

    function source_for(case_name) result(text)
        character(len=*), intent(in) :: case_name
        character(len=:), allocatable :: text, body, call_name
        logical :: add_helper

        add_helper = .false.
        call_name = "callback"
        select case (case_name)
        case ("loop")
            body = "        integer :: i"//nl// &
                "        if (flag) then"//nl// &
                "            callback => left"//nl// &
                "            do i = 1, 1"//nl// &
                "                y = x"//nl// &
                "            end do"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl
        case ("nested")
            body = "        if (flag) then"//nl// &
                "            if (x > 0.0d0) then"//nl// &
                "                callback => left"//nl// &
                "            else"//nl// &
                "                callback => right"//nl// &
                "            end if"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl
        case ("missing")
            body = "        if (flag) then"//nl// &
                "            callback => left"//nl// &
                "        end if"//nl
        case ("reassigned")
            body = "        if (flag) then"//nl// &
                "            callback => left"//nl// &
                "            callback => right"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl
        case ("null")
            body = "        if (flag) then"//nl// &
                "            callback => null()"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl
        case ("nullify")
            body = "        if (flag) then"//nl// &
                "            callback => left"//nl// &
                "            nullify(callback)"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl
        case ("inside_call")
            body = "        if (flag) then"//nl// &
                "            callback => left"//nl// &
                "            call helper(x)"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl
            add_helper = .true.
        case ("alias")
            body = "        procedure(left), pointer :: alias"//nl// &
                "        if (flag) then"//nl// &
                "            callback => left"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl// &
                "        alias => callback"//nl
            call_name = "alias"
        case default
            body = "        if (flag) then"//nl// &
                "            callback => left"//nl// &
                "        else"//nl// &
                "            callback => right"//nl// &
                "        end if"//nl
        end select

        if (case_name == "generic") then
            text = generic_source()
            return
        end if
        if (case_name == "incompatible") then
            text = incompatible_source()
            return
        end if
        text = source_prefix()// &
            "    subroutine kernel(x, flag, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        logical, intent(in) :: flag"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        procedure(left), pointer :: callback"//nl//body// &
            "        call "//trim(call_name)//"(x, y)"//nl// &
            "    end subroutine kernel"//nl
        if (add_helper) then
            text = text// &
                "    subroutine helper(x)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "    end subroutine helper"//nl
        end if
        text = text//"end module callback_flow_case"//nl
    end function source_for

    function generic_source() result(text)
        character(len=:), allocatable :: text

        text = "module callback_flow_case"//nl// &
            "    implicit none"//nl// &
            "    interface choose"//nl// &
            "        module procedure left, right"//nl// &
            "    end interface choose"//nl//"contains"//nl// &
            "    subroutine left(x, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl//"y=2*x"//nl// &
            "    end subroutine left"//nl// &
            "    subroutine right(x, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl//"y=3*x"//nl// &
            "    end subroutine right"//nl// &
            "    subroutine kernel(x, flag, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        logical, intent(in) :: flag"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        procedure(left), pointer :: callback"//nl// &
            "        if (flag) then"//nl// &
            "            callback => choose"//nl// &
            "        else"//nl// &
            "            callback => right"//nl//"        end if"//nl// &
            "        call callback(x, y)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module callback_flow_case"//nl
    end function generic_source

    function incompatible_source() result(text)
        character(len=:), allocatable :: text

        text = "module callback_flow_case"//nl// &
            "    implicit none"//nl//"contains"//nl// &
            "    subroutine left(x, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl//"y=2*x"//nl// &
            "    end subroutine left"//nl// &
            "    subroutine right(x, y)"//nl// &
            "        real(4), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl//"y=3*real(x,8)"//nl// &
            "    end subroutine right"//nl// &
            "    subroutine kernel(x, flag, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        logical, intent(in) :: flag"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        procedure(left), pointer :: callback"//nl// &
            "        if (flag) then"//nl//"            callback => left"//nl// &
            "        else"//nl//"            callback => right"//nl//"        end if"//nl// &
            "        call callback(x, y)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module callback_flow_case"//nl
    end function incompatible_source

    function positive_driver() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use callback_flow_case, only: kernel"//nl// &
            "    use callback_flow_derivatives, only: kernel_jvp, kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, y, yd, xb, yb, h1, h2, fd1, fd2"//nl// &
            "    real(8) :: hand, coarse_error, fine_error"//nl// &
            "    logical :: flag"//nl// &
            "    x = 0.7d0"//nl//"    xd = -0.35d0"//nl//"    yb = 1.7d0"//nl// &
            "    flag = .true."//nl// &
            "    hand = xd*(2.0d0 + 3.0d0*x*x)"//nl// &
            "    call kernel_jvp(x, xd, flag, y, yd)"//nl// &
            "    if (abs(y-(2.0d0*x+x*x*x)) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(yd-hand) > 1.0d-12) error stop 2"//nl// &
            "    h1 = 1.0d-3"//nl//"    h2 = 1.0d-5"//nl// &
            "    fd1 = (kernel_value(x+h1, flag)-kernel_value(x-h1, flag))/(2*h1)"//nl// &
            "    fd2 = (kernel_value(x+h2, flag)-kernel_value(x-h2, flag))/(2*h2)"//nl// &
            "    coarse_error = abs(fd1-(2.0d0+3.0d0*x*x))"//nl// &
            "    fine_error = abs(fd2-(2.0d0+3.0d0*x*x))"//nl// &
            "    if (fine_error >= coarse_error) error stop 3"//nl// &
            "    if (fine_error > 1.0d-8) error stop 4"//nl// &
            "    call kernel_vjp(x, flag, y, yb, xb)"//nl// &
            "    if (abs(xb-yb*(2.0d0+3.0d0*x*x)) > 1.0d-12) error stop 5"//nl// &
            "    if (abs(yd*yb-xd*xb) > 1.0d-12) error stop 6"//nl// &
            "    flag = .false."//nl//"    hand = xd*(3.0d0-1.5d0*x*x)"//nl// &
            "    call kernel_jvp(x, xd, flag, y, yd)"//nl// &
            "    if (abs(yd-hand) > 1.0d-12) error stop 7"//nl// &
            "    print *, 'callback flow oracle pass'"//nl// &
            "contains"//nl// &
            "    function kernel_value(x, flag) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        logical, intent(in) :: flag"//nl// &
            "        real(8) :: y"//nl// &
            "        if (flag) then"//nl//"            y=2*x+x*x*x"//nl// &
            "        else"//nl//"            y=3*x-0.5d0*x*x*x"//nl//"        end if"//nl// &
            "    end function kernel_value"//nl// &
            "end program driver"//nl
    end function positive_driver

    subroutine expect_refusal(text, label, reason)
        character(len=*), intent(in) :: text, label, reason
        type(fad_result_t) :: result

        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok) error stop "callback-flow refusal accepted: "//trim(label)
        if (.not. allocated(result%message)) then
            error stop "callback-flow refusal unnamed: "//trim(label)
        end if
        if (index(result%message, reason) == 0) then
            print *, trim(label), ": ", trim(result%message)
            error stop "callback-flow refusal lost reason"
        end if
        if (index(result%message, "line") == 0) then
            error stop "callback-flow refusal lost source line"
        end if
        if (allocated(result%code)) error stop "callback-flow refusal emitted code"
    end subroutine expect_refusal

    subroutine expect_global_refusal()
        character(len=:), allocatable :: text
        type(fad_result_t) :: result

        text = "module callback_global_case"//nl// &
            "    implicit none"//nl// &
            "    procedure(global_left), pointer :: callback"//nl// &
            "contains"//nl// &
            "    subroutine global_left(x, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl//"        y=2*x"//nl// &
            "    end subroutine global_left"//nl// &
            "    subroutine kernel(x, flag, y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        logical, intent(in) :: flag"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        if (flag) then"//nl//"            callback => global_left"//nl// &
            "        else"//nl//"            callback => global_left"//nl//"        end if"//nl// &
            "        call callback(x, y)"//nl// &
            "    end subroutine kernel"//nl//"end module callback_global_case"//nl
        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok) error stop "global callback state was accepted"
        if (.not. allocated(result%message)) error stop "global refusal unnamed"
        if (index(result%message, "global") == 0 .and. &
                index(result%message, "module") == 0) then
            print *, trim(result%message)
            error stop "global refusal lost its reason"
        end if
    end subroutine expect_global_refusal

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

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_callback_flow_oracle
