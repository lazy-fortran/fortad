program test_callback_call_oracle
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    character(len=:), allocatable :: source, dir, driver
    type(fad_result_t) :: jvp, vjp, action_jvp, refused
    integer :: stat

    source = &
        "module callback_call_case"//nl// &
        "    implicit none"//nl// &
        "    abstract interface"//nl// &
        "        real(8) function external_iface(x)"//nl// &
        "            real(8), intent(in) :: x"//nl// &
        "        end function external_iface"//nl// &
        "    end interface"//nl// &
        "contains"//nl// &
        "    real(8) function scale(x) result(y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = 2.0d0*x"//nl// &
        "    end function scale"//nl// &
        "    real(8) function kernel(x) result(y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), external :: external_scale"//nl// &
        "        procedure(scale), pointer :: callback"//nl// &
        "        procedure(external_iface), pointer :: external_callback"//nl// &
        "        callback => scale"//nl// &
        "        external_callback => external_scale"//nl// &
        "        y = callback(x) + external_callback(x) + x*x"//nl// &
        "    end function kernel"//nl// &
        "    subroutine add_action(x, y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(inout) :: y"//nl// &
        "        y = y + 4.0d0*x"//nl// &
        "    end subroutine add_action"//nl// &
        "    subroutine action_kernel(x, y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(inout) :: y"//nl// &
        "        procedure(add_action), pointer :: callback"//nl// &
        "        callback => add_action"//nl// &
        "        call callback(x, y)"//nl// &
        "    end subroutine action_kernel"//nl// &
        "end module callback_call_case"//nl// &
        "real(8) function external_scale(x) result(y)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    y = 3.0d0*x"//nl// &
        "end function external_scale"//nl

    jvp = fad_jvp(source, ["x"], from="kernel", name="kernel_jvp")
    vjp = fad_vjp(source, ["x"], dependent="y", from="kernel", &
        name="kernel_vjp")
    if (.not. jvp%ok) then
        if (allocated(jvp%message)) print *, trim(jvp%message)
        error stop "resolved callback JVP was refused"
    end if
    if (.not. vjp%ok) then
        if (allocated(vjp%message)) print *, trim(vjp%message)
        error stop "resolved callback VJP was refused"
    end if
    if (index(jvp%code, "callback") > 0 .or. &
        index(jvp%code, "external_callback") > 0) then
        error stop "callback pointer survived JVP lowering"
    end if
    if (index(vjp%code, "callback") > 0 .or. &
        index(vjp%code, "external_callback") > 0) then
        error stop "callback pointer survived VJP lowering"
    end if
    action_jvp = fad_jvp(source, ["x"], from="action_kernel", &
        name="action_kernel_jvp")
    if (.not. action_jvp%ok) then
        if (allocated(action_jvp%message)) print *, trim(action_jvp%message)
        error stop "resolved subroutine callback JVP was refused"
    end if
    if (index(action_jvp%code, "callback") > 0) then
        error stop "callback pointer survived subroutine lowering"
    end if

    dir = "build/oracle_callback_call"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create callback oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", "module derivative_mod"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code//"end module derivative_mod"//nl)

    driver = "program driver"//nl// &
        "    use callback_call_case, only: kernel"//nl// &
        "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, y, yd, xb, yb, h, fp, fm"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.4d0"//nl// &
        "    yb = 1.7d0"//nl// &
        "    call kernel_jvp(x, xd, y, yd)"//nl// &
        "    if (abs(y - 7.8125d0) > 1.0d-12) error stop 1"//nl// &
        "    if (abs(yd + 3.0d0) > 1.0d-12) error stop 2"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = kernel(x+h); fm = kernel(x-h)"//nl// &
        "    if (abs(yd - xd*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
        "    call kernel_vjp(x, y, yb, xb)"//nl// &
        "    if (abs(xb - 12.75d0) > 1.0d-12) error stop 4"//nl// &
        "    if (abs(yd*yb - xd*xb) > 1.0d-12) error stop 5"//nl// &
        "    print *, 'callback call oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)

    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
        "-Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated callback source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "callback behavioral oracle failed"
    end if

    call expect_refusal(refusal_source("reassigned"), &
        "reassignment", "target flow is unresolved")
    call expect_refusal(refusal_source("branched"), &
        "branch", "target flow is unresolved")
    call expect_refusal(refusal_source("null"), "NULL()", "NULL()")
    call expect_refusal(refusal_source("nullify"), "NULLIFY", "NULLIFY")
    call expect_refusal(refusal_source("generic"), "generic callback", &
        "target flow is unresolved")
    call expect_global_refusal()

    print *, "test_callback_call_oracle: all cases passed"

contains

    function refusal_source(case_name) result(text)
        character(len=*), intent(in) :: case_name
        character(len=:), allocatable :: text

        if (case_name == "generic") then
            text = "module callback_refusal_case"//nl// &
                "    implicit none"//nl// &
                "    interface choose"//nl// &
                "        module procedure scale"//nl// &
                "    end interface choose"//nl// &
                "contains"//nl// &
                "    real(8) function scale(x) result(y)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "        y = 2.0d0*x"//nl// &
                "    end function scale"//nl// &
                "    real(8) function kernel(x) result(y)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "        procedure(scale), pointer :: callback"//nl// &
                "        callback => choose"//nl// &
                "        y = callback(x)"//nl// &
                "    end function kernel"//nl// &
                "end module callback_refusal_case"//nl
            return
        end if
        text = "module callback_refusal_case"//nl// &
            "    implicit none"//nl// &
            "contains"//nl// &
            "    real(8) function scale(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = 2.0d0*x"//nl// &
            "    end function scale"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(scale), pointer :: callback"//nl
        select case (case_name)
        case ("reassigned")
            text = text// &
                "        callback => scale"//nl// &
                "        callback => scale"//nl
        case ("branched")
            text = text// &
                "        logical :: flag"//nl// &
                "        if (flag) callback => scale"//nl
        case ("null")
            text = text//"        callback => null()"//nl
        case ("nullify")
            text = text// &
                "        callback => scale"//nl// &
                "        nullify(callback)"//nl
        end select
        text = text// &
            "        y = callback(x)"//nl// &
            "    end function kernel"//nl// &
            "end module callback_refusal_case"//nl
    end function refusal_source

    subroutine expect_refusal(text, label, reason)
        character(len=*), intent(in) :: text, label, reason
        type(fad_result_t) :: result

        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok) error stop "callback refusal was accepted"
        if (.not. allocated(result%message)) error stop "callback refusal was unnamed"
        if (index(result%message, reason) == 0) then
            print *, trim(label), ": ", trim(result%message)
            error stop "callback refusal lost its reason"
        end if
        if (allocated(result%code)) error stop "callback refusal emitted code"
    end subroutine expect_refusal

    subroutine expect_global_refusal()
        character(len=:), allocatable :: text
        type(fad_result_t) :: result

        text = "module global_callback_case"//nl// &
            "    implicit none"//nl// &
            "    procedure(scale), pointer :: callback"//nl// &
            "contains"//nl// &
            "    real(8) function scale(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = 2.0d0*x"//nl// &
            "    end function scale"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        callback => scale"//nl// &
            "        y = callback(x)"//nl// &
            "    end function kernel"//nl// &
            "end module global_callback_case"//nl
        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok) error stop "global callback state was accepted"
        if (.not. allocated(result%message)) error stop "global callback refusal unnamed"
        if (index(result%message, "global") == 0 .and. &
            index(result%message, "module") == 0) then
            print *, trim(result%message)
            error stop "global callback refusal lost its reason"
        end if
    end subroutine expect_global_refusal

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

end program test_callback_call_oracle
