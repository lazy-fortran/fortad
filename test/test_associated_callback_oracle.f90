program test_associated_callback_oracle
    !! Independent oracle for the bounded local ASSOCIATED callback guard.
    !! The accepted path is compiled with GNU Fortran and checked against
    !! hand values, a central finite difference, and the adjoint identity.
    !! Unsafe pointer state and target shapes must remain named refusals.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    character(len=:), allocatable :: source, derivative, driver, dir
    type(fad_result_t) :: jvp, vjp
    integer :: stat

    source = positive_source()
    jvp = fad_jvp(source, ["x"], from="kernel", name="kernel_jvp")
    vjp = fad_vjp(source, ["x"], dependent="y", from="kernel", &
        name="kernel_vjp")
    if (.not. jvp%ok) then
        if (allocated(jvp%message)) print *, trim(jvp%message)
        error stop "ASSOCIATED callback JVP was refused"
    end if
    if (.not. vjp%ok) then
        if (allocated(vjp%message)) print *, trim(vjp%message)
        error stop "ASSOCIATED callback VJP was refused"
    end if
    if (index(jvp%code, "callback") > 0 .or. &
        index(vjp%code, "callback") > 0 .or. &
        index(jvp%code, "associated") > 0 .or. &
        index(vjp%code, "associated") > 0) then
        error stop "procedure pointer or ASSOCIATED survived lowering"
    end if

    dir = "build/oracle_associated_callback"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create ASSOCIATED oracle directory"
    call write_file(dir//"/primal.f90", source)
    derivative = "module associated_callback_derivatives"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module associated_callback_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivative)
    driver = positive_driver()
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -pedantic-errors "// &
        "-Wall -Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated ASSOCIATED callback source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "ASSOCIATED callback behavioral oracle failed"
    end if

    call expect_refusal(refusal_source("reassigned"), "reassignment", &
        "reassigned")
    call expect_refusal(refusal_source("branched"), "branch", "branch")
    call expect_refusal(refusal_source("nullify"), "NULLIFY", "NULLIFY")
    call expect_refusal(refusal_source("global"), "global", "global")
    call expect_refusal(refusal_source("data"), "data pointer", "pointer")
    call expect_refusal(refusal_source("arity"), "ASSOCIATED arity", &
        "exactly one")
    call expect_refusal(refusal_source("wrong_signature"), "signature", &
        "scalar REAL(8)")

    print *, "test_associated_callback_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "module associated_callback_case"//nl// &
            "    implicit none"//nl//"contains"//nl// &
            "    real(8) function scale(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = 2.0d0*x + x*x"//nl// &
            "    end function scale"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(scale), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        if (associated(callback)) then"//nl// &
            "            y = callback(x) + 1.0d0"//nl// &
            "        else"//nl// &
            "            y = 0.0d0"//nl// &
            "        end if"//nl// &
            "    end function kernel"//nl// &
            "end module associated_callback_case"//nl
    end function positive_source

    function positive_driver() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use associated_callback_case, only: kernel"//nl// &
            "    use associated_callback_derivatives, only: kernel_jvp, "// &
            "kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, y, yd, xb, yb, h, fp, fm"//nl// &
            "    x = 1.25d0"//nl// &
            "    xd = -0.4d0"//nl// &
            "    yb = 1.7d0"//nl// &
            "    call kernel_jvp(x, xd, y, yd)"//nl// &
            "    if (abs(y - 5.0625d0) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(yd + 1.8d0) > 1.0d-12) error stop 2"//nl// &
            "    h = 1.0d-6"//nl// &
            "    fp = kernel(x+h); fm = kernel(x-h)"//nl// &
            "    if (abs(yd - xd*(fp-fm)/(2.0d0*h)) > 1.0d-7) "// &
            "error stop 3"//nl// &
            "    call kernel_vjp(x, y, yb, xb)"//nl// &
            "    if (abs(xb - 7.65d0) > 1.0d-12) error stop 4"//nl// &
            "    if (abs(yd*yb - xd*xb) > 1.0d-12) error stop 5"//nl// &
            "    print *, 'ASSOCIATED callback oracle pass'"//nl// &
            "end program driver"//nl
    end function positive_driver

    function refusal_source(case_name) result(text)
        character(len=*), intent(in) :: case_name
        character(len=:), allocatable :: text

        if (case_name == "wrong_signature") then
            text = "module associated_refusal_case"//nl// &
                "    implicit none"//nl//"contains"//nl// &
                "    real(8) function scale(x) result(y)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "        y = 2.0d0*x + x*x"//nl// &
                "    end function scale"//nl// &
                "    integer function narrow(x) result(y)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "        y = int(x)"//nl// &
                "    end function narrow"//nl// &
                "    real(8) function kernel(x) result(y)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "        procedure(narrow), pointer :: callback"//nl// &
                "        callback => narrow"//nl// &
                "        if (associated(callback)) y = callback(x)"//nl// &
                "        if (.not. associated(callback)) y = 0.0d0"//nl// &
                "    end function kernel"//nl// &
                "end module associated_refusal_case"//nl
            return
        end if

        if (case_name == "global") then
            text = "module associated_global_case"//nl// &
                "    implicit none"//nl// &
                "    real(8) :: bias = 1.0d0"//nl//"contains"//nl// &
                "    real(8) function scale(x) result(y)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "        y = 2.0d0*x + bias"//nl// &
                "    end function scale"//nl// &
                "    real(8) function kernel(x) result(y)"//nl// &
                "        real(8), intent(in) :: x"//nl// &
                "        procedure(scale), pointer :: callback"//nl// &
                "        callback => scale"//nl// &
                "        if (associated(callback)) then"//nl// &
                "            y = callback(x)"//nl// &
                "        else"//nl// &
                "            y = 0.0d0"//nl// &
                "        end if"//nl// &
                "    end function kernel"//nl// &
                "end module associated_global_case"//nl// &
                ""
            return
        end if

        text = "module associated_refusal_case"//nl// &
            "    implicit none"//nl//"contains"//nl// &
            "    real(8) function scale(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = 2.0d0*x + x*x"//nl// &
            "    end function scale"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl
        select case (case_name)
        case ("reassigned")
            text = text// &
                "        procedure(scale), pointer :: callback"//nl// &
                "        callback => scale"//nl// &
                "        callback => scale"//nl// &
                "        if (associated(callback)) y = callback(x)"//nl
        case ("branched")
            text = text// &
                "        logical :: flag"//nl// &
                "        procedure(scale), pointer :: callback"//nl// &
                "        flag = .true."//nl// &
                "        if (flag) callback => scale"//nl// &
                "        if (associated(callback)) y = callback(x)"//nl
        case ("nullify")
            text = text// &
                "        procedure(scale), pointer :: callback"//nl// &
                "        callback => scale"//nl// &
                "        nullify(callback)"//nl// &
                "        if (associated(callback)) y = callback(x)"//nl
        case ("data")
            text = text// &
                "        real(8), pointer :: callback"//nl// &
                "        if (associated(callback)) y = x"//nl
        case ("arity")
            text = text// &
                "        procedure(scale), pointer :: callback"//nl// &
                "        callback => scale"//nl// &
                "        if (associated(callback, scale)) y = callback(x)"//nl
        end select
        text = text// &
            "        if (.not. associated(callback)) y = 0.0d0"//nl// &
            "    end function kernel"//nl// &
            "end module associated_refusal_case"//nl
    end function refusal_source

    subroutine expect_refusal(text, label, reason)
        character(len=*), intent(in) :: text, label, reason
        type(fad_result_t) :: result

        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok) then
            error stop "ASSOCIATED refusal was accepted"
        end if
        if (.not. allocated(result%message)) then
            error stop "ASSOCIATED refusal was unnamed"
        end if
        if (index(result%message, reason) == 0) then
            print *, trim(label), ": ", trim(result%message)
            error stop "ASSOCIATED refusal lost its reason"
        end if
        if (allocated(result%code)) error stop "ASSOCIATED refusal emitted code"
    end subroutine expect_refusal

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

end program test_associated_callback_oracle
