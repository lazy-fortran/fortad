program test_passed_procedure_callback_oracle
    !! Independent oracle for the bounded P8.6 passed-procedure slice.
    !!
    !! The positive case passes one same-file scalar REAL function directly
    !! and one fixed local procedure pointer to a same-file procedure dummy;
    !! it checks hand, finite-difference, and adjoint results after both
    !! generated modes are compiled by gfortran.
    !! Refusal cases verify that flow, dispatch, alias, global-state, and
    !! ownership boundaries are named rather than approximated.
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
        error stop "passed-procedure JVP was refused"
    end if
    if (.not. vjp%ok) then
        if (allocated(vjp%message)) print *, trim(vjp%message)
        error stop "passed-procedure VJP was refused"
    end if
    if (index(jvp%code, "callback") > 0 .or. &
        index(vjp%code, "callback") > 0) then
        error stop "procedure dummy survived passed-procedure lowering"
    end if

    dir = "build/oracle_passed_procedure_callback"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create passed-procedure oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", "module derivative_mod"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code//"end module derivative_mod"//nl)
    driver = positive_driver()
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
        "-Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated passed-procedure source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "passed-procedure numerical oracle failed"
    end if

    call expect_refusal(refusal_source("reassigned"), "reassignment", &
        "reassigned")
    call expect_refusal(refusal_source("loop"), "loop", "branch or loop")
    call expect_refusal(refusal_source("null"), "NULL", "NULL()")
    call expect_refusal(refusal_source("generic"), "generic", "generic")
    call expect_refusal(refusal_source("incompatible"), "incompatible", &
        "incompatible formal")
    call expect_refusal(refusal_source("dynamic"), "dynamic", "unresolved")
    call expect_refusal(refusal_source("alias"), "alias", "unresolved")
    call expect_refusal(global_source(), "global", "global mutable")
    call expect_refusal(ownership_source(), "ownership", "ownership")

    print *, "test_passed_procedure_callback_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "module passed_callback_case"//nl// &
            "    implicit none"//nl// &
            "    abstract interface"//nl// &
            "        real(8) function callback_iface(value)"//nl// &
            "            real(8), intent(in) :: value"//nl// &
            "        end function callback_iface"//nl// &
            "    end interface"//nl// &
            "contains"//nl// &
            "    real(8) function scale(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = 2.0d0*value"//nl// &
            "    end function scale"//nl// &
            "    real(8) function apply(callback, value) result(out)"//nl// &
            "        procedure(callback_iface) :: callback"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = callback(value) + value*value"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(scale, x) + apply(callback, x)"//nl// &
            "    end function kernel"//nl// &
            "end module passed_callback_case"//nl
    end function positive_source

    function positive_driver() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use passed_callback_case, only: kernel"//nl// &
            "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, y, yd, xb, yb, h, fp, fm"//nl// &
            "    x = 1.25d0"//nl// &
            "    xd = -0.4d0"//nl// &
            "    yb = 1.7d0"//nl// &
            "    call kernel_jvp(x, xd, y, yd)"//nl// &
            "    if (abs(y-8.125d0) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(yd+3.6d0) > 1.0d-12) error stop 2"//nl// &
            "    h = 1.0d-6"//nl// &
            "    fp = kernel(x+h)"//nl//"    fm = kernel(x-h)"//nl// &
            "    if (abs(yd-xd*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
            "    call kernel_vjp(x, y, yb, xb)"//nl// &
            "    if (abs(xb-15.3d0) > 1.0d-12) error stop 4"//nl// &
            "    if (abs(yd*yb-xd*xb) > 1.0d-12) error stop 5"//nl// &
            "    print *, 'passed-procedure callback oracle pass'"//nl// &
            "end program driver"//nl
    end function positive_driver

    function refusal_source(case_name) result(text)
        character(len=*), intent(in) :: case_name
        character(len=:), allocatable :: text

        text = "module passed_refusal_case"//nl// &
            "    implicit none"//nl// &
            "    abstract interface"//nl// &
            "        real(8) function callback_iface(value)"//nl// &
            "            real(8), intent(in) :: value"//nl// &
            "        end function callback_iface"//nl// &
            "    end interface"//nl// &
            "    interface choose"//nl// &
            "        module procedure scale"//nl// &
            "    end interface choose"//nl//"contains"//nl// &
            "    real(8) function scale(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = 2.0d0*value"//nl// &
            "    end function scale"//nl// &
            "    real(8) function apply(callback, value) result(out)"//nl// &
            "        procedure(callback_iface) :: callback"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = callback(value)"//nl// &
            "    end function apply"//nl// &
            "    real(4) function wrong_kind(value) result(out)"//nl// &
            "        real(4), intent(in) :: value"//nl// &
            "        out = value"//nl// &
            "    end function wrong_kind"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl
        select case (case_name)
        case ("reassigned")
            text = text//"        callback => scale"//nl// &
                "        callback => scale"//nl
        case ("loop")
            text = text//"        integer :: i"//nl// &
                "        do i = 1, 1"//nl// &
                "            callback => scale"//nl// &
                "        end do"//nl
        case ("null")
            text = text//"        callback => null()"//nl
        case ("generic")
            text = text//"        callback => choose"//nl
        case ("incompatible")
            text = text//"        y = apply(wrong_kind, x)"//nl
        case ("dynamic")
            text = text//"        y = apply(callback, x)"//nl// &
                "        return"//nl
        case ("alias")
            text = text//"        procedure(callback_iface), pointer :: alias"//nl// &
                "        callback => scale"//nl// &
                "        alias => callback"//nl// &
                "        y = apply(alias, x)"//nl// &
                "        return"//nl
        end select
        if (case_name /= "dynamic" .and. case_name /= "alias" .and. &
            case_name /= "incompatible") then
            text = text//"        y = apply(callback, x)"//nl
        end if
        text = text//"    end function kernel"//nl// &
            "end module passed_refusal_case"//nl
    end function refusal_source

    function global_source() result(text)
        character(len=:), allocatable :: text

        text = "module passed_global_case"//nl// &
            "    implicit none"//nl//"    real(8) :: shared"//nl// &
            "    abstract interface"//nl// &
            "        real(8) function callback_iface(value)"//nl// &
            "            real(8), intent(in) :: value"//nl// &
            "        end function callback_iface"//nl// &
            "    end interface"//nl//"contains"//nl// &
            "    real(8) function scale(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = shared*value"//nl// &
            "    end function scale"//nl// &
            "    real(8) function apply(callback, value) result(out)"//nl// &
            "        procedure(callback_iface) :: callback"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = callback(value)"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(callback, x)"//nl// &
            "    end function kernel"//nl// &
            "end module passed_global_case"//nl
    end function global_source

    function ownership_source() result(text)
        character(len=:), allocatable :: text

        text = "module passed_ownership_case"//nl// &
            "    implicit none"//nl// &
            "    abstract interface"//nl// &
            "        real(8) function callback_iface(value)"//nl// &
            "            real(8), intent(in) :: value"//nl// &
            "        end function callback_iface"//nl// &
            "    end interface"//nl//"contains"//nl// &
            "    real(8) function scale(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        real(8), allocatable :: scratch"//nl// &
            "        allocate(scratch)"//nl// &
            "        scratch = value"//nl// &
            "        out = scratch"//nl// &
            "        deallocate(scratch)"//nl// &
            "    end function scale"//nl// &
            "    real(8) function apply(callback, value) result(out)"//nl// &
            "        procedure(callback_iface) :: callback"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = callback(value)"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(callback, x)"//nl// &
            "    end function kernel"//nl// &
            "end module passed_ownership_case"//nl
    end function ownership_source

    subroutine expect_refusal(text, label, reason)
        character(len=*), intent(in) :: text, label, reason
        type(fad_result_t) :: result

        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok) error stop "passed-procedure refusal accepted"
        if (.not. allocated(result%message)) error stop "unnamed passed-procedure refusal"
        if (index(result%message, reason) == 0) then
            print *, trim(label), ": ", trim(result%message)
            error stop "passed-procedure refusal lost its reason"
        end if
        if (allocated(result%code)) error stop "refusal emitted derivative code"
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

end program test_passed_procedure_callback_oracle
