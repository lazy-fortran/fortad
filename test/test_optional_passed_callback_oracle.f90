program test_optional_passed_callback_oracle
    !! Independent oracle for a fixed callback passed by keyword to an
    !! optional procedure dummy.  The callback is supplied, so FortFront's
    !! actual/formal mapping and exact target facts remain unconditional.
    !! Omitted callbacks are accepted only for one direct PRESENT guard;
    !! dynamic flow, aliases, globals, and ownership remain refusals.
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
    call require_ok(jvp, "JVP")
    call require_ok(vjp, "VJP")
    if (index(jvp%code, "callback") > 0 .or. &
        index(vjp%code, "callback") > 0) then
        error stop "optional procedure dummy survived lowering"
    end if

    dir = "build/oracle_optional_passed_callback"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create optional callback oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", &
        "module derivative_mod"//nl//"contains"//nl//jvp%code// &
        vjp%code//"end module derivative_mod"//nl)
    driver = positive_driver()
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
        "-Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "optional callback derivative did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "optional callback numerical oracle failed"
    end if

    call check_omitted()
    call expect_refusal(unsafe_omitted_source(), "unsafe omission", &
        "without an ELSE branch")
    call expect_refusal(reassigned_source(), "reassignment", "reassigned")
    call expect_refusal(global_source(), "global state", "global mutable")
    call expect_refusal(ownership_source(), "ownership", "ownership")

    print *, "test_optional_passed_callback_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "module optional_passed_callback_case"//nl// &
            "    implicit none"//nl// &
            "    abstract interface"//nl// &
            "        real(8) function callback_iface(value)"//nl// &
            "            real(8), intent(in) :: value"//nl// &
            "        end function callback_iface"//nl// &
            "    end interface"//nl//"contains"//nl// &
            "    real(8) function scale(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = 3.0d0*value"//nl// &
            "    end function scale"//nl// &
            "    real(8) function apply(value, callback) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        procedure(callback_iface), optional :: callback"//nl// &
            "        if (present(callback)) then"//nl// &
            "            out = callback(value) + value*value"//nl// &
            "        else"//nl// &
            "            out = value*value"//nl// &
            "        end if"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(value=x, callback=callback)"//nl// &
            "    end function kernel"//nl// &
            "end module optional_passed_callback_case"//nl
    end function positive_source

    function positive_driver() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use optional_passed_callback_case, only: kernel"//nl// &
            "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, y, yd, xb, yb, h, fp, fm"//nl// &
            "    x = 1.25d0"//nl// &
            "    xd = -0.4d0"//nl// &
            "    yb = 1.7d0"//nl// &
            "    call kernel_jvp(x, xd, y, yd)"//nl// &
            "    if (abs(y-5.3125d0) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(yd+2.2d0) > 1.0d-12) error stop 2"//nl// &
            "    h = 1.0d-6"//nl// &
            "    fp = kernel(x+h)"//nl//"    fm = kernel(x-h)"//nl// &
            "    if (abs(yd-xd*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
            "    call kernel_vjp(x, y, yb, xb)"//nl// &
            "    if (abs(xb-9.35d0) > 1.0d-12) error stop 4"//nl// &
            "    if (abs(yd*yb-xd*xb) > 1.0d-12) error stop 5"//nl// &
            "    print *, 'optional callback oracle pass'"//nl// &
            "end program driver"//nl
    end function positive_driver

    function common_prefix() result(text)
        character(len=:), allocatable :: text

        text = "module optional_callback_refusal_case"//nl// &
            "    implicit none"//nl// &
            "    abstract interface"//nl// &
            "        real(8) function callback_iface(value)"//nl// &
            "            real(8), intent(in) :: value"//nl// &
            "        end function callback_iface"//nl// &
            "    end interface"//nl//"contains"//nl// &
            "    real(8) function scale(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = 3.0d0*value"//nl// &
            "    end function scale"//nl// &
            "    real(8) function apply(value, callback) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        procedure(callback_iface), optional :: callback"//nl// &
            "        if (present(callback)) then"//nl// &
            "            out = callback(value)"//nl// &
            "        else"//nl// &
            "            out = value"//nl// &
            "        end if"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl
    end function common_prefix

    function omitted_source() result(text)
        character(len=:), allocatable :: text
        text = "module optional_callback_refusal_case"//nl// &
            "    implicit none"//nl// &
            "    abstract interface"//nl// &
            "        real(8) function callback_iface(value)"//nl// &
            "            real(8), intent(in) :: value"//nl// &
            "        end function callback_iface"//nl// &
            "    end interface"//nl//"contains"//nl// &
            "    real(8) function scale(value) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        out = 3.0d0*value"//nl// &
            "    end function scale"//nl// &
            "    real(8) function apply(value, callback) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        procedure(callback_iface), optional :: callback"//nl// &
            "        out = value"//nl// &
            "        if (present(callback)) then"//nl// &
            "            out = callback(value)"//nl// &
            "        end if"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(value=x)"//nl// &
            "    end function kernel"//nl// &
            "end module optional_callback_refusal_case"//nl
    end function omitted_source

    function unsafe_omitted_source() result(text)
        character(len=:), allocatable :: text
        text = common_prefix()// &
            "        callback => scale"//nl// &
            "        y = apply(value=x)"//nl// &
            "    end function kernel"//nl// &
            "end module optional_callback_refusal_case"//nl
    end function unsafe_omitted_source

    function reassigned_source() result(text)
        character(len=:), allocatable :: text
        text = common_prefix()// &
            "        callback => scale"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(value=x, callback=callback)"//nl// &
            "    end function kernel"//nl// &
            "end module optional_callback_refusal_case"//nl
    end function reassigned_source

    function global_source() result(text)
        character(len=:), allocatable :: text
        text = "module optional_callback_global_case"//nl// &
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
            "    real(8) function apply(value, callback) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        procedure(callback_iface), optional :: callback"//nl// &
            "        if (present(callback)) then"//nl// &
            "            out = callback(value)"//nl// &
            "        else"//nl// &
            "            out = value"//nl// &
            "        end if"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(value=x, callback=callback)"//nl// &
            "    end function kernel"//nl// &
            "end module optional_callback_global_case"//nl
    end function global_source

    function ownership_source() result(text)
        character(len=:), allocatable :: text
        text = "module optional_callback_ownership_case"//nl// &
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
            "    real(8) function apply(value, callback) result(out)"//nl// &
            "        real(8), intent(in) :: value"//nl// &
            "        procedure(callback_iface), optional :: callback"//nl// &
            "        if (present(callback)) then"//nl// &
            "            out = callback(value)"//nl// &
            "        else"//nl// &
            "            out = value"//nl// &
            "        end if"//nl// &
            "    end function apply"//nl// &
            "    real(8) function kernel(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        procedure(callback_iface), pointer :: callback"//nl// &
            "        callback => scale"//nl// &
            "        y = apply(value=x, callback=callback)"//nl// &
            "    end function kernel"//nl// &
            "end module optional_callback_ownership_case"//nl
    end function ownership_source

    subroutine check_omitted()
        type(fad_result_t) :: omitted_jvp, omitted_vjp
        character(len=:), allocatable :: omitted_source_text, omitted_dir
        character(len=:), allocatable :: omitted_driver
        integer :: omitted_stat

        omitted_source_text = omitted_source()
        omitted_jvp = fad_jvp(omitted_source_text, ["x"], from="kernel", &
            name="kernel_omitted_jvp")
        omitted_vjp = fad_vjp(omitted_source_text, ["x"], dependent="y", &
            from="kernel", name="kernel_omitted_vjp")
        call require_ok(omitted_jvp, "omitted JVP")
        call require_ok(omitted_vjp, "omitted VJP")
        if (index(omitted_jvp%code, "callback") > 0 .or. &
            index(omitted_vjp%code, "callback") > 0) then
            error stop "omitted callback survived lowering"
        end if

        omitted_dir = "build/oracle_optional_passed_callback_omitted"
        call execute_command_line("mkdir -p "//omitted_dir, &
            exitstat=omitted_stat)
        if (omitted_stat /= 0) error stop "could not create omitted callback oracle directory"
        call write_file(omitted_dir//"/primal.f90", omitted_source_text)
        call write_file(omitted_dir//"/derivatives.f90", &
            "module omitted_derivative_mod"//nl//"contains"//nl// &
            omitted_jvp%code//omitted_vjp%code// &
            "end module omitted_derivative_mod"//nl)
        omitted_driver = omitted_positive_driver()
        call write_file(omitted_dir//"/driver.f90", omitted_driver)
        call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
            "-Wextra -fimplicit-none -O2 -o "//omitted_dir//"/run "// &
            omitted_dir//"/primal.f90 "//omitted_dir//"/derivatives.f90 "// &
            omitted_dir//"/driver.f90 > "//omitted_dir//"/build.log 2>&1", &
            exitstat=omitted_stat)
        if (omitted_stat /= 0) then
            call show_file(omitted_dir//"/build.log")
            error stop "omitted callback derivative did not compile"
        end if
        call execute_command_line("./"//omitted_dir//"/run > "// &
            omitted_dir//"/out.txt 2>&1", exitstat=omitted_stat)
        if (omitted_stat /= 0) then
            call show_file(omitted_dir//"/out.txt")
            error stop "omitted callback numerical oracle failed"
        end if
    end subroutine check_omitted

    function omitted_positive_driver() result(text)
        character(len=:), allocatable :: text
        text = "program driver"//nl// &
            "    use optional_callback_refusal_case, only: kernel"//nl// &
            "    use omitted_derivative_mod, only: kernel_omitted_jvp, "// &
            "kernel_omitted_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, y, yd, xb, yb, h, fp, fm"//nl// &
            "    x = 1.25d0"//nl//"    xd = -0.4d0"//nl//"    yb = 1.7d0"//nl// &
            "    call kernel_omitted_jvp(x, xd, y, yd)"//nl// &
            "    if (abs(y-1.25d0) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(yd+0.4d0) > 1.0d-12) error stop 2"//nl// &
            "    h = 1.0d-6"//nl// &
            "    fp = kernel(x+h)"//nl//"    fm = kernel(x-h)"//nl// &
            "    if (abs(yd-xd*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
            "    call kernel_omitted_vjp(x, y, yb, xb)"//nl// &
            "    if (abs(xb-1.7d0) > 1.0d-12) error stop 4"//nl// &
            "    if (abs(yd*yb-xd*xb) > 1.0d-12) error stop 5"//nl// &
            "    print *, 'omitted optional callback oracle pass'"//nl// &
            "end program driver"//nl
    end function omitted_positive_driver

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, trim(label), ": ", result%message
            error stop "optional callback generation refused"
        end if
    end subroutine require_ok

    subroutine expect_refusal(text, label, reason)
        character(len=*), intent(in) :: text, label, reason
        type(fad_result_t) :: result
        result = fad_jvp(text, ["x"], from="kernel", name="kernel_jvp")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, reason) == 0) then
            print *, trim(label), ": ", result%message
            error stop "optional callback refusal was not precise"
        end if
        result = fad_vjp(text, ["x"], dependent="y", from="kernel", &
            name="kernel_vjp")
        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, reason) == 0) then
            print *, trim(label), " VJP: ", result%message
            error stop "optional callback VJP refusal was not precise"
        end if
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

end program test_optional_passed_callback_oracle
