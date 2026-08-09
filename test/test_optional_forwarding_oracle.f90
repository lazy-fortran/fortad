program test_optional_forwarding_oracle
    !! Optional-to-optional forwarding must preserve the runtime PRESENT bit.
    !! The forwarding call is keyword-reordered so the same case exercises
    !! actual/formal mapping before both JVP and VJP generation.  The active
    !! optional VJP path is checked on the same present/omitted calls.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "pure function evaluate_optional(x, coefficient) result(y)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(in), optional :: coefficient"//nl// &
        "    real(8) :: y"//nl// &
        "    y = x"//nl// &
        "    if (present(coefficient)) y = y + x*coefficient"//nl// &
        "end function evaluate_optional"//nl// &
        "pure function kernel(x, coefficient) result(y)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(in), optional :: coefficient"//nl// &
        "    real(8) :: y"//nl// &
        "    y = evaluate_optional(coefficient=coefficient, x=x)"//nl// &
        "end function kernel"//nl
    type(fad_result_t) :: jvp, vjp, active_vjp
    integer :: unit, stat
    character(len=:), allocatable :: dir, driver

    dir = "build/oracle_optional_forwarding"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create optional-forwarding oracle directory"

    jvp = fad_jvp(source, ["x"], name="kernel_jvp", from="kernel")
    vjp = fad_vjp(source, ["x"], name="kernel_vjp", from="kernel")
    if (.not. jvp%ok) error stop "optional-forwarding JVP generation failed"
    if (.not. vjp%ok) error stop "optional-forwarding VJP generation failed"

    active_vjp = fad_vjp(source, ["coefficient"], name="active_vjp", from="kernel")
    if (.not. active_vjp%ok) error stop "active optional VJP generation failed"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') "module primal_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module primal_mod"
    close (unit)

    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", action="write")
    write (unit, '(a)') "module derivative_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') active_vjp%code
    write (unit, '(a)') "end module derivative_mod"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use primal_mod, only: kernel"//nl// &
        "    use derivative_mod, only: kernel_jvp, kernel_vjp, active_vjp"//nl// &
        "    implicit none"//nl// &
        "    call check_case(.false.)"//nl// &
        "    call check_case(.true.)"//nl// &
        "    print *, 'optional forwarding oracle pass'"//nl// &
        "contains"//nl// &
        "    subroutine check_case(has_coefficient)"//nl// &
        "        logical, intent(in) :: has_coefficient"//nl// &
        "        real(8) :: x, coefficient, y, y_hand, yd, yd_hand"//nl// &
        "        real(8) :: xb, xb_hand, coefficient_b, h"//nl// &
        "        real(8) :: fp, fm"//nl// &
        "        x = 2.0d0"//nl// &
        "        coefficient = 4.0d0"//nl// &
        "        h = 1.0d-6"//nl// &
        "        call hand_jvp(x, 0.75d0, coefficient, has_coefficient, y_hand, yd_hand)"//nl// &
        "        if (has_coefficient) then"//nl// &
        "            call kernel_jvp(y_d=yd, coefficient=coefficient, y=y, x_d=0.75d0, x=x)"//nl// &
        "            fp = kernel(coefficient=coefficient, x=x+h)"//nl// &
        "            fm = kernel(coefficient=coefficient, x=x-h)"//nl// &
        "        else"//nl// &
        "            call kernel_jvp(y_d=yd, y=y, x_d=0.75d0, x=x)"//nl// &
        "            fp = kernel(x+h)"//nl// &
        "            fm = kernel(x-h)"//nl// &
        "        end if"//nl// &
        "        call check_close(y, y_hand, 'forward primal')"//nl// &
        "        call check_close(yd, yd_hand, 'hand JVP')"//nl// &
        "        call check_close(yd, 0.75d0*(fp-fm)/(2.0d0*h), 'central finite difference')"//nl// &
        "        call hand_jvp(x, 1.0d0, coefficient, has_coefficient, y_hand, xb_hand)"//nl// &
        "        if (has_coefficient) then"//nl// &
        "            call kernel_vjp(x_b=xb, y_b=1.0d0, y=y, coefficient=coefficient, x=x)"//nl// &
        "        else"//nl// &
        "            call kernel_vjp(x_b=xb, y_b=1.0d0, y=y, x=x)"//nl// &
        "        end if"//nl// &
        "        call check_close(y, y_hand, 'reverse primal')"//nl// &
        "        call check_close(xb, xb_hand, 'hand VJP')"//nl// &
        "        call check_close(0.75d0*xb, yd, 'adjoint identity')"//nl// &
        "        if (has_coefficient) then"//nl// &
        "            call active_vjp(x=x, coefficient=coefficient, y=y, "// &
        "y_b=1.0d0, coefficient_b=coefficient_b)"//nl// &
        "        else"//nl// &
        "            call active_vjp(x=x, y=y, y_b=1.0d0, "// &
        "coefficient_b=coefficient_b)"//nl// &
        "        end if"//nl// &
        "        if (has_coefficient) then"//nl// &
        "            call check_close(coefficient_b, x, 'active optional VJP')"//nl// &
        "        else"//nl// &
        "            call check_close(coefficient_b, 0.0d0, 'omitted optional VJP')"//nl// &
        "        end if"//nl// &
        "    end subroutine check_case"//nl// &
        "    subroutine hand_jvp(x, xd, coefficient, has_coefficient, y, yd)"//nl// &
        "        real(8), intent(in) :: x, xd, coefficient"//nl// &
        "        logical, intent(in) :: has_coefficient"//nl// &
        "        real(8), intent(out) :: y, yd"//nl// &
        "        y = x"//nl// &
        "        yd = xd"//nl// &
        "        if (has_coefficient) then"//nl// &
        "            y = y + x*coefficient"//nl// &
        "            yd = yd + xd*coefficient"//nl// &
        "        end if"//nl// &
        "    end subroutine hand_jvp"//nl// &
        "    subroutine check_close(actual, expected, label)"//nl// &
        "        real(8), intent(in) :: actual, expected"//nl// &
        "        character(len=*), intent(in) :: label"//nl// &
        "        if (abs(actual-expected) > 1.0d-7) error stop label"//nl// &
        "    end subroutine check_close"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall -Wextra "// &
        "-fimplicit-none -O2 -o "//dir//"/run "//dir//"/primal.f90 "// &
        dir//"/derivatives.f90 "//dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL optional-forwarding: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL optional-forwarding: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_optional_forwarding_oracle: all cases passed"

contains

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

end program test_optional_forwarding_oracle
