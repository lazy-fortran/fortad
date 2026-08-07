program test_keyword_call_oracle
    !! Independent oracle for keyword actual-to-formal mapping during inlining.
    !! The helper receives coefficient by keyword in a different order and is
    !! also called without that passive optional.
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
        "pure function kernel(x) result(y)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8) :: y"//nl// &
        "    y = evaluate_optional(coefficient=4.0d0, x=x)"//nl// &
        "    y = y + evaluate_optional(x=x)"//nl// &
        "end function kernel"//nl
    type(fad_result_t) :: jvp, vjp
    integer :: failures, unit, stat
    character(len=:), allocatable :: dir, driver

    failures = 0
    dir = "build/oracle_keyword_call"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create keyword-call oracle directory"

    jvp = fad_jvp(source, ["x"], name="kernel_jvp", from="kernel")
    vjp = fad_vjp(source, ["x"], name="kernel_vjp", from="kernel")
    if (.not. jvp%ok) then
        print *, "FAIL keyword-call JVP generation: ", jvp%message
        failures = failures + 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL keyword-call VJP generation: ", vjp%message
        failures = failures + 1
    end if
    if (failures > 0) error stop 1

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module primal_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module primal_mod"
    close (unit)

    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module derivative_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module derivative_mod"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use primal_mod, only: kernel"//nl// &
        "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, coefficient, y, yd, xb, h, fp, fm"//nl// &
        "    real(8) :: dx, yh, ydh, x_b_expected"//nl// &
        "    x = 2.0d0"//nl// &
        "    coefficient = 4.0d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    dx = 0.75d0"//nl// &
        "    call kernel_jvp(x=x, x_d=dx, y=y, y_d=yd)"//nl// &
        "    call hand_jvp(x, dx, yh, ydh)"//nl// &
        "    call hand_jvp(x, dx, fp, fm, coefficient)"//nl// &
        "    yh = yh + fp"//nl// &
        "    ydh = ydh + fm"//nl// &
        "    call check_close(y, yh, 'absent primal')"//nl// &
        "    call check_close(yd, ydh, 'absent JVP')"//nl// &
        "    fp = kernel(x+h); fm = kernel(x-h)"//nl// &
        "    call check_close(yd, dx*(fp-fm)/(2.0d0*h), 'absent finite difference')"//nl// &
        "    call kernel_vjp(x=x, y=y, y_b=1.0d0, x_b=xb)"//nl// &
        "    x_b_expected = 6.0d0"//nl// &
        "    call check_close(xb, x_b_expected, 'absent VJP')"//nl// &
        "    call check_close(yd, dx*xb, 'absent adjoint identity')"//nl// &
        "    call kernel_jvp(y_d=yd, y=y, x_d=dx, x=x)"//nl// &
        "    call check_close(y, yh, 'reordered-call primal')"//nl// &
        "    call check_close(yd, ydh, 'reordered-call JVP')"//nl// &
        "    fp = kernel(x+h); fm = kernel(x-h)"//nl// &
        "    call check_close(yd, dx*(fp-fm)/(2.0d0*h), 'reordered-call finite difference')"//nl// &
        "    call kernel_vjp(x_b=xb, y_b=1.0d0, y=y, x=x)"//nl// &
        "    x_b_expected = 6.0d0"//nl// &
        "    call check_close(xb, x_b_expected, 'reordered-call VJP')"//nl// &
        "    call check_close(yd, dx*xb, 'reordered-call adjoint identity')"//nl// &
        "    print *, 'keyword call oracle pass'"//nl// &
        "contains"//nl// &
        "    subroutine hand_jvp(x, x_d, y, y_d, coefficient)"//nl// &
        "        real(8), intent(in) :: x, x_d"//nl// &
        "        real(8), intent(out) :: y, y_d"//nl// &
        "        real(8), intent(in), optional :: coefficient"//nl// &
        "        y = x"//nl// &
        "        y_d = x_d"//nl// &
        "        if (present(coefficient)) then"//nl// &
        "            y = y + x*coefficient"//nl// &
        "            y_d = y_d + x_d*coefficient"//nl// &
        "        end if"//nl// &
        "    end subroutine hand_jvp"//nl// &
        "    subroutine check_close(actual, expected, label)"//nl// &
        "        real(8), intent(in) :: actual, expected"//nl// &
        "        character(len=*), intent(in) :: label"//nl// &
        "        if (abs(actual-expected) > 1.0d-7) error stop label"//nl// &
        "    end subroutine check_close"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
        "-Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL keyword-call: generated code did not compile strictly"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL keyword-call: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_keyword_call_oracle: all cases passed"

contains

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

end program test_keyword_call_oracle
