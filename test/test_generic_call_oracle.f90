program test_generic_call_oracle
    !! Independent oracle for one exact same-file generic and one refusal.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module generic_call_case"//nl// &
        "    implicit none"//nl// &
        "    interface scale"//nl// &
        "        module procedure scale_integer, scale_real"//nl// &
        "    end interface scale"//nl// &
        "contains"//nl// &
        "    pure integer function scale_integer(value, offset) result(out)"//nl// &
        "        integer, intent(in) :: value"//nl// &
        "        integer, intent(in), optional :: offset"//nl// &
        "        out = 2*value"//nl// &
        "        if (present(offset)) out = out + offset"//nl// &
        "    end function scale_integer"//nl// &
        "    pure real(8) function scale_real(value, offset) result(out)"//nl// &
        "        real(8), intent(in) :: value"//nl// &
        "        real(8), intent(in), optional :: offset"//nl// &
        "        out = 3.0d0*value"//nl// &
        "        if (present(offset)) out = out + offset"//nl// &
        "    end function scale_real"//nl// &
        "    pure real(8) function kernel(x) result(y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        integer :: integer_value"//nl// &
        "        integer_value = 4"//nl// &
        "        y = scale(value=integer_value, offset=2) + "// &
        "scale(offset=2.0d0, value=x)"//nl// &
        "    end function kernel"//nl// &
        "end module generic_call_case"//nl
    character(len=*), parameter :: ambiguous_source = &
        "module ambiguous_generic_case"//nl// &
        "    implicit none"//nl// &
        "    interface choose"//nl// &
        "        module procedure choose_left, choose_right"//nl// &
        "    end interface choose"//nl// &
        "contains"//nl// &
        "    pure real(8) function choose_left(value) result(out)"//nl// &
        "        real(8), intent(in) :: value"//nl// &
        "        out = value"//nl// &
        "    end function choose_left"//nl// &
        "    pure real(8) function choose_right(value) result(out)"//nl// &
        "        real(8), intent(in) :: value"//nl// &
        "        out = 2.0d0*value"//nl// &
        "    end function choose_right"//nl// &
        "    pure real(8) function top(x) result(y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = choose(x)"//nl// &
        "    end function top"//nl// &
        "end module ambiguous_generic_case"//nl

    type(fad_result_t) :: jvp, vjp, refused
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    jvp = fad_jvp(source, ["x"], from="kernel", name="kernel_jvp")
    vjp = fad_vjp(source, ["x"], dependent="y", from="kernel", name="kernel_vjp")
    if (.not. jvp%ok) error stop "exact generic JVP was refused"
    if (.not. vjp%ok) error stop "exact generic VJP was refused"
    if (index(jvp%code, "scale(") > 0) error stop "generic JVP was not inlined"
    if (index(vjp%code, "scale(") > 0) error stop "generic VJP was not inlined"

    refused = fad_jvp(ambiguous_source, ["x"], from="top", name="top_jvp")
    if (refused%ok) error stop "ambiguous generic call was accepted"
    if (.not. allocated(refused%message)) error stop "ambiguous refusal was unnamed"
    if (index(refused%message, "ambiguous generic call") == 0) then
        error stop "ambiguous refusal lost its reason"
    end if
    if (allocated(refused%code)) error stop "ambiguous call produced derivative output"
    refused = fad_vjp(ambiguous_source, ["x"], dependent="y", from="top", &
        name="top_vjp")
    if (refused%ok) error stop "ambiguous generic VJP was accepted"
    if (.not. allocated(refused%message)) error stop "ambiguous VJP refusal was unnamed"
    if (index(refused%message, "ambiguous generic call") == 0) then
        error stop "ambiguous VJP refusal lost its reason"
    end if
    if (allocated(refused%code)) error stop "ambiguous VJP produced derivative output"

    dir = "build/oracle_generic_call"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create generic-call oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", "module derivative_mod"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code//nl//"end module derivative_mod"//nl)

    driver = "program driver"//nl// &
        "    use generic_call_case, only: kernel"//nl// &
        "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, y, yd, xb, yb, h, fp, fm"//nl// &
        "    x = 1.5d0"//nl// &
        "    xd = -0.7d0"//nl// &
        "    yb = 1.3d0"//nl// &
        "    call kernel_jvp(x, xd, y, yd)"//nl// &
        "    if (abs(y - 16.5d0) > 1.0d-12) error stop 1"//nl// &
        "    if (abs(yd - 3.0d0*xd) > 1.0d-12) error stop 2"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = kernel(x+h); fm = kernel(x-h)"//nl// &
        "    if (abs(yd - xd*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
        "    call kernel_vjp(x, y, yb, xb)"//nl// &
        "    if (abs(xb - 3.0d0*yb) > 1.0d-12) error stop 4"//nl// &
        "    if (abs(yd*yb - xd*xb) > 1.0d-12) error stop 5"//nl// &
        "    print *, 'generic call oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)

    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
        "-Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated generic-call source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "generic-call behavioral oracle failed"
    end if
    print *, "test_generic_call_oracle: all cases passed"

contains

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

end program test_generic_call_oracle
