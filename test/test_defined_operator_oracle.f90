program test_defined_operator_oracle
    !! Independent numerical oracle for a bounded same-file defined operator.
    !! The refusal cases verify that FortAD consumes FortFront's exact
    !! selection facts instead of guessing conversions or mutable state.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module defined_operator_case"//nl// &
        "    implicit none"//nl// &
        "    interface operator(.blend.)"//nl// &
        "        module procedure blend_real"//nl// &
        "    end interface"//nl// &
        "contains"//nl// &
        "    pure real(8) function blend_real(left, right) result(value)"//nl// &
        "        real(8), intent(in) :: left, right"//nl// &
        "        value = left*left + 2.0d0*right"//nl// &
        "    end function blend_real"//nl// &
        "    pure subroutine kernel(x, y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(out) :: y"//nl// &
        "        y = x .blend. 3.0d0"//nl// &
        "    end subroutine kernel"//nl// &
        "end module defined_operator_case"//nl

    character(len=*), parameter :: ambiguous_source = &
        "module defined_operator_ambiguous_case"//nl// &
        "    implicit none"//nl// &
        "    interface operator(.amb.)"//nl// &
        "        module procedure choose_left, choose_right"//nl// &
        "    end interface"//nl// &
        "contains"//nl// &
        "    pure real(8) function choose_left(left, right) result(value)"//nl// &
        "        real(8), intent(in) :: left, right"//nl// &
        "        value = left + right"//nl// &
        "    end function choose_left"//nl// &
        "    pure real(8) function choose_right(left, right) result(value)"//nl// &
        "        real(8), intent(in) :: left, right"//nl// &
        "        value = left - right"//nl// &
        "    end function choose_right"//nl// &
        "    pure real(8) function top(x) result(y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = x .amb. 1.0d0"//nl// &
        "    end function top"//nl// &
        "end module defined_operator_ambiguous_case"//nl

    character(len=*), parameter :: conversion_source = &
        "module defined_operator_conversion_case"//nl// &
        "    implicit none"//nl// &
        "    interface operator(.conv.)"//nl// &
        "        module procedure convert_real"//nl// &
        "    end interface"//nl// &
        "contains"//nl// &
        "    pure real(8) function convert_real(left, right) result(value)"//nl// &
        "        real(8), intent(in) :: left, right"//nl// &
        "        value = left + right"//nl// &
        "    end function convert_real"//nl// &
        "    pure real(8) function top(value) result(out)"//nl// &
        "        integer, intent(in) :: value"//nl// &
        "        out = value .conv. 2.0d0"//nl// &
        "    end function top"//nl// &
        "end module defined_operator_conversion_case"//nl

    character(len=*), parameter :: global_source = &
        "module defined_operator_global_case"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: shared"//nl// &
        "    interface operator(.global.)"//nl// &
        "        module procedure add_shared"//nl// &
        "    end interface"//nl// &
        "contains"//nl// &
        "    pure real(8) function add_shared(left, right) result(value)"//nl// &
        "        real(8), intent(in) :: left, right"//nl// &
        "        value = left + right + shared"//nl// &
        "    end function add_shared"//nl// &
        "    pure real(8) function top(x) result(y)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = x .global. 1.0d0"//nl// &
        "    end function top"//nl// &
        "end module defined_operator_global_case"//nl

    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: stat

    jvp = fad_jvp(source, [character(len=1) :: "x"], from="kernel", &
        name="kernel_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL defined-operator JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        from="kernel", name="kernel_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL defined-operator VJP generation: ", vjp%message
        error stop 2
    end if
    if (index(jvp%code, "blend_real(") > 0) then
        error stop "defined operator JVP was not inlined"
    end if
    if (index(vjp%code, "blend_real(") > 0) then
        error stop "defined operator VJP was not inlined"
    end if

    call expect_refusal(ambiguous_source, "ambiguous", "ambiguous")
    call expect_refusal(conversion_source, "conversion", "conversion")
    call expect_refusal(global_source, "global", "global")

    dir = "build/oracle_defined_operator"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create defined-operator oracle directory"
    call write_file(dir//"/primal.f90", source)
    call write_file(dir//"/derivatives.f90", "module derivative_mod"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code//nl// &
        "end module derivative_mod"//nl)
    driver = "program driver"//nl// &
        "    use defined_operator_case, only: kernel"//nl// &
        "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, x_d, y, y_d, x_b, y_b, h, fp, fm"//nl// &
        "    x = 1.5d0"//nl// &
        "    x_d = -0.7d0"//nl// &
        "    y_b = 1.3d0"//nl// &
        "    call kernel_jvp(x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 8.25d0) > 1.0d-12) error stop 3"//nl// &
        "    if (abs(y_d - 2.0d0*x*x_d) > 1.0d-12) error stop 4"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = (x+h)*(x+h) + 6.0d0"//nl// &
        "    fm = (x-h)*(x-h) + 6.0d0"//nl// &
        "    if (abs(y_d - x_d*(fp-fm)/(2.0d0*h)) > 1.0d-7) error stop 5"//nl// &
        "    call kernel_vjp(x, y, y_b, x_b)"//nl// &
        "    if (abs(x_b - 2.0d0*x*y_b) > 1.0d-12) error stop 6"//nl// &
        "    if (abs(y_d*y_b - x_d*x_b) > 1.0d-12) error stop 7"//nl// &
        "    print *, 'defined-operator numerical oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -pedantic-errors -Wall "// &
        "-Wextra -fimplicit-none -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated defined-operator source did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "defined-operator behavioral oracle failed"
    end if
    print *, "test_defined_operator_oracle: all cases passed"

contains

    subroutine expect_refusal(case_source, label, needle)
        character(len=*), intent(in) :: case_source, label, needle
        type(fad_result_t) :: refused

        refused = fad_jvp(case_source, [character(len=1) :: "x"], &
            from="top", name="top_jvp")
        if (label == "conversion") then
            refused = fad_jvp(case_source, [character(len=5) :: "value"], &
                from="top", name="top_jvp")
        end if
        if (refused%ok) error stop "unsafe defined operator was accepted"
        if (.not. allocated(refused%message)) then
            error stop "defined operator refusal was unnamed"
        end if
        if (index(refused%message, needle) == 0) then
            print *, "unexpected ", trim(label), " refusal: ", refused%message
            error stop "defined operator refusal lost its reason"
        end if
        if (allocated(refused%code)) error stop "refused operator emitted code"
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

end program test_defined_operator_oracle
