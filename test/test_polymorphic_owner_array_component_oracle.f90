program test_polymorphic_owner_array_component_oracle
    !! Independent numerical oracle for a fixed-source polymorphic owner array
    !! whose selected concrete component is written and then read.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, generated, driver, dir
    type(fad_result_t) :: jvp, vjp
    integer :: stat

    source = case_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], from="evaluate", &
        name="evaluate_jvp")
    call require_ok(jvp, "owner-array component JVP")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        from="evaluate", name="evaluate_vjp")
    call require_ok(vjp, "owner-array component VJP")

    dir = "build/oracle/polymorphic_owner_array_component"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create owner-array component oracle directory"
    call write_file(dir//"/primal.f90", source)
    generated = "module polymorphic_owner_array_component_derivatives"//nl// &
        "    use polymorphic_owner_array_component_case, only: base_t, child_t"//nl// &
        "contains"//nl//jvp%code//vjp%code// &
        "end module polymorphic_owner_array_component_derivatives"//nl
    call write_file(dir//"/derivatives.f90", generated)
    driver = &
        "program driver"//nl// &
        "    use polymorphic_owner_array_component_case, only: evaluate"//nl// &
        "    use polymorphic_owner_array_component_derivatives, only: "// &
        "evaluate_jvp, evaluate_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, x_d, y, y_d, y_b, x_b, h, fd"//nl// &
        "    x = 1.25d0"//nl// &
        "    x_d = -0.4d0"//nl// &
        "    y_b = -0.7d0"//nl// &
        "    call evaluate_jvp(x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 3.0d0*x*x) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - 6.0d0*x*x_d) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fd = (evaluate(x+h*x_d)-evaluate(x-h*x_d))/(2.0d0*h)"//nl// &
        "    if (abs(y_d - fd) > 1.0d-7) error stop 4"//nl// &
        "    call evaluate_vjp(x, y, y_b, x_b)"//nl// &
        "    if (abs(x_b - y_b*6.0d0*x) > 1.0d-12) error stop 5"//nl// &
        "    if (abs(y_b*y_d - x_b*x_d) > 1.0d-12) error stop 6"//nl// &
        "    print *, 'polymorphic owner-array component oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "generated owner-array component derivative did not compile"
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop "owner-array component numerical oracle failed"
    end if
    print *, "test_polymorphic_owner_array_component_oracle: all cases passed"

contains

    function case_source() result(text)
        character(len=:), allocatable :: text

        text = "module polymorphic_owner_array_component_case"//nl// &
            "    implicit none"//nl// &
            "    type :: base_t"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    end type child_t"//nl// &
            "contains"//nl// &
            "    pure function evaluate(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        type(child_t) :: child"//nl// &
            "        class(base_t), allocatable :: owners(:)"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "        allocate(owners(2), source=child)"//nl// &
            "        select type (item => owners(2))"//nl// &
            "        type is (child_t)"//nl// &
            "            item%scale = 3.0d0*x"//nl// &
            "            y = item%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "        deallocate(owners)"//nl// &
            "    end function evaluate"//nl// &
            "end module polymorphic_owner_array_component_case"//nl
    end function case_source

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

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
            print '(a)', trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_polymorphic_owner_array_component_oracle
