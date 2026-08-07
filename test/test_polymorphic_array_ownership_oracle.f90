program test_polymorphic_array_ownership_oracle
    !! Independent hand and finite-difference oracle for one fixed-size array
    !! of concrete holders with a polymorphic component owner.  Reverse mode
    !! is checked for its precise bounded-lifetime refusal.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, tangent, driver, dir
    type(fad_result_t) :: generated, refused
    integer :: unit, stat

    source = array_source()
    generated = fad_jvp(source, [character(len=1) :: "x"], from="evaluate", &
        name="evaluate_jvp")
    if (.not. generated%ok) then
        print *, "FAIL array polymorphic ownership JVP: ", generated%message
        error stop 1
    end if

    refused = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        from="evaluate", name="evaluate_vjp")
    if (refused%ok .or. .not. allocated(refused%message) .or. &
        index(refused%message, "array-element polymorphic component") == 0 .or. &
        index(refused%message, "SOURCE= ownership") == 0) then
        print *, "FAIL array polymorphic ownership reverse refusal: ", &
            refused%message
        error stop 2
    end if

    dir = "build/oracle/polymorphic_array_ownership"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 3

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') source
    close (unit)
    tangent = "module array_ownership_generated"//nl// &
        "    use array_ownership_case, only: child_t, holder_t"//nl// &
        "contains"//nl//generated%code// &
        "end module array_ownership_generated"//nl
    open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
        action="write")
    write (unit, '(a)') tangent
    close (unit)
    driver = "program driver"//nl// &
        "    use array_ownership_case, only: evaluate"//nl// &
        "    use array_ownership_generated, only: evaluate_jvp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, y, yd, h, fd"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.4d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    call evaluate_jvp(x, xd, y, yd)"//nl// &
        "    if (abs(y - 2.0d0*x*x) > 1.0d-13) error stop 4"//nl// &
        "    if (abs(yd - 4.0d0*x*xd) > 1.0d-13) error stop 5"//nl// &
        "    fd = (evaluate(x + h*xd) - evaluate(x - h*xd))/(2.0d0*h)"//nl// &
        "    if (abs(yd - fd) > 1.0d-7) error stop 6"//nl// &
        "    print *, 'polymorphic array ownership oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/tangent.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL array polymorphic generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 7
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL array polymorphic independent oracle"
        call show_file(dir//"/out.txt")
        error stop 8
    end if
    print *, "test_polymorphic_array_ownership_oracle: all cases passed"

contains

    function array_source() result(text)
        character(len=:), allocatable :: text

        text = "module array_ownership_case"//nl// &
            "    implicit none"//nl// &
            "    type :: base_t"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    end type child_t"//nl// &
            "    type :: holder_t"//nl// &
            "        class(base_t), allocatable :: payload"//nl// &
            "    end type holder_t"//nl// &
            "contains"//nl// &
            "    pure function evaluate(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        type(child_t) :: concrete_child"//nl// &
            "        type(holder_t) :: holders(2)"//nl// &
            "        concrete_child%scale = 2.0d0*x"//nl// &
            "        allocate(holders(2)%payload, source=concrete_child)"//nl// &
            "        select type (item => holders(2)%payload)"//nl// &
            "        type is (child_t)"//nl// &
            "            y = item%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "        deallocate(holders(2)%payload)"//nl// &
            "    end function evaluate"//nl// &
            "end module array_ownership_case"//nl
    end function array_source

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: input, ios

        open (newunit=input, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (input, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print '(a)', trim(line)
        end do
        close (input)
    end subroutine show_file

end program test_polymorphic_array_ownership_oracle
