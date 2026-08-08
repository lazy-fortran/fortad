program test_polymorphic_nested_ownership_oracle
    !! Independent hand and finite-difference oracle for one bounded nested
    !! polymorphic ownership case and its intentional boundaries.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, scalar_source, dir, tangent, driver
    type(fad_result_t) :: generated, generated_reverse, refused
    integer :: unit, stat

    source = nested_array_source()
    scalar_source = nested_source("allocate(box%field%payload, source=child)")
    generated = fad_jvp(source, [character(len=1) :: "x"], from="evaluate", &
        name="evaluate_jvp")
    if (.not. generated%ok) then
        print *, "FAIL nested polymorphic ownership JVP: ", generated%message
        error stop 1
    end if
    generated_reverse = fad_vjp(scalar_source, [character(len=1) :: "x"], &
        dependent="y", from="evaluate", name="evaluate_vjp")
    if (.not. generated_reverse%ok) then
        print *, "FAIL nested polymorphic ownership VJP: ", &
            generated_reverse%message
        error stop 13
    end if

    refused = fad_jvp( &
        nested_source("allocate(box%field%payload, source=make_child(x))"), &
        [character(len=1) :: "x"], from="evaluate")
    call require_refusal(refused, "factory source")

    refused = fad_jvp(nested_source( &
        "allocate(box%field%payload, source=child)"//nl// &
        "        deallocate(box%field%payload)"//nl// &
        "        allocate(box%field%payload, source=child)"), &
        [character(len=1) :: "x"], from="evaluate")
    call require_refusal(refused, "repeated acquisition")

    refused = fad_jvp(alias_source(), [character(len=1) :: "x"], &
        from="evaluate")
    call require_refusal(refused, "alias")

    refused = fad_jvp(move_source(), [character(len=1) :: "x"], &
        from="evaluate")
    call require_refusal(refused, "move_alloc")

    refused = fad_vjp(nested_array_source(), [character(len=1) :: "x"], &
        dependent="y", from="evaluate")
    call require_refusal(refused, "reverse array allocation replay")
    if (index(refused%message, "array-element polymorphic component") == 0 .or. &
        index(refused%message, "SOURCE= ownership") == 0) then
        print *, "FAIL reverse array nested ownership refusal was not precise: ", &
            refused%message
        error stop 10
    end if

    dir = "build/oracle/polymorphic_nested_ownership"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 2
    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') source
    close (unit)
    tangent = "module nested_generated"//nl// &
        "    use nested_ownership_case, only: child_t, holder_t"//nl// &
        "contains"//nl//generated%code// &
        generated_reverse%code// &
        "end module nested_generated"//nl
    open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
        action="write")
    write (unit, '(a)') tangent
    close (unit)
    driver = "program driver"//nl// &
        "    use nested_ownership_case, only: evaluate"//nl// &
        "    use nested_generated, only: evaluate_jvp, evaluate_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, y, yd, yb, xb, h, fd"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.4d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    call evaluate_jvp(x, xd, y, yd)"//nl// &
        "    if (abs(y - 2.0d0*x*x) > 1.0d-13) error stop 3"//nl// &
        "    if (abs(yd - 4.0d0*x*xd) > 1.0d-13) error stop 4"//nl// &
        "    fd = (evaluate(x+h)-evaluate(x-h))/(2.0d0*h)"//nl// &
        "    if (abs(yd/xd-fd) > 1.0d-7) error stop 5"//nl// &
        "    yb = -0.7d0"//nl// &
        "    call evaluate_vjp(x, y, yb, xb)"//nl// &
        "    if (abs(xb-yb*4.0d0*x) > 1.0d-12) error stop 12"//nl// &
        "    if (abs(xb*xd-yb*yd) > 1.0d-12) error stop 14"//nl// &
        "    if (abs(xb/yb-fd) > 1.0d-7) error stop 15"//nl// &
        "    print *, 'nested polymorphic ownership oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)
    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/tangent.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL nested generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 6
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL nested independent oracle"
        call show_file(dir//"/out.txt")
        error stop 7
    end if
    print *, "test_polymorphic_nested_ownership_oracle: all cases passed"

contains

    function nested_source(allocation) result(text)
        character(len=*), intent(in) :: allocation
        character(len=:), allocatable :: text

        text = "module nested_ownership_case"//nl// &
            "    implicit none"//nl// &
            "    type :: base_t"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    end type child_t"//nl// &
            "    type :: field_t"//nl// &
            "        class(base_t), allocatable :: payload"//nl// &
            "    end type field_t"//nl// &
            "    type :: holder_t"//nl// &
            "        type(field_t) :: field"//nl// &
            "    end type holder_t"//nl// &
            "contains"//nl// &
            "    pure function make_child(x) result(child)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(child_t) :: child"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "    end function make_child"//nl// &
            "    pure function evaluate(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        type(child_t) :: child"//nl// &
            "        type(holder_t) :: box"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "        "//trim(allocation)//nl// &
            "        select type (owner => box%field%payload)"//nl// &
            "        type is (child_t)"//nl// &
            "            y = owner%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "        deallocate(box%field%payload)"//nl// &
            "    end function evaluate"//nl// &
            "end module nested_ownership_case"//nl
    end function nested_source

    function nested_array_source() result(text)
        character(len=:), allocatable :: text

        text = nested_source( &
            "allocate(box%field%payload, source=child)")
        text = replace_text(text, "        type(holder_t) :: box"//nl, &
            "        type(holder_t) :: holders(2)"//nl)
        text = replace_text(text, "box%field%payload", &
            "holders(2)%field%payload")
        text = replace_text(text, "box%field%payload", &
            "holders(2)%field%payload")
        text = replace_text(text, "box%field%payload", &
            "holders(2)%field%payload")
    end function nested_array_source

    function alias_source() result(text)
        character(len=:), allocatable :: text

        text = nested_source( &
            "allocate(alias%field%payload, source=child)")
        text = replace_text(text, "        type(holder_t) :: box"//nl, &
            "        type(holder_t), target :: box"//nl// &
            "        type(holder_t), pointer :: alias"//nl// &
            "        alias => box"//nl)
    end function alias_source

    function move_source() result(text)
        character(len=:), allocatable :: text

        text = nested_source( &
            "allocate(box%field%payload, source=child)"//nl// &
            "        call move_alloc(box%field%payload, "// &
            "other%field%payload)")
        text = replace_text(text, "        type(holder_t) :: box"//nl, &
            "        type(holder_t) :: box, other"//nl)
    end function move_source

    function replace_text(text, old, new) result(out)
        character(len=*), intent(in) :: text, old, new
        character(len=:), allocatable :: out
        integer :: at

        at = index(text, old)
        if (at == 0) then
            out = text
        else
            out = text(:at - 1)//new//text(at + len(old):)
        end if
    end function replace_text

    subroutine require_refusal(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (result%ok) then
            print *, "FAIL ", trim(label), " was accepted"
            error stop 8
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL ", trim(label), " had no diagnostic"
            error stop 9
        end if
    end subroutine require_refusal

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

end program test_polymorphic_nested_ownership_oracle
