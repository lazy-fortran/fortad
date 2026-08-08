program test_polymorphic_unlimited_assignment_oracle
    !! Independent compiler and derivative oracle for a fixed-source scalar
    !! CLASS(*) component assignment inside one TYPE IS arm.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, ambiguous, lifetime
    character(len=:), allocatable :: dir, generated, driver
    type(fad_result_t) :: jvp, vjp, refused
    integer :: unit, stat

    source = unlimited_assignment_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], from="evaluate", &
        name="evaluate_jvp")
    call require_ok(jvp, "unlimited polymorphic JVP")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        from="evaluate", name="evaluate_vjp")
    call require_ok(vjp, "unlimited polymorphic VJP")

    ambiguous = unlimited_ambiguous_source()
    refused = fad_jvp(ambiguous, [character(len=1) :: "x"], &
        from="evaluate")
    call require_refusal(refused, "unresolved unlimited dispatch")
    if (index(refused%message, "one fixed concrete") == 0 .and. &
        index(refused%message, "dynamic") == 0) then
        print *, "FAIL unresolved unlimited dispatch refusal was not precise: ", &
            refused%message
        error stop 3
    end if

    lifetime = unlimited_lifetime_source()
    refused = fad_jvp(lifetime, [character(len=1) :: "x"], from="evaluate")
    call require_refusal(refused, "unproven unlimited ownership")
    if (index(refused%message, "ownership") == 0 .and. &
        index(refused%message, "dynamic type") == 0) then
        print *, "FAIL unlimited ownership refusal was not precise: ", &
            refused%message
        error stop 4
    end if

    dir = "build/oracle/polymorphic_unlimited_assignment"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 5
    call write_file(dir//"/primal.f90", source)
    generated = "module unlimited_assignment_generated"//nl// &
        "    use unlimited_assignment_case, only: child_t, holder_t"//nl// &
        "contains"//nl//jvp%code//vjp%code// &
        "end module unlimited_assignment_generated"//nl
    call write_file(dir//"/generated.f90", generated)
    driver = "program driver"//nl// &
        "    use unlimited_assignment_case, only: evaluate"//nl// &
        "    use unlimited_assignment_generated, only: evaluate_jvp, evaluate_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, y, yd, yb, xb, h, fd"//nl// &
        "    real(8) :: dot_forward, dot_reverse"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.4d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    call evaluate_jvp(x, xd, y, yd)"//nl// &
        "    if (abs(y - 4.0d0*x*x) > 1.0d-13) error stop 6"//nl// &
        "    if (abs(yd - 8.0d0*x*xd) > 1.0d-13) error stop 7"//nl// &
        "    fd = (evaluate(x+h)-evaluate(x-h))/(2.0d0*h)"//nl// &
        "    if (abs(yd/xd-fd) > 1.0d-7) error stop 8"//nl// &
        "    yb = -0.7d0"//nl// &
        "    call evaluate_vjp(x, y, yb, xb)"//nl// &
        "    if (abs(xb-yb*8.0d0*x) > 1.0d-12) error stop 9"//nl// &
        "    dot_forward = yb*yd"//nl// &
        "    dot_reverse = xb*xd"//nl// &
        "    if (abs(dot_forward-dot_reverse) > 1.0d-12) error stop 10"//nl// &
        "    print *, 'unlimited polymorphic assignment oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir// &
        "/generated.f90 "//dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL generated unlimited assignment did not compile"
        call show_file(dir//"/build.log")
        error stop 11
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL unlimited assignment behavioral oracle"
        call show_file(dir//"/out.txt")
        error stop 12
    end if
    print *, "test_polymorphic_unlimited_assignment_oracle: all cases passed"

contains

    function unlimited_assignment_source() result(text)
        character(len=:), allocatable :: text

        text = "module unlimited_assignment_case"//nl// &
            "    implicit none"//nl// &
            "    type :: base_t"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    end type child_t"//nl// &
            "    type :: field_t"//nl// &
            "        class(*), allocatable :: payload"//nl// &
            "    end type field_t"//nl// &
            "    type :: holder_t"//nl// &
            "        type(field_t) :: field"//nl// &
            "    end type holder_t"//nl// &
            "contains"//nl// &
            "    pure function evaluate(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        type(child_t) :: child"//nl// &
            "        type(holder_t) :: box"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "        allocate(box%field%payload, source=child)"//nl// &
            "        select type (owner => box%field%payload)"//nl// &
            "        type is (child_t)"//nl// &
            "            owner%scale = 4.0d0*x"//nl// &
            "            y = owner%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "        deallocate(box%field%payload)"//nl// &
            "    end function evaluate"//nl// &
            "end module unlimited_assignment_case"//nl
    end function unlimited_assignment_source

    function unlimited_ambiguous_source() result(text)
        character(len=:), allocatable :: text

        text = unlimited_assignment_source()
        text = replace_text(text, "        type is (child_t)"//nl// &
            "            owner%scale = 4.0d0*x", &
            "        type is (child_t)"//nl// &
            "            owner%scale = 4.0d0*x"//nl// &
            "            y = owner%scale*x"//nl// &
            "        type is (base_t)"//nl// &
            "            owner%scale = 5.0d0*x"//nl// &
            "            y = owner%scale*x")
        text = replace_text(text, "module unlimited_assignment_case", &
            "module unlimited_ambiguous_case")
        text = replace_text(text, "end module unlimited_assignment_case", &
            "end module unlimited_ambiguous_case")
    end function unlimited_ambiguous_source

    function unlimited_lifetime_source() result(text)
        character(len=:), allocatable :: text

        text = unlimited_assignment_source()
        text = replace_text(text, "        pure function evaluate(x) result(y)", &
            "        pure function make_child(x) result(child)"//nl// &
            "            real(8), intent(in) :: x"//nl// &
            "            type(child_t) :: child"//nl// &
            "            child%scale = 2.0d0*x"//nl// &
            "        end function make_child"//nl// &
            "        pure function evaluate(x) result(y)")
        text = replace_text(text, "allocate(box%field%payload, source=child)", &
            "allocate(box%field%payload, source=make_child(x))")
        text = replace_text(text, "module unlimited_assignment_case", &
            "module unlimited_lifetime_case")
        text = replace_text(text, "end module unlimited_assignment_case", &
            "end module unlimited_lifetime_case")
    end function unlimited_lifetime_source

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

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 1
        end if
    end subroutine require_ok

    subroutine require_refusal(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (result%ok) then
            print *, "FAIL ", trim(label), " was accepted"
            error stop 2
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL ", trim(label), " had no diagnostic"
            error stop 2
        end if
    end subroutine require_refusal

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: input

        open (newunit=input, file=path, status="replace", action="write")
        write (input, '(a)') text
        close (input)
    end subroutine write_file

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

end program test_polymorphic_unlimited_assignment_oracle
