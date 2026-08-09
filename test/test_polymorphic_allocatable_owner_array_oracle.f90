program test_polymorphic_allocatable_owner_array_oracle
    !! Independent oracle for one literal element of a one-dimensional
    !! allocatable polymorphic owner array.  The owner has one fixed
    !! SOURCE= acquisition and one concrete SELECT TYPE arm; all descriptor
    !! lifetime and dispatch choices outside that path remain refusals.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, tangent, driver, dir
    type(fad_result_t) :: jvp, vjp, refused
    integer :: unit, stat

    source = owner_array_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], from="evaluate", &
        name="evaluate_jvp")
    call require_ok(jvp, "allocatable owner-array JVP")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        from="evaluate", name="evaluate_vjp")
    call require_ok(vjp, "allocatable owner-array VJP")

    refused = fad_vjp(dynamic_index_source(), [character(len=1) :: "x"], &
        dependent="y", from="evaluate")
    call require_refusal(refused, "dynamic owner index", "dynamic-type replay")
    refused = fad_vjp(ambiguous_dispatch_source(), [character(len=1) :: "x"], &
        dependent="y", from="evaluate")
    call require_refusal(refused, "ambiguous owner dispatch", &
        "dynamic-type replay")
    refused = fad_vjp(factory_source(), [character(len=1) :: "x"], &
        dependent="y", from="evaluate")
    call require_refusal(refused, "factory owner source", "dynamic-type replay")
    refused = fad_vjp(alias_source(), [character(len=1) :: "x"], &
        dependent="y", from="evaluate")
    call require_refusal(refused, "TARGET owner alias", "alias")

    dir = "build/oracle/polymorphic_allocatable_owner_array"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 1
    call write_file(dir//"/primal.f90", source)
    tangent = "module allocatable_owner_array_generated"//nl// &
        "    use allocatable_owner_array_case, only: base_t, child_t"//nl// &
        "contains"//nl//jvp%code//vjp%code// &
        "end module allocatable_owner_array_generated"//nl
    call write_file(dir//"/derivatives.f90", tangent)
    driver = "program driver"//nl// &
        "    use allocatable_owner_array_case, only: evaluate"//nl// &
        "    use allocatable_owner_array_generated, only: evaluate_jvp, "// &
        "evaluate_vjp"//nl// &
        "    real(8) :: x, xd, y, yd, yb, xb, h, fp, fm, fd"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.4d0"//nl// &
        "    call evaluate_jvp(x, xd, y, yd)"//nl// &
        "    if (abs(y - 2.0d0*x*x) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(yd - 4.0d0*x*xd) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = evaluate(x + h*xd)"//nl// &
        "    fm = evaluate(x - h*xd)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(yd - fd) > 1.0d-7) error stop 4"//nl// &
        "    yb = -0.7d0"//nl// &
        "    call evaluate_vjp(x, y, yb, xb)"//nl// &
        "    if (abs(xb - yb*4.0d0*x) > 1.0d-12) error stop 5"//nl// &
        "    if (abs(xb*xd - yb*yd) > 1.0d-12) error stop 6"//nl// &
        "    if (abs(xb*xd - yb*fd) > 1.0d-7) error stop 7"//nl// &
        "    print *, 'polymorphic allocatable owner-array oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/derivatives.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop 8
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/out.txt")
        error stop 9
    end if
    print *, "test_polymorphic_allocatable_owner_array_oracle: all cases passed"

contains

    function owner_array_source() result(text)
        character(len=:), allocatable :: text

        text = "module allocatable_owner_array_case"//nl// &
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
            "            y = item%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "        deallocate(owners)"//nl// &
            "    end function evaluate"//nl// &
            "end module allocatable_owner_array_case"//nl
    end function owner_array_source

    function dynamic_index_source() result(text)
        character(len=:), allocatable :: text

        text = owner_array_source()
        text = replace_all(text, &
            "        class(base_t), allocatable :: owners(:)"//nl, &
            "        integer :: selected"//nl// &
            "        class(base_t), allocatable :: owners(:)"//nl// &
            "        selected = 2"//nl)
        text = replace_all(text, "owners(2)", "owners(selected)")
        text = rename_module(text, "dynamic_owner_array_case")
    end function dynamic_index_source

    function ambiguous_dispatch_source() result(text)
        character(len=:), allocatable :: text

        text = replace_all(owner_array_source(), &
            "        class default"//nl// &
            "            y = x"//nl, &
            "        type is (base_t)"//nl// &
            "            y = x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl)
        text = rename_module(text, "ambiguous_owner_array_case")
    end function ambiguous_dispatch_source

    function factory_source() result(text)
        character(len=:), allocatable :: text

        text = replace_all(owner_array_source(), &
            "        type(child_t) :: child"//nl// &
            "        class(base_t), allocatable :: owners(:)"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "        allocate(owners(2), source=child)", &
            "        class(base_t), allocatable :: owners(:)"//nl// &
            "        allocate(owners(2), source=make_child(x))")
        text = replace_all(text, &
            "end module allocatable_owner_array_case", &
            "    pure function make_child(x) result(child)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(child_t) :: child"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "    end function make_child"//nl// &
            "end module allocatable_owner_array_case")
        text = rename_module(text, "factory_owner_array_case")
    end function factory_source

    function alias_source() result(text)
        character(len=:), allocatable :: text

        text = replace_all(owner_array_source(), &
            "class(base_t), allocatable :: owners(:)", &
            "class(base_t), allocatable, target :: owners(:)")
        text = rename_module(text, "alias_owner_array_case")
    end function alias_source

    function rename_module(text, name) result(out)
        character(len=*), intent(in) :: text, name
        character(len=:), allocatable :: out

        out = replace_all(text, "allocatable_owner_array_case", name)
    end function rename_module

    function replace_all(text, old, new) result(out)
        character(len=*), intent(in) :: text, old, new
        character(len=:), allocatable :: out, rest
        integer :: at

        out = ""
        rest = text
        do
            at = index(rest, old)
            if (at == 0) exit
            out = out//rest(:at - 1)//new
            rest = rest(at + len(old):)
        end do
        out = out//rest
    end function replace_all

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 20
        end if
    end subroutine require_ok

    subroutine require_refusal(result, label, phrase)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label, phrase

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, phrase) == 0) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 21
        end if
    end subroutine require_refusal

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text

        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, '(a)') text
        close (unit)
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

end program test_polymorphic_allocatable_owner_array_oracle
