program test_polymorphic_ownership_oracle
    !! Independent oracle for the forward polymorphic-ownership boundary.
    !! A passive allocatable CLASS dummy keeps one fixed dynamic child through
    !! SELECT TYPE and differentiates an active scalar. The same path becomes
    !! an explicit refusal when a component of the polymorphic owner is active:
    !! the current IR has no paired dynamic-type tangent selector.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, tangent, driver, dir
    type(fad_result_t) :: generated, generated_star, refused
    type(fad_result_t) :: allocated_generated, allocated_star_generated
    integer :: stat, unit

    source = polymorphic_source("class(base_t)")
    generated = fad_jvp(source, [character(len=1) :: "x"], from="evaluate")
    if (.not. generated%ok) then
        print *, "FAIL passive polymorphic allocatable JVP: ", generated%message
        error stop 1
    end if
    generated_star = fad_jvp(polymorphic_source("class(*)"), &
        [character(len=1) :: "x"], from="evaluate", &
        name="evaluate_star_jvp")
    if (.not. generated_star%ok) then
        print *, "FAIL passive unlimited polymorphic allocatable JVP: ", &
            generated_star%message
        error stop 2
    end if

    allocated_generated = fad_jvp(source, [character(len=1) :: "x"], &
        from="allocate_evaluate", name="allocate_evaluate_jvp")
    if (.not. allocated_generated%ok) then
        print *, "FAIL fixed-source polymorphic ownership JVP: ", &
            allocated_generated%message
        error stop 15
    end if
    allocated_star_generated = fad_jvp(polymorphic_source("class(*)"), &
        [character(len=1) :: "x"], from="allocate_star_evaluate", &
        name="allocate_star_evaluate_jvp")
    if (.not. allocated_star_generated%ok) then
        print *, "FAIL fixed-source unlimited polymorphic ownership JVP: ", &
            allocated_star_generated%message
        error stop 16
    end if

    refused = fad_jvp(source, [character(len=11) :: "model%scale"], &
        from="evaluate")
    call require_refusal(refused, "class(T)")

    refused = fad_jvp(polymorphic_source("class(*)"), &
        [character(len=11) :: "model%scale"], from="evaluate")
    call require_refusal(refused, "class(*)")

    dir = "build/oracle/polymorphic_ownership"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create polymorphic ownership directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') source
    close (unit)

    tangent = &
        "module polymorphic_generated"//nl// &
        "    use polymorphic_ownership_case, only: base_t, child_t"//nl// &
        "contains"//nl//generated%code// &
        generated_star%code// &
        allocated_generated%code// &
        allocated_star_generated%code// &
        "end module polymorphic_generated"//nl
    open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
        action="write")
    write (unit, '(a)') tangent
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use polymorphic_ownership_case, only: base_t, child_t, evaluate, "// &
        "allocate_evaluate, allocate_star_evaluate"//nl// &
        "    use polymorphic_generated, only: evaluate_jvp, evaluate_star_jvp, "// &
        "allocate_evaluate_jvp, allocate_star_evaluate_jvp"//nl// &
        "    implicit none"//nl// &
        "    class(base_t), allocatable :: model"//nl// &
        "    class(*), allocatable :: universal"//nl// &
        "    real(8) :: x, x_d, y, y_d, h, fp, fm, fd, scale"//nl// &
        "    allocate(child_t :: model)"//nl// &
        "    select type (model)"//nl// &
        "    type is (child_t)"//nl// &
        "        model%scale = 3.0d0"//nl// &
        "        scale = model%scale"//nl// &
        "    end select"//nl// &
        "    allocate(child_t :: universal)"//nl// &
        "    select type (universal)"//nl// &
        "    type is (child_t)"//nl// &
        "        universal%scale = scale"//nl// &
        "    end select"//nl// &
        "    x = 1.25d0"//nl// &
        "    x_d = -0.4d0"//nl// &
        "    h = 1.0d-6"//nl// &
        "    call evaluate_jvp(model, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - scale*x) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - scale*x_d) > 1.0d-13) error stop 3"//nl// &
        "    call evaluate_star_jvp(universal, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - scale*x) > 1.0d-13) error stop 4"//nl// &
        "    if (abs(y_d - scale*x_d) > 1.0d-13) error stop 5"//nl// &
        "    fp = evaluate(model, x + h)"//nl// &
        "    fm = evaluate(model, x - h)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(fd - scale) > 1.0d-7) error stop 6"//nl// &
        "    if (abs(y_d/x_d - fd) > 1.0d-7) error stop 7"//nl// &
        "    call allocate_evaluate_jvp(x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 2.0d0*x*x) > 1.0d-13) error stop 17"//nl// &
        "    if (abs(y_d - 4.0d0*x*x_d) > 1.0d-13) error stop 18"//nl// &
        "    fp = allocate_evaluate(x + h)"//nl// &
        "    fm = allocate_evaluate(x - h)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(y_d/x_d - fd) > 1.0d-7) error stop 19"//nl// &
        "    call allocate_star_evaluate_jvp(x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 2.0d0*x*x) > 1.0d-13) error stop 20"//nl// &
        "    if (abs(y_d - 4.0d0*x*x_d) > 1.0d-13) error stop 21"//nl// &
        "    print *, 'polymorphic ownership passive JVP oracle pass'"//nl// &
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
        print *, "FAIL passive polymorphic generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 8
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL passive polymorphic ownership behavioral oracle"
        call show_file(dir//"/out.txt")
        error stop 9
    end if
    print *, "test_polymorphic_ownership_oracle: all cases passed"

contains

    function polymorphic_source(type_spec) result(text)
        character(len=*), intent(in) :: type_spec
        character(len=:), allocatable :: text

        text = "module polymorphic_ownership_case"//nl// &
            "    implicit none"//nl// &
            "    type :: base_t"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: child_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    end type child_t"//nl// &
            "contains"//nl// &
            "    pure function evaluate(model, x) result(y)"//nl// &
            "        "//trim(type_spec)//", allocatable, intent(in) :: model"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        select type (model)"//nl// &
            "        type is (child_t)"//nl// &
            "            y = model%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "    end function evaluate"//nl// &
            "    pure function allocate_evaluate(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        type(child_t) :: child"//nl// &
            "        class(base_t), allocatable :: holder"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "        allocate(holder, source=child)"//nl// &
            "        select type (holder)"//nl// &
            "        type is (child_t)"//nl// &
            "            y = holder%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "        deallocate(holder)"//nl// &
            "    end function allocate_evaluate"//nl// &
            "    pure function allocate_star_evaluate(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        type(child_t) :: child"//nl// &
            "        class(*), allocatable :: holder"//nl// &
            "        child%scale = 2.0d0*x"//nl// &
            "        allocate(holder, source=child)"//nl// &
            "        select type (holder)"//nl// &
            "        type is (child_t)"//nl// &
            "            y = holder%scale*x"//nl// &
            "        class default"//nl// &
            "            y = x"//nl// &
            "        end select"//nl// &
            "        deallocate(holder)"//nl// &
            "    end function allocate_star_evaluate"//nl// &
            "end module polymorphic_ownership_case"//nl
    end function polymorphic_source

    subroutine require_refusal(result, type_label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: type_label

        if (result%ok) then
            print *, "FAIL active ", trim(type_label), " owner was accepted"
            error stop 10
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL active ", trim(type_label), " owner had no diagnostic"
            error stop 11
        end if
        if (index(result%message, "active polymorphic allocatable ownership") == 0) then
            print *, "FAIL active ", trim(type_label), " diagnostic: ", &
                trim(result%message)
            error stop 12
        end if
        if (index(result%message, trim(type_label)) == 0) then
            print *, "FAIL active ", trim(type_label), " semantic type fact missing: ", &
                trim(result%message)
            error stop 13
        end if
        if (index(result%message, "line 10") == 0) then
            print *, "FAIL active ", trim(type_label), " source line missing: ", &
                trim(result%message)
            error stop 14
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

end program test_polymorphic_ownership_oracle
