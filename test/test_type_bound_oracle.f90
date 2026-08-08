program test_type_bound_oracle
    !! Behavioral oracle for one concrete, same-file type-bound function.
    !! The receiver is passive; the method's real argument is differentiated
    !! after implicit PASS or NOPASS is normalized to an ordinary local call.
    !! A statically declared child may also use an inherited binding resolved
    !! through FortFront's local-to-parent hierarchy query. Named PASS is
    !! checked with the passed-object dummy deliberately out of first position.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module type_bound_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8) :: scale"//nl// &
        "    contains"//nl// &
        "        procedure :: value"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    pure real(8) function value(self, x) result(y)"//nl// &
        "        class(box_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = self%scale*x + self%scale*self%scale"//nl// &
        "    end function value"//nl// &
        "    pure real(8) function top(model, x) result(y)"//nl// &
        "        type(box_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        y = model%value(x)"//nl// &
        "    end function top"//nl// &
        "end module type_bound_case"//nl

    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    jvp = fad_jvp(source, ["x"], from="top", name="top_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL type-bound JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, ["x"], dependent="y", from="top", name="top_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL type-bound VJP generation: ", vjp%message
        error stop 1
    end if

    call check_pass_case(named_pass_source(), "named_pass")
    call check_pass_case(nopass_source(), "nopass_case")
    call check_pass_case(nopass_scope_source(), "nopass_scope_case")
    call expect_refusal(ambiguous_source(), "ambiguous type", "ambiguous")
    call check_active_receiver(active_receiver_source())
    call check_active_receiver_subroutine(active_receiver_subroutine_source())
    call expect_refusal(allocatable_receiver_source(), "allocatable receiver", &
        "allocatable receivers")
    call expect_refusal(array_receiver_source(), "array receiver", &
        "array receivers")
    call check_inherited(inherited_source(), "inherited_case")
    call expect_refusal(generic_source(), "generic", "generic")
    call expect_refusal(deferred_source(), "deferred", "deferred")
    call expect_refusal(runtime_dispatch_source(), "runtime dispatch", &
        "concrete type")

    dir = "build/oracle/type_bound"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create type-bound oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", action="write")
    write (unit, '(a)') "module type_bound_derivatives"
    write (unit, '(a)') "    use type_bound_case, only: box_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module type_bound_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use type_bound_case, only: box_t, top"//nl// &
        "    use type_bound_derivatives, only: top_jvp, top_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(box_t) :: model"//nl// &
        "    real(8) :: x, x_d, y, y_d, x_b, y_b, h, fp, fm"//nl// &
        "    model%scale = 2.0d0"//nl// &
        "    x = 1.5d0"//nl// &
        "    x_d = -0.7d0"//nl// &
        "    y_b = 1.3d0"//nl// &
        "    call top_jvp(model, x, x_d, y, y_d)"//nl// &
        "    if (abs(y - 7.0d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(y_d - 2.0d0*x_d) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = top(model, x + h)"//nl// &
        "    fm = top(model, x - h)"//nl// &
        "    if (abs(y_d - (fp - fm)/(2.0d0*h)*x_d) > 1.0d-7) error stop 4"//nl// &
        "    call top_vjp(model, x, y, y_b, x_b)"//nl// &
        "    if (abs(y - 7.0d0) > 1.0d-13) error stop 5"//nl// &
        "    if (abs(x_b - 2.0d0*y_b) > 1.0d-13) error stop 6"//nl// &
        "    print *, 'type-bound oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL type-bound: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL type-bound: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_type_bound_oracle: all cases passed"

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

    subroutine expect_refusal(case_source, label, needle)
        character(len=*), intent(in) :: case_source, label, needle
        type(fad_result_t) :: result

        result = fad_jvp(case_source, ["x"], from="top")
        if (result%ok) then
            print *, "FAIL type-bound ", label, ": unsupported call was accepted"
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL type-bound ", label, ": refusal was not named"
            error stop 1
        else if (index(result%message, needle) == 0) then
            print *, "FAIL type-bound ", label, ": refusal was not named: ", &
                result%message
            error stop 1
        end if

        result = fad_vjp(case_source, ["x"], dependent="y", from="top")
        if (result%ok) then
            print *, "FAIL type-bound ", label, ": VJP refusal was accepted"
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL type-bound ", label, ": VJP refusal was not named"
            error stop 1
        else if (index(result%message, needle) == 0) then
            print *, "FAIL type-bound ", label, ": VJP refusal was not named: ", &
                result%message
            error stop 1
        end if
    end subroutine expect_refusal

    subroutine check_active_receiver(case_source)
        character(len=*), intent(in) :: case_source
        type(fad_result_t) :: jvp_case, vjp_case
        character(len=:), allocatable :: case_dir, case_driver
        integer :: case_unit, case_stat

        jvp_case = fad_jvp(case_source, [character(len=12) :: "model%scale", "x"], &
            from="top", name="top_active_receiver_jvp")
        if (.not. jvp_case%ok) then
            print *, "FAIL active receiver JVP generation: ", jvp_case%message
            error stop 1
        end if
        vjp_case = fad_vjp(case_source, [character(len=12) :: "model%scale", "x"], &
            dependent="y", from="top", name="top_active_receiver_vjp")
        if (.not. vjp_case%ok) then
            print *, "FAIL active receiver VJP generation: ", vjp_case%message
            error stop 1
        end if

        case_dir = "build/oracle/type_bound_active_receiver"
        call execute_command_line("mkdir -p "//case_dir, exitstat=case_stat)
        if (case_stat /= 0) error stop "could not create active-receiver oracle directory"

        open (newunit=case_unit, file=case_dir//"/primal.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_source
        close (case_unit)
        open (newunit=case_unit, file=case_dir//"/derivatives.f90", &
            status="replace", action="write")
        write (case_unit, '(a)') "module type_bound_active_derivatives"
        write (case_unit, '(a)') "    use active_receiver_case, only: box_t"
        write (case_unit, '(a)') "contains"
        write (case_unit, '(a)') jvp_case%code
        write (case_unit, '(a)') vjp_case%code
        write (case_unit, '(a)') "end module type_bound_active_derivatives"
        close (case_unit)

        case_driver = &
            "program driver"//nl// &
            "    use active_receiver_case, only: box_t, top"//nl// &
            "    use type_bound_active_derivatives, only: "// &
            "top_active_receiver_jvp, top_active_receiver_vjp"//nl// &
            "    implicit none"//nl// &
            "    type(box_t) :: model, model_d, model_b, plus, minus"//nl// &
            "    real(8) :: x, x_d, y, y_d, y_b, x_b, h, fp, fm, fd"//nl// &
            "    model%scale = 2.0d0"//nl// &
            "    model_d%scale = -0.7d0"//nl// &
            "    x = 1.5d0"//nl// &
            "    x_d = 0.4d0"//nl// &
            "    y_b = 1.3d0"//nl// &
            "    call top_active_receiver_jvp(model, model_d, x, x_d, y, y_d)"//nl// &
            "    if (abs(y - 3.0d0) > 1.0d-13) error stop 2"//nl// &
            "    if (abs(y_d - (-0.25d0)) > 1.0d-13) error stop 3"//nl// &
            "    h = 1.0d-6"//nl// &
            "    plus = model"//nl// &
            "    plus%scale = model%scale + h*model_d%scale"//nl// &
            "    minus = model"//nl// &
            "    minus%scale = model%scale - h*model_d%scale"//nl// &
            "    fp = top(plus, x + h*x_d)"//nl// &
            "    fm = top(minus, x - h*x_d)"//nl// &
            "    fd = (fp - fm)/(2.0d0*h)"//nl// &
            "    if (abs(y_d - fd) > 1.0d-7) error stop 4"//nl// &
            "    call top_active_receiver_vjp(model, x, y, y_b, model_b, x_b)"//nl// &
            "    if (abs(model_b%scale - 1.95d0) > 1.0d-13) error stop 5"//nl// &
            "    if (abs(x_b - 2.6d0) > 1.0d-13) error stop 6"//nl// &
            "    if (abs(y_b*y_d - (model_b%scale*model_d%scale + x_b*x_d)) > "// &
            "1.0d-13) error stop 7"//nl// &
            "    print *, 'active receiver type-bound oracle pass'"//nl// &
            "end program driver"//nl
        open (newunit=case_unit, file=case_dir//"/driver.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_driver
        close (case_unit)

        call execute_command_line("gfortran -std=f2018 -O2 -o "//case_dir//"/run "// &
            case_dir//"/primal.f90 "//case_dir//"/derivatives.f90 "// &
            case_dir//"/driver.f90 > "//case_dir//"/build.log 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL active receiver: generated code did not compile"
            call show_file(case_dir//"/build.log")
            error stop 1
        end if
        call execute_command_line("./"//case_dir//"/run > "//case_dir//"/out.txt 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL active receiver: independent oracle failed"
            call show_file(case_dir//"/out.txt")
            error stop 1
        end if
    end subroutine check_active_receiver

    subroutine check_active_receiver_subroutine(case_source)
        character(len=*), intent(in) :: case_source
        type(fad_result_t) :: jvp_case, vjp_case
        character(len=:), allocatable :: case_dir, case_driver
        integer :: case_unit, case_stat

        jvp_case = fad_jvp(case_source, [character(len=12) :: "model%scale", "x"], &
            from="top", name="top_sub_jvp")
        if (.not. jvp_case%ok) then
            print *, "FAIL active receiver subroutine JVP generation: ", &
                jvp_case%message
            error stop 1
        end if
        vjp_case = fad_vjp(case_source, [character(len=12) :: "model%scale", "x"], &
            dependent="y", from="top", name="top_sub_vjp")
        if (.not. vjp_case%ok) then
            print *, "FAIL active receiver subroutine VJP generation: ", &
                vjp_case%message
            error stop 1
        end if

        case_dir = "build/oracle/type_bound_active_subroutine"
        call execute_command_line("mkdir -p "//case_dir, exitstat=case_stat)
        if (case_stat /= 0) error stop "could not create active-subroutine oracle directory"

        open (newunit=case_unit, file=case_dir//"/primal.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_source
        close (case_unit)
        open (newunit=case_unit, file=case_dir//"/derivatives.f90", &
            status="replace", action="write")
        write (case_unit, '(a)') "module type_bound_active_sub_derivatives"
        write (case_unit, '(a)') "    use active_receiver_subroutine_case, only: box_t"
        write (case_unit, '(a)') "contains"
        write (case_unit, '(a)') jvp_case%code
        write (case_unit, '(a)') vjp_case%code
        write (case_unit, '(a)') "end module type_bound_active_sub_derivatives"
        close (case_unit)

        case_driver = &
            "program driver"//nl// &
            "    use active_receiver_subroutine_case, only: box_t, top"//nl// &
            "    use type_bound_active_sub_derivatives, only: top_sub_jvp, "// &
            "top_sub_vjp"//nl// &
            "    implicit none"//nl// &
            "    type(box_t) :: model, model_d, model_b"//nl// &
            "    real(8) :: x, x_d, y, y_d, y_b, x_b"//nl// &
            "    model%scale = 2.0d0"//nl// &
            "    model_d%scale = -0.7d0"//nl// &
            "    x = 1.5d0"//nl// &
            "    x_d = 0.4d0"//nl// &
            "    y_b = 1.3d0"//nl// &
            "    call top_sub_jvp(model, model_d, x, x_d, y, y_d)"//nl// &
            "    if (abs(y - 3.0d0) > 1.0d-13) error stop 2"//nl// &
            "    if (abs(y_d - (-0.25d0)) > 1.0d-13) error stop 3"//nl// &
            "    call top_sub_vjp(model, x, y, y_b, model_b, x_b)"//nl// &
            "    if (abs(model_b%scale - 1.95d0) > 1.0d-13) error stop 4"//nl// &
            "    if (abs(x_b - 2.6d0) > 1.0d-13) error stop 5"//nl// &
            "    print *, 'active receiver type-bound subroutine oracle pass'"//nl// &
            "end program driver"//nl
        open (newunit=case_unit, file=case_dir//"/driver.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_driver
        close (case_unit)

        call execute_command_line("gfortran -std=f2018 -O2 -o "//case_dir//"/run "// &
            case_dir//"/primal.f90 "//case_dir//"/derivatives.f90 "// &
            case_dir//"/driver.f90 > "//case_dir//"/build.log 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL active receiver subroutine: generated code did not compile"
            call show_file(case_dir//"/build.log")
            error stop 1
        end if
        call execute_command_line("./"//case_dir//"/run > "//case_dir//"/out.txt 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL active receiver subroutine: independent oracle failed"
            call show_file(case_dir//"/out.txt")
            error stop 1
        end if
    end subroutine check_active_receiver_subroutine

    subroutine check_pass_case(case_source, case_module)
        character(len=*), intent(in) :: case_source, case_module
        type(fad_result_t) :: jvp_case, vjp_case
        character(len=:), allocatable :: case_dir, case_driver
        integer :: case_unit, case_stat

        jvp_case = fad_jvp(case_source, ["x"], from="top", name="top_pass_case_jvp")
        if (.not. jvp_case%ok) then
            print *, "FAIL PASS/NOPASS JVP generation: ", jvp_case%message
            error stop 1
        end if
        vjp_case = fad_vjp(case_source, ["x"], dependent="y", from="top", &
            name="top_pass_case_vjp")
        if (.not. vjp_case%ok) then
            print *, "FAIL PASS/NOPASS VJP generation: ", vjp_case%message
            error stop 1
        end if

        case_dir = "build/oracle/type_bound_"//trim(case_module)
        call execute_command_line("mkdir -p "//case_dir, exitstat=case_stat)
        if (case_stat /= 0) error stop "could not create NOPASS oracle directory"

        open (newunit=case_unit, file=case_dir//"/primal.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_source
        close (case_unit)
        open (newunit=case_unit, file=case_dir//"/derivatives.f90", &
            status="replace", action="write")
        write (case_unit, '(a)') "module type_bound_pass_derivatives"
        write (case_unit, '(a)') "    use "//trim(case_module)//", only: box_t"
        write (case_unit, '(a)') "contains"
        write (case_unit, '(a)') jvp_case%code
        write (case_unit, '(a)') vjp_case%code
        write (case_unit, '(a)') "end module type_bound_pass_derivatives"
        close (case_unit)

        case_driver = &
            "program driver"//nl// &
            "    use "//trim(case_module)//", only: top"//nl// &
            "    use type_bound_pass_derivatives, only: top_pass_case_jvp, "// &
            "top_pass_case_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, x_d, y, y_d, x_b, y_b, h, fp, fm"//nl// &
            "    x = 1.5d0"//nl// &
            "    x_d = -0.7d0"//nl// &
            "    y_b = 1.3d0"//nl// &
            "    call top_pass_case_jvp(x, x_d, y, y_d)"//nl// &
            "    if (abs(y - 5.5d0) > 1.0d-13) error stop 2"//nl// &
            "    if (abs(y_d - 3.0d0*x_d) > 1.0d-13) error stop 3"//nl// &
            "    h = 1.0d-6"//nl// &
            "    fp = top(x + h)"//nl// &
            "    fm = top(x - h)"//nl// &
            "    if (abs(y_d - (fp - fm)/(2.0d0*h)*x_d) > 1.0d-7) error stop 4"//nl// &
            "    call top_pass_case_vjp(x, y, y_b, x_b)"//nl// &
            "    if (abs(y - 5.5d0) > 1.0d-13) error stop 5"//nl// &
            "    if (abs(x_b - 3.0d0*y_b) > 1.0d-13) error stop 6"//nl// &
            "    if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 7"//nl// &
            "    print *, 'PASS/NOPASS type-bound oracle pass'"//nl// &
            "end program driver"//nl
        open (newunit=case_unit, file=case_dir//"/driver.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_driver
        close (case_unit)

        call execute_command_line("gfortran -std=f2018 -O2 -o "//case_dir//"/run "// &
            case_dir//"/primal.f90 "//case_dir//"/derivatives.f90 "// &
            case_dir//"/driver.f90 > "//case_dir//"/build.log 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL PASS/NOPASS: generated code did not compile"
            call show_file(case_dir//"/build.log")
            error stop 1
        end if
        call execute_command_line("./"//case_dir//"/run > "//case_dir//"/out.txt 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL PASS/NOPASS: independent oracle failed"
            call show_file(case_dir//"/out.txt")
            error stop 1
        end if
    end subroutine check_pass_case

    subroutine check_inherited(case_source, case_module)
        character(len=*), intent(in) :: case_source, case_module
        type(fad_result_t) :: jvp_case, vjp_case
        character(len=:), allocatable :: case_dir, case_driver
        integer :: case_unit, case_stat

        jvp_case = fad_jvp(case_source, ["x"], from="top", &
            name="top_inherited_jvp")
        if (.not. jvp_case%ok) then
            print *, "FAIL inherited binding JVP generation: ", jvp_case%message
            error stop 1
        end if
        vjp_case = fad_vjp(case_source, ["x"], dependent="y", from="top", &
            name="top_inherited_vjp")
        if (.not. vjp_case%ok) then
            print *, "FAIL inherited binding VJP generation: ", vjp_case%message
            error stop 1
        end if

        case_dir = "build/oracle/type_bound_"//trim(case_module)
        call execute_command_line("mkdir -p "//case_dir, exitstat=case_stat)
        if (case_stat /= 0) error stop "could not create inherited oracle directory"

        open (newunit=case_unit, file=case_dir//"/primal.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_source
        close (case_unit)
        open (newunit=case_unit, file=case_dir//"/derivatives.f90", &
            status="replace", action="write")
        write (case_unit, '(a)') "module type_bound_inherited_derivatives"
        write (case_unit, '(a)') "    use "//trim(case_module)//", only: box_t"
        write (case_unit, '(a)') "contains"
        write (case_unit, '(a)') jvp_case%code
        write (case_unit, '(a)') vjp_case%code
        write (case_unit, '(a)') "end module type_bound_inherited_derivatives"
        close (case_unit)

        case_driver = &
            "program driver"//nl// &
            "    use "//trim(case_module)//", only: box_t, top"//nl// &
            "    use type_bound_inherited_derivatives, only: "// &
            "top_inherited_jvp, top_inherited_vjp"//nl// &
            "    implicit none"//nl// &
            "    type(box_t) :: model"//nl// &
            "    real(8) :: x, x_d, y, y_d, x_b, y_b, h, fp, fm"//nl// &
            "    model%scale = 2.0d0"//nl// &
            "    x = 1.5d0"//nl// &
            "    x_d = -0.7d0"//nl// &
            "    y_b = 1.3d0"//nl// &
            "    call top_inherited_jvp(model, x, x_d, y, y_d)"//nl// &
            "    if (abs(y - 4.0d0) > 1.0d-13) error stop 2"//nl// &
            "    if (abs(y_d - 2.0d0*x_d) > 1.0d-13) error stop 3"//nl// &
            "    h = 1.0d-6"//nl// &
            "    fp = top(model, x + h)"//nl// &
            "    fm = top(model, x - h)"//nl// &
            "    if (abs(y_d - (fp - fm)/(2.0d0*h)*x_d) > 1.0d-7) "// &
            "error stop 4"//nl// &
            "    call top_inherited_vjp(model, x, y, y_b, x_b)"//nl// &
            "    if (abs(y - 4.0d0) > 1.0d-13) error stop 5"//nl// &
            "    if (abs(x_b - 2.0d0*y_b) > 1.0d-13) error stop 6"//nl// &
            "    if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 7"//nl// &
            "    print *, 'inherited type-bound oracle pass'"//nl// &
            "end program driver"//nl
        open (newunit=case_unit, file=case_dir//"/driver.f90", status="replace", &
            action="write")
        write (case_unit, '(a)') case_driver
        close (case_unit)

        call execute_command_line("gfortran -std=f2018 -O2 -o "//case_dir//"/run "// &
            case_dir//"/primal.f90 "//case_dir//"/derivatives.f90 "// &
            case_dir//"/driver.f90 > "//case_dir//"/build.log 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL inherited binding: generated code did not compile"
            call show_file(case_dir//"/build.log")
            error stop 1
        end if
        call execute_command_line("./"//case_dir//"/run > "//case_dir//"/out.txt 2>&1", &
            exitstat=case_stat)
        if (case_stat /= 0) then
            print *, "FAIL inherited binding: independent oracle failed"
            call show_file(case_dir//"/out.txt")
            error stop 1
        end if
    end subroutine check_inherited

    function named_pass_source() result(text)
        character(len=:), allocatable :: text
        text = "module named_pass"//nl// &
            "    type :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure, pass(self) :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure function value(x, self) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        class(box_t), intent(in) :: self"//nl// &
            "        real(8) :: y"//nl// &
            "        y = 3.0d0*x + 1.0d0"//nl// &
            "    end function value"//nl// &
            "    pure function top(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(box_t) :: b"//nl// &
            "        real(8) :: y"//nl// &
            "        y = b%value(x)"//nl// &
            "    end function top"//nl// &
            "end module named_pass"//nl
    end function named_pass_source

    function nopass_source() result(text)
        character(len=:), allocatable :: text
        text = "module nopass_case"//nl// &
            "    type :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure, nopass :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure function value(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = 3.0d0*x + 1.0d0"//nl// &
            "    end function value"//nl// &
            "    pure function top(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(box_t) :: b"//nl// &
            "        real(8) :: y"//nl// &
            "        y = b%value(x)"//nl// &
            "    end function top"//nl// &
            "end module nopass_case"//nl
    end function nopass_source

    function nopass_scope_source() result(text)
        character(len=:), allocatable :: text
        text = "module nopass_scope_case"//nl// &
            "    type :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure, nopass :: value => two_value"//nl// &
            "    end type box_t"//nl// &
            "    type :: other_t"//nl// &
            "    contains"//nl// &
            "        procedure, nopass :: value => one_value"//nl// &
            "    end type other_t"//nl// &
            "contains"//nl// &
            "    pure real(8) function one_value(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = x + 1.0d0"//nl// &
            "    end function one_value"//nl// &
            "    pure real(8) function two_value(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = 3.0d0*x + 1.0d0"//nl// &
            "    end function two_value"//nl// &
            "    pure real(8) function prior(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(other_t) :: b"//nl// &
            "        y = b%value(x)"//nl// &
            "    end function prior"//nl// &
            "    pure real(8) function top(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(box_t) :: b"//nl// &
            "        y = b%value(x)"//nl// &
            "    end function top"//nl// &
            "end module nopass_scope_case"//nl
    end function nopass_scope_source

    function ambiguous_source() result(text)
        character(len=:), allocatable :: text
        text = "module ambiguous_one"//nl// &
            "    type :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure, nopass :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure real(8) function value(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = x + 1.0d0"//nl// &
            "    end function value"//nl// &
            "end module ambiguous_one"//nl// &
            "module ambiguous_two"//nl// &
            "    type :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure, nopass :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure real(8) function value(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = 2.0d0*x + 3.0d0"//nl// &
            "    end function value"//nl// &
            "    pure real(8) function top(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(box_t) :: b"//nl// &
            "        y = b%value(x)"//nl// &
            "    end function top"//nl// &
            "end module ambiguous_two"//nl
    end function ambiguous_source

    function active_receiver_source() result(text)
        character(len=:), allocatable :: text
        text = "module active_receiver_case"//nl// &
            "    type :: box_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    contains"//nl// &
            "        procedure :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure real(8) function value(self, x) result(y)"//nl// &
            "        class(box_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = self%scale*x"//nl// &
            "    end function value"//nl// &
            "    pure real(8) function top(model, x) result(y)"//nl// &
            "        type(box_t), intent(in) :: model"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function top"//nl// &
            "end module active_receiver_case"//nl
    end function active_receiver_source

    function allocatable_receiver_source() result(text)
        character(len=:), allocatable :: text
        text = "module allocatable_receiver_case"//nl// &
            "    type :: box_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    contains"//nl// &
            "        procedure :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure real(8) function value(self, x) result(y)"//nl// &
            "        class(box_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = self%scale*x"//nl// &
            "    end function value"//nl// &
            "    pure real(8) function top(model, x) result(y)"//nl// &
            "        type(box_t), allocatable, intent(in) :: model"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function top"//nl// &
            "end module allocatable_receiver_case"//nl
    end function allocatable_receiver_source

    function array_receiver_source() result(text)
        character(len=:), allocatable :: text
        text = "module array_receiver_case"//nl// &
            "    type :: box_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    contains"//nl// &
            "        procedure :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure real(8) function value(self, x) result(y)"//nl// &
            "        class(box_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = self%scale*x"//nl// &
            "    end function value"//nl// &
            "    pure real(8) function top(model, x) result(y)"//nl// &
            "        type(box_t), intent(in) :: model(:)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function top"//nl// &
            "end module array_receiver_case"//nl
    end function array_receiver_source

    function active_receiver_subroutine_source() result(text)
        character(len=:), allocatable :: text
        text = "module active_receiver_subroutine_case"//nl// &
            "    type :: box_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    contains"//nl// &
            "        procedure :: apply"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure subroutine apply(self, x, y)"//nl// &
            "        class(box_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        y = self%scale*x"//nl// &
            "    end subroutine apply"//nl// &
            "    pure subroutine top(model, x, y)"//nl// &
            "        type(box_t), intent(in) :: model"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8), intent(out) :: y"//nl// &
            "        call model%apply(x, y)"//nl// &
            "    end subroutine top"//nl// &
            "end module active_receiver_subroutine_case"//nl
    end function active_receiver_subroutine_source

    function inherited_source() result(text)
        character(len=:), allocatable :: text
        text = "module inherited_case"//nl// &
            "    type :: base_t"//nl// &
            "        real(8) :: scale"//nl// &
            "    contains"//nl// &
            "        procedure, pass(self) :: value"//nl// &
            "    end type base_t"//nl// &
            "    type, extends(base_t) :: box_t"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure function value(x, self) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        class(base_t), intent(in) :: self"//nl// &
            "        real(8) :: y"//nl// &
            "        y = self%scale*x + 1.0d0"//nl// &
            "    end function value"//nl// &
            "    pure function top(model, x) result(y)"//nl// &
            "        type(box_t), intent(in) :: model"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function top"//nl// &
            "end module inherited_case"//nl
    end function inherited_source

    function generic_source() result(text)
        character(len=:), allocatable :: text
        text = "module generic_case"//nl// &
            "    type :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure :: value_impl"//nl// &
            "        generic :: value => value_impl"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure function value_impl(self, x) result(y)"//nl// &
            "        class(box_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = x"//nl// &
            "    end function value_impl"//nl// &
            "    pure function top(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(box_t) :: b"//nl// &
            "        real(8) :: y"//nl// &
            "        y = b%value(x)"//nl// &
            "    end function top"//nl// &
            "end module generic_case"//nl
    end function generic_source

    function runtime_dispatch_source() result(text)
        character(len=:), allocatable :: text
        text = "module runtime_dispatch_case"//nl// &
            "    type :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure, pass(self) :: value"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure function value(self, x) result(y)"//nl// &
            "        class(box_t), intent(in) :: self"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = x"//nl// &
            "    end function value"//nl// &
            "    pure function top(model, x) result(y)"//nl// &
            "        class(box_t), intent(in) :: model"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        real(8) :: y"//nl// &
            "        y = model%value(x)"//nl// &
            "    end function top"//nl// &
            "end module runtime_dispatch_case"//nl
    end function runtime_dispatch_source

    function deferred_source() result(text)
        character(len=:), allocatable :: text
        text = "module deferred_case"//nl// &
            "    type, abstract :: box_t"//nl// &
            "    contains"//nl// &
            "        procedure(value_iface), deferred :: value"//nl// &
            "    end type box_t"//nl// &
            "    abstract interface"//nl// &
            "        pure function value_iface(self, x) result(y)"//nl// &
            "            import box_t"//nl// &
            "            class(box_t), intent(in) :: self"//nl// &
            "            real(8), intent(in) :: x"//nl// &
            "            real(8) :: y"//nl// &
            "        end function value_iface"//nl// &
            "    end interface"//nl// &
            "contains"//nl// &
            "    pure function top(x) result(y)"//nl// &
            "        real(8), intent(in) :: x"//nl// &
            "        type(box_t) :: b"//nl// &
            "        real(8) :: y"//nl// &
            "        y = b%value(x)"//nl// &
            "    end function top"//nl// &
            "end module deferred_case"//nl
    end function deferred_source

end program test_type_bound_oracle
