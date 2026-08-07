program test_allocation_lifetime_oracle
    !! Independent behavioral oracle for the bounded forward ownership slice.
    !! The primal uses explicit ALLOCATE, SOURCE=, MOVE_ALLOC, and DEALLOCATE;
    !! the generated JVP is checked against a central finite difference.
    !! Reverse mode remains a named boundary until allocation-state replay is
    !! implemented.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module allocation_lifetime_case"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    function allocation_path(x, n) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        integer, intent(in) :: n"//nl// &
        "        integer :: i"//nl// &
        "        real(8), allocatable :: scratch(:)"//nl// &
        "        real(8), allocatable :: moved(:)"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(scratch(n), mold=x)"//nl// &
        "        do i = 1, n"//nl// &
        "            scratch(i) = 2.0d0*x"//nl// &
        "        end do"//nl// &
        "        allocate(moved, source=scratch)"//nl// &
        "        call move_alloc(scratch, moved)"//nl// &
        "        out = sum(moved)"//nl// &
        "        deallocate(moved)"//nl// &
        "    end function allocation_path"//nl// &
        "end module allocation_lifetime_case"//nl
    character(len=*), parameter :: clean_source = &
        "function source_name(x) result(out)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8) :: source, out"//nl// &
        "    source = x ! allocate is only a comment here"//nl// &
        "    out = source*source"//nl// &
        "end function source_name"//nl
    character(len=*), parameter :: global_source = &
        "module global_state_case"//nl// &
        "    real(8), allocatable :: state(:)"//nl// &
        "contains"//nl// &
        "    function uses_state(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(state(1))"//nl// &
        "        state(1) = x"//nl// &
        "        out = state(1)"//nl// &
        "        deallocate(state)"//nl// &
        "    end function uses_state"//nl// &
        "end module global_state_case"//nl

    type(fad_result_t) :: clean, global, jvp, vjp
    character(len=:), allocatable :: dir, driver, derivatives
    integer :: unit, stat

    clean = fad_jvp(clean_source, [character(len=1) :: "x"], from="source_name")
    if (.not. clean%ok) then
        print *, "FAIL allocation scanner false positive: ", clean%message
        error stop 7
    end if

    global = fad_jvp(global_source, [character(len=1) :: "x"], &
        from="uses_state")
    if (global%ok .or. .not. allocated(global%message) .or. &
        index(global%message, "module-level allocatable") == 0) then
        print *, "FAIL active global mutable state was not refused: ", &
            global%message
        error stop 8
    end if

    jvp = fad_jvp(source, [character(len=1) :: "x"], from="allocation_path", &
        name="allocation_path_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL allocation lifetime JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="out", &
        from="allocation_path")
    call assert_replay_refusal(vjp)

    dir = "build/oracle/allocation_lifetime"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create allocation oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    derivatives = "module allocation_lifetime_derivatives"//nl// &
        "contains"//nl//jvp%code//nl// &
        "end module allocation_lifetime_derivatives"//nl
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') derivatives
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use allocation_lifetime_case, only: allocation_path"//nl// &
        "    use allocation_lifetime_derivatives, only: allocation_path_jvp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: out, out_d, x, x_d, h, fp, fm, fd"//nl// &
        "    x = 1.25d0"//nl// &
        "    x_d = -0.75d0"//nl// &
        "    call allocation_path_jvp(x, x_d, 3, out, out_d)"//nl// &
        "    if (abs(out - 7.5d0) > 1.0d-12) error stop 3"//nl// &
        "    if (abs(out_d - 6.0d0*x_d) > 1.0d-12) error stop 4"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = allocation_path(x + h*x_d, 3)"//nl// &
        "    fm = allocation_path(x - h*x_d, 3)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(out_d - fd) > 1.0d-7) error stop 5"//nl// &
        "    print *, 'allocation lifetime JVP oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL allocation lifetime: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 4
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL allocation lifetime: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 5
    end if
    print *, "test_allocation_lifetime_oracle: all cases passed"

contains

    subroutine assert_replay_refusal(result)
        type(fad_result_t), intent(in) :: result

        if (result%ok) then
            print *, "FAIL allocation lifetime: VJP unexpectedly generated"
            error stop 6
        end if
        if (.not. allocated(result%message) .or. &
            index(result%message, "allocation lifetime") == 0 .or. &
            index(result%message, "replay tape") == 0) then
            print *, "FAIL allocation lifetime diagnostic: ", result%message
            error stop 6
        end if
    end subroutine assert_replay_refusal

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

end program test_allocation_lifetime_oracle
