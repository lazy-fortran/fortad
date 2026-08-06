program test_allocation_lifetime_oracle
    !! Independent boundary gate for allocatable storage semantics.
    !!
    !! The primal is compiled and run with allocation, source=, mold=,
    !! automatic reallocation, deep assignment, deallocation, and move_alloc.
    !! FortAD must refuse the derivative before lowering because its IR does not
    !! carry allocation state.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module allocation_lifetime_case"//nl// &
        "    implicit none"//nl// &
        "    type :: box_t"//nl// &
        "        real(8), allocatable :: values(:)"//nl// &
        "    end type box_t"//nl// &
        "contains"//nl// &
        "    function allocation_path(x, n) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        integer, intent(in) :: n"//nl// &
        "        type(box_t) :: box"//nl// &
        "        real(8), allocatable :: scratch(:)"//nl// &
        "        real(8) :: out"//nl// &
        "        allocate(scratch(n), mold=x)"//nl// &
        "        allocate(box%values, source=scratch)"//nl// &
        "        box%values = scratch"//nl// &
        "        deallocate(box%values)"//nl// &
        "        allocate(box%values(1))"//nl// &
        "        box%values = scratch"//nl// &
        "        deallocate(scratch)"//nl// &
        "        allocate(scratch(n))"//nl// &
        "        scratch = 2.0d0*x"//nl// &
        "        deallocate(box%values)"//nl// &
        "        call move_alloc(scratch, box%values)"//nl// &
        "        out = sum(box%values)"//nl// &
        "        deallocate(box%values)"//nl// &
        "    end function allocation_path"//nl// &
        "end module allocation_lifetime_case"//nl
    character(len=*), parameter :: clean_source = &
        "function source_name(x) result(out)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8) :: source, out"//nl// &
        "    source = x ! allocate is only a comment here"//nl// &
        "    out = source*source"//nl// &
        "end function source_name"//nl

    type(fad_result_t) :: clean, jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    clean = fad_jvp(clean_source, [character(len=1) :: "x"], &
        from="source_name")
    if (.not. clean%ok) then
        print *, "FAIL allocation scanner false positive: ", clean%message
        error stop 7
    end if
    jvp = fad_jvp(source, [character(len=1) :: "x"], from="allocation_path")
    call assert_refusal(jvp, "jvp")
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="out", &
        from="allocation_path")
    call assert_refusal(vjp, "vjp")

    dir = "build/oracle/allocation_lifetime"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create allocation oracle directory"
    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') source
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use allocation_lifetime_case, only: allocation_path"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: out"//nl// &
        "    out = allocation_path(1.25d0, 3)"//nl// &
        "    if (abs(out - 7.5d0) > 1.0d-12) error stop 3"//nl// &
        "    print *, 'allocation lifetime primal oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL allocation lifetime: primal did not compile"
        call show_file(dir//"/build.log")
        error stop 4
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL allocation lifetime: primal oracle failed"
        call show_file(dir//"/out.txt")
        error stop 5
    end if
    print *, "test_allocation_lifetime_oracle: all cases passed"

contains

    subroutine assert_refusal(result, mode)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: mode

        if (result%ok) then
            print *, "FAIL allocation lifetime: ", trim(mode), &
                " unexpectedly generated a derivative"
            error stop 1
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL allocation lifetime: ", trim(mode), &
                " did not report a boundary"
            error stop 2
        end if
        if (index(result%message, "allocation lifetime") == 0 .or. &
            index(result%message, "allocatable") == 0 .or. &
            index(result%message, "line 4") == 0) then
            print *, "FAIL allocation lifetime diagnostic: ", result%message
            error stop 6
        end if
    end subroutine assert_refusal

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
