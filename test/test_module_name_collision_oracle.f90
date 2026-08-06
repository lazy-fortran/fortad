program test_module_name_collision_oracle
    !! The module and its contained procedure share one Fortran namespace.
    !! Exercise a case-only collision and ask an independent compiler to prove
    !! that the generated wrapper still exports a callable derivative.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir
    type(fad_result_t) :: jvp
    integer :: stat, unit

    source = "subroutine square(x, y)"//nl// &
        "    implicit none"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(out) :: y"//nl// &
        "    y = x*x"//nl// &
        "end subroutine square"//nl

    jvp = fad_jvp(source, ["x"], name="square_jvp", &
        module_name="SQUARE_JVP")
    if (.not. jvp%ok) then
        print *, "FAIL module_name_collision: generation failed: ", jvp%message
        error stop 1
    end if

    dir = "build/oracle_module_name_collision"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create oracle directory"

    open (newunit=unit, file=dir//"/generated.f90", status="replace", &
        action="write")
    write (unit, '(a)') jvp%code
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line( &
        "cd "//dir//" && gfortran -std=f2018 -o run generated.f90 "// &
        "driver.f90 > build.log 2>&1 && ./run > run.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL module_name_collision: generated interface did not compile and run"
        call show_file(dir//"/build.log")
        call show_file(dir//"/run.log")
        error stop 1
    end if

    print *, "test_module_name_collision_oracle: all cases passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use SQUARE_JVP_module, only: square_jvp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: y, y_d"//nl// &
            "    call square_jvp(3.0d0, 1.0d0, y, y_d)"//nl// &
            "    if (abs(y - 9.0d0) > 1.0d-14) error stop 1"//nl// &
            "    if (abs(y_d - 6.0d0) > 1.0d-14) error stop 2"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: ios, log_unit

        open (newunit=log_unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (log_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (log_unit)
    end subroutine show_file

end program test_module_name_collision_oracle
