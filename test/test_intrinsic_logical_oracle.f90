program test_intrinsic_logical_oracle
    !! Independent numerical oracle for intrinsic unary .not. lowering.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir
    type(fad_result_t) :: jvp
    integer :: unit, stat

    source = &
        "subroutine logical_kernel(x, y)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(out) :: y"//nl// &
        "    if (.not. (x > 0.0d0)) then"//nl// &
        "        y = x*x + 1.0d0"//nl// &
        "    else"//nl// &
        "        y = 3.0d0*x"//nl// &
        "    end if"//nl// &
        "end subroutine logical_kernel"//nl

    jvp = fad_jvp(source, [character(len=1) :: "x"], &
        name="logical_kernel_jvp")
    if (.not. jvp%ok) error stop "intrinsic .not. JVP generation failed: "//jvp%message

    dir = "build/oracle_intrinsic_logical"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create intrinsic logical oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') "module logical_primal"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module logical_primal"
    close (unit)

    open (newunit=unit, file=dir//"/tangent.f90", status="replace", action="write")
    write (unit, '(a)') "module logical_tangent"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') "end module logical_tangent"
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -pedantic-errors -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/tangent.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "intrinsic logical generated source did not compile"
    end if
    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "intrinsic logical independent numerical oracle failed"

    print '(a)', "test_intrinsic_logical_oracle: all cases passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use logical_primal, only: logical_kernel"//nl// &
            "    use logical_tangent, only: logical_kernel_jvp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, y, yd, yp, ym, h"//nl// &
            "    x = -0.73d0"//nl// &
            "    xd = 0.41d0"//nl// &
            "    call logical_kernel_jvp(x, xd, y, yd)"//nl// &
            "    if (abs(y - (x*x + 1.0d0)) > 1.0d-12) error stop 1"//nl// &
            "    if (abs(yd - 2.0d0*x*xd) > 1.0d-12) error stop 2"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call logical_kernel(x + h*xd, yp)"//nl// &
            "    call logical_kernel(x - h*xd, ym)"//nl// &
            "    if (abs(yd - (yp - ym)/(2.0d0*h)) > 1.0d-7) error stop 3"//nl// &
            "    x = 0.73d0"//nl// &
            "    call logical_kernel_jvp(x, xd, y, yd)"//nl// &
            "    if (abs(y - 3.0d0*x) > 1.0d-12) error stop 4"//nl// &
            "    if (abs(yd - 3.0d0*xd) > 1.0d-12) error stop 5"//nl// &
            "    print '(a)', 'intrinsic logical numerical oracle pass'"//nl// &
            "end program driver"//nl
    end function driver_text

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
            print '(a)', trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_intrinsic_logical_oracle
