program test_assumed_size_boundary_oracle
    !! Assumed-size arrays have no descriptor and cannot receive a whole-array
    !! tangent or adjoint assignment.  They are therefore a named refusal;
    !! explicit- and assumed-shape arrays retain compiled numerical behavior.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0
    call check_refusal(failures)
    call check_shape("explicit", "z(n)", failures)
    call check_shape("assumed", "z(:)", failures)
    if (failures /= 0) error stop 1
    print '(a)', "test_assumed_size_boundary_oracle: all cases passed"

contains

    subroutine check_refusal(failures)
        integer, intent(inout) :: failures
        character(len=:), allocatable :: source
        type(fad_result_t) :: jvp, vjp

        source = kernel_source("z(*)")
        jvp = fad_jvp(source, [character(len=1) :: "z"], from="kernel")
        call require_refusal(jvp, "forward mode", failures)
        vjp = fad_vjp(source, [character(len=1) :: "z"], dependent="y", &
            from="kernel")
        call require_refusal(vjp, "reverse mode", failures)
    end subroutine check_refusal

    subroutine require_refusal(result, prefix, failures)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: prefix
        integer, intent(inout) :: failures

        if (result%ok) then
            print *, "FAIL assumed-size ", trim(prefix), " was accepted"
            failures = failures + 1
            return
        end if
        if (index(result%message, "active assumed-size dummy 'z'") == 0) then
            print *, "FAIL assumed-size ", trim(prefix), " diagnostic: ", &
                result%message
            failures = failures + 1
        end if
        if (index(result%message, "line ") == 0) then
            print *, "FAIL assumed-size ", trim(prefix), " lacks source line"
            failures = failures + 1
        end if
    end subroutine require_refusal

    subroutine check_shape(label, declaration, failures)
        character(len=*), intent(in) :: label, declaration
        integer, intent(inout) :: failures
        character(len=:), allocatable :: source, dir, driver
        type(fad_result_t) :: jvp, vjp
        integer :: stat

        source = kernel_source(declaration)
        jvp = fad_jvp(source, [character(len=1) :: "z"], from="kernel", &
            name="kernel_jvp")
        vjp = fad_vjp(source, [character(len=1) :: "z"], dependent="y", &
            from="kernel", name="kernel_vjp")
        if (.not. jvp%ok .or. .not. vjp%ok) then
            print *, "FAIL ", trim(label), " shape generation"
            if (.not. jvp%ok) print *, "  JVP: ", jvp%message
            if (.not. vjp%ok) print *, "  VJP: ", vjp%message
            failures = failures + 1
            return
        end if

        dir = "build/oracle/assumed_size/"//trim(label)
        call execute_command_line("mkdir -p "//dir, exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL ", trim(label), " could not create oracle directory"
            failures = failures + 1
            return
        end if
        call write_file(dir//"/primal.f90", "module primal_mod"//nl// &
            "contains"//nl//source//"end module primal_mod"//nl)
        call write_file(dir//"/derivatives.f90", "module derivative_mod"//nl// &
            "contains"//nl//jvp%code//nl//vjp%code// &
            "end module derivative_mod"//nl)
        driver = driver_source(label)
        call write_file(dir//"/driver.f90", driver)

        call execute_command_line( &
            "gfortran -std=f2018 -pedantic-errors -O2 -J"//dir//" -I"//dir// &
            " -o "//dir//"/run "//dir//"/primal.f90 "//dir// &
            "/derivatives.f90 "//dir//"/driver.f90 > "//dir// &
            "/build.log 2>&1", exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL ", trim(label), " generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if
        call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
            exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL ", trim(label), " numerical oracle"
            call show_file(dir//"/out.txt")
            failures = failures + 1
        end if
    end subroutine check_shape

    function kernel_source(declaration) result(text)
        character(len=*), intent(in) :: declaration
        character(len=:), allocatable :: text

        text = "subroutine kernel(n, z, y)"//nl// &
            "    integer, intent(in) :: n"//nl// &
            "    real(8), intent(in) :: "//trim(declaration)//nl// &
            "    real(8), intent(out) :: y"//nl// &
            "    y = z(1)*z(1) + 2.0d0*z(2)"//nl// &
            "end subroutine kernel"//nl
    end function kernel_source

    function driver_source(label) result(text)
        character(len=*), intent(in) :: label
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use primal_mod, only: kernel"//nl// &
            "    use derivative_mod, only: kernel_jvp, kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 3"//nl// &
            "    real(8) :: z(n), zd(n), zb(n), y, yd, yb, yp, ym, fd, h"//nl// &
            "    real(8) :: lhs, rhs"//nl// &
            "    z = [1.2d0, -0.7d0, 0.4d0]"//nl// &
            "    zd = [-0.3d0, 0.8d0, 0.2d0]"//nl// &
            "    call kernel_jvp(n, z, zd, y, yd)"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call kernel(n, z + h*zd, yp)"//nl// &
            "    call kernel(n, z - h*zd, ym)"//nl// &
            "    fd = (yp - ym)/(2.0d0*h)"//nl// &
            "    if (abs(yd-fd) > 1.0d-7) error stop 1"//nl// &
            "    yb = 1.0d0"//nl// &
            "    call kernel_vjp(n, z, y, yb, zb)"//nl// &
            "    lhs = sum(zb*zd)"//nl// &
            "    rhs = yd*yb"//nl// &
            "    if (abs(lhs-rhs) > 1.0d-12) error stop 2"//nl// &
            "    print '(a)', '"//trim(label)//" shape oracle pass'"//nl// &
            "end program driver"//nl
    end function driver_source

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit

        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, '(a)') text
        close (unit)
    end subroutine write_file

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: ios, unit

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print '(a)', "    "//trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_assumed_size_boundary_oracle
