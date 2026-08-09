program test_spread_oracle
    !! Independent behavioral oracle for the linear SPREAD intrinsic.
    !! Forward mode must spread the source tangent; reverse mode must sum the
    !! output cotangent over the replicated dimension.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module spread_case"//nl// &
        "    implicit none"//nl// &
        "contains"//nl// &
        "    pure subroutine top(x, y)"//nl// &
        "        real(8), intent(in) :: x(:)"//nl// &
        "        real(8), intent(out) :: y(:,:)"//nl// &
        "        y = spread(x, 2, 2)"//nl// &
        "    end subroutine top"//nl// &
        "end module spread_case"//nl

    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    jvp = fad_jvp(source, ["x"], from="top", name="top_jvp")
    vjp = fad_vjp(source, ["x"], dependent="y", from="top", name="top_vjp")
    call require_ok(jvp, "JVP")
    call require_ok(vjp, "VJP")

    dir = "build/oracle/spread"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create SPREAD oracle directory"

    call write_text(dir//"/primal.f90", source)
    call write_text(dir//"/derivatives.f90", &
        "module spread_derivatives"//nl//"contains"//nl//jvp%code//vjp%code// &
        "end module spread_derivatives"//nl)

    driver = &
        "program driver"//nl// &
        "    use spread_case, only: top"//nl// &
        "    use spread_derivatives, only: top_jvp, top_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x(3), x_d(3), x_b(3)"//nl// &
        "    real(8) :: y(3, 2), y_d(3, 2), y_b(3, 2)"//nl// &
        "    real(8) :: yp(3, 2), ym(3, 2), h, fd, dot"//nl// &
        "    x = [1.0d0, 2.0d0, 3.0d0]"//nl// &
        "    x_d = [0.1d0, -0.2d0, 0.3d0]"//nl// &
        "    y_b = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0], [3, 2])"//nl// &
        "    call top_jvp(x, x_d, y, y_d)"//nl// &
        "    if (any(abs(y - spread(x, 2, 2)) > 1.0d-13)) error stop 1"//nl// &
        "    if (any(abs(y_d - spread(x_d, 2, 2)) > 1.0d-13)) error stop 2"//nl// &
        "    call top_vjp(x, y, y_b, x_b)"//nl// &
        "    if (any(abs(x_b - [5.0d0, 7.0d0, 9.0d0]) > 1.0d-13)) error stop 3"//nl// &
        "    dot = sum(y_b*y_d)"//nl// &
        "    if (abs(dot - sum(x_d*x_b)) > 1.0d-13) error stop 4"//nl// &
        "    h = 1.0d-5"//nl// &
        "    call top(x + h*x_d, yp)"//nl// &
        "    call top(x - h*x_d, ym)"//nl// &
        "    fd = (sum(yp) - sum(ym))/(2.0d0*h)"//nl// &
        "    if (abs(fd - sum(y_d)) > 1.0d-9) error stop 5"//nl// &
        "    print *, 'spread numerical oracle pass'"//nl// &
        "contains"//nl// &
        "    subroutine check_unused()"//nl// &
        "    end subroutine check_unused"//nl// &
        "end program driver"//nl
    call write_text(dir//"/driver.f90", driver)

    call execute_command_line( &
        "${FC:-gfortran} -std=f2018 -O0 -J"//dir//" -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90", &
        exitstat=stat)
    if (stat /= 0) error stop "generated SPREAD derivatives did not compile"
    call execute_command_line(dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "SPREAD numerical oracle failed"
    print *, "test_spread_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (.not. result%ok) then
            if (allocated(result%message)) print *, trim(result%message)
            error stop trim(label)//" generation refused"
        end if
    end subroutine require_ok

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: local_unit

        open (newunit=local_unit, file=path, status="replace", action="write")
        write (local_unit, '(a)') text
        close (local_unit)
    end subroutine write_text

end program test_spread_oracle
