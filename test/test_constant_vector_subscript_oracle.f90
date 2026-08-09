program test_constant_vector_subscript_oracle
    !! Independent JVP/VJP oracle for static unique vector subscripts.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, tangent, adjoint, driver, dir
    type(fad_result_t) :: jvp, vjp, refused
    integer :: unit, stat

    source = positive_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], name="k_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL constant vector JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        name="k_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL constant vector VJP generation: ", vjp%message
        error stop 2
    end if

    dir = "build/oracle/constant_vector_subscript"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 3

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module constant_vector_primal"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module constant_vector_primal"
    close (unit)

    tangent = "module constant_vector_tangent"//nl// &
        "    implicit none"//nl//"contains"//nl//jvp%code// &
        "end module constant_vector_tangent"//nl
    open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
        action="write")
    write (unit, '(a)') tangent
    close (unit)

    adjoint = "module constant_vector_adjoint"//nl// &
        "    implicit none"//nl//"contains"//nl//vjp%code// &
        "end module constant_vector_adjoint"//nl
    open (newunit=unit, file=dir//"/adjoint.f90", status="replace", &
        action="write")
    write (unit, '(a)') adjoint
    close (unit)

    driver = driver_text()
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/tangent.f90 "// &
        dir//"/adjoint.f90 "//dir//"/driver.f90 > "//dir// &
        "/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL constant vector generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 4
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL constant vector independent oracle"
        call show_file(dir//"/out.txt")
        error stop 5
    end if

    refused = fad_jvp(dynamic_source(), [character(len=1) :: "x"])
    call require_refusal(refused, "unsupported vector subscript")
    refused = fad_jvp(duplicate_source(), [character(len=1) :: "x"])
    call require_refusal(refused, "unsupported vector subscript")
    refused = fad_jvp(out_of_range_source(), [character(len=1) :: "x"])
    call require_refusal(refused, "unsupported vector subscript")

    print *, "test_constant_vector_subscript_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(x, y)"//nl// &
            "    real(8), intent(in) :: x(3)"//nl// &
            "    real(8), intent(out) :: y(3)"//nl// &
            "    integer, parameter :: idx(3) = (/2, 3, 1/)"//nl// &
            "    y = 2.0d0 * x(idx)"//nl// &
            "end subroutine k"//nl
    end function positive_source

    function dynamic_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(x, idx, y)"//nl// &
            "    real(8), intent(in) :: x(:)"//nl// &
            "    integer, intent(in) :: idx(:)"//nl// &
            "    real(8), intent(out) :: y(:)"//nl// &
            "    y = x(idx)"//nl// &
            "end subroutine k"//nl
    end function dynamic_source

    function duplicate_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(x, y)"//nl// &
            "    real(8), intent(in) :: x(3)"//nl// &
            "    real(8), intent(out) :: y(2)"//nl// &
            "    integer, parameter :: idx(2) = (/1, 1/)"//nl// &
            "    y = x(idx)"//nl// &
            "end subroutine k"//nl
    end function duplicate_source

    function out_of_range_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(x, y)"//nl// &
            "    real(8), intent(in) :: x(3)"//nl// &
            "    real(8), intent(out) :: y(2)"//nl// &
            "    integer, parameter :: idx(2) = (/1, 4/)"//nl// &
            "    y = x(idx)"//nl// &
            "end subroutine k"//nl
    end function out_of_range_source

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use constant_vector_primal, only: k"//nl// &
            "    use constant_vector_tangent, only: k_jvp"//nl// &
            "    use constant_vector_adjoint, only: k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x(3), xd(3), y(3), yd(3), yp(3), ym(3)"//nl// &
            "    real(8) :: seed(3), xb(3), h, lhs, rhs"//nl// &
            "    real(8) :: expected(3), expected_d(3)"//nl// &
            "    logical :: bad"//nl// &
            "    x = [0.7d0, -0.4d0, 1.1d0]"//nl// &
            "    xd = [-0.2d0, 0.6d0, 0.3d0]"//nl// &
            "    call k_jvp(x, xd, y, yd)"//nl// &
            "    expected = 2.0d0 * [x(2), x(3), x(1)]"//nl// &
            "    expected_d = 2.0d0 * [xd(2), xd(3), xd(1)]"//nl// &
            "    bad = maxval(abs(y-expected)) > 1.0d-12"//nl// &
            "    bad = bad .or. maxval(abs(yd-expected_d)) > 1.0d-12"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call k(x+h*xd, yp)"//nl// &
            "    call k(x-h*xd, ym)"//nl// &
            "    bad = bad .or. maxval(abs(yd-(yp-ym)/(2.0d0*h))) > 1.0d-7"//nl// &
            "    seed = [0.31d0, -0.77d0, 1.12d0]"//nl// &
            "    call k_vjp(x, y, seed, xb)"//nl// &
            "    if (maxval(abs(xb-2.0d0*[seed(3), seed(1), seed(2)])) > 1.0d-12) bad=.true."//nl// &
            "    lhs = sum(seed*expected_d)"//nl// &
            "    rhs = sum(xb*xd)"//nl// &
            "    if (abs(lhs-rhs) > 1.0d-12*max(1.0d0,abs(lhs))) bad=.true."//nl// &
            "    if (bad) error stop 1"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine require_refusal(result, needle)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: needle

        if (result%ok) then
            print *, "FAIL expected refusal: accepted unsupported vector"
            error stop 6
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL expected vector diagnostic: empty message"
            error stop 7
        end if
        if (index(result%message, needle) == 0) then
            print *, "FAIL unexpected vector diagnostic: ", result%message
            error stop 8
        end if
    end subroutine require_refusal

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=512) :: line

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_constant_vector_subscript_oracle
