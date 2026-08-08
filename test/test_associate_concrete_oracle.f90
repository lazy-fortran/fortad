program test_associate_concrete_oracle
    !! Independent oracle for the bounded concrete ASSOCIATE slice.
    !! The positive path checks a hand JVP, central differences, and the
    !! reverse adjoint identity. The negative paths check named boundaries.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, tangent, derivatives, driver, dir
    type(fad_result_t) :: jvp, vjp, refused
    integer :: unit, stat

    source = positive_source()
    jvp = fad_jvp(source, [character(len=1) :: "x"], name="associate_jvp")
    if (.not. jvp%ok) error stop "concrete ASSOCIATE JVP generation failed: "//jvp%message
    vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="y", &
        name="associate_vjp")
    if (.not. vjp%ok) error stop "concrete ASSOCIATE VJP generation failed: "//vjp%message

    dir = "build/oracle/associate_concrete"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create ASSOCIATE oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') "module associate_primal"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module associate_primal"
    close (unit)

    tangent = "module associate_tangent"//nl// &
        "contains"//nl//jvp%code// &
        "end module associate_tangent"//nl
    open (newunit=unit, file=dir//"/tangent.f90", status="replace", action="write")
    write (unit, '(a)') tangent
    close (unit)

    derivatives = "module associate_derivatives"//nl// &
        "contains"//nl//vjp%code// &
        "end module associate_derivatives"//nl
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", action="write")
    write (unit, '(a)') derivatives
    close (unit)

    driver = driver_text()
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -pedantic-errors -O2 -J"//dir//" -I"//dir// &
        " -o "//dir//"/run "//dir//"/primal.f90 "//dir//"/tangent.f90 "// &
        dir//"/derivatives.f90 "//dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL: generated concrete ASSOCIATE source did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL: concrete ASSOCIATE independent oracle"
        call show_file(dir//"/out.txt")
        error stop 2
    end if

    refused = fad_jvp(expression_source(), [character(len=1) :: "x"])
    call require_refusal(refused, "selector is not a resolved storage designator", &
        "computed ASSOCIATE selector")

    refused = fad_jvp(allocatable_source(), [character(len=1) :: "x"])
    call require_refusal(refused, "allocatable selector lifetime is not tracked", &
        "allocatable ASSOCIATE selector")

    refused = fad_jvp(array_element_source(), [character(len=1) :: "x"])
    call require_refusal(refused, "only scalar selectors are supported", &
        "array-element ASSOCIATE selector")

    print '(a)', "test_associate_concrete_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine associate_kernel(x, y)"//nl// &
            "    real(8), intent(in) :: x"//nl// &
            "    real(8), intent(out) :: y"//nl// &
            "    associate (alias => x)"//nl// &
            "        y = alias*alias + 2.0d0*alias"//nl// &
            "    end associate"//nl// &
            "end subroutine associate_kernel"//nl
    end function positive_source

    function expression_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine associate_expression(x, y)"//nl// &
            "    real(8), intent(in) :: x"//nl// &
            "    real(8), intent(out) :: y"//nl// &
            "    associate (alias => x + 1.0d0)"//nl// &
            "        y = alias"//nl// &
            "    end associate"//nl// &
            "end subroutine associate_expression"//nl
    end function expression_source

    function allocatable_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine associate_allocatable(x, y)"//nl// &
            "    real(8), allocatable, intent(inout) :: x"//nl// &
            "    real(8), intent(out) :: y"//nl// &
            "    associate (alias => x)"//nl// &
            "        y = alias"//nl// &
            "    end associate"//nl// &
            "end subroutine associate_allocatable"//nl
    end function allocatable_source

    function array_element_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine associate_array_element(x, y)"//nl// &
            "    real(8), intent(in) :: x(3)"//nl// &
            "    real(8), intent(out) :: y"//nl// &
            "    associate (alias => x(2))"//nl// &
            "        y = alias"//nl// &
            "    end associate"//nl// &
            "end subroutine associate_array_element"//nl
    end function array_element_source

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use associate_primal, only: associate_kernel"//nl// &
            "    use associate_tangent, only: associate_jvp"//nl// &
            "    use associate_derivatives, only: associate_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, y, yd, yp, ym, seed, xb, h"//nl// &
            "    real(8) :: expected, lhs, rhs"//nl// &
            "    x = 0.73d0"//nl// &
            "    xd = -0.41d0"//nl// &
            "    call associate_jvp(x, xd, y, yd)"//nl// &
            "    expected = (2.0d0*x + 2.0d0)*xd"//nl// &
            "    if (abs(yd - expected) > 1.0d-12) error stop 3"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call associate_kernel(x + h*xd, yp)"//nl// &
            "    call associate_kernel(x - h*xd, ym)"//nl// &
            "    if (abs(yd - (yp - ym)/(2.0d0*h)) > 1.0d-7) error stop 4"//nl// &
            "    seed = 0.67d0"//nl// &
            "    call associate_vjp(x, y, seed, xb)"//nl// &
            "    if (abs(xb - seed*(2.0d0*x + 2.0d0)) > 1.0d-12) error stop 5"//nl// &
            "    lhs = seed*yd"//nl// &
            "    rhs = xb*xd"//nl// &
            "    if (abs(lhs - rhs) > 1.0d-12*max(1.0d0, abs(lhs))) error stop 6"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine require_refusal(result, reason, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: reason, label

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, reason) == 0) then
            print *, "FAIL: ", trim(label), " was accepted or unnamed: ", &
                result%message
            error stop 7
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

end program test_associate_concrete_oracle
