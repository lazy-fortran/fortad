program test_contiguous_section_oracle
    !! Independent P7.3 oracle for one contiguous rank-one array section.
    !!
    !! The positive path checks the hand derivative, central differences, and
    !! the adjoint identity.  The negative path checks that an assumed-shape
    !! base without CONTIGUOUS is refused instead of being treated as owned
    !! storage.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, tangent, derivatives, driver, dir
    type(fad_result_t) :: jvp, vjp, refused
    integer :: unit, stat

    source = positive_source()
    jvp = fad_jvp(source, [character(len=1) :: "a"], name="k_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL contiguous section JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, [character(len=1) :: "a"], dependent="y", &
        name="k_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL contiguous section VJP generation: ", vjp%message
        error stop 2
    end if

    dir = "build/oracle/contiguous_section"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 3

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module section_primal"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module section_primal"
    close (unit)

    tangent = "module section_tangent"//nl// &
        "    implicit none"//nl// &
        "contains"//nl//jvp%code// &
        "end module section_tangent"//nl
    open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
        action="write")
    write (unit, '(a)') tangent
    close (unit)

    derivatives = "module section_derivatives"//nl// &
        "    implicit none"//nl// &
        "contains"//nl//vjp%code// &
        "end module section_derivatives"//nl
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", &
        action="write")
    write (unit, '(a)') derivatives
    close (unit)

    driver = driver_text()
    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        "gfortran -std=f2018 -O2 -J"//dir//" -I"//dir//" -o "// &
        dir//"/run "//dir//"/primal.f90 "//dir//"/tangent.f90 "// &
        dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL contiguous section generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 4
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL contiguous section independent oracle"
        call show_file(dir//"/out.txt")
        error stop 5
    end if

    refused = fad_jvp(noncontiguous_source(), [character(len=1) :: "x"])
    if (refused%ok .or. .not. allocated(refused%message) .or. &
        index(refused%message, "not declared contiguous or owning") == 0) then
        print *, "FAIL non-contiguous assumed-shape section was accepted: ", &
            refused%message
        error stop 6
    end if

    print *, "test_contiguous_section_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(a, y)"//nl// &
            "    real(8), intent(in) :: a(3)"//nl// &
            "    real(8), intent(out) :: y(5)"//nl// &
            "    y(1) = 0.0d0"//nl// &
            "    y(2:4) = a(1:3)"//nl// &
            "    y(5) = 1.0d0"//nl// &
            "end subroutine k"//nl
    end function positive_source

    function noncontiguous_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(x, y)"//nl// &
            "    real(8), intent(in) :: x(:)"//nl// &
            "    real(8), intent(out) :: y(:)"//nl// &
            "    y(2:4) = x(1:3)"//nl// &
            "end subroutine k"//nl
    end function noncontiguous_source

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use section_primal, only: k"//nl// &
            "    use section_tangent, only: k_jvp"//nl// &
            "    use section_derivatives, only: k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: a(3), ad(3), y(5), yd(5), yp(5), ym(5)"//nl// &
            "    real(8) :: seed(5), ab(3), h, lhs, rhs"//nl// &
            "    real(8) :: expected(5), expected_d(5)"//nl// &
            "    integer :: i"//nl// &
            "    logical :: bad"//nl// &
            "    a = [0.73d0, -0.41d0, 1.17d0]"//nl// &
            "    ad = [-0.19d0, 0.61d0, -0.37d0]"//nl// &
            "    call k_jvp(a, ad, y, yd)"//nl// &
            "    expected = [0.0d0, a(1), a(2), a(3), 1.0d0]"//nl// &
            "    expected_d = [0.0d0, ad(1), ad(2), ad(3), 0.0d0]"//nl// &
            "    bad = .false."//nl// &
            "    do i = 1, 5"//nl// &
            "        if (abs(y(i) - expected(i)) > 1.0d-12) bad = .true."//nl// &
            "        if (abs(yd(i) - expected_d(i)) > 1.0d-12) bad = .true."//nl// &
            "    end do"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call k(a + h*ad, yp)"//nl// &
            "    call k(a - h*ad, ym)"//nl// &
            "    do i = 1, 5"//nl// &
            "        if (abs(yd(i) - (yp(i) - ym(i))/(2.0d0*h)) > 1.0d-7) bad = .true."//nl// &
            "    end do"//nl// &
            "    seed = [0.0d0, 0.31d0, -0.77d0, 1.12d0, 0.0d0]"//nl// &
            "    call k_vjp(a, y, seed, ab)"//nl// &
            "    do i = 1, 3"//nl// &
            "        if (abs(ab(i) - seed(i + 1)) > 1.0d-12) bad = .true."//nl// &
            "    end do"//nl// &
            "    lhs = sum(seed*expected_d)"//nl// &
            "    rhs = sum(ab*ad)"//nl// &
            "    if (abs(lhs - rhs) > 1.0d-12*max(1.0d0, abs(lhs))) bad = .true."//nl// &
            "    if (bad) then"//nl// &
            "        print *, 'y, expected =', y, expected"//nl// &
            "        print *, 'yd, expected_d =', yd, expected_d"//nl// &
            "        print *, 'ab, lhs, rhs =', ab, lhs, rhs"//nl// &
            "        error stop 1"//nl// &
            "    end if"//nl// &
            "end program driver"//nl
    end function driver_text

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

end program test_contiguous_section_oracle
