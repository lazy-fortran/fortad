program test_contiguous_section_rank2_oracle
    !! Independent oracle for bounded contiguous rank-two array sections.
    !!
    !! The positive case compares generated JVP/VJP products with hand
    !! derivatives, central finite differences, and the adjoint identity.
    !! Generated products are compiled and executed by gfortran.  Boundary
    !! cases keep strides, noncontiguous assumed-shape storage, and rank-three
    !! sections as named refusals.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, tangent, derivatives, driver, dir
    type(fad_result_t) :: jvp, vjp, refused
    integer :: unit, stat

    source = positive_source()
    jvp = fad_jvp(source, [character(len=1) :: "a"], name="k_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL rank-two contiguous section JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, [character(len=1) :: "a"], dependent="y", &
        name="k_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL rank-two contiguous section VJP generation: ", vjp%message
        error stop 2
    end if

    dir = "build/oracle/contiguous_section_rank2"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop 3

    open (newunit=unit, file=dir//"/primal.f90", status="replace", &
        action="write")
    write (unit, '(a)') "module section_rank2_primal"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module section_rank2_primal"
    close (unit)

    tangent = "module section_rank2_tangent"//nl// &
        "    implicit none"//nl// &
        "contains"//nl//jvp%code// &
        "end module section_rank2_tangent"//nl
    open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
        action="write")
    write (unit, '(a)') tangent
    close (unit)

    derivatives = "module section_rank2_derivatives"//nl// &
        "    implicit none"//nl// &
        "contains"//nl//vjp%code// &
        "end module section_rank2_derivatives"//nl
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
        print *, "FAIL rank-two generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 4
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL rank-two contiguous section independent oracle"
        call show_file(dir//"/out.txt")
        error stop 5
    end if

    refused = fad_jvp(strided_source(), [character(len=1) :: "a"])
    call require_refusal(refused, "noncontiguous")

    refused = fad_jvp(assumed_shape_source(), [character(len=1) :: "a"])
    call require_refusal(refused, "not declared contiguous or owning")

    refused = fad_jvp(vector_source(), [character(len=1) :: "a"])
    call require_refusal(refused, "unsupported vector subscript")

    refused = fad_jvp(rank_three_source(), [character(len=1) :: "a"])
    call require_refusal(refused, "rank greater than two")

    print *, "test_contiguous_section_rank2_oracle: all cases passed"

contains

    function positive_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(a, y)"//nl// &
            "    real(8), intent(in) :: a(2,3)"//nl// &
            "    real(8), intent(out) :: y(4,5)"//nl// &
            "    y = 0.0d0"//nl// &
            "    y(2:3,2:3) = 2.5d0*a(1:2,2:3)"//nl// &
            "end subroutine k"//nl
    end function positive_source

    function strided_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(a, y)"//nl// &
            "    real(8), intent(in) :: a(4,2)"//nl// &
            "    real(8), intent(out) :: y(2,2)"//nl// &
            "    y = a(1:3:2,:)"//nl// &
            "end subroutine k"//nl
    end function strided_source

    function rank_three_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(a, y)"//nl// &
            "    real(8), intent(in) :: a(2,2,2)"//nl// &
            "    real(8), intent(out) :: y(2,2,2)"//nl// &
            "    y(1:2,1:2,1:2) = a(1:2,1:2,1:2)"//nl// &
            "end subroutine k"//nl
    end function rank_three_source

    function assumed_shape_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(a, y)"//nl// &
            "    real(8), intent(in) :: a(:,:)"//nl// &
            "    real(8), intent(out) :: y(2,2)"//nl// &
            "    y = a(1:2,1:2)"//nl// &
            "end subroutine k"//nl
    end function assumed_shape_source

    function vector_source() result(text)
        character(len=:), allocatable :: text

        text = "subroutine k(a, idx, y)"//nl// &
            "    real(8), intent(in) :: a(4,2)"//nl// &
            "    integer, intent(in) :: idx(2)"//nl// &
            "    real(8), intent(out) :: y(2,2)"//nl// &
            "    y = a(idx,:)"//nl// &
            "end subroutine k"//nl
    end function vector_source

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use section_rank2_primal, only: k"//nl// &
            "    use section_rank2_tangent, only: k_jvp"//nl// &
            "    use section_rank2_derivatives, only: k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: a(2,3), ad(2,3), y(4,5), yd(4,5)"//nl// &
            "    real(8) :: yp(4,5), ym(4,5), expected(4,5)"//nl// &
            "    real(8) :: expected_d(4,5), seed(4,5), ab(2,3)"//nl// &
            "    real(8) :: h, lhs, rhs"//nl// &
            "    integer :: i, j"//nl// &
            "    logical :: bad"//nl// &
            "    a = reshape([0.7d0, -0.4d0, 1.1d0, 0.3d0, -0.9d0, 1.4d0], &"//nl// &
            "        [2,3])"//nl// &
            "    ad = reshape([-0.2d0, 0.6d0, -0.3d0, 0.8d0, 0.5d0, -0.7d0], &"//nl// &
            "        [2,3])"//nl// &
            "    call k_jvp(a, ad, y, yd)"//nl// &
            "    expected = 0.0d0"//nl// &
            "    expected_d = 0.0d0"//nl// &
            "    expected(2:3,2:3) = 2.5d0*a(1:2,2:3)"//nl// &
            "    expected_d(2:3,2:3) = 2.5d0*ad(1:2,2:3)"//nl// &
            "    bad = .false."//nl// &
            "    do j = 1, 5"//nl// &
            "        do i = 1, 4"//nl// &
            "            if (abs(y(i,j)-expected(i,j)) > 1.0d-12) bad=.true."//nl// &
            "            if (abs(yd(i,j)-expected_d(i,j)) > 1.0d-12) bad=.true."//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call k(a+h*ad, yp)"//nl// &
            "    call k(a-h*ad, ym)"//nl// &
            "    do j = 1, 5"//nl// &
            "        do i = 1, 4"//nl// &
            "            if (abs(yd(i,j)-(yp(i,j)-ym(i,j))/(2.0d0*h)) > 1.0d-7) &"//nl// &
            "                bad=.true."//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    seed = reshape([0.0d0, 0.0d0, 0.0d0, 0.0d0, &"//nl// &
            "        0.0d0, 0.31d0, -0.77d0, 0.0d0, &"//nl// &
            "        0.0d0, 1.12d0, -0.44d0, 0.0d0, &"//nl// &
            "        0.0d0, 0.0d0, 0.0d0, 0.0d0, &"//nl// &
            "        0.0d0, 0.0d0, 0.0d0, 0.0d0, &"//nl// &
            "        0.0d0, 0.0d0, 0.0d0, 0.0d0], [4,5])"//nl// &
            "    call k_vjp(a, y, seed, ab)"//nl// &
            "    if (maxval(abs(ab(:,1))) > 1.0d-12) bad=.true."//nl// &
            "    if (maxval(abs(ab(:,2)-2.5d0*seed(2:3,2))) > 1.0d-12) bad=.true."//nl// &
            "    if (maxval(abs(ab(:,3)-2.5d0*seed(2:3,3))) > 1.0d-12) bad=.true."//nl// &
            "    lhs = sum(seed*expected_d)"//nl// &
            "    rhs = sum(ab*ad)"//nl// &
            "    if (abs(lhs-rhs) > 1.0d-12*max(1.0d0,abs(lhs))) bad=.true."//nl// &
            "    if (bad) then"//nl// &
            "        print *, 'y, expected =', y, expected"//nl// &
            "        print *, 'yd, expected_d =', yd, expected_d"//nl// &
            "        print *, 'ab, lhs, rhs =', ab, lhs, rhs"//nl// &
            "        error stop 1"//nl// &
            "    end if"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine require_refusal(result, needle)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: needle

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL expected refusal containing '", trim(needle), "': ", &
                result%message
            error stop 6
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

end program test_contiguous_section_rank2_oracle
