program test_auto_realloc_assignment_oracle
    !! Independent behavioral oracle for one whole-allocatable assignment.
    !! The reference compiler owns the allocation transition; hand formulas,
    !! central differences, and JVP/VJP adjoint identities check the result.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module auto_realloc_case"//nl// &
        "contains"//nl// &
        "    function scalar_owner(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), allocatable :: slot"//nl// &
        "        real(8) :: out"//nl// &
        "        slot = x*x"//nl// &
        "        out = slot + 2.0d0"//nl// &
        "    end function scalar_owner"//nl// &
        "    function rank1_owner(x, values) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(in) :: values(:)"//nl// &
        "        real(8), allocatable :: slot(:)"//nl// &
        "        real(8) :: out"//nl// &
        "        slot = values"//nl// &
        "        out = x*sum(slot*slot)"//nl// &
        "    end function rank1_owner"//nl// &
        "    function rank2_owner(x, values) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(in) :: values(:,:)"//nl// &
        "        real(8), allocatable :: slot(:,:)"//nl// &
        "        real(8) :: out"//nl// &
        "        slot = values"//nl// &
        "        out = x*sum(slot*slot)"//nl// &
        "    end function rank2_owner"//nl// &
        "    function repeated_owner(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), allocatable :: slot"//nl// &
        "        real(8) :: out"//nl// &
        "        slot = x"//nl// &
        "        slot = slot + x"//nl// &
        "        out = slot"//nl// &
        "    end function repeated_owner"//nl// &
        "    function rank3_owner(x, values) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), intent(in) :: values(:,:,:)"//nl// &
        "        real(8), allocatable :: slot(:,:,:)"//nl// &
        "        real(8) :: out"//nl// &
        "        slot = values"//nl// &
        "        out = x*sum(slot*slot)"//nl// &
        "    end function rank3_owner"//nl// &
        "end module auto_realloc_case"//nl
    character(len=*), parameter :: repeated_source = &
        "function repeated_owner(x) result(out)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), allocatable :: slot"//nl// &
        "    real(8) :: out"//nl// &
        "    slot = x"//nl// &
        "    slot = slot + x"//nl// &
        "    out = slot"//nl// &
        "end function repeated_owner"//nl
    character(len=*), parameter :: rank3_source = &
        "function rank3_owner(x, values) result(out)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), intent(in) :: values(:,:,:)"//nl// &
        "    real(8), allocatable :: slot(:,:,:)"//nl// &
        "    real(8) :: out"//nl// &
        "    slot = values"//nl// &
        "    out = x*sum(slot*slot)"//nl// &
        "end function rank3_owner"//nl

    type(fad_result_t) :: scalar_jvp, scalar_vjp, rank1_jvp, rank2_jvp, rank2_vjp
    type(fad_result_t) :: repeated_vjp, rank3
    character(len=:), allocatable :: dir, derivatives, driver
    integer :: unit, stat

    scalar_jvp = fad_jvp(source, [character(len=1) :: "x"], from="scalar_owner", &
        name="scalar_owner_jvp")
    scalar_vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="out", &
        from="scalar_owner", name="scalar_owner_vjp")
    rank1_jvp = fad_jvp(source, [character(len=6) :: "x", "values"], &
        from="rank1_owner", name="rank1_owner_jvp")
    rank2_jvp = fad_jvp(source, [character(len=6) :: "x", "values"], &
        from="rank2_owner", name="rank2_owner_jvp")
    rank2_vjp = fad_vjp(source, [character(len=6) :: "x", "values"], &
        dependent="out", from="rank2_owner", name="rank2_owner_vjp")
    call require_ok(scalar_jvp, "scalar JVP")
    call require_ok(scalar_vjp, "scalar VJP")
    call require_ok(rank1_jvp, "rank-one JVP")
    call require_ok(rank2_jvp, "rank-two JVP")
    call require_ok(rank2_vjp, "rank-two VJP")

    repeated_vjp = fad_vjp(repeated_source, [character(len=1) :: "x"], &
        dependent="out", from="repeated_owner")
    call require_refusal(repeated_vjp, "repeated automatic reallocation")
    rank3 = fad_jvp(rank3_source, [character(len=6) :: "x", "values"], &
        from="rank3_owner")
    call require_refusal(rank3, "only concrete scalar through rank-two")

    dir = "build/oracle/auto_realloc_assignment"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create automatic reallocation oracle directory"
    call write_file(dir//"/primal.f90", source)
    derivatives = "module auto_realloc_derivatives"//nl// &
        "contains"//nl//scalar_jvp%code//nl//scalar_vjp%code//nl// &
        rank1_jvp%code//nl//rank2_jvp%code//nl//rank2_vjp%code//nl// &
        "end module auto_realloc_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = &
        "program driver"//nl// &
        "    use auto_realloc_case, only: scalar_owner, rank1_owner, "// &
        "rank2_owner"//nl// &
        "    use auto_realloc_derivatives, only: scalar_owner_jvp, "// &
        "scalar_owner_vjp, rank1_owner_jvp, rank2_owner_jvp, "// &
        "rank2_owner_vjp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, out, outd, fp, fm, fd, h, outb, xb"//nl// &
        "    real(8) :: values(3), valuesd(3), out1, out1d"//nl// &
        "    real(8) :: valuesp(3), valuesm(3), fdp, fdm"//nl// &
        "    real(8), allocatable :: matrix(:,:), matrixd(:,:), "// &
        "matrixp(:,:), matrixm(:,:), matrixb(:,:)"//nl// &
        "    integer :: nrow, ncol"//nl// &
        "    real(8) :: out2, out2d"//nl// &
        "    x = 1.25d0"//nl// &
        "    xd = -0.7d0"//nl// &
        "    call scalar_owner_jvp(x, xd, out, outd)"//nl// &
        "    if (abs(out - (x*x + 2.0d0)) > 1.0d-12) error stop 1"//nl// &
        "    if (abs(outd - 2.0d0*x*xd) > 1.0d-12) error stop 2"//nl// &
        "    h = 1.0d-6"//nl// &
        "    fp = scalar_owner(x + h*xd)"//nl// &
        "    fm = scalar_owner(x - h*xd)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(outd - fd) > 1.0d-7) error stop 3"//nl// &
        "    outb = 1.3d0"//nl// &
        "    call scalar_owner_vjp(x, out, outb, xb)"//nl// &
        "    if (abs(xb - outb*2.0d0*x) > 1.0d-12) error stop 4"//nl// &
        "    if (abs(xb*xd - outb*outd) > 1.0d-12) error stop 5"//nl// &
        "    values = [0.4d0, -0.8d0, 1.1d0]"//nl// &
        "    valuesd = [0.3d0, -0.2d0, 0.5d0]"//nl// &
        "    call rank1_owner_jvp(x, xd, values, valuesd, out1, out1d)"//nl// &
        "    if (abs(out1 - x*sum(values*values)) > 1.0d-12) error stop 6"//nl// &
        "    if (abs(out1d - (xd*sum(values*values) + "// &
        "2.0d0*x*sum(values*valuesd))) > 1.0d-12) error stop 7"//nl// &
        "    valuesp = values + h*valuesd"//nl// &
        "    valuesm = values - h*valuesd"//nl// &
        "    fdp = rank1_owner(x + h*xd, valuesp)"//nl// &
        "    fdm = rank1_owner(x - h*xd, valuesm)"//nl// &
        "    fd = (fdp - fdm)/(2.0d0*h)"//nl// &
        "    if (abs(out1d - fd) > 1.0d-7) error stop 8"//nl// &
        "    nrow = 2"//nl// &
        "    ncol = 3"//nl// &
        "    allocate(matrix(nrow, ncol), matrixd(nrow, ncol), "// &
        "matrixp(nrow, ncol), matrixm(nrow, ncol), matrixb(nrow, ncol))"//nl// &
        "    matrix = reshape([0.4d0, -0.8d0, 1.1d0, 0.2d0, "// &
        "-0.5d0, 0.7d0], [nrow, ncol])"//nl// &
        "    matrixd = reshape([0.3d0, -0.2d0, 0.5d0, -0.1d0, "// &
        "0.6d0, -0.4d0], [nrow, ncol])"//nl// &
        "    call rank2_owner_jvp(x, xd, matrix, matrixd, out2, out2d)"//nl// &
        "    if (abs(out2 - x*sum(matrix*matrix)) > 1.0d-12) error stop 9"//nl// &
        "    if (abs(out2d - (xd*sum(matrix*matrix) + "// &
        "2.0d0*x*sum(matrix*matrixd))) > 1.0d-12) error stop 10"//nl// &
        "    matrixp = matrix + h*matrixd"//nl// &
        "    matrixm = matrix - h*matrixd"//nl// &
        "    fdp = rank2_owner(x + h*xd, matrixp)"//nl// &
        "    fdm = rank2_owner(x - h*xd, matrixm)"//nl// &
        "    fd = (fdp - fdm)/(2.0d0*h)"//nl// &
        "    if (abs(out2d - fd) > 1.0d-7) error stop 11"//nl// &
        "    outb = -0.9d0"//nl// &
        "    call rank2_owner_vjp(x, matrix, out2, outb, xb, matrixb)"//nl// &
        "    if (abs(xb - outb*sum(matrix*matrix)) > 1.0d-12) error stop 12"//nl// &
        "    if (maxval(abs(matrixb - outb*2.0d0*x*matrix)) > 1.0d-12) "// &
        "error stop 13"//nl// &
        "    if (abs(xb*xd + sum(matrixb*matrixd) - outb*out2d) > "// &
        "1.0d-12) error stop 14"//nl// &
        "    print *, 'automatic reallocation assignment oracle pass'"//nl// &
        "end program driver"//nl
    call write_file(dir//"/driver.f90", driver)
    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/driver_build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/driver_build.log")
        error stop "automatic reallocation oracle driver did not compile"
    end if
    call execute_command_line("./"//dir//"/run", exitstat=stat)
    if (stat /= 0) error stop "automatic reallocation assignment oracle failed"
    print *, "test_auto_realloc_assignment_oracle: all cases passed"

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label

        if (.not. result%ok) then
            print *, "FAIL ", trim(label), ": ", result%message
            error stop 10
        end if
    end subroutine require_ok

    subroutine require_refusal(result, expected)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: expected

        if (result%ok) then
            print *, "FAIL refusal: transformation unexpectedly succeeded; expected '"// &
                trim(expected)//"'"
            error stop 11
        end if
        if (.not. allocated(result%message)) then
            print *, "FAIL refusal: diagnostic is missing; expected '"// &
                trim(expected)//"'"
            error stop 11
        end if
        if (index(result%message, expected) == 0) then
            print *, "FAIL refusal: expected '", trim(expected), "' got '", &
                result%message, "'"
            error stop 11
        end if
    end subroutine require_refusal

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: file_unit

        open (newunit=file_unit, file=path, status="replace", action="write")
        write (file_unit, '(a)') text
        close (file_unit)
    end subroutine write_file

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: file_unit, ios

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

end program test_auto_realloc_assignment_oracle
