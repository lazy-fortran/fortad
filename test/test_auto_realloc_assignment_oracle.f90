program test_auto_realloc_assignment_oracle
    !! Independent behavioral oracle for one whole-allocatable assignment.
    !! The reference compiler owns the allocation transition; hand formulas,
    !! central differences, and a JVP/VJP adjoint identity check the result.
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
        "    function repeated_owner(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), allocatable :: slot"//nl// &
        "        real(8) :: out"//nl// &
        "        slot = x"//nl// &
        "        slot = slot + x"//nl// &
        "        out = slot"//nl// &
        "    end function repeated_owner"//nl// &
        "    function rank2_owner(x) result(out)"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8), allocatable :: slot(:,:)"//nl// &
        "        real(8) :: out"//nl// &
        "        slot = x"//nl// &
        "        out = sum(slot)"//nl// &
        "    end function rank2_owner"//nl// &
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
    character(len=*), parameter :: rank2_source = &
        "function rank2_owner(x) result(out)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8), allocatable :: slot(:,:)"//nl// &
        "    real(8) :: out"//nl// &
        "    slot = x"//nl// &
        "    out = sum(slot)"//nl// &
        "end function rank2_owner"//nl

    type(fad_result_t) :: scalar_jvp, scalar_vjp, rank1_jvp, repeated_vjp, rank2
    character(len=:), allocatable :: dir, derivatives, driver
    integer :: unit, stat

    scalar_jvp = fad_jvp(source, [character(len=1) :: "x"], from="scalar_owner", &
        name="scalar_owner_jvp")
    scalar_vjp = fad_vjp(source, [character(len=1) :: "x"], dependent="out", &
        from="scalar_owner", name="scalar_owner_vjp")
    rank1_jvp = fad_jvp(source, [character(len=6) :: "x", "values"], &
        from="rank1_owner", name="rank1_owner_jvp")
    call require_ok(scalar_jvp, "scalar JVP")
    call require_ok(scalar_vjp, "scalar VJP")
    call require_ok(rank1_jvp, "rank-one JVP")

    repeated_vjp = fad_vjp(repeated_source, [character(len=1) :: "x"], &
        dependent="out", from="repeated_owner")
    call require_refusal(repeated_vjp, "repeated automatic reallocation")
    rank2 = fad_jvp(rank2_source, [character(len=1) :: "x"], from="rank2_owner")
    call require_refusal(rank2, "only concrete scalar and rank-one")

    dir = "build/oracle/auto_realloc_assignment"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create automatic reallocation oracle directory"
    call write_file(dir//"/primal.f90", source)
    derivatives = "module auto_realloc_derivatives"//nl// &
        "contains"//nl//scalar_jvp%code//nl//scalar_vjp%code//nl// &
        rank1_jvp%code//nl//"end module auto_realloc_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = &
        "program driver"//nl// &
        "    use auto_realloc_case, only: scalar_owner, rank1_owner"//nl// &
        "    use auto_realloc_derivatives, only: scalar_owner_jvp, "// &
        "scalar_owner_vjp, rank1_owner_jvp"//nl// &
        "    implicit none"//nl// &
        "    real(8) :: x, xd, out, outd, fp, fm, fd, h, outb, xb"//nl// &
        "    real(8) :: values(3), valuesd(3), out1, out1d"//nl// &
        "    real(8) :: valuesp(3), valuesm(3), fdp, fdm"//nl// &
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
