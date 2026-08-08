program test_active_allocatable_component_rank4_oracle
    !! Independent behavioral gate for scalar accesses to a concrete REAL
    !! rank-four allocatable component.
    !!
    !! The generated JVP is checked against a hand derivative and central
    !! finite differences.  The generated VJP is checked against the reverse
    !! adjoint identity.  A rank-five case remains a named refusal.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    type(fad_result_t) :: jvp, vjp, refused
    character(len=:), allocatable :: dir, derivatives, driver
    integer :: unit, stat

    jvp = fad_jvp(positive_source(), &
        [character(len=40) :: "box%values(1,2,1,2)", &
        "box%values(2,1,2,1)", "box%values(2,2,1,1)", "scale"], &
        from="kernel", name="kernel_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL rank-four allocatable component JVP generation: ", &
            jvp%message
        error stop 1
    end if
    vjp = fad_vjp(positive_source(), &
        [character(len=40) :: "box%values(1,2,1,2)", &
        "box%values(2,1,2,1)", "box%values(2,2,1,1)", "scale"], &
        dependent="out", from="kernel", name="kernel_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL rank-four allocatable component VJP generation: ", &
            vjp%message
        error stop 2
    end if

    dir = "build/oracle/active_allocatable_component_rank4"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create rank-four oracle directory"
    call write_file(dir//"/primal.f90", positive_source())
    derivatives = "module active_allocatable_component_rank4_derivatives"//nl// &
        "    use active_allocatable_component_rank4_case, only: box_t"//nl// &
        "contains"//nl//jvp%code//nl//vjp%code// &
        "end module active_allocatable_component_rank4_derivatives"//nl
    call write_file(dir//"/derivatives.f90", derivatives)
    driver = driver_text()
    call write_file(dir//"/driver.f90", driver)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL rank-four generated source did not compile"
        call show_file(dir//"/build.log")
        error stop 3
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL rank-four independent behavioral oracle"
        call show_file(dir//"/out.txt")
        error stop 4
    end if

    refused = fad_jvp(rank5_source(), &
        [character(len=40) :: "box%values(1,1,1,1,1)"], from="kernel")
    call require_refusal(refused, "rank greater than four")
    refused = fad_vjp(rank5_source(), &
        [character(len=40) :: "box%values(1,1,1,1,1)"], dependent="out", &
        from="kernel")
    call require_refusal(refused, "rank greater than four")

    print *, "test_active_allocatable_component_rank4_oracle: all cases passed"

contains

    function positive_source() result(source)
        character(len=:), allocatable :: source

        source = "module active_allocatable_component_rank4_case"//nl// &
            "    implicit none"//nl// &
            "    type :: box_t"//nl// &
            "        real(8), allocatable :: values(:,:,:,:)"//nl// &
            "        real(8) :: bias"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure subroutine kernel(box, scale, out)"//nl// &
            "        type(box_t), intent(inout) :: box"//nl// &
            "        real(8), intent(in) :: scale"//nl// &
            "        real(8), intent(out) :: out"//nl// &
            "        box%values(1,2,1,2) = "// &
            "            box%values(1,2,1,2)*scale + box%bias"//nl// &
            "        out = box%values(1,2,1,2)*box%values(2,1,2,1) + "// &
            "            scale*box%values(2,2,1,1)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module active_allocatable_component_rank4_case"//nl
    end function positive_source

    function rank5_source() result(source)
        character(len=:), allocatable :: source

        source = "module rank5_allocatable_component_case"//nl// &
            "    implicit none"//nl// &
            "    type :: box_t"//nl// &
            "        real(8), allocatable :: values(:,:,:,:,:)"//nl// &
            "    end type box_t"//nl// &
            "contains"//nl// &
            "    pure subroutine kernel(box, out)"//nl// &
            "        type(box_t), intent(in) :: box"//nl// &
            "        real(8), intent(out) :: out"//nl// &
            "        out = box%values(1,1,1,1,1)"//nl// &
            "    end subroutine kernel"//nl// &
            "end module rank5_allocatable_component_case"//nl
    end function rank5_source

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = "program driver"//nl// &
            "    use active_allocatable_component_rank4_case, only: box_t, kernel"//nl// &
            "    use active_allocatable_component_rank4_derivatives, only: "// &
            "kernel_jvp, kernel_vjp"//nl// &
            "    implicit none"//nl// &
            "    type(box_t) :: box, initial, box_d, box_b, plus, minus"//nl// &
            "    real(8) :: scale, scale_d, out, out_d, out_b, scale_b"//nl// &
            "    real(8) :: h, fp, fm, fd, lhs, rhs"//nl// &
            "    allocate(box%values(2,2,2,2), initial%values(2,2,2,2), "// &
            "box_d%values(2,2,2,2), box_b%values(2,2,2,2))"//nl// &
            "    box%values = 0.0d0"//nl// &
            "    box%values(1,2,1,2) = 2.0d0"//nl// &
            "    box%values(2,1,2,1) = 3.0d0"//nl// &
            "    box%values(2,2,1,1) = 4.0d0"//nl// &
            "    box%bias = 0.5d0"//nl// &
            "    initial = box"//nl// &
            "    box_d%values = 0.0d0"//nl// &
            "    box_d%values(1,2,1,2) = 0.4d0"//nl// &
            "    box_d%values(2,1,2,1) = 0.2d0"//nl// &
            "    box_d%values(2,2,1,1) = 0.1d0"//nl// &
            "    scale = 1.5d0"//nl// &
            "    scale_d = -0.3d0"//nl// &
            "    call kernel_jvp(box, box_d, scale, scale_d, out, out_d)"//nl// &
            "    if (abs(out - 16.5d0) > 1.0d-13) error stop 10"//nl// &
            "    if (abs(out_d + 0.35d0) > 1.0d-13) error stop 11"//nl// &
            "    box_d%values = 0.0d0"//nl// &
            "    box_d%values(1,2,1,2) = 0.4d0"//nl// &
            "    box_d%values(2,1,2,1) = 0.2d0"//nl// &
            "    box_d%values(2,2,1,1) = 0.1d0"//nl// &
            "    h = 1.0d-6"//nl// &
            "    plus = initial"//nl// &
            "    plus%values = initial%values + h*box_d%values"//nl// &
            "    minus = initial"//nl// &
            "    minus%values = initial%values - h*box_d%values"//nl// &
            "    call kernel(plus, scale + h*scale_d, fp)"//nl// &
            "    call kernel(minus, scale - h*scale_d, fm)"//nl// &
            "    fd = (fp-fm)/(2.0d0*h)"//nl// &
            "    if (abs(out_d-fd) > 1.0d-6) then"//nl// &
            "        print *, 'JVP central mismatch', out_d, fd"//nl// &
            "        error stop 12"//nl// &
            "    end if"//nl// &
            "    out_b = 1.7d0"//nl// &
            "    call kernel_vjp(initial, scale, out, out_b, box_b, scale_b)"//nl// &
            "    if (abs(box_b%values(1,2,1,2)-7.65d0) > 1.0d-13) error stop 13"//nl// &
            "    if (abs(box_b%values(2,1,2,1)-5.95d0) > 1.0d-13) error stop 14"//nl// &
            "    if (abs(box_b%values(2,2,1,1)-2.55d0) > 1.0d-13) error stop 15"//nl// &
            "    if (abs(scale_b-17.0d0) > 1.0d-13) error stop 16"//nl// &
            "    lhs = out_b*out_d"//nl// &
            "    rhs = box_b%values(1,2,1,2)*box_d%values(1,2,1,2) + "// &
            "box_b%values(2,1,2,1)*box_d%values(2,1,2,1) + &"//nl// &
            "        box_b%values(2,2,1,1)*box_d%values(2,2,1,1) + "// &
            "scale_b*scale_d"//nl// &
            "    if (abs(lhs-rhs) > 1.0d-13) error stop 17"//nl// &
            "    print *, 'active rank-four allocatable component oracle pass'"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine require_refusal(result, needle)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: needle

        if (result%ok .or. .not. allocated(result%message) .or. &
            index(result%message, needle) == 0) then
            print *, "FAIL expected refusal containing: ", trim(needle)
            if (allocated(result%message)) print *, trim(result%message)
            error stop 20
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
        integer :: ios, file_unit

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

end program test_active_allocatable_component_rank4_oracle
