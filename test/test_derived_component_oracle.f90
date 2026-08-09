program test_derived_component_oracle
    !! Independent behavioral gate for real components of a concrete value.
    !! The case covers a scalar inherited component, a nested component, and
    !! an array component.  Integer storage is carried through the primal but
    !! never receives a tangent or adjoint.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module derived_component_case"//nl// &
        "    implicit none"//nl// &
        "    type :: inner_t"//nl// &
        "        real(8) :: q"//nl// &
        "    end type inner_t"//nl// &
        "    type :: base_t"//nl// &
        "        real(8) :: base"//nl// &
        "    end type base_t"//nl// &
        "    type, extends(base_t) :: state_t"//nl// &
        "        type(inner_t) :: inner"//nl// &
        "        real(8) :: values(2)"//nl// &
        "        integer :: tag"//nl// &
        "    end type state_t"//nl// &
        "contains"//nl// &
        "    pure real(8) function top(state, a) result(out)"//nl// &
        "        type(state_t), intent(in) :: state"//nl// &
        "        real(8), intent(in) :: a"//nl// &
        "        out = state%base + state%inner%q*a + state%values(1)*a + "// &
        "            state%base*state%inner%q"//nl// &
        "    end function top"//nl// &
        "end module derived_component_case"//nl

    type(fad_result_t) :: jvp, vjp
    character(len=32) :: independent_paths(4)
    character(len=32) :: bad_independent(1)
    character(len=:), allocatable :: dir, driver
    integer :: unit, stat

    independent_paths = [character(len=32) :: "state%base", "state%inner%q", &
        "state%values(1)", "a"]
    bad_independent = [character(len=32) :: "state"]
    jvp = fad_jvp(source, bad_independent, from="top")
    if (jvp%ok .or. .not. allocated(jvp%message) .or. &
        index(jvp%message, "must name a real component") == 0) then
        print *, "FAIL derived-component: whole-object activity was accepted"
        error stop 1
    end if
    vjp = fad_vjp(source, bad_independent, dependent="out", from="top")
    if (vjp%ok .or. .not. allocated(vjp%message) .or. &
        index(vjp%message, "must name a real component") == 0) then
        print *, "FAIL derived-component: reverse whole-object activity was accepted"
        error stop 1
    end if
    jvp = fad_jvp(source, [character(len=32) :: "state%base"], from="top", &
        n_directions="n_dir")
    if (jvp%ok .or. .not. allocated(jvp%message) .or. &
        index(jvp%message, "vector mode for derived components") == 0) then
        print *, "FAIL derived-component: vector mode was accepted"
        error stop 1
    end if
    jvp = fad_jvp(source, independent_paths, from="top", name="top_jvp")
    if (.not. jvp%ok) then
        print *, "FAIL derived-component JVP generation: ", jvp%message
        error stop 1
    end if
    vjp = fad_vjp(source, independent_paths, dependent="out", from="top", &
        name="top_vjp")
    if (.not. vjp%ok) then
        print *, "FAIL derived-component VJP generation: ", vjp%message
        error stop 1
    end if

    dir = "build/oracle/derived_components"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create derived-component oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//"/derivatives.f90", status="replace", action="write")
    write (unit, '(a)') "module derived_component_derivatives"
    write (unit, '(a)') "    use derived_component_case, only: state_t"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module derived_component_derivatives"
    close (unit)

    driver = &
        "program driver"//nl// &
        "    use derived_component_case, only: state_t, top"//nl// &
        "    use derived_component_derivatives, only: top_jvp, top_vjp"//nl// &
        "    implicit none"//nl// &
        "    type(state_t) :: state, state_d, plus, minus, state_b"//nl// &
        "    real(8) :: a, a_d, out, out_d, out_b, a_b, h, fp, fm, fd"//nl// &
        "    state%base = 2.0d0"//nl// &
        "    state%inner%q = 3.0d0"//nl// &
        "    state%values = [4.0d0, 8.0d0]"//nl// &
        "    state%tag = 7"//nl// &
        "    state_d%base = -0.2d0"//nl// &
        "    state_d%inner%q = 0.3d0"//nl// &
        "    state_d%values = [-0.4d0, 0.0d0]"//nl// &
        "    state_d%tag = 0"//nl// &
        "    a = 1.5d0"//nl// &
        "    a_d = 0.7d0"//nl// &
        "    out_b = 1.3d0"//nl// &
        "    call top_jvp(state, state_d, a, a_d, out, out_d)"//nl// &
        "    if (abs(out - 18.5d0) > 1.0d-13) error stop 2"//nl// &
        "    if (abs(out_d - 4.55d0) > 1.0d-13) error stop 3"//nl// &
        "    h = 1.0d-6"//nl// &
        "    plus = state"//nl// &
        "    plus%base = state%base + h*state_d%base"//nl// &
        "    plus%inner%q = state%inner%q + h*state_d%inner%q"//nl// &
        "    plus%values(1) = state%values(1) + h*state_d%values(1)"//nl// &
        "    minus = state"//nl// &
        "    minus%base = state%base - h*state_d%base"//nl// &
        "    minus%inner%q = state%inner%q - h*state_d%inner%q"//nl// &
        "    minus%values(1) = state%values(1) - h*state_d%values(1)"//nl// &
        "    fp = top(plus, a + h*a_d)"//nl// &
        "    fm = top(minus, a - h*a_d)"//nl// &
        "    fd = (fp - fm)/(2.0d0*h)"//nl// &
        "    if (abs(out_d - fd) > 1.0d-6) error stop 4"//nl// &
        "    call top_vjp(state, a, out, out_b, state_b, a_b)"//nl// &
        "    if (abs(state_b%base - 5.2d0) > 1.0d-13) error stop 5"//nl// &
        "    if (abs(state_b%inner%q - 4.55d0) > 1.0d-13) error stop 6"//nl// &
        "    if (abs(state_b%values(1) - 1.95d0) > 1.0d-13) error stop 7"//nl// &
        "    if (abs(a_b - 9.1d0) > 1.0d-13) error stop 8"//nl// &
        "    if (abs(out_b*out_d - (state_b%base*state_d%base + &"//nl// &
        "            state_b%inner%q*state_d%inner%q + &"//nl// &
        "            state_b%values(1)*state_d%values(1) + a_b*a_d)) > &"//nl// &
        "            1.0d-13) error stop 9"//nl// &
        "    print *, 'derived component oracle pass'"//nl// &
        "end program driver"//nl
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/derivatives.f90 "//dir//"/driver.f90 > "// &
        dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL derived-component: generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL derived-component: independent oracle failed"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_derived_component_oracle: all cases passed"

contains

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

end program test_derived_component_oracle
