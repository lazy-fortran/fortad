program test_small_power_expansion_oracle
    !! Independent oracle for small-power expansion in the emitted derivative.
    !!
    !! Emission policy: `x*x`, never `pow(x,2)`.  Differentiation produces
    !! small integer powers constantly - every `b**2` denominator and every
    !! `a**(b-1)` in a power rule - so leaving them as `**` taxes almost every
    !! derivative with a library power where one multiply would do.  The check
    !! asserts that a kernel whose derivative is full of squares and cubes
    !! emits no `**` at all, then compiles and runs the generated JVP against
    !! a hand-derived directional derivative to prove the expansion keeps the
    !! value exact, not merely textually changed.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir, driver
    type(fad_result_t) :: result
    integer :: stat, unit

    source = &
        "function f(x) result(z)"//nl// &
        "    real(8), intent(in) :: x"//nl// &
        "    real(8) :: z"//nl// &
        "    z = x**3 + x**2 + 1.0d0/(x*x + 2.0d0)"//nl// &
        "end function f"//nl

    result = fad_jvp(source, ["x"])
    if (.not. result%ok) error stop "small-power JVP generation failed"

    ! The tangent of x**3 + x**2 + 1/(x**2+2) is
    !   3*x**2*dx + 2*x*dx - 2*x/(x**2+2)**2 * dx.
    ! Every power here is small enough to expand, so the emitted derivative -
    ! primal recomputation and tangent alike - must contain no `**` at all.
    if (index(result%code, "**") /= 0) then
        print *, "FAIL: emitted derivative still contains `**`:"
        print *, result%code
        error stop 1
    end if

    dir = "build/oracle_small_power_expansion"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create small-power oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') source
    close (unit)

    open (newunit=unit, file=dir//"/tangent.f90", status="replace", action="write")
    write (unit, '(a)') "module fad_generated"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') result%code
    write (unit, '(a)') "end module fad_generated"
    close (unit)

    driver = driver_text()
    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line("gfortran -O2 -o "//dir//"/run "// &
        dir//"/primal.f90 "//dir//"/tangent.f90 "//dir// &
        "/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) error stop "small-power generated code did not compile"
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) error stop "small-power independent oracle failed"
    print *, "test_small_power_expansion_oracle: passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_generated, only: f_jvp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, xd, z, zd, want, want_d"//nl// &
            "    x = 1.7d0"//nl// &
            "    xd = 0.41d0"//nl// &
            "    call f_jvp(x, xd, z, zd)"//nl// &
            "    want = x**3 + x**2 + 1.0d0/(x*x + 2.0d0)"//nl// &
            "    want_d = (3.0d0*x*x + 2.0d0*x - 2.0d0*x/&"//nl// &
            "        (x*x + 2.0d0)**2)*xd"//nl// &
            "    if (abs(z - want) > 1.0d-13*max(1.0d0, abs(want))) error stop 1"//nl// &
            "    if (abs(zd - want_d) > 1.0d-13*max(1.0d0, abs(want_d))) error stop 2"//nl// &
            "end program driver"//nl
    end function driver_text

end program test_small_power_expansion_oracle
