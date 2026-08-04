program test_tangent_only_oracle
    !! Behavioural oracle for tangent-only forward mode and imported kinds.
    !!
    !! Two things are checked together because they arrived together and a
    !! consumer needs both: a kernel that gets `dp` from `iso_fortran_env` must
    !! produce a derivative that compiles - which it cannot unless the `use`
    !! comes with it - and a tangent-only contract must return the same tangent
    !! the with-primal one does.
    !!
    !! The oracle is central differences on the untouched kernel. The agreement
    !! between the two generated routines is a separate check: it pins that
    !! dropping the primal removed only dead code.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source
    type(fad_result_t) :: jvp_full, jvp_only, vjp_only
    integer :: failures, stat, unit
    character(len=:), allocatable :: dir

    failures = 0

    ! Deliberately written the way fortnum writes its kernels: `dp` imported
    ! with a rename, declarations in terms of it, one `intent(out)` value.
    source = "subroutine det2(a, b, c, d, value)"//nl// &
             "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
             "    implicit none"//nl// &
             "    real(dp), intent(in) :: a, b, c, d"//nl// &
             "    real(dp), intent(out) :: value"//nl// &
             "    value = a*d - b*c"//nl// &
             "end subroutine det2"//nl

    jvp_full = fad_jvp(source, ["a", "b", "c", "d"], name="det2_jvp_full")
    jvp_only = fad_jvp(source, ["a", "b", "c", "d"], name="det2_jvp", &
                       with_primal=.false.)
    vjp_only = fad_vjp(source, ["a", "b", "c", "d"], name="det2_vjp", &
                       with_primal=.false.)

    call require(jvp_full%ok, "with-primal jvp generation", failures)
    call require(jvp_only%ok, "tangent-only jvp generation", failures)
    call require(vjp_only%ok, "gradient-only vjp generation", failures)
    if (failures > 0) call finish(failures)

    ! The import has to travel with the derivative or `real(dp)` is undefined.
    call require(index(jvp_only%code, "iso_fortran_env") > 0, &
                 "tangent-only jvp carries the import", failures)
    call require(index(jvp_only%code, "dp => real64") > 0, &
                 "the rename is reproduced, not flattened", failures)
    call require(index(vjp_only%code, "iso_fortran_env") > 0, &
                 "gradient-only vjp carries the import", failures)
    ! A tangent-only routine must not take the primal value back.
    call require(index(jvp_only%code, "value_d") > 0, &
                 "tangent-only jvp returns the tangent", failures)
    if (failures > 0) call finish(failures)

    dir = "build/oracle_tangent_only"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    open (newunit=unit, file=dir//"/gen.f90", status="replace", action="write")
    write (unit, '(a)') "module gen"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') jvp_full%code
    write (unit, '(a)') jvp_only%code
    write (unit, '(a)') vjp_only%code
    write (unit, '(a)') "end module gen"
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line("cd "//dir//" && gfortran -O2 -o run gen.f90 "// &
                              "driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL tangent_only: generated code did not compile"
        call show_file(dir//"/build.log")
        failures = failures + 1
        call finish(failures)
    end if
    call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL tangent_only: oracle mismatch"
        call show_file(dir//"/out.txt")
        failures = failures + 1
    else
        print *, "pass tangent_only_against_finite_differences"
    end if

    call finish(failures)

contains

    subroutine require(condition, label, failures)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        integer, intent(inout) :: failures

        if (condition) then
            print *, "pass "//label
        else
            print *, "FAIL "//label
            failures = failures + 1
        end if
    end subroutine require

    subroutine finish(failures)
        integer, intent(in) :: failures

        if (failures == 0) then
            print *, "test_tangent_only_oracle: all cases passed"
        else
            print *, "test_tangent_only_oracle: ", failures, " case(s) FAILED"
            error stop 1
        end if
    end subroutine finish

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    use gen, only: det2, det2_jvp, det2_jvp_full, det2_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(dp) :: x(4), v(4), g(4), h, fp, fm, t, tf, val, fd"//nl// &
            "    integer :: i"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    x = [0.7d0, -1.3d0, 2.1d0, 0.4d0]"//nl// &
            "    v = [0.3d0, 1.1d0, -0.5d0, 0.9d0]"//nl// &
            ! Tangent-only against with-primal: identical, bit for bit.
            "    call det2_jvp(x(1), v(1), x(2), v(2), x(3), v(3), x(4), v(4), t)"//nl// &
            "    call det2_jvp_full(x(1), v(1), x(2), v(2), x(3), v(3), x(4), &"//nl// &
            "                       v(4), val, tf)"//nl// &
            "    if (abs(t - tf) > 0.0d0) then"//nl// &
            "        print *, 'tangent routines disagree', t, tf"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            ! Directional derivative against central differences.
            "    h = 1.0d-6"//nl// &
            "    call det2(x(1) + h*v(1), x(2) + h*v(2), x(3) + h*v(3), &"//nl// &
            "              x(4) + h*v(4), fp)"//nl// &
            "    call det2(x(1) - h*v(1), x(2) - h*v(2), x(3) - h*v(3), &"//nl// &
            "              x(4) - h*v(4), fm)"//nl// &
            "    fd = (fp - fm)/(2.0d0*h)"//nl// &
            "    if (abs(t - fd) > 1.0d-6*max(1.0d0, abs(fd))) then"//nl// &
            "        print *, 'jvp disagrees with finite differences', t, fd"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            ! The gradient contracted with the direction is the same number.
            "    call det2_vjp(x(1), x(2), x(3), x(4), 1.0d0, g(1), g(2), g(3), g(4))"//nl// &
            "    if (abs(dot_product(g, v) - t) > 1.0d-13*max(1.0d0, abs(t))) then"//nl// &
            "        print *, 'adjoint identity violated', dot_product(g, v), t"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (bad) error stop 1"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        integer :: ios
        character(len=512) :: buf

        open (unit=11, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (11, '(a)', iostat=ios) buf
            if (ios /= 0) exit
            print *, "    ", trim(buf)
        end do
        close (11)
    end subroutine show_file

end program test_tangent_only_oracle
