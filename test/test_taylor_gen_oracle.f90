program test_taylor_gen_oracle
    !! Independent behavioural oracle for the Taylor-mode transformation.
    !!
    !! The arithmetic is already pinned against closed-form series elsewhere.
    !! What this checks is the *transformation*: that fortad rewrites a kernel
    !! into the right sequence of Taylor calls.
    !!
    !! The oracle is a closed-form derivative. For `f(x) = exp(x)*sin(x)` the
    !! `k`-th derivative is `2^(k/2) exp(x) sin(x + k*pi/4)`, which is known
    !! exactly at every order and owes nothing to any derivative machinery.
    !! Orders one and two are cross-checked against the generated first-order
    !! tangent as well, so a transformation error cannot hide behind an
    !! arithmetic error.
    use fortad, only: fad_taylor, fad_jvp, fad_result_t
    implicit none

    integer :: failures

    failures = 0

    call check(failures)
    call check_refusals(failures)

    if (failures == 0) then
        print *, "test_taylor_gen_oracle: all cases passed"
    else
        print *, "test_taylor_gen_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check_refusals(failures)
        !! Constructs a coefficient array cannot represent must be refused.
        integer, intent(inout) :: failures
        character(len=1), parameter :: nl = achar(10)
        type(fad_result_t) :: res

        res = fad_taylor("subroutine k(n, a, s)"//nl// &
                         "    integer, intent(in) :: n"//nl// &
                         "    real(8), intent(in) :: a(n)"//nl// &
                         "    real(8), intent(out) :: s"//nl// &
                         "    s = a(1)*a(1)"//nl// &
                         "end subroutine k"//nl, ["a"])
        if (res%ok) then
            print *, "FAIL taylor_refuses_arrays: expected a refusal"
            failures = failures + 1
        else if (index(res%message, "array") == 0) then
            print *, "FAIL taylor_refuses_arrays: wrong reason: ", res%message
            failures = failures + 1
        else
            print *, "pass taylor_refuses_arrays"
        end if

        res = fad_taylor("function f(x) result(z)"//nl// &
                         "    real(8), intent(in) :: x"//nl// &
                         "    real(8) :: z"//nl// &
                         "    integer :: i"//nl// &
                         "    z = 0.0d0"//nl// &
                         "    do i = 1, 3"//nl// &
                         "        z = z + x"//nl// &
                         "    end do"//nl// &
                         "end function f"//nl, ["x"])
        if (res%ok) then
            print *, "FAIL taylor_refuses_loops: expected a refusal"
            failures = failures + 1
        else
            print *, "pass taylor_refuses_loops"
        end if
    end subroutine check_refusals

    subroutine check(failures)
        !! Generate, compile against the Taylor runtime, and compare with the
        !! closed-form derivatives.
        integer, intent(inout) :: failures
        character(len=1), parameter :: nl = achar(10)
        type(fad_result_t) :: tay, jvp
        character(len=:), allocatable :: dir, source
        integer :: stat, unit

        source = "function f(x) result(z)"//nl// &
                 "    real(8), intent(in) :: x"//nl// &
                 "    real(8) :: z"//nl// &
                 "    z = exp(x)*sin(x)"//nl// &
                 "end function f"//nl

        dir = "build/oracle_taylor_gen"
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        tay = fad_taylor(source, ["x"], name="f_taylor")
        if (.not. tay%ok) then
            print *, "FAIL taylor_gen: generation failed: ", tay%message
            failures = failures + 1
            return
        end if
        jvp = fad_jvp(source, ["x"], name="f_jvp")
        if (.not. jvp%ok) then
            print *, "FAIL taylor_gen: jvp generation failed: ", jvp%message
            failures = failures + 1
            return
        end if

        open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_generated"
        write (unit, '(a)') "    use fortad_taylor"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') tay%code
        write (unit, '(a)') jvp%code
        write (unit, '(a)') "end module fad_generated"
        close (unit)

        open (newunit=unit, file=dir//"/driver.f90", status="replace", &
              action="write")
        write (unit, '(a)') driver_text()
        close (unit)

        ! The generated code calls into fortad_taylor.  Compile the small
        ! runtime directly: fo builds the executable/tests but does not
        ! promise a libfortad.a archive for this source-level oracle.
        call execute_command_line( &
            "cd "//dir//" && gfortran -O2 -o run "// &
            "../../src/fortad_kinds.f90 ../../src/fortad_taylor.f90 "// &
            "derivs.f90 driver.f90 "// &
            "> build.log 2>&1", exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL taylor_gen: generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if
        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL taylor_gen: oracle mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass taylor_gen_against_closed_form"
    end subroutine check

    function driver_text() result(text)
        !! Compare every order against the closed form, and orders 0 and 1
        !! against the generated first-order tangent.
        character(len=1), parameter :: nl = achar(10)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fortad_taylor, only: tay_var, tay_derivative"//nl// &
            "    use fad_generated, only: f_taylor, f_jvp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: d = 6"//nl// &
            "    real(8), parameter :: pi = acos(-1.0d0)"//nl// &
            "    real(8) :: xt(0:d), zt(0:d)"//nl// &
            "    real(8) :: x, z, zd, want, got"//nl// &
            "    integer :: k"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    x = 0.7d0"//nl// &
            "    call tay_var(x, 1.0d0, xt)"//nl// &
            "    call f_taylor(d, xt, zt)"//nl// &
            ! Closed form: the k-th derivative of exp(x)sin(x) is
            ! 2^(k/2) exp(x) sin(x + k*pi/4).
            "    do k = 0, d"//nl// &
            "        want = sqrt(2.0d0)**k*exp(x)*sin(x + k*pi/4.0d0)"//nl// &
            "        got = tay_derivative(zt, k)"//nl// &
            "        if (abs(got - want) > 1.0d-11*max(1.0d0, abs(want))) then"//nl// &
            "            print *, 'order', k, ' got', got, ' want', want"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
            ! Cross-check the first two orders against the first-order tangent.
            "    call f_jvp(x, 1.0d0, z, zd)"//nl// &
            "    if (abs(zt(0) - z) > 1.0d-13*max(1.0d0, abs(z))) then"//nl// &
            "        print *, 'value disagrees with the tangent routine'"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (abs(tay_derivative(zt, 1) - zd) > &"//nl// &
            "        1.0d-13*max(1.0d0, abs(zd))) then"//nl// &
            "        print *, 'first derivative disagrees with the tangent'"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (bad) error stop 1"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        !! Echo a file to stdout, for failure diagnostics.
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=512) :: buf

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) buf
            if (ios /= 0) exit
            print *, "    ", trim(buf)
        end do
        close (unit)
    end subroutine show_file

end program test_taylor_gen_oracle
