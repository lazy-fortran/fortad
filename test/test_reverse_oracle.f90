program test_reverse_oracle
    !! Independent behavioural oracle for reverse mode.
    !!
    !! Three checks per kernel, each able to fail independently:
    !!
    !! 1. The gradient matches central finite differences, with a step-size
    !!    convergence requirement so a constant-factor error cannot pass.
    !! 2. The **adjoint identity** `<u, J v> = <J^T u, v>` holds for random `u`
    !!    and `v`, comparing the generated VJP against the generated JVP. This
    !!    is the check that catches a transposition mistake, which finite
    !!    differences alone can miss when the Jacobian is nearly symmetric.
    !! 3. The recomputed primal still equals the original.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: BRANCH_SOURCE = &
        "function f(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x, y"//nl// &
        "    real(8) :: z"//nl// &
        "    if (x > y) then"//nl// &
        "        z = x*x + sin(y)"//nl// &
        "    else"//nl// &
        "        z = exp(y)*x"//nl// &
        "    end if"//nl// &
        "end function f"//nl
    character(len=*), parameter :: ASYM_SOURCE = &
        "function f(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x, y"//nl// &
        "    real(8) :: t"//nl// &
        "    real(8) :: z"//nl// &
        "    t = x + y"//nl// &
        "    if (x > y) then"//nl// &
        "        t = t*t"//nl// &
        "        t = sin(t) + x"//nl// &
        "        z = t*y"//nl// &
        "    else"//nl// &
        "        z = log(1.0d0 + t*t)*x"//nl// &
        "    end if"//nl// &
        "end function f"//nl
    integer :: failures

    failures = 0

    call check("rev_product_and_sin", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: z"//nl// &
               "    z = x*y + sin(x)"//nl// &
               "end function f"//nl, "1.3d0", "0.7d0", failures)

    call check("rev_quotient_and_power", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: z"//nl// &
               "    real(8) :: t"//nl// &
               "    t = x*y + sin(x)"//nl// &
               "    z = exp(t) / (1.0d0 + y**2)"//nl// &
               "end function f"//nl, "0.4d0", "1.1d0", failures)

    call check("rev_transcendentals", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: z"//nl// &
               "    z = tanh(x)*log(y) + sqrt(x*x + y*y) + atan2(x, y)"//nl// &
               "end function f"//nl, "0.9d0", "1.7d0", failures)

    call check("rev_reused_variable", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: t"//nl// &
               "    real(8) :: z"//nl// &
               "    t = x + y"//nl// &
               "    t = t*t + sin(t)"//nl// &
               "    t = exp(0.5d0*t)"//nl// &
               "    z = t*x"//nl// &
               "end function f"//nl, "0.6d0", "0.45d0", failures)

    call check("rev_deep_chain", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: a"//nl// &
               "    real(8) :: b"//nl// &
               "    real(8) :: z"//nl// &
               "    a = sin(x*y)"//nl// &
               "    b = exp(a) + cos(a*x)"//nl// &
               "    z = log(1.0d0 + b*b) * sqrt(abs(a) + 2.0d0)"//nl// &
               "end function f"//nl, "0.6d0", "1.25d0", failures)

    ! Branches: both arms, and an asymmetric case where the two arms assign
    ! different numbers of intermediates so the join needs a real merge.
    call check("rev_branch_then_arm", BRANCH_SOURCE, "1.5d0", "0.4d0", failures)
    call check("rev_branch_else_arm", BRANCH_SOURCE, "0.4d0", "1.5d0", failures)
    call check("rev_branch_asymmetric_then", ASYM_SOURCE, "1.4d0", "0.5d0", &
               failures)
    call check("rev_branch_asymmetric_else", ASYM_SOURCE, "0.5d0", "1.4d0", &
               failures)

    if (failures == 0) then
        print *, "test_reverse_oracle: all cases passed"
    else
        print *, "test_reverse_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(label, source, xval, yval, failures)
        !! Generate JVP and VJP, compile both with the primal, cross-check.
        character(len=*), intent(in) :: label, source, xval, yval
        integer, intent(inout) :: failures
        type(fad_result_t) :: jvp, vjp
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_reverse/"//label
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        jvp = fad_jvp(source, ["x", "y"], name="f_jvp")
        if (.not. jvp%ok) then
            print *, "FAIL ", label, ": jvp generation failed: ", jvp%message
            failures = failures + 1
            return
        end if

        vjp = fad_vjp(source, ["x", "y"], name="f_vjp")
        if (.not. vjp%ok) then
            print *, "FAIL ", label, ": vjp generation failed: ", vjp%message
            failures = failures + 1
            return
        end if

        open (newunit=unit, file=dir//"/primal.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_primal"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') source
        write (unit, '(a)') "end module fad_primal"
        close (unit)

        open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_generated"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') jvp%code
        write (unit, '(a)') vjp%code
        write (unit, '(a)') "end module fad_generated"
        close (unit)

        open (newunit=unit, file=dir//"/driver.f90", status="replace", &
              action="write")
        write (unit, '(a)') driver_text(xval, yval)
        close (unit)

        call execute_command_line( &
            "cd "//dir//" && gfortran -O2 -o run primal.f90 derivs.f90 "// &
            "driver.f90 > build.log 2>&1", exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL ", label, ": generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if

        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL ", label, ": oracle mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass ", label
    end subroutine check

    function driver_text(xval, yval) result(text)
        !! Finite differences, then the adjoint identity against the JVP.
        character(len=*), intent(in) :: xval, yval
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_primal, only: f"//nl// &
            "    use fad_generated, only: f_jvp, f_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, y, z, zb, xb, yb"//nl// &
            "    real(8) :: zj, zdj, xd, yd, h, gx1, gx2, gy1, gy2"//nl// &
            "    real(8) :: lhs, rhs, u, v1, v2"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    x = "//xval//nl// &
            "    y = "//yval//nl// &
            ! gradient by one reverse sweep
            "    zb = 1.0d0"//nl// &
            "    call f_vjp(x, y, z, zb, xb, yb)"//nl// &
            "    if (abs(z - f(x, y)) > 1.0d-12*max(1.0d0, abs(z))) then"//nl// &
            "        print *, 'primal mismatch', z, f(x, y)"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    h = 1.0d-4"//nl// &
            "    gx1 = (f(x + h, y) - f(x - h, y))/(2.0d0*h)"//nl// &
            "    gy1 = (f(x, y + h) - f(x, y - h))/(2.0d0*h)"//nl// &
            "    h = 0.5d-4"//nl// &
            "    gx2 = (f(x + h, y) - f(x - h, y))/(2.0d0*h)"//nl// &
            "    gy2 = (f(x, y + h) - f(x, y - h))/(2.0d0*h)"//nl// &
            "    if (abs(gx2 - xb) > 1.0d-6*max(1.0d0, abs(xb)) + 1.0d-9) then"//nl// &
            "        print *, 'd/dx mismatch: ad =', xb, ' fd =', gx2"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (abs(gy2 - yb) > 1.0d-6*max(1.0d0, abs(yb)) + 1.0d-9) then"//nl// &
            "        print *, 'd/dy mismatch: ad =', yb, ' fd =', gy2"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (abs(gx1 - xb) > 1.0d-11 .and. &"//nl// &
            "        abs(gx2 - xb) > 0.40d0*abs(gx1 - xb)) then"//nl// &
            "        print *, 'no second-order convergence in x'"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            ! adjoint identity <u, J v> = <J^T u, v>
            "    u = 0.83d0"//nl// &
            "    v1 = -0.41d0"//nl// &
            "    v2 = 1.27d0"//nl// &
            "    xd = v1"//nl// &
            "    yd = v2"//nl// &
            "    call f_jvp(x, xd, y, yd, zj, zdj)"//nl// &
            "    lhs = u*zdj"//nl// &
            "    zb = u"//nl// &
            "    call f_vjp(x, y, z, zb, xb, yb)"//nl// &
            "    rhs = xb*v1 + yb*v2"//nl// &
            "    if (abs(lhs - rhs) > 1.0d-12*max(1.0d0, abs(lhs))) then"//nl// &
            "        print *, 'adjoint identity violated:', lhs, rhs"//nl// &
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

end program test_reverse_oracle
