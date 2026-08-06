program test_hessian_oracle
    !! Independent behavioural oracle for forward-over-reverse Hessian-vector
    !! products.
    !!
    !! Three checks, each able to fail on its own:
    !!
    !! 1. `H v` against central differences **of the gradient**, which is a
    !!    genuinely independent second-order oracle rather than a rearrangement
    !!    of the first-order one.
    !! 2. **Symmetry**: `u^T H v = v^T H u`. A Hessian is symmetric for any
    !!    twice-differentiable function, and an error in the second
    !!    differentiation pass almost always breaks symmetry. Finite differences
    !!    cannot check this, because they are symmetric by construction.
    !! 3. The gradient recovered alongside still matches the first-order result.
    use fortad, only: fad_vjp, fad_hvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0

    call check("hess_exp_product", &
        "function f(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x, y"//nl// &
        "    real(8) :: z"//nl// &
        "    z = exp(x*y) + sin(x)*y*y"//nl// &
        "end function f"//nl, "0.4d0", "0.7d0", failures)

    call check("hess_chain", &
        "function f(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x, y"//nl// &
        "    real(8) :: t"//nl// &
        "    real(8) :: z"//nl// &
        "    t = x*x + y*y"//nl// &
        "    z = log(1.0d0 + t)*tanh(x)"//nl// &
        "end function f"//nl, "0.6d0", "0.9d0", failures)

    call check("hess_transcendental", &
        "function f(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x, y"//nl// &
        "    real(8) :: z"//nl// &
        "    z = sqrt(1.0d0 + x*x + y*y)*cos(x - y)"//nl// &
        "end function f"//nl, "0.5d0", "1.1d0", failures)

    if (failures == 0) then
        print *, "test_hessian_oracle: all cases passed"
    else
        print *, "test_hessian_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(label, source, xval, yval, failures)
        !! Generate the VJP and the HVP, compile with the primal, cross-check.
        character(len=*), intent(in) :: label, source, xval, yval
        integer, intent(inout) :: failures
        type(fad_result_t) :: vjp, hvp
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_hessian/"//label
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        vjp = fad_vjp(source, ["x", "y"], name="f_vjp")
        if (.not. vjp%ok) then
            print *, "FAIL ", label, ": vjp generation failed: ", vjp%message
            failures = failures + 1
            return
        end if

        hvp = fad_hvp(source, ["x", "y"])
        if (.not. hvp%ok) then
            print *, "FAIL ", label, ": hvp generation failed: ", hvp%message
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
        write (unit, '(a)') vjp%code
        write (unit, '(a)') hvp%code
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
        !! Build both Hessian columns from HVPs, difference the gradient, and
        !! check symmetry.
        character(len=*), intent(in) :: xval, yval
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_generated, only: f_vjp, fad_hvp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, y, z, zb, xb, yb"//nl// &
            "    real(8) :: zd, xbd, ybd, xd, yd"//nl// &
            "    real(8) :: h11, h12, h21, h22"//nl// &
            "    real(8) :: gx1, gy1, gx2, gy2, h, lhs, rhs"//nl// &
            "    real(8) :: u1, u2, v1, v2, hv1, hv2, hu1, hu2"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    x = "//xval//nl// &
            "    y = "//yval//nl// &
        ! Hessian columns via HVP with unit directions.
        "    call fad_hvp(x, 1.0d0, y, 0.0d0, z, zd, 1.0d0, xb, h11, yb, h21)"//nl// &
            "    call fad_hvp(x, 0.0d0, y, 1.0d0, z, zd, 1.0d0, xb, h12, yb, h22)"//nl// &
        ! Gradient must still be right.
        "    zb = 1.0d0"//nl// &
            "    call f_vjp(x, y, z, zb, gx1, gy1)"//nl// &
            "    if (abs(gx1 - xb) > 1.0d-12*max(1.0d0, abs(gx1))) then"//nl// &
            "        print *, 'gradient from hvp disagrees', gx1, xb"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
        ! Second derivatives against central differences of the gradient.
        "    h = 1.0d-5"//nl// &
            "    zb = 1.0d0"//nl// &
            "    call f_vjp(x + h, y, z, zb, gx1, gy1)"//nl// &
            "    call f_vjp(x - h, y, z, zb, gx2, gy2)"//nl// &
            "    if (abs((gx1 - gx2)/(2.0d0*h) - h11) > &"//nl// &
            "        1.0d-5*max(1.0d0, abs(h11))) then"//nl// &
            "        print *, 'H(1,1) mismatch:', h11, (gx1 - gx2)/(2.0d0*h)"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (abs((gy1 - gy2)/(2.0d0*h) - h21) > &"//nl// &
            "        1.0d-5*max(1.0d0, abs(h21))) then"//nl// &
            "        print *, 'H(2,1) mismatch:', h21, (gy1 - gy2)/(2.0d0*h)"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    call f_vjp(x, y + h, z, zb, gx1, gy1)"//nl// &
            "    call f_vjp(x, y - h, z, zb, gx2, gy2)"//nl// &
            "    if (abs((gy1 - gy2)/(2.0d0*h) - h22) > &"//nl// &
            "        1.0d-5*max(1.0d0, abs(h22))) then"//nl// &
            "        print *, 'H(2,2) mismatch:', h22, (gy1 - gy2)/(2.0d0*h)"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
        ! Symmetry: u^T H v = v^T H u, which finite differences cannot check.
        "    u1 = 0.83d0; u2 = -0.41d0"//nl// &
            "    v1 = 1.27d0; v2 = 0.55d0"//nl// &
            "    call fad_hvp(x, v1, y, v2, z, zd, 1.0d0, xb, hv1, yb, hv2)"//nl// &
            "    call fad_hvp(x, u1, y, u2, z, zd, 1.0d0, xb, hu1, yb, hu2)"//nl// &
            "    lhs = u1*hv1 + u2*hv2"//nl// &
            "    rhs = v1*hu1 + v2*hu2"//nl// &
            "    if (abs(lhs - rhs) > 1.0d-11*max(1.0d0, abs(lhs))) then"//nl// &
            "        print *, 'Hessian not symmetric:', lhs, rhs"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (abs(h12 - h21) > 1.0d-11*max(1.0d0, abs(h12))) then"//nl// &
            "        print *, 'off-diagonal mismatch:', h12, h21"//nl// &
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

end program test_hessian_oracle
