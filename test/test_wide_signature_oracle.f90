program test_wide_signature_oracle
    !! Behavioural oracle for procedures whose dummy-argument list does not fit
    !! on one line.
    !!
    !! A forward-over-reverse pass differentiates the adjoint fortad emitted
    !! itself, and that adjoint has roughly twice as many arguments as the
    !! primal, so its signature is wrapped across continuation lines. Reading
    !! only the first physical line finds no closing parenthesis and lowers the
    !! routine with no parameters at all, which produces an argument-less HVP
    !! that no caller can use.
    !!
    !! The checks here are independent of the generator: the gradient against
    !! central differences of the primal, the Hessian product against central
    !! differences of that gradient, and Hessian symmetry.
    use fortad, only: fad_vjp, fad_hvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "function wide(alpha_one, alpha_two, alpha_three, alpha_four, &"//nl// &
        "        alpha_five, alpha_six) result(y)"//nl// &
        "    real(8), intent(in) :: alpha_one, alpha_two, alpha_three"//nl// &
        "    real(8), intent(in) :: alpha_four, alpha_five, alpha_six"//nl// &
        "    real(8) :: hidden, y"//nl// &
        "    hidden = tanh(alpha_one*alpha_two + alpha_three*alpha_four &"//nl// &
        "        + alpha_five)"//nl// &
        "    y = hidden*alpha_six*exp(alpha_one)"//nl// &
        "end function wide"//nl
    integer :: failures

    failures = 0
    call check_wide_products(failures)

    if (failures == 0) then
        print *, "test_wide_signature_oracle: all cases passed"
    else
        print *, "test_wide_signature_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check_wide_products(failures)
        integer, intent(inout) :: failures
        type(fad_result_t) :: vjp, hvp
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_wide_signature"
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        vjp = fad_vjp(SOURCE, [character(len=11) :: "alpha_one", "alpha_two", &
            "alpha_three", "alpha_four", "alpha_five", "alpha_six"], &
            name="wide_vjp")
        hvp = fad_hvp(SOURCE, [character(len=11) :: "alpha_one", "alpha_two", &
            "alpha_three", "alpha_four", "alpha_five", "alpha_six"], &
            name="wide_hvp")
        if (.not. (vjp%ok .and. hvp%ok)) then
            print *, "FAIL wide signature: generation failed"
            if (.not. vjp%ok) print *, "  vjp: ", vjp%message
            if (.not. hvp%ok) print *, "  hvp: ", hvp%message
            failures = failures + 1
            return
        end if

        open (newunit=unit, file=dir//"/primal.f90", status="replace", &
            action="write")
        write (unit, '(a)') "module fad_primal"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') SOURCE
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
        write (unit, '(a)') driver_text()
        close (unit)

        call execute_command_line( &
            "cd "//dir//" && gfortran -O2 -o run primal.f90 derivs.f90 "// &
            "driver.f90 > build.log 2>&1", exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL wide signature: generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if

        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
            exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL wide signature: product oracle mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass wide_signature_value_vjp_hvp"
    end subroutine check_wide_products

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_primal, only: wide"//nl// &
            "    use fad_generated, only: wide_vjp, wide_hvp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: q(6), v(6), u(6), g(6), hv(6), hu(6)"//nl// &
            "    real(8) :: gp(6), gm(6), gfd(6)"//nl// &
            "    real(8) :: y, yd, base, step, lhs, rhs"//nl// &
            "    integer :: i"//nl// &
            "    logical :: bad"//nl// &
            "    q = [0.4d0, -0.7d0, 0.6d0, -0.2d0, 0.1d0, 1.3d0]"//nl// &
            "    v = [0.3d0, -0.4d0, 0.7d0, 0.2d0, -0.5d0, 0.6d0]"//nl// &
            "    u = [-0.2d0, 0.9d0, 0.1d0, -0.8d0, 0.4d0, 0.5d0]"//nl// &
            "    bad = .false."//nl// &
            "    step = 1.0d-5"//nl// &
            "    call gradient(q, g, y)"//nl// &
            "    base = value_at(q)"//nl// &
            "    if (abs(y - base) > 1.0d-12) then"//nl// &
            "        print *, 'primal mismatch', y, base"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    do i = 1, 6"//nl// &
            "        gfd(i) = central(q, i, step)"//nl// &
            "    end do"//nl// &
            "    if (maxval(abs(g - gfd)) > 2.0d-7) then"//nl// &
            "        print *, 'gradient mismatch', maxval(abs(g - gfd))"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    call hessian_product(q, v, hv)"//nl// &
            "    call gradient(q + step*v, gp, y)"//nl// &
            "    call gradient(q - step*v, gm, y)"//nl// &
            "    if (maxval(abs(hv - (gp - gm)/(2.0d0*step))) > 2.0d-6) then"//nl// &
            "        print *, 'hvp mismatch', &"//nl// &
            "            maxval(abs(hv - (gp - gm)/(2.0d0*step)))"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    call hessian_product(q, u, hu)"//nl// &
            "    lhs = dot_product(u, hv)"//nl// &
            "    rhs = dot_product(v, hu)"//nl// &
            "    if (abs(lhs - rhs) > 1.0d-10*max(1.0d0, abs(lhs))) then"//nl// &
            "        print *, 'hessian symmetry', lhs, rhs"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (bad) error stop 1"//nl// &
            "contains"//nl// &
            driver_helpers()// &
            "end program driver"//nl
    end function driver_text

    function driver_helpers() result(text)
        character(len=:), allocatable :: text

        text = &
            "    real(8) function value_at(p)"//nl// &
            "        real(8), intent(in) :: p(6)"//nl// &
            "        value_at = wide(p(1), p(2), p(3), p(4), p(5), p(6))"//nl// &
            "    end function value_at"//nl// &
            "    real(8) function central(p, i, step)"//nl// &
            "        real(8), intent(in) :: p(6), step"//nl// &
            "        integer, intent(in) :: i"//nl// &
            "        real(8) :: a(6), b(6)"//nl// &
            "        a = p; b = p"//nl// &
            "        a(i) = a(i) + step"//nl// &
            "        b(i) = b(i) - step"//nl// &
            "        central = (value_at(a) - value_at(b))/(2.0d0*step)"//nl// &
            "    end function central"//nl// &
            "    subroutine gradient(p, gout, yout)"//nl// &
            "        real(8), intent(in) :: p(6)"//nl// &
            "        real(8), intent(out) :: gout(6), yout"//nl// &
            "        call wide_vjp(p(1), p(2), p(3), p(4), p(5), p(6), &"//nl// &
            "            yout, 1.0d0, gout(1), gout(2), gout(3), gout(4), &"//nl// &
            "            gout(5), gout(6))"//nl// &
            "    end subroutine gradient"//nl// &
            "    subroutine hessian_product(p, d, hout)"//nl// &
            "        real(8), intent(in) :: p(6), d(6)"//nl// &
            "        real(8), intent(out) :: hout(6)"//nl// &
            "        real(8) :: yv, ydv, gout(6)"//nl// &
            "        call wide_hvp(p(1), d(1), p(2), d(2), p(3), d(3), &"//nl// &
            "            p(4), d(4), p(5), d(5), p(6), d(6), yv, ydv, 1.0d0, &"//nl// &
            "            gout(1), hout(1), gout(2), hout(2), gout(3), hout(3), &"//nl// &
            "            gout(4), hout(4), gout(5), hout(5), gout(6), hout(6))"//nl// &
            "    end subroutine hessian_product"//nl
    end function driver_helpers

    subroutine show_file(path)
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

end program test_wide_signature_oracle
