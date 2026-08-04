program test_forward_oracle
    !! Independent behavioural oracle for forward mode.
    !!
    !! For each kernel: generate the JVP, compile the generated Fortran with a
    !! driver, run it, and compare the tangent against central finite
    !! differences with a step-size convergence check. The oracle is the
    !! mathematics, not another AD tool and not fortad's own output.
    !!
    !! A single-step finite-difference comparison would pass for a derivative
    !! that is wrong by a constant factor at one point, so each case also
    !! verifies that halving the step reduces the error roughly fourfold, which
    !! is what a correct central difference must do.
    use fortad, only: fad_jvp, fad_roundtrip, fad_result_t
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
    integer :: failures

    failures = 0

    call check("product_and_sin", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: z"//nl// &
               "    z = x*y + sin(x)"//nl// &
               "end function f"//nl, &
               ["x", "y"], "1.3d0", "0.7d0", failures)

    call check("quotient_and_power", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: z"//nl// &
               "    real(8) :: t"//nl// &
               "    t = x*y + sin(x)"//nl// &
               "    z = exp(t) / (1.0d0 + y**2)"//nl// &
               "end function f"//nl, &
               ["x", "y"], "0.4d0", "1.1d0", failures)

    call check("transcendentals", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: z"//nl// &
               "    z = tanh(x)*log(y) + sqrt(x*x + y*y) + atan2(x, y)"//nl// &
               "end function f"//nl, &
               ["x", "y"], "0.9d0", "1.7d0", failures)

    call check("inverse_trig_and_erf", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: z"//nl// &
               "    z = asin(x)*acos(x) + atan(y) + erf(x*y)"//nl// &
               "end function f"//nl, &
               ["x", "y"], "0.35d0", "0.8d0", failures)

    call check("nested_chain", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: a"//nl// &
               "    real(8) :: b"//nl// &
               "    real(8) :: z"//nl// &
               "    a = sin(x*y)"//nl// &
               "    b = exp(a) + cos(a*x)"//nl// &
               "    z = log(1.0d0 + b*b) * sqrt(abs(a) + 2.0d0)"//nl// &
               "end function f"//nl, &
               ["x", "y"], "0.6d0", "1.25d0", failures)

    call check("inactive_variable_is_left_alone", &
               "function f(x, y) result(z)"//nl// &
               "    real(8), intent(in) :: x, y"//nl// &
               "    real(8) :: c"//nl// &
               "    real(8) :: z"//nl// &
               "    c = 3.0d0*y"//nl// &
               "    z = c*sin(x)"//nl// &
               "end function f"//nl, &
               ["x"], "1.1d0", "2.0d0", failures)

    ! Both arms of the same branch, so neither is left untested. A branch is
    ! differentiated arm by arm: the condition itself carries no tangent, and
    ! the derivative is only valid away from the switching surface.
    call check("branch_then_arm", BRANCH_SOURCE, ["x", "y"], "1.5d0", "0.4d0", &
               failures)
    call check("branch_else_arm", BRANCH_SOURCE, ["x", "y"], "0.4d0", "1.5d0", &
               failures)

    if (failures == 0) then
        print *, "test_forward_oracle: all cases passed"
    else
        print *, "test_forward_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(label, source, independents, xval, yval, failures)
        !! Generate, compile, run, and compare against central differences.
        character(len=*), intent(in) :: label, source
        character(len=*), intent(in) :: independents(:)
        character(len=*), intent(in) :: xval, yval
        integer, intent(inout) :: failures
        type(fad_result_t) :: res
        character(len=:), allocatable :: dir, driver
        integer :: stat, unit
        logical :: ok

        dir = "build/oracle/"//label
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        res = fad_jvp(source, independents)
        if (.not. res%ok) then
            print *, "FAIL ", label, ": generation failed: ", res%message
            failures = failures + 1
            return
        end if

        open (newunit=unit, file=dir//"/primal.f90", status="replace", &
              action="write")
        write (unit, '(a)') source
        close (unit)

        ! Wrapping the generated procedure in a module gives the driver a
        ! checked explicit interface. Without it an argument-count mismatch
        ! links happily and corrupts the result instead of failing the build.
        open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_generated"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') res%code
        write (unit, '(a)') "end module fad_generated"
        close (unit)

        driver = build_driver(size(independents), xval, yval)
        open (newunit=unit, file=dir//"/driver.f90", status="replace", &
              action="write")
        write (unit, '(a)') driver
        close (unit)

        call execute_command_line( &
            "gfortran -O2 -o "//dir//"/run "//dir//"/primal.f90 "// &
            dir//"/tangent.f90 "//dir//"/driver.f90 > "//dir//"/build.log 2>&1", &
            exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL ", label, ": generated code did not compile; see ", &
                dir//"/build.log"
            failures = failures + 1
            return
        end if

        call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
                                  exitstat=stat)
        ok = stat == 0
        if (.not. ok) then
            print *, "FAIL ", label, ": oracle mismatch; see ", dir//"/out.txt"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass ", label
    end subroutine check

    function build_driver(n_indep, xval, yval) result(text)
        !! A driver that seeds one tangent direction at a time, compares with
        !! central differences at two step sizes, and checks the error falls
        !! like h**2.
        integer, intent(in) :: n_indep
        character(len=*), intent(in) :: xval, yval
        character(len=:), allocatable :: text
        character(len=:), allocatable :: call_line, ndir

        ! The generated signature carries a tangent only for the independents
        ! that were requested, so the call must match it exactly.
        if (n_indep >= 2) then
            call_line = "        call f_jvp(x, xd, y, yd, z, zd)"
            ndir = "2"
        else
            call_line = "        call f_jvp(x, xd, y, z, zd)"
            ndir = "1"
        end if

        text = &
            "program driver"//nl// &
            "    use fad_generated, only: f_jvp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, y, z, zd, zp, zm, fd1, fd2, e1, e2, h"//nl// &
            "    real(8) :: xd, yd"//nl// &
            "    integer :: k"//nl// &
            "    real(8), external :: f"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    x = "//xval//nl// &
            "    y = "//yval//nl// &
            "    do k = 1, "//ndir//nl// &
            "        xd = 0.0d0"//nl// &
            "        yd = 0.0d0"//nl// &
            "        if (k == 1) xd = 1.0d0"//nl// &
            "        if (k == 2) yd = 1.0d0"//nl// &
            call_line//nl// &
            "        h = 1.0d-4"//nl// &
            "        zp = f(x + h*xd, y + h*yd)"//nl// &
            "        zm = f(x - h*xd, y - h*yd)"//nl// &
            "        fd1 = (zp - zm)/(2.0d0*h)"//nl// &
            "        h = 0.5d-4"//nl// &
            "        zp = f(x + h*xd, y + h*yd)"//nl// &
            "        zm = f(x - h*xd, y - h*yd)"//nl// &
            "        fd2 = (zp - zm)/(2.0d0*h)"//nl// &
            "        e1 = abs(fd1 - zd)"//nl// &
            "        e2 = abs(fd2 - zd)"//nl// &
            "        if (abs(z - f(x, y)) > 1.0d-12*max(1.0d0, abs(z))) then"//nl// &
            "            print *, 'primal mismatch, direction', k, z, f(x, y)"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "        if (e2 > 1.0d-6*max(1.0d0, abs(zd)) + 1.0d-9) then"//nl// &
            "            print *, 'tangent mismatch, direction', k"//nl// &
            "            print *, '  ad =', zd, ' fd =', fd2, ' err =', e2"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "        if (e1 > 1.0d-11 .and. e2 > 0.40d0*e1) then"//nl// &
            "            print *, 'no second-order convergence, direction', k"//nl// &
            "            print *, '  e(h) =', e1, ' e(h/2) =', e2"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
            "    if (bad) error stop 1"//nl// &
            "end program driver"//nl
    end function build_driver

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

end program test_forward_oracle
