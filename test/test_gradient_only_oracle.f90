program test_gradient_only_oracle
    !! Behavioural oracle for gradient-only reverse mode.
    !!
    !! Dropping the primal value lets dead-loop elimination remove the forward
    !! sweep outright. That is a large edit to the emitted routine made on a
    !! liveness argument, so it is checked two ways that owe nothing to each
    !! other: the gradient must match central finite differences of the
    !! *original* kernel, and it must equal the gradient the with-primal
    !! routine produces. Finite differences pin the value; the agreement
    !! between the two routines pins that only dead code was removed.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortad, only: fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0

    ! A linear recurrence: nothing in its adjoint reads a primal value, so the
    ! whole forward loop is expected to disappear.
    call check("euler", &
               "subroutine k(n, z, y)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: z(n)"//nl// &
               "    real(8), intent(out) :: y"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: dt, state"//nl// &
               "    dt = 2.1d0/real(n, 8)"//nl// &
               "    state = 1.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        state = state + dt*(-1.2d0*state + 0.05d0*z(i))"//nl// &
               "    end do"//nl// &
               "    y = state"//nl// &
               "end subroutine k"//nl, .true., failures)

    ! A nonlinear recurrence: the adjoint needs primal values, so the forward
    ! sweep must survive. The same test therefore also checks that dead-loop
    ! elimination is not over-eager.
    call check("nonlinear", &
               "subroutine k(n, z, y)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: z(n)"//nl// &
               "    real(8), intent(out) :: y"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: state"//nl// &
               "    state = 1.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        state = state*exp(0.01d0*z(i))"//nl// &
               "    end do"//nl// &
               "    y = state"//nl// &
               "end subroutine k"//nl, .false., failures)

    if (failures == 0) then
        print *, "test_gradient_only_oracle: all cases passed"
    else
        print *, "test_gradient_only_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(label, source, expect_loop_gone, failures)
        character(len=*), intent(in) :: label, source
        logical, intent(in) :: expect_loop_gone
        integer, intent(inout) :: failures
        type(fad_result_t) :: full, grad
        character(len=:), allocatable :: dir
        integer :: stat, unit
        logical :: has_forward

        full = fad_vjp(source, ["z"], name="k_full")
        grad = fad_vjp(source, ["z"], name="k_grad", with_primal=.false.)
        if (.not. full%ok .or. .not. grad%ok) then
            print *, "FAIL "//label//": generation failed"
            failures = failures + 1
            return
        end if

        ! The forward sweep is the only loop running upwards.
        has_forward = index(grad%code, "do i = 1, n") > 0
        if (expect_loop_gone .and. has_forward) then
            print *, "FAIL "//label//": forward sweep was not eliminated"
            failures = failures + 1
            return
        end if
        if (.not. expect_loop_gone .and. .not. has_forward) then
            print *, "FAIL "//label//": forward sweep wrongly eliminated"
            failures = failures + 1
            return
        end if

        dir = "build/oracle_gradient_only_"//label
        call execute_command_line("mkdir -p "//dir, exitstat=stat)
        open (newunit=unit, file=dir//"/gen.f90", status="replace", action="write")
        write (unit, '(a)') "module gen"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') source
        write (unit, '(a)') full%code
        write (unit, '(a)') grad%code
        write (unit, '(a)') "end module gen"
        close (unit)

        open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
        write (unit, '(a)') driver_text()
        close (unit)

        call execute_command_line("cd "//dir//" && gfortran -O2 -o run gen.f90 "// &
                                  "driver.f90 > build.log 2>&1", exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL "//label//": generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if
        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL "//label//": oracle mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass gradient_only_"//label
    end subroutine check

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use gen, only: k, k_full, k_grad"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 24"//nl// &
            "    real(8) :: z(n), gb(n), gf(n), y, yp, ym, h, fd"//nl// &
            "    integer :: i"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    do i = 1, n"//nl// &
            "        z(i) = 0.3d0*sin(0.7d0*i) + 0.1d0*i"//nl// &
            "    end do"//nl// &
            "    call k_grad(n, z, 1.0d0, gb)"//nl// &
            "    call k_full(n, z, y, 1.0d0, gf)"//nl// &
            "    do i = 1, n"//nl// &
            "        if (abs(gb(i) - gf(i)) > 0.0d0) then"//nl// &
            "            print *, 'routines disagree at', i, gb(i), gf(i)"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
            ! Central differences on the untouched kernel.
            "    do i = 1, n"//nl// &
            "        h = 1.0d-5*max(1.0d0, abs(z(i)))"//nl// &
            "        z(i) = z(i) + h"//nl// &
            "        call k(n, z, yp)"//nl// &
            "        z(i) = z(i) - 2.0d0*h"//nl// &
            "        call k(n, z, ym)"//nl// &
            "        z(i) = z(i) + h"//nl// &
            "        fd = (yp - ym)/(2.0d0*h)"//nl// &
            "        if (abs(gb(i) - fd) > 1.0d-6*max(1.0d0, abs(fd))) then"//nl// &
            "            print *, 'fd mismatch at', i, gb(i), fd"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
            "    if (bad) error stop 1"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=512) :: buf

        open (unit=10, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (10, '(a)', iostat=ios) buf
            if (ios /= 0) exit
            print *, "    ", trim(buf)
        end do
        close (10)
    end subroutine show_file

end program test_gradient_only_oracle
