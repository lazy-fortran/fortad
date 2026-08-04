program test_reverse_loop_oracle
    !! Independent behavioural oracle for reverse mode over reduction loops.
    !!
    !! The gradient of a reduction is the workload reverse mode exists for, so
    !! it gets all three checks: element-wise finite differences over the whole
    !! gradient, the adjoint identity against the generated JVP, and the
    !! recomputed primal.
    !!
    !! Coverage spans the four loop shapes fortad distinguishes: a linear
    !! accumulation, a per-iteration temporary, an array-element write, and a
    !! nonlinear loop-carried recurrence. Only the last needs a tape, which is
    !! the whole point of distinguishing them.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0

    call check("revloop_dot_sin", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        s = s + a(i)*sin(b(i))"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("revloop_with_temporary", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: t"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        t = sqrt(a(i)*a(i) + b(i)*b(i))"//nl// &
               "        s = s + exp(-t)*tanh(a(i))"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("revloop_subtraction", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        s = s - log(1.0d0 + a(i)*a(i))*cos(b(i))"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("revloop_multi_term", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        s = s + a(i)*sin(b(i)) + log(1.0d0 + b(i)*b(i)) &"//nl// &
               "            - exp(-a(i)*a(i))"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    ! An array written element by element inside the loop: the adjoint is a
    ! scatter into c_b(i), and nothing is saved because c(i) survives the loop.
    call check("revloop_element_write", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    real(8) :: c(n)"//nl// &
               "    integer :: i"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        c(i) = a(i)*sin(b(i))"//nl// &
               "        s = s + c(i)*c(i)"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("revloop_element_and_temporary", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    real(8) :: c(n)"//nl// &
               "    real(8) :: t"//nl// &
               "    integer :: i"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        t = sqrt(1.0d0 + a(i)*a(i))"//nl// &
               "        c(i) = t*tanh(b(i))"//nl// &
               "        s = s + exp(-c(i))"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    ! Nonlinear loop-carried recurrences: the one case that genuinely needs a
    ! tape, so it is the one case where fortad pays for one.
    call check("revloop_taped_recurrence", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: u"//nl// &
               "    u = 1.0d0"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        u = u*exp(0.05d0*a(i))"//nl// &
               "        s = s + u*b(i)"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("revloop_taped_with_temporary", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: u"//nl// &
               "    real(8) :: t"//nl// &
               "    u = 0.5d0"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        t = tanh(a(i)) + 0.5d0"//nl// &
               "        u = u*t + 0.1d0*b(i)"//nl// &
               "        s = s + u*u"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    ! A second recurrence shape, with the accumulator reading the carried
    ! variable rather than a temporary of it.
    ! The LSTM shape from the Enzyme benchmark suite: `h` is read at the top of
    ! the body and assigned at the bottom, so the read sees the previous
    ! iteration. Nothing in its own right-hand side reveals that, which is what
    ! made an earlier version recompute it and produce a silently wrong
    ! gradient.
    call check("revloop_read_before_assign", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: h"//nl// &
               "    real(8) :: g"//nl// &
               "    h = 0.3d0"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        g = tanh(0.5d0*h + a(i))"//nl// &
               "        h = g*b(i)"//nl// &
               "        s = s + h*h"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    ! The RK4 shape: the accumulated term never mentions the accumulator, but
    ! depends on it through a temporary computed earlier in the same iteration.
    ! A syntactic check calls this a linear accumulation and produces a reversed
    ! gradient; the dependence has to be followed transitively.
    call check("revloop_transitive_dependence", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: u"//nl// &
               "    real(8) :: k1"//nl// &
               "    u = 0.5d0"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        k1 = tanh(u) + 0.1d0*a(i)"//nl// &
               "        u = u + 0.05d0*k1*b(i)"//nl// &
               "        s = s + u"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("revloop_recurrence_product", &
                       "subroutine k(n, a, b, s)"//nl// &
                       "    integer, intent(in) :: n"//nl// &
                       "    real(8), intent(in) :: a(n)"//nl// &
                       "    real(8), intent(in) :: b(n)"//nl// &
                       "    real(8), intent(out) :: s"//nl// &
                       "    integer :: i"//nl// &
                       "    real(8) :: u"//nl// &
                       "    u = 1.0d0"//nl// &
                       "    s = 0.0d0"//nl// &
                       "    do i = 1, n"//nl// &
                       "        u = u*exp(0.1d0*a(i))"//nl// &
                       "        s = s + u*b(i)"//nl// &
                       "    end do"//nl// &
                       "end subroutine k"//nl, failures)

    if (failures == 0) then
        print *, "test_reverse_loop_oracle: all cases passed"
    else
        print *, "test_reverse_loop_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(label, source, failures)
        !! Generate JVP and VJP, compile with the primal, cross-check.
        character(len=*), intent(in) :: label, source
        integer, intent(inout) :: failures
        type(fad_result_t) :: jvp, vjp
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_revloop/"//label
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        jvp = fad_jvp(source, ["a", "b"], name="k_jvp")
        if (.not. jvp%ok) then
            print *, "FAIL ", label, ": jvp generation failed: ", jvp%message
            failures = failures + 1
            return
        end if

        vjp = fad_vjp(source, ["a", "b"], name="k_vjp")
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
        write (unit, '(a)') driver_text()
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

    function driver_text() result(text)
        !! Every gradient component against central differences, then the
        !! adjoint identity against the JVP on random vectors.
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_primal, only: k"//nl// &
            "    use fad_generated, only: k_jvp, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 10"//nl// &
            "    real(8) :: a(n), b(n), ab(n), bb(n), ad(n), bd(n)"//nl// &
            "    real(8) :: ap(n), am(n), bp(n), bm(n)"//nl// &
            "    real(8) :: s, sb, sp, sm, g, h, sj, sdj, lhs, rhs, u"//nl// &
            "    integer :: i"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    do i = 1, n"//nl// &
            "        a(i) = 0.3d0 + 0.11d0*i"//nl// &
            "        b(i) = 0.7d0 + 0.07d0*i"//nl// &
            "    end do"//nl// &
            "    sb = 1.0d0"//nl// &
            "    call k_vjp(n, a, b, s, sb, ab, bb)"//nl// &
            "    call k(n, a, b, sp)"//nl// &
            "    if (abs(s - sp) > 1.0d-12*max(1.0d0, abs(s))) then"//nl// &
            "        print *, 'primal mismatch', s, sp"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    h = 1.0d-6"//nl// &
            "    do i = 1, n"//nl// &
            "        ap = a; am = a"//nl// &
            "        ap(i) = ap(i) + h; am(i) = am(i) - h"//nl// &
            "        call k(n, ap, b, sp); call k(n, am, b, sm)"//nl// &
            "        g = (sp - sm)/(2.0d0*h)"//nl// &
            "        if (abs(g - ab(i)) > 1.0d-5*max(1.0d0, abs(ab(i)))) then"//nl// &
            "            print *, 'da mismatch at', i, ab(i), g"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "        bp = b; bm = b"//nl// &
            "        bp(i) = bp(i) + h; bm(i) = bm(i) - h"//nl// &
            "        call k(n, a, bp, sp); call k(n, a, bm, sm)"//nl// &
            "        g = (sp - sm)/(2.0d0*h)"//nl// &
            "        if (abs(g - bb(i)) > 1.0d-5*max(1.0d0, abs(bb(i)))) then"//nl// &
            "            print *, 'db mismatch at', i, bb(i), g"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
            "    do i = 1, n"//nl// &
            "        ad(i) = sin(0.9d0*i)"//nl// &
            "        bd(i) = cos(1.3d0*i)"//nl// &
            "    end do"//nl// &
            "    u = 0.83d0"//nl// &
            "    call k_jvp(n, a, ad, b, bd, sj, sdj)"//nl// &
            "    lhs = u*sdj"//nl// &
            "    sb = u"//nl// &
            "    call k_vjp(n, a, b, s, sb, ab, bb)"//nl// &
            "    rhs = sum(ab*ad) + sum(bb*bd)"//nl// &
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

end program test_reverse_loop_oracle
