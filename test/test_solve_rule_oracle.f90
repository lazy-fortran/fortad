program test_solve_rule_oracle
    !! Independent behavioural oracle for structured statement rules, on the
    !! case the whole mechanism exists for: a linear solve.
    !!
    !! The dossier's central asymptotic claim is that differentiating `A x = b`
    !! through the implicit function theorem beats differentiating the solver's
    !! iterations, and that no analysis fortad could do would find the better
    !! form on its own. This test checks that fortad emits the better form and
    !! that the better form is right.
    !!
    !! The generated tangent solves `A x_d = b_d - A_d x` with the **same
    !! matrix** as the primal solve, so a solver holding a factorisation reuses
    !! it. Nothing differentiates the solver.
    !!
    !! The oracle is central finite differences on the whole solve, which knows
    !! nothing about how the derivative was obtained.
    use fortad, only: fad_jvp, fad_add_call_rule, fad_clear_rules, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "subroutine k(n, amat, rhs, x, s)"//nl// &
        "    integer, intent(in) :: n"//nl// &
        "    real(8), intent(in) :: amat(n, n)"//nl// &
        "    real(8), intent(in) :: rhs(n)"//nl// &
        "    real(8), intent(out) :: x(n)"//nl// &
        "    real(8), intent(out) :: s"//nl// &
        "    call linsolve(n, amat, rhs, x)"//nl// &
        "    s = sum(x*x)"//nl// &
        "end subroutine k"//nl
    integer :: failures

    failures = 0

    call fad_clear_rules()
    call check_refusal(failures)

    ! Implicit differentiation: differentiating A x = b gives A x_d = b_d - A_d x.
    call fad_add_call_rule("linsolve", 4, &
        tangent=[character(len=80) :: &
                 "call linsolve($1, $2, $3d - matmul($2d, $4), $4d)"], &
        adjoint=[character(len=80) :: &
                 "call linsolve_transposed($1, $2, $4b, fad_lambda)"])
    call check_generated(failures)

    if (failures == 0) then
        print *, "test_solve_rule_oracle: all cases passed"
    else
        print *, "test_solve_rule_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check_refusal(failures)
        !! Without a rule the call must be refused by name, not assumed inert.
        integer, intent(inout) :: failures
        type(fad_result_t) :: res

        res = fad_jvp(SOURCE, ["rhs"])
        if (res%ok) then
            print *, "FAIL unregistered_call: expected a refusal"
            failures = failures + 1
        else if (index(res%message, "linsolve") == 0) then
            print *, "FAIL unregistered_call: refusal does not name the call: ", &
                res%message
            failures = failures + 1
        else
            print *, "pass unregistered_call (refused: ", trim(res%message), ")"
        end if
    end subroutine check_refusal

    subroutine check_generated(failures)
        !! Compile the generated tangent against a real solver and difference it.
        integer, intent(inout) :: failures
        type(fad_result_t) :: jvp
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_solve"
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        jvp = fad_jvp(SOURCE, ["rhs"], name="k_jvp")
        if (.not. jvp%ok) then
            print *, "FAIL solve_rule: generation failed: ", jvp%message
            failures = failures + 1
            return
        end if

        ! The generated tangent must reuse the primal matrix, which is the
        ! whole point. Checking the emitted text here is legitimate: it is a
        ! statement about what fortad emitted, not about what it computed.
        if (index(jvp%code, "call linsolve(n, amat,") == 0) then
            print *, "FAIL solve_rule: tangent does not reuse the primal matrix"
            print *, jvp%code
            failures = failures + 1
            return
        end if

        open (newunit=unit, file=dir//"/support.f90", status="replace", &
              action="write")
        write (unit, '(a)') solver_text()
        close (unit)

        open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_generated"
        write (unit, '(a)') "    use fad_support, only: linsolve"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') jvp%code
        write (unit, '(a)') "end module fad_generated"
        close (unit)

        open (newunit=unit, file=dir//"/driver.f90", status="replace", &
              action="write")
        write (unit, '(a)') driver_text()
        close (unit)

        call execute_command_line( &
            "cd "//dir//" && gfortran -O2 -o run support.f90 derivs.f90 "// &
            "driver.f90 > build.log 2>&1", exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL solve_rule: generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if
        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL solve_rule: oracle mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass solve_rule_implicit_differentiation"
    end subroutine check_generated

    function solver_text() result(text)
        !! A plain Gaussian-elimination solver and the primal kernel. The
        !! solver is deliberately iterative-looking and completely opaque to
        !! fortad: nothing differentiates it.
        character(len=:), allocatable :: text

        text = &
            "module fad_support"//nl// &
            "    implicit none"//nl// &
            "contains"//nl// &
            "    subroutine linsolve(n, a, b, x)"//nl// &
            "        integer, intent(in) :: n"//nl// &
            "        real(8), intent(in) :: a(n, n), b(n)"//nl// &
            "        real(8), intent(out) :: x(n)"//nl// &
            "        real(8) :: m(n, n), r(n), factor"//nl// &
            "        integer :: i, j, k"//nl// &
            "        m = a"//nl// &
            "        r = b"//nl// &
            "        do k = 1, n - 1"//nl// &
            "            do i = k + 1, n"//nl// &
            "                factor = m(i, k)/m(k, k)"//nl// &
            "                do j = k, n"//nl// &
            "                    m(i, j) = m(i, j) - factor*m(k, j)"//nl// &
            "                end do"//nl// &
            "                r(i) = r(i) - factor*r(k)"//nl// &
            "            end do"//nl// &
            "        end do"//nl// &
            "        do i = n, 1, -1"//nl// &
            "            x(i) = r(i)"//nl// &
            "            do j = i + 1, n"//nl// &
            "                x(i) = x(i) - m(i, j)*x(j)"//nl// &
            "            end do"//nl// &
            "            x(i) = x(i)/m(i, i)"//nl// &
            "        end do"//nl// &
            "    end subroutine linsolve"//nl// &
            ""//nl// &
            "    subroutine k(n, amat, rhs, x, s)"//nl// &
            "        integer, intent(in) :: n"//nl// &
            "        real(8), intent(in) :: amat(n, n), rhs(n)"//nl// &
            "        real(8), intent(out) :: x(n), s"//nl// &
            "        call linsolve(n, amat, rhs, x)"//nl// &
            "        s = sum(x*x)"//nl// &
            "    end subroutine k"//nl// &
            "end module fad_support"//nl
    end function solver_text

    function driver_text() result(text)
        !! Directional derivative against central differences of the whole
        !! solve, in both the right-hand side and the matrix.
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_support, only: k"//nl// &
            "    use fad_generated, only: k_jvp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 5"//nl// &
            "    real(8) :: a(n,n), ad(n,n), b(n), bd(n)"//nl// &
            "    real(8) :: x(n), xd(n), s, sd, sp, sm, fd, h"//nl// &
            "    integer :: i, j"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    do j = 1, n"//nl// &
            "        do i = 1, n"//nl// &
            "            a(i,j) = merge(4.0d0, 0.0d0, i == j) + 0.1d0/(i + j)"//nl// &
            "            ad(i,j) = 0.01d0*sin(1.7d0*i + 0.9d0*j)"//nl// &
            "        end do"//nl// &
            "        b(j) = 1.0d0 + 0.3d0*j"//nl// &
            "        bd(j) = cos(0.7d0*j)"//nl// &
            "    end do"//nl// &
            "    call k_jvp(n, a, ad, b, bd, x, xd, s, sd)"//nl// &
            "    call k(n, a, b, x, sp)"//nl// &
            "    if (abs(s - sp) > 1.0d-12*max(1.0d0, abs(s))) then"//nl// &
            "        print *, 'primal mismatch', s, sp"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call k(n, a + h*ad, b + h*bd, x, sp)"//nl// &
            "    call k(n, a - h*ad, b - h*bd, x, sm)"//nl// &
            "    fd = (sp - sm)/(2.0d0*h)"//nl// &
            "    if (abs(fd - sd) > 1.0d-6*max(1.0d0, abs(sd))) then"//nl// &
            "        print *, 'tangent mismatch: ad =', sd, ' fd =', fd"//nl// &
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

end program test_solve_rule_oracle
