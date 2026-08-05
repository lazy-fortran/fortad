program test_fixed_point_rule_oracle
    !! Independent oracle for a converged fixed-point structured rule.
    !!
    !! The opaque primal iterates x = G(x,p) to convergence. The registered
    !! products apply (I-G_x)^{-1} G_p in tangent mode and the transposed
    !! Christianson solve in reverse mode; the driver compares both with fresh
    !! complete fixed-point solves.
    use fortad, only: fad_jvp, fad_vjp, fad_add_call_rule, fad_clear_rules, &
                      fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "subroutine k(p, x, s)"//nl// &
        "    real(8), intent(in) :: p(2)"//nl// &
        "    real(8), intent(out) :: x(2)"//nl// &
        "    real(8), intent(out) :: s"//nl// &
        "    call fixed_point_solve(p, x)"//nl// &
        "    s = 1.3d0*x(1) - 0.4d0*x(2)"//nl// &
        "end subroutine k"//nl
    type(fad_result_t) :: jvp, vjp
    integer :: stat, unit
    character(len=:), allocatable :: dir

    call fad_clear_rules()
    call fad_add_call_rule("fixed_point_solve", 2, &
        tangent=[character(len=128) :: &
                 "call fixed_point_tangent($1, $1d, $2, $2d)"], &
        adjoint=[character(len=128) :: &
                 "call fixed_point_adjoint($1, $2, $2b, $1b)"], stat=stat)
    if (stat /= 0) error stop "fixed-point rule registration failed"

    jvp = fad_jvp(SOURCE, [character(len=1) :: "p"], name="k_jvp")
    vjp = fad_vjp(SOURCE, [character(len=1) :: "p"], dependent="s", &
                  name="k_vjp")
    if (.not. jvp%ok) then
        print *, "FAIL fixed-point forward generation: ", jvp%message
        error stop 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL fixed-point reverse generation: ", vjp%message
        error stop 1
    end if

    dir = "build/oracle_fixed_point_rule"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    open (newunit=unit, file=dir//"/support.f90", status="replace", &
          action="write")
    write (unit, '(a)') support_text()
    close (unit)
    open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
          action="write")
    write (unit, '(a)') "module fad_generated"
    write (unit, '(a)') "    use fixed_point_support, only: fixed_point_solve, fixed_point_tangent, fixed_point_adjoint"
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
        "cd "//dir//" && gfortran -O2 -o run support.f90 derivs.f90 "// &
        "driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL fixed-point generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                              exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL fixed-point oracle"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_fixed_point_rule_oracle: all cases passed"

contains

    function support_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "module fixed_point_support"//nl// &
            "    implicit none"//nl// &
            "contains"//nl// &
            "    subroutine fixed_point_solve(p, x)"//nl// &
            "        real(8), intent(in) :: p(2)"//nl// &
            "        real(8), intent(out) :: x(2)"//nl// &
            "        real(8) :: next(2)"//nl// &
            "        integer :: iteration"//nl// &
            "        x = 0.0d0"//nl// &
            "        do iteration = 1, 1000"//nl// &
            "            next(1) = tanh(0.2d0*x(1) + 0.1d0*x(2) + p(1))"//nl// &
            "            next(2) = tanh(0.05d0*x(1) + 0.25d0*x(2) + p(2))"//nl// &
            "            if (maxval(abs(next-x)) <= 1.0d-14) then"//nl// &
            "                x = next"//nl// &
            "                return"//nl// &
            "            end if"//nl// &
            "            x = next"//nl// &
            "        end do"//nl// &
            "        error stop 'fixed-point solve did not converge'"//nl// &
            "    end subroutine fixed_point_solve"//nl// &
            "    subroutine fixed_point_tangent(p, pd, x, xd)"//nl// &
            "        real(8), intent(in) :: p(2), pd(2), x(2)"//nl// &
            "        real(8), intent(out) :: xd(2)"//nl// &
            "        real(8) :: s1, s2, next(2)"//nl// &
            "        integer :: iteration"//nl// &
            "        s1 = 1.0d0-x(1)*x(1); s2 = 1.0d0-x(2)*x(2)"//nl// &
            "        xd = 0.0d0"//nl// &
            "        do iteration = 1, 1000"//nl// &
            "            next(1) = s1*(0.2d0*xd(1)+0.1d0*xd(2)+pd(1))"//nl// &
            "            next(2) = s2*(0.05d0*xd(1)+0.25d0*xd(2)+pd(2))"//nl// &
            "            if (maxval(abs(next-xd)) <= 1.0d-14) then"//nl// &
            "                xd = next"//nl// &
            "                return"//nl// &
            "            end if"//nl// &
            "            xd = next"//nl// &
            "        end do"//nl// &
            "        error stop 'fixed-point tangent did not converge'"//nl// &
            "    end subroutine fixed_point_tangent"//nl// &
            "    subroutine fixed_point_adjoint(p, x, xb, pb)"//nl// &
            "        real(8), intent(in) :: p(2), x(2), xb(2)"//nl// &
            "        real(8), intent(inout) :: pb(2)"//nl// &
            "        real(8) :: s1, s2, lambda(2), next(2)"//nl// &
            "        integer :: iteration"//nl// &
            "        logical :: converged"//nl// &
            "        s1 = 1.0d0-x(1)*x(1); s2 = 1.0d0-x(2)*x(2)"//nl// &
            "        lambda = 0.0d0"//nl// &
            "        converged = .false."//nl// &
            "        do iteration = 1, 1000"//nl// &
            "            next(1) = 0.2d0*s1*lambda(1) + 0.05d0*s2*lambda(2) + xb(1)"//nl// &
            "            next(2) = 0.1d0*s1*lambda(1) + 0.25d0*s2*lambda(2) + xb(2)"//nl// &
            "            if (maxval(abs(next-lambda)) <= 1.0d-14) then"//nl// &
            "                lambda = next"//nl// &
            "                converged = .true."//nl// &
            "                exit"//nl// &
            "            end if"//nl// &
            "            lambda = next"//nl// &
            "        end do"//nl// &
            "        if (.not. converged) error stop 'fixed-point adjoint did not converge'"//nl// &
            "        pb(1) = pb(1)+s1*lambda(1)"//nl// &
            "        pb(2) = pb(2)+s2*lambda(2)"//nl// &
            "    end subroutine fixed_point_adjoint"//nl// &
            "end module fixed_point_support"//nl
    end function support_text

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fixed_point_support, only: fixed_point_solve"//nl// &
            "    use fad_generated, only: k_jvp, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: p(2), pd(2), pp(2), pm(2), pb(2)"//nl// &
            "    real(8) :: x(2), xd(2), s, sd, sb, sp, sm, h, fd, err"//nl// &
            "    integer :: i"//nl// &
            "    p = [0.1d0, -0.2d0]"//nl// &
            "    pd = [0.4d0, -0.6d0]"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call k_jvp(p, pd, x, xd, s, sd)"//nl// &
            "    pp = p + h*pd; pm = p - h*pd"//nl// &
            "    call fixed_point_solve(pp, x); sp = 1.3d0*x(1)-0.4d0*x(2)"//nl// &
            "    call fixed_point_solve(pm, x); sm = 1.3d0*x(1)-0.4d0*x(2)"//nl// &
            "    fd = (sp-sm)/(2.0d0*h)"//nl// &
            "    err = abs(sd-fd)/max(1.0d0,abs(sd))"//nl// &
            "    if (err > 1.0d-8) then"//nl// &
            "        print *, 'jvp finite-difference error', err, sd, fd"//nl// &
            "        error stop 2"//nl// &
            "    end if"//nl// &
            "    sb = 1.0d0"//nl// &
            "    call k_vjp(p, x, s, sb, pb)"//nl// &
            "    err = abs(sd-sum(pb*pd))/max(1.0d0,abs(sd))"//nl// &
            "    if (err > 1.0d-10) then"//nl// &
            "        print *, 'vjp adjoint identity error', err, sd, sum(pb*pd), pb"//nl// &
            "        error stop 3"//nl// &
            "    end if"//nl// &
            "    do i = 1, 2"//nl// &
            "        pp = p; pm = p; pp(i) = pp(i)+h; pm(i) = pm(i)-h"//nl// &
            "        call fixed_point_solve(pp, x); sp = 1.3d0*x(1)-0.4d0*x(2)"//nl// &
            "        call fixed_point_solve(pm, x); sm = 1.3d0*x(1)-0.4d0*x(2)"//nl// &
            "        fd = (sp-sm)/(2.0d0*h)"//nl// &
            "        if (abs(pb(i)-fd) > 1.0d-8) then"//nl// &
            "            print *, 'vjp component error', i, pb(i), fd"//nl// &
            "            error stop 4"//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        character(len=512) :: line

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, trim(line)
        end do
        close (unit)
    end subroutine show_file

end program test_fixed_point_rule_oracle
