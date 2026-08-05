program test_implicit_root_rule_oracle
    !! Independent oracle for a nonlinear root IFT structured rule.
    !!
    !! The opaque primal is a twelve-step Newton solve for
    !! F(x,p)=x**3+p(1)*x-p(2)=0. The registered rule differentiates the
    !! converged equation, not the Newton trace. The driver compares both
    !! generated products with fresh complete root solves.
    use fortad, only: fad_jvp, fad_vjp, fad_add_call_rule, fad_clear_rules, &
                      fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "subroutine k(p, x, s)"//nl// &
        "    real(8), intent(in) :: p(2)"//nl// &
        "    real(8), intent(out) :: x"//nl// &
        "    real(8), intent(out) :: s"//nl// &
        "    call root_solve(p, x)"//nl// &
        "    s = x*x"//nl// &
        "end subroutine k"//nl
    type(fad_result_t) :: jvp, vjp
    integer :: stat, unit
    character(len=:), allocatable :: dir

    call fad_clear_rules()
    call fad_add_call_rule("root_solve", 2, &
        tangent=[character(len=128) :: &
                 "call root_tangent($1, $1d, $2, $2d)"], &
        adjoint=[character(len=128) :: &
                 "call root_adjoint($1, $2, $2b, $1b)"], stat=stat)
    if (stat /= 0) error stop "root IFT rule registration failed"

    jvp = fad_jvp(SOURCE, [character(len=1) :: "p"], name="k_jvp")
    vjp = fad_vjp(SOURCE, [character(len=1) :: "p"], dependent="s", &
                  name="k_vjp")
    if (.not. jvp%ok) then
        print *, "FAIL root IFT forward generation: ", jvp%message
        error stop 1
    end if
    if (.not. vjp%ok) then
        print *, "FAIL root IFT reverse generation: ", vjp%message
        error stop 1
    end if

    dir = "build/oracle_implicit_root_rule"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    open (newunit=unit, file=dir//"/support.f90", status="replace", &
          action="write")
    write (unit, '(a)') support_text()
    close (unit)
    open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
          action="write")
    write (unit, '(a)') "module fad_generated"
    write (unit, '(a)') "    use root_support, only: root_solve, root_tangent, root_adjoint"
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
        print *, "FAIL root IFT generated code did not compile"
        call show_file(dir//"/build.log")
        error stop 1
    end if
    call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                              exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL root IFT oracle"
        call show_file(dir//"/out.txt")
        error stop 1
    end if
    print *, "test_implicit_root_rule_oracle: all cases passed"

contains

    function support_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "module root_support"//nl// &
            "    implicit none"//nl// &
            "contains"//nl// &
            "    subroutine root_solve(p, x)"//nl// &
            "        real(8), intent(in) :: p(2)"//nl// &
            "        real(8), intent(out) :: x"//nl// &
            "        integer :: iteration"//nl// &
            "        x = 1.0d0"//nl// &
            "        do iteration = 1, 12"//nl// &
            "            x = x - (x*x*x + p(1)*x - p(2))/(3.0d0*x*x + p(1))"//nl// &
            "        end do"//nl// &
            "    end subroutine root_solve"//nl// &
            "    subroutine root_tangent(p, pd, x, xd)"//nl// &
            "        real(8), intent(in) :: p(2), pd(2), x"//nl// &
            "        real(8), intent(out) :: xd"//nl// &
            "        real(8) :: fx"//nl// &
            "        fx = 3.0d0*x*x + p(1)"//nl// &
            "        xd = -(x*pd(1) - pd(2))/fx"//nl// &
            "    end subroutine root_tangent"//nl// &
            "    subroutine root_adjoint(p, x, xb, pb)"//nl// &
            "        real(8), intent(in) :: p(2), x, xb"//nl// &
            "        real(8), intent(inout) :: pb(2)"//nl// &
            "        real(8) :: fx"//nl// &
            "        fx = 3.0d0*x*x + p(1)"//nl// &
            "        pb(1) = pb(1) - xb*x/fx"//nl// &
            "        pb(2) = pb(2) + xb/fx"//nl// &
            "    end subroutine root_adjoint"//nl// &
            "end module root_support"//nl
    end function support_text

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use root_support, only: root_solve"//nl// &
            "    use fad_generated, only: k_jvp, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: p(2), pd(2), pp(2), pm(2), pb(2)"//nl// &
            "    real(8) :: x, xd, s, sd, sb, sp, sm, h, fd, err"//nl// &
            "    integer :: i"//nl// &
            "    p = [0.7d0, 3.0d0]"//nl// &
            "    pd = [0.4d0, -0.6d0]"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call k_jvp(p, pd, x, xd, s, sd)"//nl// &
            "    pp = p + h*pd; pm = p - h*pd"//nl// &
            "    call root_solve(pp, x); sp = x*x"//nl// &
            "    call root_solve(pm, x); sm = x*x"//nl// &
            "    fd = (sp - sm)/(2.0d0*h)"//nl// &
            "    err = abs(sd - fd)/max(1.0d0, abs(sd))"//nl// &
            "    if (err > 1.0d-8) then"//nl// &
            "        print *, 'jvp finite-difference error', err, sd, fd"//nl// &
            "        error stop 2"//nl// &
            "    end if"//nl// &
            "    sb = 1.0d0"//nl// &
            "    call k_vjp(p, x, s, sb, pb)"//nl// &
            "    err = abs(sd - sum(pb*pd))/max(1.0d0, abs(sd))"//nl// &
            "    if (err > 1.0d-10) then"//nl// &
            "        print *, 'vjp adjoint identity error', err, sd, sum(pb*pd), pb"//nl// &
            "        error stop 3"//nl// &
            "    end if"//nl// &
            "    do i = 1, 2"//nl// &
            "        pp = p; pm = p; pp(i) = pp(i) + h; pm(i) = pm(i) - h"//nl// &
            "        call root_solve(pp, x); sp = x*x"//nl// &
            "        call root_solve(pm, x); sm = x*x"//nl// &
            "        fd = (sp - sm)/(2.0d0*h)"//nl// &
            "        if (abs(pb(i) - fd) > 1.0d-8) then"//nl// &
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

end program test_implicit_root_rule_oracle
