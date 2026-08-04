program test_registry_oracle
    !! Independent behavioural oracle for user-registered derivative rules.
    !!
    !! The registry is where fortad stops knowing the mathematics and starts
    !! trusting the user, so the test checks the two things fortad is actually
    !! responsible for:
    !!
    !! 1. That an unregistered procedure is **refused**, naming the procedure,
    !!    rather than guessed at or silently treated as constant. Silently
    !!    treating an unknown call as inactive is the single most dangerous
    !!    failure an AD tool can have, because the result looks plausible.
    !! 2. That once a rule is registered, the generated derivative is correct
    !!    against finite differences, in both forward and reverse mode - which
    !!    also confirms the rule is read transposed for reverse rather than
    !!    reimplemented.
    use fortad, only: fad_jvp, fad_vjp, fad_add_rule, fad_clear_rules, &
                      fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: SOURCE = &
        "function f(x, y) result(z)"//nl// &
        "    real(8), intent(in) :: x, y"//nl// &
        "    real(8) :: z"//nl// &
        "    z = softclip(x, y) + x*y"//nl// &
        "end function f"//nl
    integer :: failures

    failures = 0

    call fad_clear_rules()
    call check_refusal(failures)

    ! softclip(u, v) = v*tanh(u/v), so
    !   d/du = 1 - tanh(u/v)**2
    !   d/dv = tanh(u/v) - (u/v)*(1 - tanh(u/v)**2)
    call fad_add_rule("softclip", &
                      ["1.0d0 - tanh($1/$2)**2                                  ", &
                       "tanh($1/$2) - ($1/$2)*(1.0d0 - tanh($1/$2)**2)          "])
    call check_generated(failures)

    if (failures == 0) then
        print *, "test_registry_oracle: all cases passed"
    else
        print *, "test_registry_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check_refusal(failures)
        !! An unknown call must be refused by name, in both modes.
        integer, intent(inout) :: failures
        type(fad_result_t) :: res

        res = fad_jvp(SOURCE, ["x", "y"])
        if (res%ok) then
            print *, "FAIL unregistered_forward: expected a refusal"
            failures = failures + 1
        else if (index(res%message, "softclip") == 0) then
            print *, "FAIL unregistered_forward: refusal does not name the "// &
                "procedure: ", res%message
            failures = failures + 1
        else
            print *, "pass unregistered_forward (refused: ", &
                trim(res%message), ")"
        end if

        res = fad_vjp(SOURCE, ["x", "y"])
        if (res%ok) then
            print *, "FAIL unregistered_reverse: expected a refusal"
            failures = failures + 1
        else if (index(res%message, "softclip") == 0) then
            print *, "FAIL unregistered_reverse: refusal does not name the "// &
                "procedure: ", res%message
            failures = failures + 1
        else
            print *, "pass unregistered_reverse"
        end if
    end subroutine check_refusal

    subroutine check_generated(failures)
        !! With the rule registered, both modes must be right.
        integer, intent(inout) :: failures
        type(fad_result_t) :: jvp, vjp
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_registry"
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        jvp = fad_jvp(SOURCE, ["x", "y"], name="f_jvp")
        if (.not. jvp%ok) then
            print *, "FAIL registered_forward: generation failed: ", jvp%message
            failures = failures + 1
            return
        end if
        vjp = fad_vjp(SOURCE, ["x", "y"], name="f_vjp")
        if (.not. vjp%ok) then
            print *, "FAIL registered_reverse: generation failed: ", vjp%message
            failures = failures + 1
            return
        end if

        open (newunit=unit, file=dir//"/support.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_support"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') "    pure function softclip(u, v) result(w)"
        write (unit, '(a)') "        real(8), intent(in) :: u, v"
        write (unit, '(a)') "        real(8) :: w"
        write (unit, '(a)') "        w = v*tanh(u/v)"
        write (unit, '(a)') "    end function softclip"
        write (unit, '(a)') ""
        write (unit, '(a)') "    function f(x, y) result(z)"
        write (unit, '(a)') "        real(8), intent(in) :: x, y"
        write (unit, '(a)') "        real(8) :: z"
        write (unit, '(a)') "        z = softclip(x, y) + x*y"
        write (unit, '(a)') "    end function f"
        write (unit, '(a)') "end module fad_support"
        close (unit)

        open (newunit=unit, file=dir//"/derivs.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_generated"
        write (unit, '(a)') "    use fad_support, only: softclip"
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
            print *, "FAIL registered: generated code did not compile"
            call show_file(dir//"/build.log")
            failures = failures + 1
            return
        end if
        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL registered: oracle mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
            return
        end if
        print *, "pass registered_forward_and_reverse"
    end subroutine check_generated

    function driver_text() result(text)
        !! Finite differences on both modes, plus the adjoint identity.
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_support, only: f"//nl// &
            "    use fad_generated, only: f_jvp, f_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: x, y, z, zd, zb, xb, yb, h, g, lhs, rhs"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    x = 0.7d0"//nl// &
            "    y = 1.3d0"//nl// &
            "    h = 1.0d-6"//nl// &
            "    call f_jvp(x, 1.0d0, y, 0.0d0, z, zd)"//nl// &
            "    g = (f(x + h, y) - f(x - h, y))/(2.0d0*h)"//nl// &
            "    if (abs(g - zd) > 1.0d-6*max(1.0d0, abs(zd))) then"//nl// &
            "        print *, 'forward d/dx mismatch:', zd, g"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    call f_jvp(x, 0.0d0, y, 1.0d0, z, zd)"//nl// &
            "    g = (f(x, y + h) - f(x, y - h))/(2.0d0*h)"//nl// &
            "    if (abs(g - zd) > 1.0d-6*max(1.0d0, abs(zd))) then"//nl// &
            "        print *, 'forward d/dy mismatch:', zd, g"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    zb = 1.0d0"//nl// &
            "    call f_vjp(x, y, z, zb, xb, yb)"//nl// &
            "    g = (f(x + h, y) - f(x - h, y))/(2.0d0*h)"//nl// &
            "    if (abs(g - xb) > 1.0d-6*max(1.0d0, abs(xb))) then"//nl// &
            "        print *, 'reverse d/dx mismatch:', xb, g"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    g = (f(x, y + h) - f(x, y - h))/(2.0d0*h)"//nl// &
            "    if (abs(g - yb) > 1.0d-6*max(1.0d0, abs(yb))) then"//nl// &
            "        print *, 'reverse d/dy mismatch:', yb, g"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    call f_jvp(x, 0.83d0, y, -0.41d0, z, zd)"//nl// &
            "    lhs = 1.27d0*zd"//nl// &
            "    zb = 1.27d0"//nl// &
            "    call f_vjp(x, y, z, zb, xb, yb)"//nl// &
            "    rhs = xb*0.83d0 + yb*(-0.41d0)"//nl// &
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

end program test_registry_oracle
