program test_reverse_nest_oracle
    !! Independent behavioural oracle for reverse mode over nested loops.
    !!
    !! A two-dimensional reduction is the natural shape of a PDE residual or a
    !! grid functional, and it is where a rank assumption hides: zeroing an
    !! adjoint with `a_b(:)` compiles at rank one and fails at rank two, and a
    !! subscript handled positionally works until the second index appears.
    !! Both are checked here by construction rather than by inspection.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0

    call check("nest_dot_sin_2d", &
               "subroutine k(m, n, a, b, s)"//nl// &
               "    integer, intent(in) :: m, n"//nl// &
               "    real(8), intent(in) :: a(m, n)"//nl// &
               "    real(8), intent(in) :: b(m, n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i, j"//nl// &
               "    s = 0.0d0"//nl// &
               "    do j = 1, n"//nl// &
               "        do i = 1, m"//nl// &
               "            s = s + a(i,j)*sin(b(i,j))"//nl// &
               "        end do"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("nest_with_temporary", &
               "subroutine k(m, n, a, b, s)"//nl// &
               "    integer, intent(in) :: m, n"//nl// &
               "    real(8), intent(in) :: a(m, n)"//nl// &
               "    real(8), intent(in) :: b(m, n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i, j"//nl// &
               "    real(8) :: t"//nl// &
               "    s = 0.0d0"//nl// &
               "    do j = 1, n"//nl// &
               "        do i = 1, m"//nl// &
               "            t = sqrt(1.0d0 + a(i,j)*a(i,j))"//nl// &
               "            s = s + t*tanh(b(i,j)) - log(1.0d0 + t)"//nl// &
               "        end do"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    if (failures == 0) then
        print *, "test_reverse_nest_oracle: all cases passed"
    else
        print *, "test_reverse_nest_oracle: ", failures, " case(s) FAILED"
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

        dir = "build/oracle_nest/"//label
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
        !! Every gradient entry against central differences, then the adjoint
        !! identity against the generated tangent.
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_primal, only: k"//nl// &
            "    use fad_generated, only: k_jvp, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: m = 4, n = 5"//nl// &
            "    real(8) :: a(m,n), b(m,n), ab(m,n), bb(m,n)"//nl// &
            "    real(8) :: ad(m,n), bd(m,n), ap(m,n), am(m,n)"//nl// &
            "    real(8) :: s, sb, sp, sm, g, h, sj, sdj, lhs, rhs, u"//nl// &
            "    integer :: i, j"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    do j = 1, n"//nl// &
            "        do i = 1, m"//nl// &
            "            a(i,j) = 0.3d0 + 0.11d0*i + 0.07d0*j"//nl// &
            "            b(i,j) = 0.7d0 + 0.05d0*i - 0.03d0*j"//nl// &
            "            ad(i,j) = sin(0.9d0*i + 0.4d0*j)"//nl// &
            "            bd(i,j) = cos(1.3d0*i - 0.2d0*j)"//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    sb = 1.0d0"//nl// &
            "    call k_vjp(m, n, a, b, s, sb, ab, bb)"//nl// &
            "    call k(m, n, a, b, sp)"//nl// &
            "    if (abs(s - sp) > 1.0d-12*max(1.0d0, abs(s))) then"//nl// &
            "        print *, 'primal mismatch', s, sp"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    h = 1.0d-6"//nl// &
            "    do j = 1, n"//nl// &
            "        do i = 1, m"//nl// &
            "            ap = a; am = a"//nl// &
            "            ap(i,j) = ap(i,j) + h; am(i,j) = am(i,j) - h"//nl// &
            "            call k(m, n, ap, b, sp); call k(m, n, am, b, sm)"//nl// &
            "            g = (sp - sm)/(2.0d0*h)"//nl// &
            "            if (abs(g - ab(i,j)) > 1.0d-5*max(1.0d0, abs(ab(i,j)))) then"//nl// &
            "                print *, 'da mismatch at', i, j, ab(i,j), g"//nl// &
            "                bad = .true."//nl// &
            "            end if"//nl// &
            "            ap = b; am = b"//nl// &
            "            ap(i,j) = ap(i,j) + h; am(i,j) = am(i,j) - h"//nl// &
            "            call k(m, n, a, ap, sp); call k(m, n, a, am, sm)"//nl// &
            "            g = (sp - sm)/(2.0d0*h)"//nl// &
            "            if (abs(g - bb(i,j)) > 1.0d-5*max(1.0d0, abs(bb(i,j)))) then"//nl// &
            "                print *, 'db mismatch at', i, j, bb(i,j), g"//nl// &
            "                bad = .true."//nl// &
            "            end if"//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    u = 0.83d0"//nl// &
            "    call k_jvp(m, n, a, ad, b, bd, sj, sdj)"//nl// &
            "    lhs = u*sdj"//nl// &
            "    sb = u"//nl// &
            "    call k_vjp(m, n, a, b, s, sb, ab, bb)"//nl// &
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

end program test_reverse_nest_oracle
