program test_forward_vector_oracle
    !! Independent behavioural oracle for vector forward mode.
    !!
    !! Vector mode is fortad's central performance claim, so it gets the
    !! strictest check: `n_dir` directions carried through one primal sweep must
    !! agree, direction by direction, with (a) the scalar JVP run once per
    !! direction and (b) central finite differences along that direction. If
    !! carrying directions together changed any answer, this fails.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0

    call check("vector_dot_with_sin", &
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

    call check("vector_nonlinear_stencil", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: t"//nl// &
               "    s = 0.0d0"//nl// &
               "    do i = 2, n"//nl// &
               "        t = a(i) - a(i-1)"//nl// &
               "        s = s + t*t/(1.0d0 + b(i)*b(i))"//nl// &
               "    end do"//nl// &
               "end subroutine k"//nl, failures)

    call check("vector_loop_carried_recurrence", &
               "subroutine k(n, a, b, s)"//nl// &
               "    integer, intent(in) :: n"//nl// &
               "    real(8), intent(in) :: a(n)"//nl// &
               "    real(8), intent(in) :: b(n)"//nl// &
               "    real(8), intent(out) :: s"//nl// &
               "    integer :: i"//nl// &
               "    real(8) :: u"//nl// &
               "    u = 1.0d0"//nl// &
               "    do i = 1, n"//nl// &
               "        u = u*exp(0.1d0*a(i)) + b(i)*u"//nl// &
               "    end do"//nl// &
               "    s = u"//nl// &
               "end subroutine k"//nl, failures)

    if (failures == 0) then
        print *, "test_forward_vector_oracle: all cases passed"
    else
        print *, "test_forward_vector_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(label, source, failures)
        !! Generate scalar and vector JVPs, compile both with the primal, and
        !! cross-check them against each other and against finite differences.
        character(len=*), intent(in) :: label, source
        integer, intent(inout) :: failures
        type(fad_result_t) :: scalar_res, vector_res
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_vector/"//label
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        scalar_res = fad_jvp(source, ["a", "b"], name="k_jvp")
        if (.not. scalar_res%ok) then
            print *, "FAIL ", label, ": scalar generation failed: ", &
                scalar_res%message
            failures = failures + 1
            return
        end if

        vector_res = fad_jvp(source, ["a", "b"], name="k_jvp_v", &
                             n_directions="nd")
        if (.not. vector_res%ok) then
            print *, "FAIL ", label, ": vector generation failed: ", &
                vector_res%message
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

        open (newunit=unit, file=dir//"/tangent.f90", status="replace", &
              action="write")
        write (unit, '(a)') "module fad_generated"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "contains"
        write (unit, '(a)') scalar_res%code
        write (unit, '(a)') vector_res%code
        write (unit, '(a)') "end module fad_generated"
        close (unit)

        open (newunit=unit, file=dir//"/driver.f90", status="replace", &
              action="write")
        write (unit, '(a)') driver_text()
        close (unit)

        call execute_command_line( &
            "cd "//dir//" && gfortran -O2 -o run primal.f90 tangent.f90 "// &
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
        !! Four directions at once, cross-checked against the scalar tangent and
        !! against a directional central difference.
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_primal, only: k"//nl// &
            "    use fad_generated, only: k_jvp, k_jvp_v"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 12, nd = 4"//nl// &
            "    real(8) :: a(n), b(n)"//nl// &
            "    real(8) :: av(nd, n), bv(nd, n), sv(nd)"//nl// &
            "    real(8) :: ad(n), bd(n)"//nl// &
            "    real(8) :: s, sdv, sd1, sp, sm, fd, h"//nl// &
            "    integer :: i, j"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    do i = 1, n"//nl// &
            "        a(i) = 0.3d0 + 0.11d0*i"//nl// &
            "        b(i) = 0.7d0 + 0.07d0*i"//nl// &
            "        do j = 1, nd"//nl// &
            "            av(j, i) = sin(0.9d0*i + 0.4d0*j)"//nl// &
            "            bv(j, i) = cos(1.3d0*i - 0.2d0*j)"//nl// &
            "        end do"//nl// &
            "    end do"//nl// &
            "    call k_jvp_v(nd, n, a, av, b, bv, s, sv)"//nl// &
            "    call k(n, a, b, sp)"//nl// &
            "    if (abs(s - sp) > 1.0d-12*max(1.0d0, abs(s))) then"//nl// &
            "        print *, 'primal mismatch', s, sp"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    do j = 1, nd"//nl// &
            "        ad = av(j, :)"//nl// &
            "        bd = bv(j, :)"//nl// &
            "        call k_jvp(n, a, ad, b, bd, s, sd1)"//nl// &
            "        sdv = sv(j)"//nl// &
            "        if (abs(sdv - sd1) > 1.0d-13*max(1.0d0, abs(sd1))) then"//nl// &
            "            print *, 'vector vs scalar mismatch, dir', j, sdv, sd1"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "        h = 1.0d-6"//nl// &
            "        call k(n, a + h*ad, b + h*bd, sp)"//nl// &
            "        call k(n, a - h*ad, b - h*bd, sm)"//nl// &
            "        fd = (sp - sm)/(2.0d0*h)"//nl// &
            "        if (abs(fd - sdv) > 1.0d-5*max(1.0d0, abs(sdv)) + 1.0d-8) then"//nl// &
            "            print *, 'vector vs fd mismatch, dir', j, sdv, fd"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
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

end program test_forward_vector_oracle
