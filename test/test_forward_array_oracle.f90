program test_forward_array_oracle
    !! Independent behavioural oracle for forward mode over loops and arrays.
    !!
    !! Same contract as the scalar oracle: generate, compile, run, and compare
    !! the tangent against central finite differences with a step-size
    !! convergence check. These kernels carry the constructs real numerical code
    !! is made of - a reduction loop, a stencil over array elements, and a
    !! loop-carried recurrence - so passing here means the loop and subscript
    !! handling is right, not just the expression rules.
    use fortad, only: fad_jvp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    integer :: failures

    failures = 0

    call check("dot_with_sin", &
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

    call check("nonlinear_stencil", &
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

    call check("loop_carried_recurrence", &
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
        print *, "test_forward_array_oracle: all cases passed"
    else
        print *, "test_forward_array_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine check(label, source, failures)
        !! Generate, compile, run, and compare against central differences.
        character(len=*), intent(in) :: label, source
        integer, intent(inout) :: failures
        type(fad_result_t) :: res
        character(len=:), allocatable :: dir
        integer :: stat, unit

        dir = "build/oracle_array/"//label
        call execute_command_line("mkdir -p "//dir, exitstat=stat)

        res = fad_jvp(source, ["a", "b"])
        if (.not. res%ok) then
            print *, "FAIL ", label, ": generation failed: ", res%message
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
        write (unit, '(a)') res%code
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
        !! Seeds a random tangent direction over both arrays at once, which
        !! exercises every element's contribution in a single sweep, then
        !! checks against a directional central difference.
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use fad_primal, only: k"//nl// &
            "    use fad_generated, only: k_jvp"//nl// &
            "    implicit none"//nl// &
            "    integer, parameter :: n = 12"//nl// &
            "    real(8) :: a(n), b(n), ad(n), bd(n)"//nl// &
            "    real(8) :: s, sd, sp, sm, fd1, fd2, e1, e2, h"//nl// &
            "    integer :: i"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    do i = 1, n"//nl// &
            "        a(i) = 0.3d0 + 0.11d0*i"//nl// &
            "        b(i) = 0.7d0 + 0.07d0*i"//nl// &
            "        ad(i) = sin(0.9d0*i)"//nl// &
            "        bd(i) = cos(1.3d0*i)"//nl// &
            "    end do"//nl// &
            "    call k_jvp(n, a, ad, b, bd, s, sd)"//nl// &
            "    call k(n, a, b, sp)"//nl// &
            "    if (abs(s - sp) > 1.0d-12*max(1.0d0, abs(s))) then"//nl// &
            "        print *, 'primal mismatch', s, sp"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    h = 1.0d-5"//nl// &
            "    call k(n, a + h*ad, b + h*bd, sp)"//nl// &
            "    call k(n, a - h*ad, b - h*bd, sm)"//nl// &
            "    fd1 = (sp - sm)/(2.0d0*h)"//nl// &
            "    h = 0.5d-5"//nl// &
            "    call k(n, a + h*ad, b + h*bd, sp)"//nl// &
            "    call k(n, a - h*ad, b - h*bd, sm)"//nl// &
            "    fd2 = (sp - sm)/(2.0d0*h)"//nl// &
            "    e1 = abs(fd1 - sd)"//nl// &
            "    e2 = abs(fd2 - sd)"//nl// &
            "    if (e2 > 1.0d-6*max(1.0d0, abs(sd)) + 1.0d-9) then"//nl// &
            "        print *, 'tangent mismatch: ad =', sd, ' fd =', fd2"//nl// &
            "        bad = .true."//nl// &
            "    end if"//nl// &
            "    if (e1 > 1.0d-10*max(1.0d0, abs(sd)) .and. e2 > 0.40d0*e1) then"//nl// &
            "        print *, 'no second-order convergence:', e1, e2"//nl// &
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

end program test_forward_array_oracle
