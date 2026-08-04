program test_element_target_oracle
    !! Reverse mode over array-element writes outside a loop.
    !!
    !! `point(1) = ...` names a storage location rather than a variable, so
    !! there is nothing to give an SSA version to. Reverse mode refused this
    !! shape outright until fortfem's toroidal coordinate map - three element
    !! writes in straight-line code - made it the only thing standing between
    !! fortad and that operator.
    !!
    !! The oracle is central differences of the untouched kernel, one seed at a
    !! time, so each element's row of the Jacobian is checked separately. A
    !! scatter that went to the wrong element would still satisfy a single
    !! contracted check.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortad, only: fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir
    type(fad_result_t) :: vjp
    integer :: failures, stat, unit

    failures = 0
    source = "subroutine k(a, b, point)"//nl// &
             "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
             "    implicit none"//nl// &
             "    real(dp), intent(in) :: a, b"//nl// &
             "    real(dp), intent(out) :: point(3)"//nl// &
             "    real(dp) :: t"//nl// &
             "    t = sinh(a)"//nl// &
             "    point(1) = t*cos(b)"//nl// &
             "    point(2) = t*sin(b)"//nl// &
             "    point(3) = a*a + b"//nl// &
             "end subroutine k"//nl

    vjp = fad_vjp(source, ["a", "b"], dependent="point", name="k_vjp", &
                  with_primal=.false.)
    if (.not. vjp%ok) then
        print *, "FAIL element_target: generation failed: ", vjp%message
        error stop 1
    end if
    print *, "pass element_target_generation"

    dir = "build/oracle_element_target"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    open (newunit=unit, file=dir//"/gen.f90", status="replace", action="write")
    write (unit, '(a)') "module gen"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module gen"
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line("cd "//dir//" && gfortran -O2 -o run gen.f90 "// &
                              "driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL element_target: generated code did not compile"
        call show_file(dir//"/build.log")
        failures = failures + 1
    else
        call execute_command_line("cd "//dir//" && ./run > out.txt 2>&1", &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, "FAIL element_target: oracle mismatch"
            call show_file(dir//"/out.txt")
            failures = failures + 1
        else
            print *, "pass element_target_against_finite_differences"
        end if
    end if

    if (failures == 0) then
        print *, "test_element_target_oracle: all cases passed"
    else
        print *, "test_element_target_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "    use gen, only: k, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(dp) :: a, b, seed(3), g(2), plus(3), minus(3), h"//nl// &
            "    integer :: row"//nl// &
            "    logical :: bad"//nl// &
            "    bad = .false."//nl// &
            "    a = 0.63d0"//nl// &
            "    b = 1.17d0"//nl// &
            "    h = 1.0d-6"//nl// &
            ! One seed at a time: each row of the Jacobian separately, so a
            ! scatter landing on the wrong element cannot hide in a sum.
            "    do row = 1, 3"//nl// &
            "        seed = 0.0d0"//nl// &
            "        seed(row) = 1.0d0"//nl// &
            "        call k_vjp(a, b, seed, g(1), g(2))"//nl// &
            "        call k(a + h, b, plus)"//nl// &
            "        call k(a - h, b, minus)"//nl// &
            "        if (abs(g(1) - (plus(row) - minus(row))/(2*h)) > &"//nl// &
            "            1.0d-6*max(1.0d0, abs(g(1)))) then"//nl// &
            "            print *, 'row', row, 'd/da', g(1), &"//nl// &
            "                (plus(row) - minus(row))/(2*h)"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "        call k(a, b + h, plus)"//nl// &
            "        call k(a, b - h, minus)"//nl// &
            "        if (abs(g(2) - (plus(row) - minus(row))/(2*h)) > &"//nl// &
            "            1.0d-6*max(1.0d0, abs(g(2)))) then"//nl// &
            "            print *, 'row', row, 'd/db', g(2), &"//nl// &
            "                (plus(row) - minus(row))/(2*h)"//nl// &
            "            bad = .true."//nl// &
            "        end if"//nl// &
            "    end do"//nl// &
            "    if (bad) error stop 1"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        integer :: ios
        character(len=512) :: buf

        open (unit=12, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (12, '(a)', iostat=ios) buf
            if (ios /= 0) exit
            print *, "    ", trim(buf)
        end do
        close (12)
    end subroutine show_file

end program test_element_target_oracle
