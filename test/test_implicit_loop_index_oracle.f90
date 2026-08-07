program test_implicit_loop_index_oracle
    !! Behavioural oracle for legacy code whose loop index is implicit.
    !!
    !! Tapenade's fixed-form corpus contains many routines that rely on
    !! Fortran's I-N implicit integer rule.  Generated FortAD procedures use
    !! implicit none, so the loop index must be synthesized as an integer
    !! declaration before the generated source is compiled.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=:), allocatable :: source, dir
    type(fad_result_t) :: jvp, vjp
    integer :: stat, unit

    source = &
        "subroutine k(n, a, b, s)"//nl// &
        "    integer, intent(in) :: n"//nl// &
        "    real(8), intent(in) :: a(n), b(n)"//nl// &
        "    real(8), intent(out) :: s"//nl// &
        "    s = 0.0d0"//nl// &
        "    do i = 1, n"//nl// &
        "        s = s + a(i)*b(i)"//nl// &
        "    end do"//nl// &
        "end subroutine k"//nl

    jvp = fad_jvp(source, [character(len=1) :: "a", "b"], name="k_jvp")
    if (.not. jvp%ok) error stop "implicit-index JVP generation failed"
    vjp = fad_vjp(source, [character(len=1) :: "a", "b"], name="k_vjp")
    if (.not. vjp%ok) error stop "implicit-index VJP generation failed"

    dir = "build/oracle_implicit_loop_index"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create implicit-index oracle directory"

    open (newunit=unit, file=dir//"/primal.f90", status="replace", action="write")
    write (unit, '(a)') "module primal_mod"
    ! Keep the primal's legacy implicit integer rule so this is an exact
    ! source-form test; the generated derivative itself uses implicit none.
    write (unit, '(a)') "    implicit integer (i-n)"
    write (unit, '(a)') "contains"
    write (unit, '(a)') source
    write (unit, '(a)') "end module primal_mod"
    close (unit)

    open (newunit=unit, file=dir//"/derivs.f90", status="replace", action="write")
    write (unit, '(a)') "module derivs_mod"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "contains"
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') "end module derivs_mod"
    close (unit)

    open (newunit=unit, file=dir//"/driver.f90", status="replace", action="write")
    write (unit, '(a)') driver_text()
    close (unit)

    call execute_command_line( &
        "cd "//dir//" && gfortran -std=f2018 -pedantic-errors -O2 -o run "// &
        "primal.f90 derivs.f90 driver.f90 > build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//"/build.log")
        error stop "implicit-index generated source did not compile"
    end if

    call execute_command_line("cd "//dir//" && ./run", exitstat=stat)
    if (stat /= 0) error stop "implicit-index independent oracle failed"
    print '(a)', "test_implicit_loop_index_oracle: all cases passed"

contains

    function driver_text() result(text)
        character(len=:), allocatable :: text

        text = &
            "program driver"//nl// &
            "    use primal_mod, only: k"//nl// &
            "    use derivs_mod, only: k_jvp, k_vjp"//nl// &
            "    implicit none"//nl// &
            "    real(8) :: a(4), b(4), ad(4), bd(4), ab(4), bb(4)"//nl// &
            "    real(8) :: s, sd, sb, expected, lhs, rhs"//nl// &
            "    integer :: i"//nl// &
            "    a = [1.0d0, 1.5d0, 2.0d0, 2.5d0]"//nl// &
            "    b = [0.5d0, -1.0d0, 2.0d0, 3.0d0]"//nl// &
            "    ad = [0.2d0, -0.3d0, 0.4d0, -0.5d0]"//nl// &
            "    bd = [-0.6d0, 0.7d0, -0.8d0, 0.9d0]"//nl// &
            "    call k_jvp(4, a, ad, b, bd, s, sd)"//nl// &
            "    expected = sum(a*b)"//nl// &
            "    if (abs(s - expected) > 1.0d-12) error stop 1"//nl// &
            "    expected = sum(ad*b + a*bd)"//nl// &
            "    if (abs(sd - expected) > 1.0d-12) error stop 2"//nl// &
            "    sb = 0.73d0"//nl// &
            "    call k_vjp(4, a, b, s, sb, ab, bb)"//nl// &
            "    if (maxval(abs(ab - sb*b)) > 1.0d-12) error stop 3"//nl// &
            "    if (maxval(abs(bb - sb*a)) > 1.0d-12) error stop 4"//nl// &
            "    lhs = sb*sd"//nl// &
            "    rhs = sum(ab*ad) + sum(bb*bd)"//nl// &
            "    if (abs(lhs - rhs) > 1.0d-12) error stop 5"//nl// &
            "    do i = 1, 4"//nl// &
            "        if (abs(ab(i) - sb*b(i)) > 1.0d-12) error stop 6"//nl// &
            "    end do"//nl// &
            "end program driver"//nl
    end function driver_text

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: io, ios

        open (newunit=io, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (io, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print '(a)', trim(line)
        end do
        close (io)
    end subroutine show_file

end program test_implicit_loop_index_oracle
