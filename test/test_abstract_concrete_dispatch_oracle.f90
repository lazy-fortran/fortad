program test_abstract_concrete_dispatch_oracle
    !! The abstract intermediate supplies a concrete binding, but only the
    !! concrete leaf is a valid runtime dispatch arm.
    use fortad, only: fad_jvp, fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: source = &
        "module abstract_concrete_dispatch_case"//nl// &
        "    implicit none"//nl// &
        "    type, abstract :: base_t"//nl// &
        "    contains"//nl// &
        "        procedure :: value => base_value"//nl// &
        "    end type base_t"//nl// &
        "    type, extends(base_t), abstract :: middle_t"//nl// &
        "    contains"//nl// &
        "        procedure :: value => middle_value"//nl// &
        "    end type middle_t"//nl// &
        "    type, extends(middle_t) :: leaf_t"//nl// &
        "    end type leaf_t"//nl// &
        "contains"//nl// &
        "    pure function base_value(self, x) result(y)"//nl// &
        "        class(base_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = x"//nl// &
        "    end function base_value"//nl// &
        "    pure function middle_value(self, x) result(y)"//nl// &
        "        class(middle_t), intent(in) :: self"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = 2.0d0*x"//nl// &
        "    end function middle_value"//nl// &
        "    pure function top(model, x) result(y)"//nl// &
        "        class(base_t), intent(in) :: model"//nl// &
        "        real(8), intent(in) :: x"//nl// &
        "        real(8) :: y"//nl// &
        "        y = model%value(x)"//nl// &
        "    end function top"//nl// &
        "end module abstract_concrete_dispatch_case"//nl

    type(fad_result_t) :: jvp, vjp
    character(len=:), allocatable :: dir, driver
    integer :: stat, unit

    jvp = fad_jvp(source, ["x"], from="top", name="top_jvp")
    vjp = fad_vjp(source, ["x"], dependent="y", from="top", name="top_vjp")
    call require_ok(jvp, 'JVP')
    call require_ok(vjp, 'VJP')

    dir = 'build/oracle/abstract_concrete_dispatch'
    call execute_command_line('mkdir -p '//dir, exitstat=stat)
    if (stat /= 0) error stop 'could not create oracle directory'
    open (newunit=unit, file=dir//'/primal.f90', status='replace', &
        action='write')
    write (unit, '(a)') source
    close (unit)
    open (newunit=unit, file=dir//'/derivatives.f90', status='replace', &
        action='write')
    write (unit, '(a)') 'module abstract_concrete_dispatch_derivatives'
    write (unit, '(a)') '    use abstract_concrete_dispatch_case, only: base_t, middle_t, leaf_t'
    write (unit, '(a)') 'contains'
    write (unit, '(a)') jvp%code
    write (unit, '(a)') vjp%code
    write (unit, '(a)') 'end module abstract_concrete_dispatch_derivatives'
    close (unit)

    driver = &
        'program driver'//nl// &
        '    use abstract_concrete_dispatch_case, only: leaf_t, top'//nl// &
        '    use abstract_concrete_dispatch_derivatives, only: top_jvp, top_vjp'//nl// &
        '    implicit none'//nl// &
        '    type(leaf_t) :: model'//nl// &
        '    real(8) :: x, x_d, y, y_d, x_b, y_b, h, fp, fm'//nl// &
        '    model = leaf_t()'//nl// &
        '    x = 1.5d0'//nl// &
        '    x_d = -0.75d0'//nl// &
        '    y_b = 1.3d0'//nl// &
        '    call top_jvp(model, x, x_d, y, y_d)'//nl// &
        '    if (abs(y - 3.0d0) > 1.0d-13) error stop 2'//nl// &
        '    if (abs(y_d - 2.0d0*x_d) > 1.0d-13) error stop 3'//nl// &
        '    call top_vjp(model, x, y, y_b, x_b)'//nl// &
        '    if (abs(x_b - 2.0d0*y_b) > 1.0d-13) error stop 4'//nl// &
        '    h = 1.0d-6'//nl// &
        '    fp = top(model, x + h)'//nl// &
        '    fm = top(model, x - h)'//nl// &
        '    if (abs((fp - fm)/(2.0d0*h) - 2.0d0) > 1.0d-7) error stop 5'//nl// &
        '    if (abs(y_b*y_d - x_b*x_d) > 1.0d-13) error stop 6'//nl// &
        '    print *, ''abstract concrete dispatch oracle pass'''//nl// &
        'end program driver'//nl
    open (newunit=unit, file=dir//'/driver.f90', status='replace', &
        action='write')
    write (unit, '(a)') driver
    close (unit)

    call execute_command_line( &
        'gfortran -std=f2018 -O2 -J'//dir//' -I'//dir//' -o '// &
        dir//'/run '//dir//'/primal.f90 '//dir//'/derivatives.f90 '// &
        dir//'/driver.f90 > '//dir//'/build.log 2>&1', exitstat=stat)
    if (stat /= 0) then
        print *, 'FAIL generated abstract concrete dispatch did not compile'
        call show_file(dir//'/build.log')
        error stop 1
    end if
    call execute_command_line('./'//dir//'/run > '//dir//'/out.txt 2>&1', &
        exitstat=stat)
    if (stat /= 0) then
        print *, 'FAIL abstract concrete dispatch behavioral oracle'
        call show_file(dir//'/out.txt')
        error stop 1
    end if
    print *, 'test_abstract_concrete_dispatch_oracle: all cases passed'

contains

    subroutine require_ok(result, label)
        type(fad_result_t), intent(in) :: result
        character(len=*), intent(in) :: label
        if (.not. result%ok) then
            print *, 'FAIL ', trim(label), ': ', result%message
            error stop 1
        end if
    end subroutine require_ok

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: ios, file_unit
        open (newunit=file_unit, file=path, status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, '    ', trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_abstract_concrete_dispatch_oracle
