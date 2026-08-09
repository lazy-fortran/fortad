program test_component_dependent_vjp_oracle
    !! Independent VJP oracle for a concrete REAL array component dependent.
    !! The seed is a component-shaped dummy; the containing shadow is used
    !! only for the requested independent component adjoints.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortad, only: fad_vjp, fad_result_t
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: dir = 'build/oracle/component_dependent_vjp'
    character(len=*), parameter :: source_path = dir// &
        '/component_dependent_case.f90'
    character(len=*), parameter :: explicit_path = dir// &
        '/component_dependent_explicit.f90'
    character(len=*), parameter :: explicit_driver = dir// &
        '/explicit_driver.f90'
    character(len=*), parameter :: explicit_exe = dir//'/explicit_run'
    character(len=*), parameter :: cli_path = dir// &
        '/component_dependent_case_b.f90'
    character(len=*), parameter :: cli_driver = dir//'/cli_driver.f90'
    character(len=*), parameter :: cli_exe = dir//'/cli_run'
    character(len=64) :: independent_paths(4)
    character(len=:), allocatable :: cli, command
    character(len=:), allocatable :: source
    type(fad_result_t) :: vjp, bad
    integer :: unit, stat

    source = &
        'module data_types'//nl// &
        '    implicit none'//nl// &
        '    integer, parameter :: mcell = 4'//nl// &
        '    type :: griddata'//nl// &
        '        real(8) :: x(mcell), y(mcell)'//nl// &
        '    end type griddata'//nl// &
        '    type :: solutiondata'//nl// &
        '        real(8) :: a(mcell), b(mcell), c(mcell)'//nl// &
        '    end type solutiondata'//nl// &
        'end module data_types'//nl// &
        'subroutine function(grddat, soldat)'//nl// &
        '    use data_types'//nl// &
        '    implicit none'//nl// &
        '    type(griddata) :: grddat'//nl// &
        '    type(solutiondata) :: soldat(2)'//nl// &
        '    soldat(1)%a = soldat(2)%b * grddat%x + soldat(2)%c + '// &
        'grddat%y'//nl// &
        'end subroutine function'//nl
    independent_paths = [character(len=64) :: 'grddat%x', 'grddat%y', &
        'soldat(2)%b', 'soldat(2)%c']

    call execute_command_line('mkdir -p '//dir, exitstat=stat)
    if (stat /= 0) error stop 'could not create component-dependent oracle directory'
    open (newunit=unit, file=source_path, status='replace', action='write')
    write (unit, '(a)') source
    close (unit)

    vjp = fad_vjp(source, independent_paths, dependent='soldat(1)%a', &
        from='function', name='function_vjp', &
        module_name='component_dependent_derivatives')
    if (.not. vjp%ok) then
        write (error_unit, '(a)') 'FAIL: component-dependent VJP refused: '// &
            trim(vjp%message)
        error stop 1
    end if
    if (index(vjp%code, 'fad_dep_soldat_1__a_b') == 0) then
        error stop 'component-dependent VJP did not expose a shaped seed'
    end if
    if (count_substring(vjp%code, 'soldat_b)') /= 1) then
        error stop 'component-dependent VJP emitted duplicate soldat_b dummies'
    end if
    bad = fad_vjp(source, [character(len=64) :: 'soldat(2)%b'], &
        dependent='soldat', from='function')
    if (bad%ok .or. .not. allocated(bad%message) .or. &
        index(bad%message, 'must name a concrete REAL component') == 0) then
        error stop 'whole-object component dependent was accepted'
    end if

    open (newunit=unit, file=explicit_path, status='replace', action='write')
    write (unit, '(a)') vjp%code
    close (unit)
    call write_driver(explicit_driver, 'component_dependent_derivatives')
    command = 'gfortran -std=f2018 -Wall -Wextra -O2 -o '//quote(explicit_exe)// &
        ' '//quote(source_path)//' '//quote(explicit_path)//' '// &
        quote(explicit_driver)//' > '//quote(dir//'/explicit_build.log')// &
        ' 2>&1'
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//'/explicit_build.log')
        error stop 'explicit component-dependent VJP did not compile'
    end if
    call execute_command_line(quote(explicit_exe), wait=.true., exitstat=stat)
    if (stat /= 0) then
        call show_file(dir//'/explicit_out.txt')
        error stop 'explicit component-dependent VJP failed the adjoint oracle'
    end if

    cli = locate_cli()
    if (len_trim(cli) > 0) then
        command = quote(cli)//' -b -root function -O '//quote(dir)// &
            ' -o component_dependent_case '//quote(source_path)// &
            ' > '//quote(dir//'/cli.log')//' 2>&1'
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) then
            call show_file(dir//'/cli.log')
            error stop 'automatic component-dependent VJP generation failed'
        end if
        call write_driver(cli_driver, 'component_dependent_case_vjp_mod')
        command = 'gfortran -std=f2018 -Wall -Wextra -O2 -o '//quote(cli_exe)// &
            ' '//quote(source_path)//' '//quote(cli_path)//' '// &
            quote(cli_driver)//' > '//quote(dir//'/cli_build.log')//' 2>&1'
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) then
            call show_file(dir//'/cli_build.log')
            error stop 'automatic component-dependent VJP did not compile'
        end if
        call execute_command_line(quote(cli_exe), wait=.true., exitstat=stat)
        if (stat /= 0) then
            call show_file(dir//'/cli_out.txt')
            error stop 'automatic component-dependent VJP failed the adjoint oracle'
        end if
    else
        write (error_unit, '(a)') &
            'NOTE: automatic CLI half skipped because fortad is not built'
    end if
    print *, 'PASS: concrete REAL component-dependent explicit and automatic VJP'

contains

    subroutine write_driver(path, module_name)
        character(len=*), intent(in) :: path, module_name
        integer :: u

        open (newunit=u, file=path, status='replace', action='write')
        write (u, '(a)') 'module component_dependent_driver'
        write (u, '(a)') '    use data_types'
        write (u, '(a)') '    use '//trim(module_name)//', only: function_vjp'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    subroutine run_case()'
        write (u, '(a)') '        type(griddata) :: g, gd, gb'
        write (u, '(a)') '        type(solutiondata) :: s(2), sd(2), sb(2)'
        write (u, '(a)') '        real(8) :: seed(4), h, lhs, rhs'
        write (u, '(a)') '        real(8) :: fp(4), fm(4), expected(4)'
        write (u, '(a)') '        type(griddata) :: gp, gm'
        write (u, '(a)') '        type(solutiondata) :: sp(2), sm(2)'
        write (u, '(a)') '        integer :: i'
        write (u, '(a)') '        g%x = [4d0, 5d0, 6d0, 7d0]'
        write (u, '(a)') '        g%y = [0.5d0, 1d0, 1.5d0, 2d0]'
        write (u, '(a)') '        s(1)%a = 0d0'
        write (u, '(a)') '        s(2)%b = [2d0, 3d0, 4d0, 5d0]'
        write (u, '(a)') '        s(2)%c = [1d0, 1.5d0, 2d0, 2.5d0]'
        write (u, '(a)') '        gd%x = [0.1d0, -0.2d0, 0.3d0, -0.4d0]'
        write (u, '(a)') '        gd%y = [0.05d0, 0.1d0, -0.15d0, 0.2d0]'
        write (u, '(a)') '        sd(2)%b = [0.2d0, 0.1d0, -0.1d0, 0.3d0]'
        write (u, '(a)') '        sd(2)%c = [0.4d0, -0.3d0, 0.2d0, -0.2d0]'
        write (u, '(a)') '        seed = [1d0, 2d0, 3d0, 4d0]'
        write (u, '(a)') '        gb%x = -99d0; gb%y = -99d0'
        write (u, '(a)') '        sb(2)%b = -99d0; sb(2)%c = -99d0'
        write (u, '(a)') '        call function_vjp(g, s, seed, gb, sb)'
        write (u, '(a)') '        if (maxval(abs(s(1)%a - [9.5d0, 17.5d0, '// &
            '27.5d0, 39.5d0])) > 1d-12) error stop 2'
        write (u, '(a)') '        if (maxval(abs(gb%x - [2d0, 6d0, 12d0, 20d0])) > 1d-12) error stop 3'
        write (u, '(a)') '        if (maxval(abs(gb%y - seed)) > 1d-12) error stop 4'
        write (u, '(a)') '        if (maxval(abs(sb(2)%b - [4d0, 10d0, 18d0, 28d0])) > 1d-12) error stop 5'
        write (u, '(a)') '        if (maxval(abs(sb(2)%c - seed)) > 1d-12) error stop 6'
        write (u, '(a)') '        h = 1d-6'
        write (u, '(a)') '        gp = g; gp%x = g%x + h*gd%x; gp%y = g%y + h*gd%y'
        write (u, '(a)') '        sp = s; sp(2)%b = s(2)%b + h*sd(2)%b; sp(2)%c = s(2)%c + h*sd(2)%c'
        write (u, '(a)') '        call function(gp, sp); fp = sp(1)%a'
        write (u, '(a)') '        gm = g; gm%x = g%x - h*gd%x; gm%y = g%y - h*gd%y'
        write (u, '(a)') '        sm = s; sm(2)%b = s(2)%b - h*sd(2)%b; sm(2)%c = s(2)%c - h*sd(2)%c'
        write (u, '(a)') '        call function(gm, sm); fm = sm(1)%a'
        write (u, '(a)') '        expected = (fp - fm)/(2d0*h)'
        write (u, '(a)') '        lhs = sum(seed*expected)'
        write (u, '(a)') '        rhs = sum(gb%x*gd%x + gb%y*gd%y + '// &
            'sb(2)%b*sd(2)%b + sb(2)%c*sd(2)%c)'
        write (u, '(a)') '        if (abs(lhs-rhs) > 1d-7) error stop 7'
        write (u, '(a)') '    end subroutine run_case'
        write (u, '(a)') 'end module component_dependent_driver'
        write (u, '(a)') 'program driver'
        write (u, '(a)') '    use component_dependent_driver, only: run_case'
        write (u, '(a)') '    interface'
        write (u, '(a)') '        subroutine function(grddat, soldat)'
        write (u, '(a)') '            use data_types'
        write (u, '(a)') '            type(griddata) :: grddat'
        write (u, '(a)') '            type(solutiondata) :: soldat(2)'
        write (u, '(a)') '        end subroutine function'
        write (u, '(a)') '    end interface'
        write (u, '(a)') '    call run_case()'
        write (u, '(a)') 'end program driver'
        close (u)
    end subroutine write_driver

    function locate_cli() result(path)
        character(len=:), allocatable :: path
        character(len=1024) :: buffer
        integer :: length
        logical :: exists

        path = ''
        call get_environment_variable('FORTAD_CLI', buffer, length=length)
        if (length > 0) then
            inquire (file=buffer(:length), exist=exists)
            if (exists) then
                path = buffer(:length)
                return
            end if
        end if
        path = 'build/fo/bin/fortad'
        inquire (file=path, exist=exists)
        if (.not. exists) path = ''
    end function locate_cli

    function quote(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text

        text = '"'//trim(value)//'"'
    end function quote

    integer function count_substring(text, needle) result(n)
        character(len=*), intent(in) :: text, needle
        integer :: at, from

        n = 0
        from = 1
        do
            at = index(text(from:), needle)
            if (at == 0) exit
            n = n + 1
            from = from + at + len_trim(needle) - 1
            if (from > len(text)) exit
        end do
    end function count_substring

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: u, ios

        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            write (error_unit, '(a)') trim(line)
        end do
        close (u)
    end subroutine show_file

end program test_component_dependent_vjp_oracle
