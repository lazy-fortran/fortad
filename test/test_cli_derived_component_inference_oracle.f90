program test_cli_derived_component_inference_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=*), parameter :: dir = 'build/oracle/cli_derived_components'
    character(len=*), parameter :: source_path = dir//'/program.f90'
    character(len=*), parameter :: output_path = dir//'/probe_d.f90'
    character(len=*), parameter :: driver_path = dir//'/driver.f90'
    character(len=*), parameter :: executable_path = dir//'/run'
    character(len=:), allocatable :: cli, command
    integer :: stat

    cli = locate_cli()
    if (len_trim(cli) == 0) then
        write (error_unit, '(A)') 'SKIP: fortad CLI app is not built'
        stop 0
    end if

    call execute_command_line('mkdir -p '//dir, exitstat=stat)
    if (stat /= 0) error stop 'could not create CLI oracle directory'
    call write_source()
    call write_driver()

    command = quote(cli)//' -d -root function -O '//quote(dir)// &
        ' -o probe '//quote(source_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'automatic derived-component inference failed'

    command = 'gfortran -std=f2018 -O2 -o '//quote(executable_path)//' '// &
        quote(source_path)//' '//quote(output_path)//' '//quote(driver_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) then
        write (error_unit, '(A)') &
            'FAIL: inferred derived-component JVP did not compile'
        error stop 1
    end if
    call execute_command_line(quote(executable_path), wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'inferred derived-component JVP failed oracle'
    print *, 'PASS: CLI infers real derived-component independents'

contains

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

    subroutine write_source()
        integer :: unit

        open (newunit=unit, file=source_path, status='replace', action='write')
        write (unit, '(A)') 'module data_types'
        write (unit, '(A)') '  implicit none'
        write (unit, '(A)') '  integer, parameter :: mcell = 4'
        write (unit, '(A)') '  type :: griddata'
        write (unit, '(A)') '    real(8) :: x(mcell)'
        write (unit, '(A)') '    real(8) :: y(mcell)'
        write (unit, '(A)') '  end type griddata'
        write (unit, '(A)') '  type :: solutiondata'
        write (unit, '(A)') '    real(8) :: a(mcell)'
        write (unit, '(A)') '    real(8) :: b(mcell)'
        write (unit, '(A)') '    real(8) :: c(mcell)'
        write (unit, '(A)') '  end type solutiondata'
        write (unit, '(A)') 'end module data_types'
        write (unit, '(A)') 'subroutine function(grddat, soldat)'
        write (unit, '(A)') '  use data_types'
        write (unit, '(A)') '  implicit none'
        write (unit, '(A)') '  type(griddata) :: grddat'
        write (unit, '(A)') '  type(solutiondata) :: soldat(2)'
        write (unit, '(A)') &
            '  soldat(1)%a = soldat(2)%b * grddat%x + soldat(2)%c + grddat%y'
        write (unit, '(A)') 'end subroutine function'
        close (unit)
    end subroutine write_source

    subroutine write_driver()
        integer :: unit

        open (newunit=unit, file=driver_path, status='replace', action='write')
        write (unit, '(A)') 'program driver'
        write (unit, '(A)') '  use data_types'
        write (unit, '(A)') '  use program_jvp_mod, only: function_jvp'
        write (unit, '(A)') '  type(griddata) :: g, gd'
        write (unit, '(A)') '  type(solutiondata) :: s(2), sd(2)'
        write (unit, '(A)') '  g%x = 4.0d0; g%y = 5.0d0'
        write (unit, '(A)') '  s(2)%b = 2.0d0; s(2)%c = 3.0d0'
        write (unit, '(A)') '  gd%x = 0.2d0; gd%y = 0.3d0'
        write (unit, '(A)') '  sd(2)%b = 0.5d0; sd(2)%c = 0.7d0'
        write (unit, '(A)') '  call function_jvp(g, gd, s, sd)'
        write (unit, '(A)') &
            '  if (abs(s(1)%a(1) - 16.0d0) > 1.0d-12) error stop 1'
        write (unit, '(A)') &
            '  if (abs(sd(1)%a(1) - 3.4d0) > 1.0d-12) error stop 2'
        write (unit, '(A)') 'end program driver'
        close (unit)
    end subroutine write_driver

    function quote(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text

        text = '"'//trim(value)//'"'
    end function quote

end program test_cli_derived_component_inference_oracle
