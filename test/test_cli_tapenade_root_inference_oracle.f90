program test_cli_tapenade_root_inference_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: buffer
    character(len=:), allocatable :: cli, directory, input_path, generated
    character(len=:), allocatable :: driver, executable, command, compiler
    character(len=:), allocatable :: module_name
    integer :: length, separator, stat, compiler_length, unit
    logical :: exists

    call get_environment_variable('FORTAD_CLI', buffer, length=length)
    if (length == 0) then
        write (error_unit, '(a)') 'SKIP: fortad CLI app is not built'
        stop 0
    end if
    cli = buffer(:length)
    inquire (file=cli, exist=exists)
    if (.not. exists) error stop 'FORTAD_CLI does not name an executable'

    separator = scan(cli, '/'//achar(92), back=.true.)
    if (separator > 1) then
        directory = cli(:separator - 1)
    else
        directory = '.'
    end if
    input_path = directory//'/fortad-tapenade-root-input.f90'
    generated = directory//'/selected_b.f90'
    driver = directory//'/fortad-tapenade-root-driver.f90'
    executable = directory//'/fortad-tapenade-root-driver'
    module_name = 'fortad_tapenade_root_input_vjp_mod'

    open (newunit=unit, file=input_path, status='replace', action='write')
    write (unit, '(a)') 'subroutine first(x, y)'
    write (unit, '(a)') '  real :: x, y'
    write (unit, '(a)') '  y = x + 1.0'
    write (unit, '(a)') 'end subroutine first'
    write (unit, '(a)') 'subroutine selected(x, y)'
    write (unit, '(a)') '  real :: x, y'
    write (unit, '(a)') '  y = x*x'
    write (unit, '(a)') 'end subroutine selected'
    close (unit)

    command = quote(cli)//' -b -O '//quote(directory)//' -o selected '// &
        quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'Tapenade root inference command failed'
    inquire (file=generated, exist=exists)
    if (.not. exists) error stop 'inferred root did not use the output stem'

    open (newunit=unit, file=driver, status='replace', action='write')
    write (unit, '(a)') 'program root_driver'
    write (unit, '(a)') '  use '//module_name//', only: selected_vjp'
    write (unit, '(a)') '  real :: x, y, yb, xb'
    write (unit, '(a)') '  x = 2.5; yb = 1.0; xb = 0.0'
    write (unit, '(a)') '  call selected_vjp(x, y, yb, xb)'
    write (unit, '(a)') '  if (abs(y - 6.25) > 1.0e-5) error stop 1'
    write (unit, '(a)') '  if (abs(xb - 5.0) > 1.0e-5) error stop 2'
    write (unit, '(a)') 'end program root_driver'
    close (unit)

    call get_environment_variable('FC', buffer, length=compiler_length)
    if (compiler_length > 0) then
        compiler = buffer(:compiler_length)
    else
        compiler = 'gfortran'
    end if
    command = compiler//' -std=f2018 -J '//quote(directory)//' -I '// &
        quote(directory)//' -o '//quote(executable)//' '//quote(generated)// &
        ' '//quote(driver)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'inferred root VJP did not compile'
    call execute_command_line(quote(executable), wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'inferred root VJP failed its hand oracle'

    call delete_file(input_path)
    call delete_file(generated)
    call delete_file(driver)
    call delete_file(executable)
    call delete_file(directory//'/'//module_name//'.mod')
    print *, 'pass cli_tapenade_root_inference'

contains

    function quote(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text

        text = '"'//trim(value)//'"'
    end function quote

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: file_unit

        inquire (file=path, exist=exists)
        if (.not. exists) return
        open (newunit=file_unit, file=path, status='old')
        close (file_unit, status='delete')
    end subroutine delete_file

end program test_cli_tapenade_root_inference_oracle
