program test_cli_tapenade_vector_oracle
    !! Independent compiled oracle for Tapenade's `-vector` spelling.
    !!
    !! The command intentionally omits `-root`, `-O`, and `--directions`:
    !! `-o` supplies a safe output stem, selects the matching procedure, and
    !! `-vector` supplies the stable `nd` direction-count dummy.
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: buffer
    character(len=:), allocatable :: cli, directory, input_path, generated
    character(len=:), allocatable :: driver, executable, command, compiler
    integer :: length, stat, compiler_length, unit, separator
    logical :: exists

    call locate_cli(cli)
    if (len_trim(cli) == 0) then
        write (error_unit, '(a)') 'SKIP: fortad CLI app is not built'
        stop 0
    end if
    inquire (file=cli, exist=exists)
    if (.not. exists) error stop 'FORTAD_CLI does not name an executable'

    directory = 'build/oracle/cli_tapenade_vector'
    input_path = directory//'/cli_vector_input.f90'
    generated = directory//'/square_d.f90'
    driver = directory//'/driver.f90'
    executable = directory//'/run'

    call execute_command_line('mkdir -p '//directory, wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'could not create CLI oracle directory'

    open (newunit=unit, file=input_path, status='replace', action='write')
    write (unit, '(a)') 'subroutine square(x, y)'
    write (unit, '(a)') '  real(8), intent(in) :: x'
    write (unit, '(a)') '  real(8), intent(out) :: y'
    write (unit, '(a)') '  y = x*x'
    write (unit, '(a)') 'end subroutine square'
    close (unit)

    command = quote(cli)//' -d -vector -o '//quote(directory//'/square')// &
        ' '//quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('Tapenade -vector command failed')
    inquire (file=generated, exist=exists)
    if (.not. exists) call fail('Tapenade -vector output was not written')

    open (newunit=unit, file=driver, status='replace', action='write')
    write (unit, '(a)') 'program vector_cli_oracle'
    write (unit, '(a)') '  use cli_vector_input_jvp_mod, only: square_jvp'
    write (unit, '(a)') '  real(8) :: x, y, xd(2), yd(2)'
    write (unit, '(a)') '  x = 1.75d0; y = 0.0d0'
    write (unit, '(a)') '  xd = [-0.4d0, 0.6d0]; yd = 0.0d0'
    write (unit, '(a)') '  call square_jvp(2, x, xd, y, yd)'
    write (unit, '(a)') '  if (abs(y - x*x) > 1.0d-12) error stop 1'
    write (unit, '(a)') '  if (abs(yd(1) - 2.0d0*x*xd(1)) > 1.0d-12) error stop 2'
    write (unit, '(a)') '  if (abs(yd(2) - 2.0d0*x*xd(2)) > 1.0d-12) error stop 3'
    write (unit, '(a)') 'end program vector_cli_oracle'
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
    if (stat /= 0) call fail('Tapenade -vector output did not compile')
    call execute_command_line(quote(executable), wait=.true., exitstat=stat)
    if (stat /= 0) call fail('Tapenade -vector output failed its hand oracle')

    call cleanup()
    print *, 'pass cli_tapenade_vector'

contains

    subroutine locate_cli(path)
        character(len=:), allocatable, intent(out) :: path
        character(len=1024) :: value
        integer :: value_length

        call get_environment_variable('FORTAD_CLI', value, length=value_length)
        if (value_length > 0) then
            path = value(:value_length)
            return
        end if
        call get_command_argument(0, value, length=value_length)
        if (value_length == 0) then
            path = ''
            return
        end if
        separator = scan(value(:value_length), '/'//achar(92), back=.true.)
        if (separator > 0) then
            path = value(:separator)//'fortad'
        else
            path = 'fortad'
        end if
    end subroutine locate_cli

    function quote(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text

        text = '"'//trim(value)//'"'
    end function quote

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: '//trim(message)
        call cleanup()
        error stop 1
    end subroutine fail

    subroutine cleanup()
        call delete_file(input_path)
        call delete_file(generated)
        call delete_file(driver)
        call delete_file(executable)
        call delete_file(directory//'/cli_vector_input_jvp_mod.mod')
    end subroutine cleanup

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: file_unit

        inquire (file=path, exist=exists)
        if (.not. exists) return
        open (newunit=file_unit, file=path, status='old')
        close (file_unit, status='delete')
    end subroutine delete_file

end program test_cli_tapenade_vector_oracle
