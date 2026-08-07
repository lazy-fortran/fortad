program test_cli_all_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: executable_buffer, environment_buffer
    character(len=:), allocatable :: executable_path, directory, separator
    character(len=:), allocatable :: cli_path, input_path, jvp_path, vjp_path
    character(len=:), allocatable :: driver_path, executable, command
    integer :: path_length, separator_pos, stat, compiler_length
    logical :: found

    call locate_cli(cli_path, directory, separator, found)
    if (.not. found) then
        write (error_unit, '(a)') &
            'SKIP: fortad CLI app is not built; set FORTAD_CLI to test it'
        stop 0
    end if

    input_path = directory//separator//'fortad_all_input.f90'
    jvp_path = directory//separator//'fortad_all_input_jvp.f90'
    vjp_path = directory//separator//'fortad_all_input_vjp.f90'
    driver_path = directory//separator//'fortad_all_driver.f90'
    executable = directory//separator//'fortad_all_driver'
    call write_fixture(input_path)

    command = quote(cli_path)//' all '//quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('fortad all command failed')
    call require_file(jvp_path, 'all JVP output')
    call require_file(vjp_path, 'all VJP output')

    call write_driver(driver_path)
    call get_environment_variable('FC', environment_buffer, length=compiler_length)
    if (compiler_length > 0) then
        command = environment_buffer(:compiler_length)
    else
        command = 'gfortran'
    end if
    command = command//' -J '//quote(directory)//' -I '//quote(directory)// &
        ' -o '//quote(executable)//' '//quote(jvp_path)//' '//quote(vjp_path)// &
        ' '//quote(driver_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('all generated modules did not compile')
    call execute_command_line(quote(executable), wait=.true., exitstat=stat)
    if (stat /= 0) call fail('all generated products failed the hand oracle')

    call delete_file(input_path)
    call delete_file(jvp_path)
    call delete_file(vjp_path)
    call delete_file(driver_path)
    call delete_file(executable)
    call delete_if_present(directory//separator//'fortad_all_input_jvp_mod.mod')
    call delete_if_present(directory//separator//'fortad_all_input_vjp_mod.mod')
    print *, 'pass cli_all'

contains

    subroutine locate_cli(path, run_directory, path_separator, found)
        character(len=:), allocatable, intent(out) :: path, run_directory
        character(len=:), allocatable, intent(out) :: path_separator
        logical, intent(out) :: found

        call get_environment_variable('FORTAD_CLI', environment_buffer, &
            length=path_length)
        if (path_length > 0) then
            path = environment_buffer(:path_length)
            inquire (file=path, exist=found)
        else
            found = .false.
        end if

        call get_command_argument(0, executable_buffer, length=path_length)
        executable_path = executable_buffer(:path_length)
        separator_pos = scan(executable_path, '/'//achar(92), back=.true.)
        if (separator_pos > 0) then
            run_directory = executable_path(:separator_pos - 1)
            path_separator = executable_path(separator_pos:separator_pos)
        else
            run_directory = '.'
            path_separator = '/'
        end if

        if (found) return
        path = run_directory//path_separator//'fortad'
        inquire (file=path, exist=found)
        if (.not. found) then
            path = run_directory//path_separator//'..'//path_separator//'app'// &
                path_separator//'fortad'
            inquire (file=path, exist=found)
        end if
    end subroutine locate_cli

    subroutine write_fixture(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') 'module fortad_all_fixture'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') 'subroutine square(x, y)'
        write (unit, '(a)') 'real, intent(in) :: x'
        write (unit, '(a)') 'real, intent(out) :: y'
        write (unit, '(a)') 'y = x*x'
        write (unit, '(a)') 'end subroutine square'
        write (unit, '(a)') 'end module fortad_all_fixture'
        close (unit)
    end subroutine write_fixture

    subroutine write_driver(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') 'program all_driver'
        write (unit, '(a)') '    use fortad_all_input_jvp_mod, only: square_jvp'
        write (unit, '(a)') '    use fortad_all_input_vjp_mod, only: square_vjp'
        write (unit, '(a)') '    real :: x, x_d, y, y_d, y_b, x_b'
        write (unit, '(a)') '    x = 3.0; x_d = 1.0; y_b = 1.0'
        write (unit, '(a)') '    call square_jvp(x, x_d, y, y_d)'
        write (unit, '(a)') &
            '    if (abs(y - 9.0) > 1.0e-5 .or. abs(y_d - 6.0) > 1.0e-5) error stop 1'
        write (unit, '(a)') '    call square_vjp(x, y, y_b, x_b)'
        write (unit, '(a)') &
            '    if (abs(y - 9.0) > 1.0e-5 .or. abs(x_b - 6.0) > 1.0e-5) error stop 2'
        write (unit, '(a)') 'end program all_driver'
        close (unit)
    end subroutine write_driver

    subroutine require_file(path, description)
        character(len=*), intent(in) :: path, description
        logical :: present

        inquire (file=path, exist=present)
        if (.not. present) call fail(description//' was not written')
    end subroutine require_file

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='old')
        close (unit, status='delete')
    end subroutine delete_file

    subroutine delete_if_present(path)
        character(len=*), intent(in) :: path
        logical :: present

        inquire (file=path, exist=present)
        if (present) call delete_file(path)
    end subroutine delete_if_present

    function quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted

        quoted = '"'//text//'"'
    end function quote

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: '//message
        error stop 1
    end subroutine fail

end program test_cli_all_oracle
