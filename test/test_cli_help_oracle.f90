program test_cli_help_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: executable_buffer, environment_buffer, line
    character(len=:), allocatable :: executable_path, directory, separator
    character(len=:), allocatable :: cli_path, output_path, command, help_text
    integer :: path_length, separator_pos, unit, stat, ios
    logical :: exists

    call get_environment_variable('FORTAD_CLI', environment_buffer, &
        length=path_length)
    if (path_length > 0) then
        cli_path = environment_buffer(:path_length)
        inquire (file=cli_path, exist=exists)
    else
        exists = .false.
    end if

    call get_command_argument(0, executable_buffer, length=path_length)
    executable_path = executable_buffer(:path_length)
    separator_pos = scan(executable_path, '/'//achar(92), back=.true.)
    if (separator_pos > 0) then
        directory = executable_path(:separator_pos - 1)
        separator = executable_path(separator_pos:separator_pos)
    else
        directory = '.'
        separator = '/'
    end if

    if (.not. exists) then
        cli_path = directory//separator//'fortad'
        inquire (file=cli_path, exist=exists)
        if (.not. exists) then
            cli_path = directory//separator//'..'//separator//'app'// &
                separator//'fortad'
            inquire (file=cli_path, exist=exists)
        end if
        if (.not. exists) then
            cli_path = cli_path//'.exe'
            inquire (file=cli_path, exist=exists)
        end if
    end if
    if (.not. exists) then
        write (error_unit, '(a)') &
            'SKIP: fortad CLI app is not built; set FORTAD_CLI to test it'
        stop 0
    end if

    output_path = directory//separator//'fortad-help-output.txt'
    command = '"'//cli_path//'" --help > "'//output_path//'" 2>&1'
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('fortad --help returned a failure status')

    open (newunit=unit, file=output_path, status='old', action='read', iostat=ios)
    if (ios /= 0) call fail('could not read fortad --help output')
    help_text = ''
    do
        read (unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        help_text = help_text//trim(line)//new_line('a')
    end do
    close (unit, status='delete')

    call require_text(help_text, '--proc NAME')
    call require_text(help_text, '--rule SPEC')
    call require_text(help_text, '--call-rule SPEC')
    print *, 'pass cli_help'

contains

    subroutine require_text(text, expected)
        character(len=*), intent(in) :: text, expected

        if (index(text, expected) == 0) then
            call fail('help output omitted '//expected)
        end if
    end subroutine require_text

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: '//message
        error stop 1
    end subroutine fail

end program test_cli_help_oracle
