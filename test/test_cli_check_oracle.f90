program test_cli_check_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: executable_buffer, environment_buffer
    character(len=:), allocatable :: executable_path, directory, separator
    character(len=:), allocatable :: cli_path, input_path, invalid_path
    character(len=:), allocatable :: legacy_path, check_path, object_path
    character(len=:), allocatable :: command, legacy_source, check_source
    integer :: path_length, separator_pos, stat
    logical :: exists

    call locate_cli(cli_path, directory, separator, exists)
    if (.not. exists) then
        write (error_unit, '(a)') &
            'SKIP: fortad CLI app is not built; set FORTAD_CLI to test it'
        stop 0
    end if

    input_path = directory // separator // 'fortad-check-input.f90'
    invalid_path = directory // separator // 'fortad-check-invalid.f90'
    legacy_path = directory // separator // 'fortad-check-legacy.f90'
    check_path = directory // separator // 'fortad-check-output.f90'
    object_path = directory // separator // 'fortad-check-output.o'
    call write_valid_fixture(input_path)
    call write_invalid_fixture(invalid_path)

    command = quote(cli_path) // ' --roundtrip --proc square --output ' // &
        quote(legacy_path) // ' ' // quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('legacy roundtrip command failed')

    command = quote(cli_path) // ' check --proc square --output ' // &
        quote(check_path) // ' ' // quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('compact check command failed')

    call read_text(legacy_path, legacy_source)
    call read_text(check_path, check_source)
    if (legacy_source /= check_source) then
        call fail('compact check differs from legacy roundtrip output')
    end if
    call compile_source(check_path, object_path)

    call delete_file(check_path)
    command = quote(cli_path) // ' check --output ' // quote(check_path) // &
        ' ' // quote(invalid_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat == 0) call fail('check accepted source with no procedure')
    inquire (file=check_path, exist=exists)
    if (exists) call fail('failed check wrote normalized output')

    command = quote(cli_path) // ' check --indep x --output ' // &
        quote(check_path) // ' ' // quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat == 0) call fail('check accepted a derivative-only option')
    inquire (file=check_path, exist=exists)
    if (exists) call fail('invalid check options wrote output')

    call delete_file(input_path)
    call delete_file(invalid_path)
    call delete_file(legacy_path)
    print *, 'pass cli_check'

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
        separator_pos = scan(executable_path, '/' // achar(92), back=.true.)
        if (separator_pos > 0) then
            run_directory = executable_path(:separator_pos - 1)
            path_separator = executable_path(separator_pos:separator_pos)
        else
            run_directory = '.'
            path_separator = '/'
        end if

        if (found) return
        path = run_directory // path_separator // 'fortad'
        inquire (file=path, exist=found)
        if (.not. found) then
            path = run_directory // path_separator // '..' // path_separator // &
                'app' // path_separator // 'fortad'
            inquire (file=path, exist=found)
        end if
        if (.not. found) then
            path = path // '.exe'
            inquire (file=path, exist=found)
        end if
    end subroutine locate_cli

    subroutine write_valid_fixture(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') 'module check_fixture'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') 'subroutine square(x, y)'
        write (unit, '(a)') 'real, intent(in) :: x'
        write (unit, '(a)') 'real, intent(out) :: y'
        write (unit, '(a)') 'y = x*x'
        write (unit, '(a)') 'end subroutine square'
        write (unit, '(a)') 'end module check_fixture'
        close (unit)
    end subroutine write_valid_fixture

    subroutine write_invalid_fixture(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') 'this is not Fortran'
        close (unit)
    end subroutine write_invalid_fixture

    subroutine read_text(path, text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        character(len=4096) :: line
        integer :: unit, ios

        text = ''
        open (newunit=unit, file=path, status='old', action='read')
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            text = text // trim(line) // new_line('a')
        end do
        close (unit)
    end subroutine read_text

    subroutine compile_source(path, output)
        character(len=*), intent(in) :: path, output
        character(len=:), allocatable :: compiler
        integer :: compiler_length

        call get_environment_variable('FC', environment_buffer, &
            length=compiler_length)
        if (compiler_length > 0) then
            compiler = environment_buffer(:compiler_length)
        else
            compiler = 'gfortran'
        end if
        command = compiler // ' -c ' // quote(path) // ' -o ' // quote(output)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) call fail('checked source did not compile')
        call delete_file(output)
    end subroutine compile_source

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='old')
        close (unit, status='delete')
    end subroutine delete_file

    function quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted

        quoted = '"' // text // '"'
    end function quote

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: ' // message
        error stop 1
    end subroutine fail

end program test_cli_check_oracle
