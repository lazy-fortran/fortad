program test_cli_compact_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: executable_buffer, environment_buffer
    character(len=:), allocatable :: executable_path, directory, separator
    character(len=:), allocatable :: cli_path, input_path, legacy_path
    character(len=:), allocatable :: compact_path, source_first_path, command
    character(len=:), allocatable :: legacy_source, compact_source
    character(len=:), allocatable :: source_first_source
    character(len=7), parameter :: modes(3) = [character(len=7) :: &
        'forward', 'reverse', 'hessian']
    character(len=3), parameter :: products(3) = [character(len=3) :: &
        'jvp', 'vjp', 'hvp']
    integer :: path_length, separator_pos, stat, index
    logical :: exists

    call locate_cli(cli_path, directory, separator, exists)
    if (.not. exists) then
        write (error_unit, '(a)') &
            'SKIP: fortad CLI app is not built; set FORTAD_CLI to test it'
        stop 0
    end if

    input_path = directory // separator // 'fortad-compact-input.f90'
    call write_fixture(input_path)
    do index = 1, size(modes)
        legacy_path = directory // separator // 'fortad-legacy-' // &
            trim(modes(index)) // '.f90'
        compact_path = directory // separator // 'fortad-compact-' // &
            trim(modes(index)) // '.f90'
        source_first_path = directory // separator // 'fortad-source-first-' // &
            trim(modes(index)) // '.f90'
        command = quote(cli_path) // ' --mode ' // trim(modes(index)) // &
            ' --indep x --proc square --module compact_' // &
            trim(modes(index)) // ' --output ' // quote(legacy_path) // &
            ' ' // quote(input_path)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) call fail('legacy '//trim(modes(index))//' command failed')

        command = quote(cli_path) // ' ' // products(index) // &
            ' x --proc square --module compact_' // trim(modes(index)) // &
            ' --output ' // quote(compact_path) // ' ' // quote(input_path)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) call fail('compact '//trim(modes(index))//' command failed')

        command = quote(cli_path) // ' ' // products(index) // ' ' // &
            quote(input_path) // ' x --proc square --module compact_' // &
            trim(modes(index)) // ' --output ' // quote(source_first_path)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) call fail('source-first '//trim(modes(index))// &
            ' command failed')

        call read_text(legacy_path, legacy_source)
        call read_text(compact_path, compact_source)
        call read_text(source_first_path, source_first_source)
        if (legacy_source /= compact_source) then
            call fail(products(index)//' differs from legacy CLI')
        end if
        if (compact_source /= source_first_source) then
            call fail(products(index)//' differs from source-first CLI')
        end if
        call compile_source(compact_path, products(index), modes(index))
        call compile_source(source_first_path, products(index), modes(index))
        call delete_file(legacy_path)
        call delete_file(compact_path)
        call delete_file(source_first_path)
    end do

    call require_conflict('vjp x --mode forward', 'mode')
    call require_conflict('vjp x --indep x', 'independent names')
    call require_conflict('jvp x --roundtrip', 'roundtrip')
    call require_ambiguous_source_first()

    call delete_file(input_path)
    print *, 'pass cli_compact'

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

    subroutine write_fixture(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') 'module compact_fixture'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') 'subroutine square(x, y)'
        write (unit, '(a)') 'real, intent(in) :: x'
        write (unit, '(a)') 'real, intent(out) :: y'
        write (unit, '(a)') 'y = x*x'
        write (unit, '(a)') 'end subroutine square'
        write (unit, '(a)') 'end module compact_fixture'
        close (unit)
    end subroutine write_fixture

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

    subroutine compile_source(path, product, legacy_mode)
        character(len=*), intent(in) :: path, product, legacy_mode
        character(len=:), allocatable :: compiler, object_path, module_path
        integer :: compiler_length

        call get_environment_variable('FC', environment_buffer, &
            length=compiler_length)
        if (compiler_length > 0) then
            compiler = environment_buffer(:compiler_length)
        else
            compiler = 'gfortran'
        end if
        object_path = directory // separator // 'fortad-compact-' // &
            product // '.o'
        command = compiler // ' -c ' // quote(path) // ' -o ' // &
            quote(object_path)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) call fail(product//' generated source did not compile')
        call delete_file(object_path)
        module_path = 'compact_' // trim(legacy_mode) // '.mod'
        call delete_if_present(module_path)
    end subroutine compile_source

    subroutine require_conflict(arguments, description)
        character(len=*), intent(in) :: arguments, description

        compact_path = directory // separator // 'fortad-conflict.f90'
        command = quote(cli_path) // ' ' // arguments // ' --proc square' // &
            ' --output ' // quote(compact_path) // ' ' // quote(input_path)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat == 0) call fail('conflicting '//description//' succeeded')
        inquire (file=compact_path, exist=exists)
        if (exists) call fail('conflicting '//description//' wrote output')
    end subroutine require_conflict

    subroutine require_ambiguous_source_first()
        compact_path = directory // separator // 'fortad-conflict.f90'
        command = quote(cli_path) // ' jvp ' // quote(input_path) // ' ' // &
            quote(input_path) // ' --output ' // quote(compact_path)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat == 0) call fail('two source paths were accepted as names')
        inquire (file=compact_path, exist=exists)
        if (exists) call fail('ambiguous source-first command wrote output')
    end subroutine require_ambiguous_source_first

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

        quoted = '"' // text // '"'
    end function quote

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: ' // message
        error stop 1
    end subroutine fail

end program test_cli_compact_oracle
