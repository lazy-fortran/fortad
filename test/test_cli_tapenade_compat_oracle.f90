program test_cli_tapenade_compat_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: environment_buffer
    character(len=:), allocatable :: cli, directory, input_path
    character(len=:), allocatable :: parser_path, forward_path, reverse_path
    character(len=:), allocatable :: vector_path, command, compiler
    character(len=:), allocatable :: oracle_path, oracle_executable
    character(len=:), allocatable :: text
    integer :: length, separator, stat, compiler_length
    logical :: exists

    call get_environment_variable('FORTAD_CLI', environment_buffer, length=length)
    if (length == 0) then
        write (*, '(a)') 'SKIP: fortad CLI app is not built; set FORTAD_CLI to test it'
        stop 0
    end if
    cli = environment_buffer(:length)
    inquire (file=cli, exist=exists)
    if (.not. exists) then
        write (*, '(a)') 'SKIP: FORTAD_CLI does not name an executable'
        stop 0
    end if

    separator = scan(cli, '/' // achar(92), back=.true.)
    if (separator > 1) then
        directory = cli(:separator - 1)
    else
        directory = '.'
    end if
    input_path = directory // '/fortad-tapenade-compat-input.f'
    parser_path = directory // '/fortad-tapenade-compat_p.f90'
    forward_path = directory // '/fortad-tapenade-compat_d.f90'
    reverse_path = directory // '/fortad-tapenade-compat_b.f90'
    vector_path = directory // '/fortad-tapenade-compat-vector_d.f90'
    call write_fixture(input_path)

    command = quote(cli)//' -p -root square -O '//quote(directory)// &
        ' -o fortad-tapenade-compat '//quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('Tapenade parser flags failed')
    call require_file(parser_path, 'Tapenade parser output')

    command = quote(cli)//' -d -root square -O '//quote(directory)// &
        ' -o fortad-tapenade-compat '//quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('Tapenade forward flags failed')
    call require_file(forward_path, 'Tapenade forward output')

    command = quote(cli)//' -b -root square -O '//quote(directory)// &
        ' -o fortad-tapenade-compat '//quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('Tapenade reverse flags failed')
    call require_file(reverse_path, 'Tapenade reverse output')

    command = quote(cli)//' -d -root square -multi -O '//quote(directory)// &
        ' -o fortad-tapenade-compat-vector '//quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('Tapenade multidirectional flags failed')
    call require_file(vector_path, 'Tapenade multidirectional output')

    command = quote(cli) // ' -b -root square -multi -O ' // quote(directory) // &
        ' -o fortad-tapenade-compat-reverse-vector ' // quote(input_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat == 0) call fail('Tapenade reverse multidirectional mode was accepted')

    call get_environment_variable('FC', environment_buffer, length=compiler_length)
    if (compiler_length > 0) then
        compiler = environment_buffer(:compiler_length)
    else
        compiler = 'gfortran'
    end if
    call compile_generated(parser_path, 'parser')
    call compile_generated(forward_path, 'forward')
    call compile_generated(reverse_path, 'reverse')
    call compile_generated(vector_path, 'vector')

    oracle_path = directory // '/fortad-tapenade-compat-oracle.f90'
    oracle_executable = directory // '/fortad-tapenade-compat-oracle'
    call write_oracle(oracle_path)
    command = compiler // ' -std=f2018 -J' // quote(directory) // ' -I' // &
        quote(directory) // ' -o ' // quote(oracle_executable) // ' ' // &
        quote(forward_path) // ' ' // quote(reverse_path) // ' ' // &
        quote(oracle_path)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('generated Tapenade aliases did not link')
    call execute_command_line(quote(oracle_executable), wait=.true., exitstat=stat)
    if (stat /= 0) call fail('generated Tapenade aliases failed the hand oracle')

    call read_text(forward_path, text)
    if (index(text, 'subroutine square_jvp') == 0) then
        call fail('Tapenade forward alias selected the wrong procedure')
    end if
    call read_text(reverse_path, text)
    if (index(text, 'subroutine square_vjp') == 0) then
        call fail('Tapenade reverse alias selected the wrong procedure')
    end if

    call delete_file(input_path)
    call delete_file(parser_path)
    call delete_file(forward_path)
    call delete_file(reverse_path)
    call delete_file(vector_path)
    call delete_file(oracle_path)
    call delete_file(oracle_executable)
    call delete_modules()
    print *, 'pass cli_tapenade_compat'

contains

    subroutine write_fixture(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') '      subroutine square(x,y)'
        write (unit, '(a)') '      double precision x,y'
        write (unit, '(a)') '      y=x*x'
        write (unit, '(a)') '      return'
        write (unit, '(a)') '      end'
        close (unit)
    end subroutine write_fixture

    subroutine compile_generated(path, label)
        character(len=*), intent(in) :: path, label
        character(len=:), allocatable :: object_path

        object_path = directory//'/fortad-tapenade-compat-'//trim(label)//'.o'
        command = compiler//' -std=f2018 -J'//quote(directory)//' -c '// &
            quote(path)//' -o '//quote(object_path)
        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) call fail(trim(label)//' generated source did not compile')
        call delete_file(object_path)
    end subroutine compile_generated

    subroutine require_file(path, label)
        character(len=*), intent(in) :: path, label

        inquire (file=path, exist=exists)
        if (.not. exists) call fail(trim(label)//' was not written')
    end subroutine require_file

    subroutine read_text(path, value)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: value
        character(len=4096) :: line
        integer :: unit, ios

        value = ''
        open (newunit=unit, file=path, status='old', action='read')
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            value = value//trim(line)//new_line('a')
        end do
        close (unit)
    end subroutine read_text

    function quote(value) result(out)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: out

        out = '"'//trim(value)//'"'
    end function quote

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: unit

        inquire (file=path, exist=exists)
        if (.not. exists) return
        open (newunit=unit, file=path, status='old')
        close (unit, status='delete')
    end subroutine delete_file

    subroutine delete_if_present(name)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path

        path = directory//'/'//name
        call delete_file(path)
    end subroutine delete_if_present

    subroutine delete_modules()
        call delete_if_present('fortad_tapenade_compat_input_p_mod.mod')
        call delete_if_present('fortad_tapenade_compat_input_d_mod.mod')
        call delete_if_present('fortad_tapenade_compat_input_b_mod.mod')
        call delete_if_present('fortad_tapenade_compat_input_jvp_mod.mod')
        call delete_if_present('fortad_tapenade_compat_input_vjp_mod.mod')
    end subroutine delete_modules

    subroutine write_oracle(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(a)') 'program tapenade_compat_oracle'
        write (unit, '(a)') '    use fortad_tapenade_compat_input_jvp_mod, only: square_jvp'
        write (unit, '(a)') '    use fortad_tapenade_compat_input_vjp_mod, only: square_vjp'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real(8) :: x, xd, y, yd, yb, xb, h, fd'
        write (unit, '(a)') &
            '    x = 1.75d0; xd = -0.4d0; y = 0.0d0; yd = 0.0d0'
        write (unit, '(a)') '    call square_jvp(x, xd, y, yd)'
        write (unit, '(a)') '    if (abs(y-x*x) > 1.0d-12) error stop 1'
        write (unit, '(a)') &
            '    if (abs(yd-2.0d0*x*xd) > 1.0d-12) error stop 2'
        write (unit, '(a)') '    h = 1.0d-6'
        write (unit, '(a)') &
            '    fd = (((x+h*xd)**2)-((x-h*xd)**2))/(2.0d0*h)'
        write (unit, '(a)') '    if (abs(yd-fd) > 1.0d-8) error stop 3'
        write (unit, '(a)') '    y = 0.0d0; yb = 1.0d0; xb = 0.0d0'
        write (unit, '(a)') '    call square_vjp(x, y, yb, xb)'
        write (unit, '(a)') '    if (abs(y-x*x) > 1.0d-12) error stop 4'
        write (unit, '(a)') &
            '    if (abs(xb-2.0d0*x) > 1.0d-12) error stop 5'
        write (unit, '(a)') 'end program tapenade_compat_oracle'
        close (unit)
    end subroutine write_oracle

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: '//trim(message)
        call delete_file(input_path)
        call delete_file(parser_path)
        call delete_file(forward_path)
        call delete_file(reverse_path)
        call delete_file(vector_path)
        call delete_file(oracle_path)
        call delete_file(oracle_executable)
        call delete_modules()
        error stop 1
    end subroutine fail

end program test_cli_tapenade_compat_oracle
