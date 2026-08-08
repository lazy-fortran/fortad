program test_cli_legacy_reverse_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=:), allocatable :: cli, dir, source, generated, driver
    character(len=:), allocatable :: ambiguous_source, diagnostic, ambiguous_output
    character(len=:), allocatable :: executable, command, compiler
    character(len=:), allocatable :: diagnostic_text
    character(len=1024) :: buffer
    integer :: stat, unit, compiler_length
    logical :: exists

    cli = locate_cli()
    if (len_trim(cli) == 0) then
        write (error_unit, '(a)') 'SKIP: fortad CLI app is not built'
        stop 0
    end if

    dir = 'build/oracle/cli_legacy_reverse'
    source = dir//'/legacy_square.f90'
    generated = dir//'/legacy_square_vjp.f90'
    driver = dir//'/driver.f90'
    executable = dir//'/run'
    call execute_command_line('mkdir -p '//dir, exitstat=stat)
    if (stat /= 0) error stop 'could not create CLI oracle directory'

    open (newunit=unit, file=source, status='replace', action='write')
    write (unit, '(a)') 'subroutine square(x, y)'
    write (unit, '(a)') '  real :: x, y'
    write (unit, '(a)') '  y = x*x'
    write (unit, '(a)') 'end subroutine square'
    close (unit)

    open (newunit=unit, file=driver, status='replace', action='write')
    write (unit, '(a)') 'program legacy_reverse_driver'
    write (unit, '(a)') '  use legacy_square_vjp_mod, only: square_vjp'
    write (unit, '(a)') '  real :: x, y, y_b, x_b'
    write (unit, '(a)') '  x = 2.5; y_b = 1.0; x_b = 0.0'
    write (unit, '(a)') '  call square_vjp(x, y, y_b, x_b)'
    write (unit, '(a)') '  if (abs(y - 6.25) > 1.0e-5) error stop 1'
    write (unit, '(a)') '  if (abs(x_b - 5.0) > 1.0e-5) error stop 2'
    write (unit, '(a)') 'end program legacy_reverse_driver'
    close (unit)

    command = quote(cli)//' vjp '//quote(source)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'legacy vjp inference command failed'
    inquire (file=generated, exist=exists)
    if (.not. exists) error stop 'legacy vjp did not use the inferred output name'

    call get_environment_variable('FC', buffer, length=compiler_length)
    if (compiler_length > 0) then
        compiler = buffer(:compiler_length)
    else
        compiler = 'gfortran'
    end if
    command = compiler//' -std=f2018 -J '//quote(dir)//' -I '//quote(dir)// &
        ' -o '//quote(executable)//' '//quote(generated)//' '//quote(driver)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'inferred legacy VJP did not compile'
    call execute_command_line(quote(executable), wait=.true., exitstat=stat)
    if (stat /= 0) error stop 'inferred legacy VJP failed its hand oracle'

    ambiguous_source = dir//'/ambiguous.f90'
    diagnostic = dir//'/ambiguous.stderr'
    ambiguous_output = dir//'/ambiguous_vjp.f90'
    call write_ambiguous_fixture(ambiguous_source)
    command = quote(cli)//' vjp '//quote(ambiguous_source)//' 2> '//quote(diagnostic)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat == 0) error stop 'ambiguous reverse inference unexpectedly succeeded'
    inquire (file=ambiguous_output, exist=exists)
    if (exists) error stop 'ambiguous reverse inference wrote an output'
    call read_text(diagnostic, diagnostic_text)
    if (index(diagnostic_text, 'candidates: y,z') == 0) then
        error stop 'ambiguous reverse diagnostic omitted output candidates'
    end if
    if (index(diagnostic_text, '--dep NAME') == 0) then
        error stop 'ambiguous reverse diagnostic omitted --dep guidance'
    end if

    call delete_file(ambiguous_source)
    call delete_file(diagnostic)
    print *, 'PASS: CLI infers legacy reverse dependent and independents'

contains

    function locate_cli() result(path)
        character(len=:), allocatable :: path
        character(len=1024) :: value
        integer :: n
        logical :: found

        path = ''
        call get_environment_variable('FORTAD_CLI', value, length=n)
        if (n > 0) then
            inquire (file=value(:n), exist=found)
            if (found) then
                path = value(:n)
                return
            end if
        end if
        path = 'build/fo/bin/fortad'
        inquire (file=path, exist=found)
        if (.not. found) path = ''
    end function locate_cli

    function quote(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text

        text = '"'//trim(value)//'"'
    end function quote

    subroutine write_ambiguous_fixture(path)
        character(len=*), intent(in) :: path
        integer :: file_unit

        open (newunit=file_unit, file=path, status='replace', action='write')
        write (file_unit, '(a)') 'subroutine ambiguous(x, y, z)'
        write (file_unit, '(a)') '  real :: x, y, z'
        write (file_unit, '(a)') '  y = x*x'
        write (file_unit, '(a)') '  z = x+1.0'
        write (file_unit, '(a)') 'end subroutine ambiguous'
        close (file_unit)
    end subroutine write_ambiguous_fixture

    subroutine read_text(path, text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        character(len=4096) :: line
        integer :: file_unit, ios

        text = ''
        open (newunit=file_unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) error stop 'could not read CLI diagnostic'
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            text = text//trim(line)//new_line('a')
        end do
        close (file_unit)
    end subroutine read_text

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: file_unit
        logical :: present

        inquire (file=path, exist=present)
        if (.not. present) return
        open (newunit=file_unit, file=path, status='old')
        close (file_unit, status='delete')
    end subroutine delete_file

end program test_cli_legacy_reverse_oracle
