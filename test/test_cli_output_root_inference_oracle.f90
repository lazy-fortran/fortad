program test_cli_output_root_inference_oracle
    !! The output basename may select a multi-procedure source without --proc.
    !! The oracle compiles the generated reverse product and checks the
    !! selected routine against its analytic derivative and finite difference.
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=1024) :: buffer
    character(len=:), allocatable :: cli, directory, input_path, generated
    character(len=:), allocatable :: driver, executable, command, compiler
    integer :: compiler_length, stat, unit
    logical :: exists

    call locate_cli(cli)
    if (len_trim(cli) == 0) then
        write (error_unit, '(a)') 'SKIP: fortad CLI app is not built'
        stop 0
    end if
    inquire (file=cli, exist=exists)
    if (.not. exists) error stop 'FORTAD_CLI does not name an executable'

    directory = 'build/oracle/cli_output_root'
    input_path = directory//'/cli_output_root_input.f90'
    generated = directory//'/selected_vjp.f90'
    driver = directory//'/driver.f90'
    executable = directory//'/run'
    call execute_command_line('mkdir -p '//quote(directory), wait=.true., &
        exitstat=stat)
    if (stat /= 0) error stop 'could not create CLI oracle directory'

    open (newunit=unit, file=input_path, status='replace', action='write')
    write (unit, '(a)') 'subroutine first(x, y)'
    write (unit, '(a)') '  real(8), intent(in) :: x'
    write (unit, '(a)') '  real(8), intent(out) :: y'
    write (unit, '(a)') '  y = x + 1.0d0'
    write (unit, '(a)') 'end subroutine first'
    write (unit, '(a)') 'subroutine selected(x, y)'
    write (unit, '(a)') '  real(8), intent(in) :: x'
    write (unit, '(a)') '  real(8), intent(out) :: y'
    write (unit, '(a)') '  y = x*x'
    write (unit, '(a)') 'end subroutine selected'
    close (unit)

    command = quote(cli)//' vjp '//quote(input_path)//' --output '// &
        quote(generated)
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) call fail('output-basename root inference command failed')
    inquire (file=generated, exist=exists)
    if (.not. exists) call fail('output-basename inference did not write output')

    open (newunit=unit, file=driver, status='replace', action='write')
    write (unit, '(a)') 'program output_root_driver'
    write (unit, '(a)') &
        '  use cli_output_root_input_vjp_mod, only: selected_vjp'
    write (unit, '(a)') '  real(8) :: x, y, yb, xb, h, fd'
    write (unit, '(a)') '  x = 2.5d0; yb = 1.0d0; xb = 0.0d0'
    write (unit, '(a)') '  call selected_vjp(x, y, yb, xb)'
    write (unit, '(a)') '  h = 1.0d-6'
    write (unit, '(a)') &
        '  fd = (((x+h)**2)-((x-h)**2))/(2.0d0*h)'
    write (unit, '(a)') '  if (abs(y - 6.25d0) > 1.0d-12) error stop 1'
    write (unit, '(a)') '  if (abs(xb - 5.0d0) > 1.0d-12) error stop 2'
    write (unit, '(a)') '  if (abs(xb - fd) > 1.0d-8) error stop 3'
    write (unit, '(a)') 'end program output_root_driver'
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
    if (stat /= 0) call fail('selected output-root derivative did not compile')
    call execute_command_line(quote(executable), wait=.true., exitstat=stat)
    if (stat /= 0) call fail('selected output-root derivative failed its oracle')

    call cleanup()
    print *, 'pass cli_output_root_inference'

contains

    subroutine locate_cli(path)
        character(len=:), allocatable, intent(out) :: path
        integer :: value_length, separator_pos

        path = ''
        call get_environment_variable('FORTAD_CLI', buffer, &
            length=value_length)
        if (value_length > 0) then
            path = buffer(:value_length)
            return
        end if
        call get_command_argument(0, buffer, length=value_length)
        if (value_length == 0) return
        separator_pos = scan(buffer(:value_length), '/'//achar(92), back=.true.)
        if (separator_pos > 0) then
            path = buffer(:separator_pos)//'fortad'
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
        call delete_file(directory//'/cli_output_root_input_vjp_mod.mod')
    end subroutine cleanup

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: file_unit

        inquire (file=path, exist=exists)
        if (.not. exists) return
        open (newunit=file_unit, file=path, status='old')
        close (file_unit, status='delete')
    end subroutine delete_file

end program test_cli_output_root_inference_oracle
