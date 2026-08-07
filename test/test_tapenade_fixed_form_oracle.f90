program test_tapenade_fixed_form_oracle
    !! Production-CLI oracle for a Tapenade-style fixed-form kernel.
    !!
    !! The checked-in test writes a `.f` input, asks the real CLI to transform
    !! it, compiles the generated JVP with an independent compiler, and checks
    !! both the hand derivative and a central finite difference.
    implicit none

    character(len=1024) :: executable_buffer, environment_buffer
    character(len=:), allocatable :: executable_path, bin_dir, separator
    character(len=:), allocatable :: cli_path, dir, command
    integer :: length, separator_pos, unit, stat
    logical :: exists

    call get_environment_variable("FORTAD_CLI", environment_buffer, &
        length=length)
    if (length > 0) then
        cli_path = environment_buffer(:length)
        inquire (file=cli_path, exist=exists)
    else
        exists = .false.
    end if

    call get_command_argument(0, executable_buffer, length=length)
    executable_path = executable_buffer(:length)
    separator_pos = scan(executable_path, "/"//achar(92), back=.true.)
    if (separator_pos > 0) then
        bin_dir = executable_path(:separator_pos - 1)
        separator = executable_path(separator_pos:separator_pos)
    else
        bin_dir = "."
        separator = "/"
    end if
    if (.not. exists) then
        cli_path = bin_dir//separator//"fortad"
        inquire (file=cli_path, exist=exists)
        if (.not. exists) then
            cli_path = bin_dir//separator//".."//separator//"app"// &
                separator//"fortad"
            inquire (file=cli_path, exist=exists)
        end if
        if (.not. exists) then
            cli_path = cli_path//".exe"
            inquire (file=cli_path, exist=exists)
        end if
    end if
    if (.not. exists) then
        print *, "SKIP: fortad CLI app is not built; set FORTAD_CLI to test it"
        stop 0
    end if

    dir = "build/oracle_tapenade_fixed_form"
    call execute_command_line("mkdir -p "//dir, exitstat=stat)
    if (stat /= 0) error stop "could not create fixed-form oracle directory"

    open (newunit=unit, file=dir//"/scale.f", status="replace", action="write")
    write (unit, '(a)') "C Tapenade-style fixed-form kernel"
    write (unit, '(a)') "      subroutine scale(x,y)"
    write (unit, '(a)') "      double precision x,y"
    write (unit, '(a)') "      y=x*x"
    write (unit, '(a)') "      end"
    close (unit)

    command = '"'//cli_path//'" --indep x --proc scale '// &
        "--module generated_fixed_form --name scale_jvp -o "// &
        dir//"/generated.f90 "//dir//"/scale.f > "//dir//"/transform.log 2>&1"
    call execute_command_line(command, wait=.true., exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL fixed-form CLI transformation"
        call show_file(dir//"/transform.log")
        error stop 2
    end if
    call require_file_omits(dir//"/generated.f90", &
        "pure subroutine scale_jvp")

    open (newunit=unit, file=dir//"/driver.f90", status="replace", &
        action="write")
    write (unit, '(a)') "program driver"
    write (unit, '(a)') "    use generated_fixed_form, only: scale_jvp"
    write (unit, '(a)') "    implicit none"
    write (unit, '(a)') "    real(8) :: x, xd, y, yd, h, fd"
    write (unit, '(a)') "    x = 1.75d0"
    write (unit, '(a)') "    xd = -0.4d0"
    write (unit, '(a)') "    h = 1.0d-6"
    write (unit, '(a)') "    call scale_jvp(x, xd, y, yd)"
    write (unit, '(a)') "    if (abs(y-x*x) > 1.0d-13) error stop 1"
    write (unit, '(a)') &
        "    if (abs(yd-2.0d0*x*xd) > 1.0d-13) error stop 2"
    write (unit, '(a)') &
        "    fd = (((x+h*xd)**2)-((x-h*xd)**2))/(2.0d0*h)"
    write (unit, '(a)') "    if (abs(yd-fd) > 1.0d-8) error stop 3"
    write (unit, '(a)') "end program driver"
    close (unit)

    call execute_command_line("gfortran -std=f2018 -O2 -J"//dir//" -I"// &
        dir//" -o "//dir//"/run "//dir//"/generated.f90 "// &
        dir//"/driver.f90 > "//dir//"/build.log 2>&1", exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL fixed-form generated JVP did not compile"
        call show_file(dir//"/build.log")
        error stop 3
    end if
    call execute_command_line("./"//dir//"/run > "//dir//"/out.txt 2>&1", &
        exitstat=stat)
    if (stat /= 0) then
        print *, "FAIL fixed-form hand/finite-difference oracle"
        call show_file(dir//"/out.txt")
        error stop 4
    end if

    print *, "test_tapenade_fixed_form_oracle: all cases passed"

contains

    subroutine require_file_omits(path, forbidden)
        character(len=*), intent(in) :: path, forbidden
        character(len=512) :: line
        integer :: file_unit, ios

        open (newunit=file_unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) error stop "could not inspect generated source"
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(lower_ascii(line), forbidden) > 0) then
                close (file_unit)
                print *, "FAIL generated ordinary legacy routine is PURE"
                error stop 5
            end if
        end do
        close (file_unit)
    end subroutine require_file_omits

    function lower_ascii(text) result(lower)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower
        integer :: i, code

        lower = text
        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lower(i:i) = achar(code + iachar('a') - iachar('A'))
            end if
        end do
    end function lower_ascii

    subroutine show_file(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: file_unit, ios

        open (newunit=file_unit, file=path, status="old", action="read", &
            iostat=ios)
        if (ios /= 0) return
        do
            read (file_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print *, "    ", trim(line)
        end do
        close (file_unit)
    end subroutine show_file

end program test_tapenade_fixed_form_oracle
