program fortad_cli
    !! Command-line driver.
    !!
    !!     fortad --indep x,y kernel.f90
    !!     fortad --indep a,b --directions nd -o kernel_d.f90 kernel.f90
    !!     fortad --mode reverse --indep x --no-primal kernel.f90
    !!
    !! Reads Fortran, writes Fortran. Nothing else is needed to use the result:
    !! compile the generated file with the rest of your project.
    use fortad, only: fad_jvp, fad_vjp, fad_hvp, fad_roundtrip, &
                      fad_result_t, fad_version
    implicit none

    character(len=:), allocatable :: input_path, output_path, indep_list
    character(len=:), allocatable :: directions, proc_name, source, mode
    character(len=:), allocatable :: module_name
    character(len=32), allocatable :: independents(:)
    type(fad_result_t) :: res
    logical :: roundtrip_only, with_primal
    integer :: unit, stat

    call parse_arguments(input_path, output_path, indep_list, directions, &
                         proc_name, mode, module_name, roundtrip_only, &
                         with_primal, stat)
    if (stat /= 0) then
        call usage()
        error stop 2
    end if

    call read_file(input_path, source, stat)
    if (stat /= 0) then
        write (error_unit_or_output(), '(a)') "fortad: cannot read "//input_path
        error stop 2
    end if

    if (roundtrip_only) then
        res = fad_roundtrip(source)
    else if (mode == "reverse") then
        independents = split_commas(indep_list)
        res = run_reverse(source, independents, proc_name, module_name, &
                          with_primal)
    else if (mode == "hessian") then
        independents = split_commas(indep_list)
        res = run_hessian(source, independents, proc_name, module_name)
    else
        independents = split_commas(indep_list)
        res = fad_jvp(source, independents, name=proc_name, &
                      module_name=module_name, n_directions=directions, &
                      with_primal=with_primal)
    end if

    if (.not. res%ok) then
        write (error_unit_or_output(), '(a)') "fortad: "//res%message
        error stop 1
    end if

    if (len(output_path) > 0) then
        open (newunit=unit, file=output_path, status="replace", action="write")
        write (unit, '(a)') res%code
        close (unit)
    else
        write (*, '(a)') res%code
    end if

contains

    function run_reverse(source, independents, proc_name, module_name, &
                         with_primal) result(res)
        !! Reverse mode. A blank name or module means "use the default".
        character(len=*), intent(in) :: source, independents(:)
        character(len=*), intent(in) :: proc_name, module_name
        logical, intent(in) :: with_primal
        type(fad_result_t) :: res

        res = fad_vjp(source, independents, name=proc_name, &
                      module_name=module_name, with_primal=with_primal)
    end function run_reverse

    function run_hessian(source, independents, proc_name, module_name) result(res)
        !! Forward-over-reverse Hessian-vector product.
        character(len=*), intent(in) :: source, independents(:)
        character(len=*), intent(in) :: proc_name, module_name
        type(fad_result_t) :: res

        res = fad_hvp(source, independents, name=proc_name, &
                      module_name=module_name)
    end function run_hessian

    subroutine parse_arguments(input_path, output_path, indep_list, directions, &
                               proc_name, mode, module_name, roundtrip_only, &
                         with_primal, stat)
        !! Parse the command line.
        character(len=:), allocatable, intent(out) :: input_path, output_path
        character(len=:), allocatable, intent(out) :: indep_list, directions
        character(len=:), allocatable, intent(out) :: proc_name, mode
        character(len=:), allocatable, intent(out) :: module_name
        logical, intent(out) :: roundtrip_only, with_primal
        integer, intent(out) :: stat
        character(len=1024) :: arg
        integer :: i, n, length

        input_path = ""
        output_path = ""
        indep_list = ""
        directions = ""
        proc_name = ""
        mode = "forward"
        module_name = ""
        roundtrip_only = .false.
        with_primal = .true.
        stat = 0

        n = command_argument_count()
        i = 1
        do while (i <= n)
            call get_command_argument(i, arg, length)
            select case (trim(arg(1:length)))
            case ("--indep", "-i")
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                indep_list = trim(arg(1:length))
            case ("--directions", "-d")
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                directions = trim(arg(1:length))
            case ("--name")
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                proc_name = trim(arg(1:length))
            case ("-o", "--output")
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                output_path = trim(arg(1:length))
            case ("-m", "--mode")
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                mode = trim(arg(1:length))
                select case (mode)
                case ("forward", "reverse", "hessian")
                    continue
                case default
                    stat = 1
                    return
                end select
            case ("--module")
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                module_name = trim(arg(1:length))
            case ("--no-primal")
                ! Emit the gradient alone. Everything the primal value kept
                ! alive is then dead and is removed.
                with_primal = .false.
            case ("--roundtrip")
                roundtrip_only = .true.
            case ("--version")
                write (*, '(a)') "fortad "//fad_version()
                stop
            case ("--help", "-h")
                call usage()
                stop
            case default
                if (length > 0) then
                    if (arg(1:1) == "-") then
                        stat = 1
                        return
                    end if
                    input_path = trim(arg(1:length))
                end if
            end select
            i = i + 1
        end do

        if (len(input_path) == 0) stat = 1
        if (.not. roundtrip_only .and. len(indep_list) == 0) stat = 1
    end subroutine parse_arguments

    subroutine usage()
        !! Print usage.
        write (*, '(a)') "fortad "//fad_version()// &
            " - source-transformation automatic differentiation for Fortran"
        write (*, '(a)') ""
        write (*, '(a)') "usage: fortad --indep <names> [options] <file.f90>"
        write (*, '(a)') ""
        write (*, '(a)') "  -i, --indep a,b       independent variables (required)"
        write (*, '(a)') "  -m, --mode MODE       forward (default), reverse, "// &
            "or hessian"
        write (*, '(a)') "      --module NAME     wrap the result in a module, "// &
            "for a checked interface"
        write (*, '(a)') "  -d, --directions nd   vector mode: name of the "// &
            "direction-count argument"
        write (*, '(a)') "      --name f_jvp      name of the generated procedure"
        write (*, '(a)') "  -o, --output path     write here instead of stdout"
        write (*, '(a)') "      --no-primal       return the derivative only, "// &
            "not the primal value"
        write (*, '(a)') "      --roundtrip       parse and re-emit, do not "// &
            "differentiate"
        write (*, '(a)') "      --version         print version and exit"
    end subroutine usage

    function split_commas(list) result(parts)
        !! Split a comma-separated list into fixed-width names.
        character(len=*), intent(in) :: list
        character(len=32), allocatable :: parts(:)
        integer :: i, n, start

        n = 1
        do i = 1, len(list)
            if (list(i:i) == ",") n = n + 1
        end do
        allocate (parts(n))
        parts = ""
        n = 0
        start = 1
        do i = 1, len(list) + 1
            if (i > len(list)) then
                n = n + 1
                parts(n) = adjustl(list(start:i - 1))
            else if (list(i:i) == ",") then
                n = n + 1
                parts(n) = adjustl(list(start:i - 1))
                start = i + 1
            end if
        end do
    end function split_commas

    subroutine read_file(path, text, stat)
        !! Read a whole file.
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: stat
        character(len=4096) :: buf
        integer :: unit, ios

        text = ""
        open (newunit=unit, file=path, status="old", action="read", iostat=stat)
        if (stat /= 0) return
        do
            read (unit, '(a)', iostat=ios) buf
            if (ios /= 0) exit
            text = text//trim(buf)//new_line('a')
        end do
        close (unit)
    end subroutine read_file

    integer function error_unit_or_output() result(unit)
        !! Standard error.
        use, intrinsic :: iso_fortran_env, only: error_unit

        unit = error_unit
    end function error_unit_or_output

end program fortad_cli
