program fortad_cli
    !! Command-line driver.
    !!
    !!     fortad jvp kernel.f90
    !!     fortad jvp a,b --directions nd -o kernel_d.f90 kernel.f90
    !!     fortad vjp x --no-primal kernel.f90
    !!     fortad check kernel.f90
    !!
    !! Reads Fortran, writes Fortran. Nothing else is needed to use the result:
    !! compile the generated file with the rest of your project.
    use fortad, only: fad_add_rule
    use fortad, only: fad_jvp, fad_vjp, fad_hvp, fad_roundtrip, &
        fad_result_t, fad_version
    use fortad_ir, only: fad_proc_t, FAD_INTENT_OUT
    use fortad_lower, only: lower_source, lower_status_t
    use fortfront, only: is_fixed_form_file, normalize_fixed_form_source_text
    implicit none

    character(len=:), allocatable :: input_path, output_path, indep_list, dep_name
    character(len=:), allocatable :: from_name
    character(len=:), allocatable :: directions, proc_name, source, mode
    character(len=:), allocatable :: module_name
    character(len=:), allocatable :: inference_message
    character(len=32), allocatable :: independents(:)
    type(fad_result_t) :: res
    logical :: roundtrip_only, with_primal, verbose
    integer :: unit, stat

    call parse_arguments(input_path, output_path, indep_list, directions, &
        proc_name, mode, module_name, roundtrip_only, &
        with_primal, dep_name, from_name, verbose, stat)
    if (stat /= 0) then
        call usage()
        error stop 2
    end if

    call read_file(input_path, source, stat)
    if (stat /= 0) then
        write (error_unit_or_output(), '(a)') "fortad: cannot read "//input_path
        error stop 2
    end if
    if (is_fixed_form_file(input_path)) then
        call normalize_fixed_form_source_text(source)
    end if

    if (len_trim(indep_list) == 0 .and. .not. roundtrip_only) then
        call infer_cli_defaults(source, input_path, mode, from_name, proc_name, &
            output_path, module_name, indep_list, inference_message, verbose, stat)
        if (stat /= 0) then
            write (error_unit_or_output(), '(a)') "fortad: "//inference_message
            error stop 2
        end if
    end if

    if (roundtrip_only) then
        res = fad_roundtrip(source, from=from_name)
    else if (mode == "reverse") then
        independents = split_commas(indep_list)
        res = run_reverse(source, independents, proc_name, module_name, &
            with_primal, dep_name, from_name)
    else if (mode == "hessian") then
        independents = split_commas(indep_list)
        res = run_hessian(source, independents, proc_name, module_name, from_name)
    else
        independents = split_commas(indep_list)
        res = fad_jvp(source, independents, name=proc_name, from=from_name, &
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

    subroutine register_call_rule(spec, stat)
        !! Register one `NAME:n_args:tangents|adjoints` call rule.
        use, intrinsic :: iso_fortran_env, only: error_unit
        use fortad, only: fad_add_call_rule
        character(len=*), intent(in) :: spec
        integer, intent(out) :: stat
        integer, parameter :: MAX_LINES = 16
        character(len=256) :: tangent(MAX_LINES), adjoint(MAX_LINES)
        integer :: c1, c2, bar, n_args, n_t, n_a, ios

        stat = 0
        c1 = index(spec, ":")
        if (c1 > 0) then
            c2 = index(spec(c1 + 1:), ":")
        else
            c2 = 0
        end if
        bar = index(spec, "|")
        if (c1 <= 1 .or. c2 == 0 .or. bar == 0) then
            write (error_unit, '(a)') "fortad: a call rule reads "// &
                "NAME:n_args:tangent;...|adjoint;..., got: "//trim(spec)
            stat = 1
            return
        end if
        c2 = c1 + c2
        read (spec(c1 + 1:c2 - 1), *, iostat=ios) n_args
        if (ios /= 0) then
            write (error_unit, '(a)') "fortad: a call rule needs an argument "// &
                "count, got: "//trim(spec(c1 + 1:c2 - 1))
            stat = 1
            return
        end if

        call split_lines(spec(c2 + 1:bar - 1), tangent, n_t, MAX_LINES)
        call split_lines(spec(bar + 1:), adjoint, n_a, MAX_LINES)
        call fad_add_call_rule(trim(spec(:c1 - 1)), n_args, tangent(:n_t), &
            adjoint(:n_a), stat)
        if (stat /= 0) write (error_unit, '(a)') &
            "fortad: could not register the call rule for "//trim(spec(:c1 - 1))
    end subroutine register_call_rule

    subroutine split_lines(text, lines, n_lines, cap)
        !! Split on ";" into statement templates.
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: lines(:)
        integer, intent(out) :: n_lines
        integer, intent(in) :: cap
        integer :: from, at

        n_lines = 0
        from = 1
        do
            if (from > len(text)) exit
            if (n_lines >= cap) exit
            at = index(text(from:), ";")
            n_lines = n_lines + 1
            if (at == 0) then
                lines(n_lines) = adjustl(text(from:))
                exit
            end if
            lines(n_lines) = adjustl(text(from:from + at - 2))
            from = from + at
        end do
    end subroutine split_lines

    subroutine register_rule(spec, stat)
        !! Register one `NAME:partial;partial;...` rule.
        !!
        !! The partials are Fortran written over `$1`, `$2`, ..., one per
        !! argument, in order. Splitting on the first colon keeps a partial
        !! free to contain one.
        use, intrinsic :: iso_fortran_env, only: error_unit
        use fortad, only: fad_add_rule
        character(len=*), intent(in) :: spec
        integer, intent(out) :: stat
        integer, parameter :: MAX_PARTIALS = 16
        character(len=256) :: partials(MAX_PARTIALS)
        integer :: colon, n_partials, from, at

        stat = 0
        colon = index(spec, ":")
        if (colon <= 1 .or. colon == len_trim(spec)) then
            write (error_unit, '(a)') &
                "fortad: a rule reads NAME:partial;partial;..., got: "//trim(spec)
            stat = 1
            return
        end if

        n_partials = 0
        from = colon + 1
        do
            at = index(spec(from:), ";")
            if (n_partials >= MAX_PARTIALS) exit
            n_partials = n_partials + 1
            if (at == 0) then
                partials(n_partials) = adjustl(spec(from:))
                exit
            end if
            partials(n_partials) = adjustl(spec(from:from + at - 2))
            from = from + at
        end do

        call fad_add_rule(trim(spec(:colon - 1)), partials(:n_partials), stat)
        if (stat /= 0) then
            write (error_unit, '(a)') &
                "fortad: could not register the rule for "//trim(spec(:colon - 1))
        end if
    end subroutine register_rule

    function run_reverse(source, independents, proc_name, module_name, &
            with_primal, dep_name, from_name) result(res)
        !! Reverse mode. A blank name, module or dependent means "the default".
        character(len=*), intent(in) :: source, independents(:)
        character(len=*), intent(in) :: proc_name, module_name, dep_name
        character(len=*), intent(in) :: from_name
        logical, intent(in) :: with_primal
        type(fad_result_t) :: res

        res = fad_vjp(source, independents, dependent=dep_name, name=proc_name, &
            module_name=module_name, with_primal=with_primal, &
            from=from_name)
    end function run_reverse

    function run_hessian(source, independents, proc_name, module_name, &
            from_name) result(res)
        !! Forward-over-reverse Hessian-vector product.
        character(len=*), intent(in) :: source, independents(:)
        character(len=*), intent(in) :: proc_name, module_name
        character(len=*), intent(in) :: from_name
        type(fad_result_t) :: res

        res = fad_hvp(source, independents, name=proc_name, &
            module_name=module_name, from=from_name)
    end function run_hessian

    subroutine parse_arguments(input_path, output_path, indep_list, directions, &
            proc_name, mode, module_name, roundtrip_only, &
            with_primal, dep_name, from_name, verbose, stat)
        !! Parse the command line.
        character(len=:), allocatable, intent(out) :: input_path, output_path
        character(len=:), allocatable, intent(out) :: indep_list, directions
        character(len=:), allocatable, intent(out) :: proc_name, mode
        character(len=:), allocatable, intent(out) :: module_name
        logical, intent(out) :: roundtrip_only, with_primal
        character(len=:), allocatable, intent(out) :: dep_name
        character(len=:), allocatable, intent(out) :: from_name
        logical, intent(out) :: verbose
        integer, intent(out) :: stat
        character(len=1024) :: arg
        integer :: i, n, length
        logical :: check_syntax, compact_syntax, source_first_syntax

        input_path = ""
        output_path = ""
        indep_list = ""
        directions = ""
        proc_name = ""
        mode = "forward"
        module_name = ""
        roundtrip_only = .false.
        with_primal = .true.
        dep_name = ""
        from_name = ""
        verbose = .false.
        stat = 0
        check_syntax = .false.
        compact_syntax = .false.
        source_first_syntax = .false.

        n = command_argument_count()
        i = 1
        if (n > 0) then
            call get_command_argument(1, arg, length)
            if (length > 0) then
                select case (trim(arg(1:length)))
                case ("jvp")
                    compact_syntax = .true.
                    mode = "forward"
                case ("vjp")
                    compact_syntax = .true.
                    mode = "reverse"
                case ("hvp")
                    compact_syntax = .true.
                    mode = "hessian"
                case ("check")
                    check_syntax = .true.
                    roundtrip_only = .true.
                    i = 2
                end select
                if (compact_syntax) then
                    i = 2
                    if (i > n) then
                        stat = 1
                        return
                    end if
                    call get_command_argument(i, arg, length)
                    if (length == 0) then
                        stat = 1
                        return
                    end if
                    if (arg(1:1) == "-") then
                        stat = 1
                        return
                    end if
                    if (existing_file(arg(1:length))) then
                        ! The source-first spelling is deliberately selected
                        ! only for an existing first path.  This keeps the
                        ! legacy `PRODUCT NAMES ... FILE` form deterministic,
                        ! while refusing the one ambiguous case of two paths.
                        source_first_syntax = .true.
                        input_path = trim(arg(1:length))
                        if (i + 1 <= n) then
                            call get_command_argument(i + 1, arg, length)
                            if (length == 0) then
                                stat = 1
                                return
                            end if
                            if (arg(1:1) /= "-") then
                                if (existing_file(arg(1:length))) then
                                    stat = 1
                                    return
                                end if
                                indep_list = trim(arg(1:length))
                                i = 4
                            else
                                ! Omitted names are inferred after the source
                                ! has been read.  Options may follow the path.
                                i = 3
                            end if
                        else
                            ! `fortad jvp source.f90` is the compact default.
                            i = 3
                        end if
                    else
                        indep_list = trim(arg(1:length))
                        i = 3
                    end if
                end if
            end if
        end if
        do while (i <= n)
            call get_command_argument(i, arg, length)
            select case (trim(arg(1:length)))
            case ("--indep", "-i")
                if (compact_syntax .or. check_syntax) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                indep_list = trim(arg(1:length))
            case ("--directions", "-d")
                if (check_syntax) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                directions = trim(arg(1:length))
            case ("--name")
                if (check_syntax) then
                    stat = 1
                    return
                end if
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
                if (compact_syntax .or. check_syntax) then
                    stat = 1
                    return
                end if
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
                if (check_syntax) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                module_name = trim(arg(1:length))
            case ("--dep")
                ! Which output to differentiate. Needed when the primal has
                ! more than one `intent(out)` argument and fortad cannot tell
                ! which one the caller means.
                if (check_syntax) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, length=length)
                if (allocated(dep_name)) deallocate (dep_name)
                allocate (character(len=length) :: dep_name)
                call get_command_argument(i, dep_name)
            case ("--call-rule")
                ! The derivative of a subroutine call, as statements rather
                ! than partials, because a subroutine has no single result.
                ! Written NAME:n_args:tangent;tangent|adjoint;adjoint over
                ! $k for the k-th actual, $kd for its tangent and $kb for its
                ! adjoint. Enzyme spells the same thing as a custom rule.
                if (check_syntax) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, length=length)
                block
                    character(len=:), allocatable :: spec
                    allocate (character(len=length) :: spec)
                    call get_command_argument(i, spec)
                    call register_call_rule(spec, stat)
                    if (stat /= 0) return
                end block
            case ("--proc")
                ! Which procedure of a module to differentiate. The rest of
                ! the file stays available, so a call to a sibling is inlined
                ! rather than needing a rule.
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, length=length)
                if (allocated(from_name)) deallocate (from_name)
                allocate (character(len=length) :: from_name)
                call get_command_argument(i, from_name)
            case ("--rule")
                ! The derivative of a routine fortad cannot see inside: a
                ! kernel behind a C binding, a table lookup, a library call.
                ! Enzyme needs the same thing, spelled as a custom rule in C.
                ! Written NAME:dNAME/darg1;dNAME/darg2, over $1, $2, ... - for
                ! example  --rule 'bessel_i0:bessel_i1($1)'
                if (check_syntax) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, length=length)
                block
                    character(len=:), allocatable :: spec
                    allocate (character(len=length) :: spec)
                    call get_command_argument(i, spec)
                    call register_rule(spec, stat)
                    if (stat /= 0) return
                end block
            case ("--no-primal")
                ! Emit the gradient alone. Everything the primal value kept
                ! alive is then dead and is removed.
                if (check_syntax) then
                    stat = 1
                    return
                end if
                with_primal = .false.
            case ("--verbose")
                verbose = .true.
            case ("--roundtrip")
                if (compact_syntax .or. check_syntax) then
                    stat = 1
                    return
                end if
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
                    if (source_first_syntax) then
                        stat = 1
                        return
                    end if
                    input_path = trim(arg(1:length))
                end if
            end select
            i = i + 1
        end do

        if (len(input_path) == 0) stat = 1
        if (.not. roundtrip_only .and. len(indep_list) == 0 .and. &
            .not. (compact_syntax .and. source_first_syntax)) stat = 1
    end subroutine parse_arguments

    subroutine infer_cli_defaults(source, input_path, mode, from_name, proc_name, &
            output_path, module_name, indep_list, message, verbose, stat)
        !! Infer the common CLI arguments from the selected primal.
        !!
        !! The library already lowers a source before differentiation, so the
        !! CLI uses that same IR instead of maintaining a second Fortran
        !! signature parser.  Explicit names and `--proc` remain available for
        !! multi-output or deliberately non-default transformations.
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: input_path, mode
        character(len=:), allocatable, intent(inout) :: from_name, proc_name
        character(len=:), allocatable, intent(inout) :: output_path, module_name
        character(len=:), allocatable, intent(out) :: indep_list, message
        logical, intent(in) :: verbose
        integer, intent(out) :: stat
        type(fad_proc_t) :: primal
        type(lower_status_t) :: lower_status
        character(len=:), allocatable :: product, generated_name

        stat = 0
        indep_list = ""
        message = ""
        if (len_trim(from_name) > 0) then
            call lower_source(source, primal, lower_status, trim(from_name))
        else
            call lower_source(source, primal, lower_status)
        end if
        if (.not. lower_status%ok) then
            stat = 1
            message = "cannot infer command arguments: "//trim(lower_status%message)
            return
        end if

        ! Make the selected default visible to the later transformation call.
        ! This is equivalent to lower_source's first-procedure default.
        if (len_trim(from_name) == 0) from_name = primal%name
        call infer_independent_names(primal, indep_list, stat)
        if (stat /= 0) then
            message = "could not infer independent variables for "//trim(primal%name)// &
                "; use the explicit NAMES argument or --indep NAMES"
            return
        end if

        select case (trim(mode))
        case ("reverse")
            product = "vjp"
        case ("hessian")
            product = "hvp"
        case default
            product = "jvp"
        end select
        generated_name = trim(primal%name)//"_"//trim(product)
        if (len_trim(proc_name) == 0) proc_name = generated_name
        if (len_trim(module_name) == 0) then
            module_name = module_name_from_path(input_path, product)
        end if
        if (len_trim(output_path) == 0) then
            output_path = output_path_from_path(input_path, product)
        end if
        if (verbose) then
            write (error_unit_or_output(), '(a)') &
                "fortad: inferred procedure = "//trim(primal%name)
            write (error_unit_or_output(), '(a)') &
                "fortad: inferred independent variables = "//trim(indep_list)
            write (error_unit_or_output(), '(a)') &
                "fortad: generated procedure = "//trim(proc_name)
            write (error_unit_or_output(), '(a)') &
                "fortad: output module = "//trim(module_name)
            write (error_unit_or_output(), '(a)') &
                "fortad: output file = "//trim(output_path)
        end if
    end subroutine infer_cli_defaults

    subroutine infer_independent_names(primal, indep_list, stat)
        !! Select dummy arguments that can carry an incoming derivative.
        !!
        !! An absent INTENT is treated as INOUT, which is the useful default
        !! for legacy Fortran. Explicit INTENT(OUT) dummies are outputs, not
        !! independent variables.
        type(fad_proc_t), intent(in) :: primal
        character(len=:), allocatable, intent(out) :: indep_list
        integer, intent(out) :: stat
        integer :: i, decl_index
        character(len=:), allocatable :: name

        indep_list = ""
        stat = 0
        if (.not. allocated(primal%params)) then
            stat = 1
            return
        end if
        do i = 1, size(primal%params)
            name = dummy_name(primal%params(i))
            if (len_trim(name) == 0) cycle
            decl_index = primal%decl_index(name)
            if (decl_index == 0) cycle
            if (primal%decls(decl_index)%is_result) cycle
            if (primal%decls(decl_index)%intent == FAD_INTENT_OUT) cycle
            if (len_trim(indep_list) == 0) then
                indep_list = trim(name)
            else
                indep_list = trim(indep_list)//","//trim(name)
            end if
        end do
        if (len_trim(indep_list) == 0) stat = 1
    end subroutine infer_independent_names

    function dummy_name(parameter) result(name)
        !! Return the object name from a procedure dummy specification.
        character(len=*), intent(in) :: parameter
        character(len=:), allocatable :: name
        integer :: cut

        name = adjustl(trim(parameter))
        cut = index(name, "(")
        if (cut > 1) name = trim(name(:cut - 1))
        cut = index(name, "=")
        if (cut > 1) name = trim(name(:cut - 1))
    end function dummy_name

    function output_path_from_path(input_path, product) result(path)
        !! Put an inferred derivative beside its input source.
        character(len=*), intent(in) :: input_path, product
        character(len=:), allocatable :: path
        integer :: dot, separator

        separator = max(scan(input_path, "/", back=.true.), &
            scan(input_path, achar(92), back=.true.))
        dot = scan(input_path, ".", back=.true.)
        if (dot <= separator) dot = len_trim(input_path) + 1
        path = trim(input_path(:dot - 1))//"_"//trim(product)//".f90"
    end function output_path_from_path

    function module_name_from_path(input_path, product) result(name)
        !! Make a valid, predictable wrapper name from the input basename.
        character(len=*), intent(in) :: input_path, product
        character(len=:), allocatable :: name, stem
        integer :: dot, separator, i
        character :: c

        separator = max(scan(input_path, "/", back=.true.), &
            scan(input_path, achar(92), back=.true.))
        dot = scan(input_path, ".", back=.true.)
        if (dot <= separator) dot = len_trim(input_path) + 1
        if (dot - 1 > separator) then
            stem = input_path(separator + 1:dot - 1)
        else
            stem = "fortad"
        end if
        do i = 1, len_trim(stem)
            c = stem(i:i)
            if (.not. ((c >= "a" .and. c <= "z") .or. &
                (c >= "A" .and. c <= "Z") .or. &
                (c >= "0" .and. c <= "9") .or. c == "_")) then
                stem(i:i) = "_"
            end if
        end do
        if (len_trim(stem) == 0) stem = "fortad"
        if (stem(1:1) >= "0" .and. stem(1:1) <= "9") stem = "fortad_"//stem
        name = trim(stem)//"_"//trim(product)//"_mod"
    end function module_name_from_path

    logical function existing_file(path) result(found)
        !! Return whether PATH names an existing input file.
        character(len=*), intent(in) :: path

        inquire (file=path, exist=found)
    end function existing_file

    subroutine usage()
        !! Print usage.
        write (*, '(a)') "fortad "//fad_version()// &
            " - source-transformation automatic differentiation for Fortran"
        write (*, '(a)') ""
        write (*, '(a)') "usage: fortad PRODUCT <file.f90> [names] [options]"
        write (*, '(a)') "       fortad PRODUCT <names> [options] <file.f90>"
        write (*, '(a)') "       fortad check [--proc NAME] [-o PATH] <file.f90>"
        write (*, '(a)') "       fortad --indep <names> [options] <file.f90>"
        write (*, '(a)') ""
        write (*, '(a)') "  PRODUCT               jvp, vjp, or hvp"
        write (*, '(a)') "  check                 parse and re-emit without differentiating"
        write (*, '(a)') "  -i, --indep a,b       independent variables (legacy form)"
        write (*, '(a)') "  -m, --mode MODE       forward (default), reverse, "// &
            "or hessian"
        write (*, '(a)') "      --module NAME     wrap the result in a module, "// &
            "for a checked interface"
        write (*, '(a)') "  -d, --directions nd   vector mode: name of the "// &
            "direction-count argument"
        write (*, '(a)') "      --name f_jvp      name of the generated procedure"
        write (*, '(a)') "      --proc NAME       target procedure in the input"
        write (*, '(a)') "  -o, --output path     write here instead of stdout"
        write (*, '(a)') "      --dep name        which output to "// &
            "differentiate, when there is more than one"
        write (*, '(a)') "      --no-primal       return the derivative only, "// &
            "not the primal value"
        write (*, '(a)') "      --verbose         print inferred defaults"
        write (*, '(a)') "      --roundtrip       parse and re-emit, do not "// &
            "differentiate"
        write (*, '(a)') "      --rule SPEC       register scalar partials"
        write (*, '(a)') "      --call-rule SPEC  register tangent and adjoint "// &
            "statements"
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
