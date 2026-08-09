program fortad_cli
    !! Command-line driver.
    !!
    !!     fortad kernel.f90
    !!     fortad jvp kernel.f90
    !!     fortad all kernel.f90
    !!     fortad jvp a,b --directions nd -o kernel_d.f90 kernel.f90
    !!     fortad vjp x --no-primal kernel.f90
    !!     fortad check kernel.f90
    !!
    !! Reads Fortran, writes Fortran. Nothing else is needed to use the result:
    !! compile the generated file with the rest of your project.
    use fortad, only: fad_add_rule
    use fortad, only: fad_jvp, fad_vjp, fad_hvp, fad_roundtrip, &
        fad_result_t, fad_version
    use fortad_ir, only: fad_proc_t, fad_base_name, FAD_ASSIGN, FAD_CALL_STMT, &
        FAD_INDEX, FAD_INTENT_OUT, FAD_VAR
    use fortad_lower, only: lower_source, lower_status_t
    use fortfront, only: is_fixed_form_file, normalize_fixed_form_source_text
    implicit none

    character(len=:), allocatable :: input_path, output_path, indep_list, dep_name
    character(len=:), allocatable :: output_directory, output_stem
    character(len=:), allocatable :: from_name
    character(len=:), allocatable :: directions, proc_name, source, mode
    character(len=:), allocatable :: module_name
    character(len=:), allocatable :: inference_message, candidate_names
    character(len=:), allocatable :: explicit_indep
    character(len=:), allocatable :: output_root_hint
    character(len=32), allocatable :: independents(:)
    type(fad_result_t) :: res
    logical :: roundtrip_only, with_primal, verbose, all_products
    logical :: tapenade_compat, tapenade_multi
    logical :: source_first_inference
    integer :: unit, stat

    call parse_arguments(input_path, output_path, output_directory, output_stem, &
        indep_list, directions, &
        proc_name, mode, module_name, roundtrip_only, &
        with_primal, dep_name, from_name, verbose, all_products, &
        source_first_inference, tapenade_compat, tapenade_multi, stat)
    if (stat /= 0) then
        call usage()
        error stop 2
    end if

    call finalize_tapenade_output(input_path, mode, output_path, output_directory, &
        output_stem, tapenade_compat, stat)
    if (stat /= 0) then
        write (error_unit_or_output(), '(a)') &
            "fortad: invalid Tapenade-compatible output specification"
        error stop 2
    end if
    if (tapenade_multi .and. mode == "reverse") then
        write (error_unit_or_output(), '(a)') &
            "fortad: Tapenade -vector/-multi is supported for forward mode only"
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

    if (all_products) then
        call run_all_products(source, input_path, indep_list, directions, &
            dep_name, from_name, with_primal, verbose, stat, inference_message)
        if (stat /= 0) then
            write (error_unit_or_output(), '(a)') "fortad: "//inference_message
            error stop 1
        end if
        stop
    end if

    if (.not. roundtrip_only .and. (len_trim(indep_list) == 0 .or. &
        source_first_inference)) then
        explicit_indep = indep_list
        output_root_hint = output_stem
        if (len_trim(output_root_hint) == 0) output_root_hint = output_path
        call infer_cli_defaults(source, input_path, mode, from_name, proc_name, &
            output_path, module_name, indep_list, inference_message, verbose, stat, &
            legacy_compat=(tapenade_compat .or. mode == "reverse"), &
            root_hint=output_root_hint, &
            root_selection=(tapenade_compat .or. len_trim(output_path) > 0))
        if (stat /= 0) then
            write (error_unit_or_output(), '(a)') "fortad: "//inference_message
            error stop 2
        end if
        if (len_trim(explicit_indep) > 0) indep_list = explicit_indep
    end if

    if (mode == "reverse" .and. len_trim(dep_name) == 0) then
        call infer_tapenade_dependent(source, from_name, dep_name, candidate_names, stat)
        if (stat /= 0) then
            if (len_trim(candidate_names) > 0) then
                write (error_unit_or_output(), '(a)') &
                    "fortad: could not infer Tapenade dependent; candidates: "// &
                    trim(candidate_names)//"; use --dep NAME"
            else
                write (error_unit_or_output(), '(a)') &
                    "fortad: could not infer Tapenade dependent; use --dep NAME"
            end if
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

    subroutine run_all_products(source, input_path, indep_list, directions, &
            dep_name, from_name, with_primal, verbose, stat, message)
        !! Emit the inferred JVP and VJP products beside the source.
        character(len=*), intent(in) :: source, input_path, directions
        character(len=:), allocatable, intent(inout) :: indep_list, from_name
        character(len=:), allocatable, intent(in) :: dep_name
        logical, intent(in) :: with_primal, verbose
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: explicit_indep, inferred_names
        character(len=:), allocatable :: jvp_name, vjp_name
        character(len=:), allocatable :: jvp_module, vjp_module
        character(len=:), allocatable :: jvp_output, vjp_output
        character(len=:), allocatable :: inference_message
        character(len=32), allocatable :: independents(:)
        type(fad_result_t) :: jvp, vjp
        integer :: unit, ios

        stat = 0
        message = ""
        explicit_indep = trim(indep_list)
        jvp_name = ""
        jvp_module = ""
        jvp_output = ""
        call infer_cli_defaults(source, input_path, "forward", from_name, &
            jvp_name, jvp_output, jvp_module, inferred_names, &
            inference_message, verbose, stat)
        if (stat /= 0) then
            message = inference_message
            return
        end if
        if (len_trim(explicit_indep) > 0) then
            indep_list = explicit_indep
        else
            indep_list = inferred_names
        end if

        vjp_name = ""
        vjp_module = ""
        vjp_output = ""
        call infer_cli_defaults(source, input_path, "reverse", from_name, &
            vjp_name, vjp_output, vjp_module, inferred_names, &
            inference_message, verbose, stat)
        if (stat /= 0) then
            message = inference_message
            return
        end if
        if (len_trim(explicit_indep) > 0) then
            indep_list = explicit_indep
        else
            indep_list = inferred_names
        end if
        independents = split_commas(indep_list)

        jvp = fad_jvp(source, independents, name=jvp_name, from=from_name, &
            module_name=jvp_module, n_directions=directions, &
            with_primal=with_primal)
        if (.not. jvp%ok) then
            stat = 1
            message = "JVP: "//jvp%message
            return
        end if
        vjp = fad_vjp(source, independents, dependent=dep_name, name=vjp_name, &
            module_name=vjp_module, with_primal=with_primal, from=from_name)
        if (.not. vjp%ok) then
            stat = 1
            message = "VJP: "//vjp%message
            return
        end if

        open (newunit=unit, file=jvp_output, status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) then
            stat = 1
            message = "cannot write "//jvp_output
            return
        end if
        write (unit, '(a)', iostat=ios) jvp%code
        close (unit)
        if (ios /= 0) then
            stat = 1
            message = "cannot write "//jvp_output
            return
        end if

        open (newunit=unit, file=vjp_output, status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) then
            stat = 1
            message = "cannot write "//vjp_output
            return
        end if
        write (unit, '(a)', iostat=ios) vjp%code
        close (unit)
        if (ios /= 0) then
            stat = 1
            message = "cannot write "//vjp_output
        end if
    end subroutine run_all_products

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

    subroutine parse_arguments(input_path, output_path, output_directory, output_stem, &
            indep_list, directions, &
            proc_name, mode, module_name, roundtrip_only, &
            with_primal, dep_name, from_name, verbose, all_products, &
            source_first_inference, tapenade_compat, tapenade_multi, stat)
        !! Parse the command line.
        character(len=:), allocatable, intent(out) :: input_path, output_path
        character(len=:), allocatable, intent(out) :: output_directory, output_stem
        character(len=:), allocatable, intent(out) :: indep_list, directions
        character(len=:), allocatable, intent(out) :: proc_name, mode
        character(len=:), allocatable, intent(out) :: module_name
        logical, intent(out) :: roundtrip_only, with_primal
        character(len=:), allocatable, intent(out) :: dep_name
        character(len=:), allocatable, intent(out) :: from_name
        logical, intent(out) :: verbose, all_products, source_first_inference
        logical, intent(out) :: tapenade_compat, tapenade_multi
        integer, intent(out) :: stat
        character(len=1024) :: arg
        integer :: i, n, length
        logical :: check_syntax, compact_syntax, source_first_syntax
        logical :: bare_source_syntax

        input_path = ""
        output_path = ""
        output_directory = ""
        output_stem = ""
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
        all_products = .false.
        source_first_inference = .false.
        tapenade_compat = .false.
        tapenade_multi = .false.
        stat = 0
        check_syntax = .false.
        compact_syntax = .false.
        source_first_syntax = .false.
        bare_source_syntax = .false.

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
                case ("all")
                    compact_syntax = .true.
                    all_products = .true.
                    mode = "all"
                case ("check")
                    check_syntax = .true.
                    roundtrip_only = .true.
                    i = 2
                case default
                    if (existing_file(arg(1:length))) then
                        ! A bare source is the shortest useful invocation:
                        ! infer the first procedure's inputs and emit its JVP.
                        ! Keep it distinct from PRODUCT's source-first spelling
                        ! so the compact parser does not reinterpret the path.
                        compact_syntax = .true.
                        bare_source_syntax = .true.
                        source_first_inference = .true.
                        source_first_syntax = .true.
                        mode = "forward"
                        input_path = trim(arg(1:length))
                        i = 2
                    end if
                end select
                if (compact_syntax .and. .not. bare_source_syntax) then
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
        if (bare_source_syntax .and. i <= n) then
            call get_command_argument(i, arg, length)
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
                i = i + 1
            end if
        end if
        do while (i <= n)
            call get_command_argument(i, arg, length)
            select case (trim(arg(1:length)))
            case ("-p")
                ! Tapenade parser mode: round-trip the selected procedure.
                tapenade_compat = .true.
                roundtrip_only = .true.
                mode = "parser"
            case ("-b")
                ! Tapenade reverse mode.
                tapenade_compat = .true.
                mode = "reverse"
                source_first_inference = .true.
            case ("-d")
                ! Tapenade uses bare -d for forward mode.  FortAD's native
                ! vector spelling is --directions NAME; retain it when the
                ! following argument is an explicit non-file value.
                tapenade_compat = .true.
                source_first_inference = .true.
                if (i < n) then
                    call get_command_argument(i + 1, arg, length)
                    if (length > 0) then
                        if (arg(1:1) /= "-") then
                            if (.not. existing_file(arg(1:length))) then
                                directions = trim(arg(1:length))
                                i = i + 1
                            else
                                mode = "forward"
                            end if
                        else
                            mode = "forward"
                        end if
                    else
                        mode = "forward"
                    end if
                else
                    mode = "forward"
                end if
            case ("--indep", "-i")
                if ((compact_syntax .and. .not. bare_source_syntax) .or. &
                    check_syntax) then
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
            case ("--directions")
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
                if (check_syntax .or. all_products) then
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
            case ("-o")
                if (all_products) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                output_stem = trim(arg(1:length))
            case ("--output")
                if (all_products) then
                    stat = 1
                    return
                end if
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                output_path = trim(arg(1:length))
            case ("-O")
                tapenade_compat = .true.
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, arg, length)
                output_directory = trim(arg(1:length))
            case ("-root", "--root")
                tapenade_compat = .true.
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, length=length)
                if (allocated(from_name)) deallocate (from_name)
                allocate (character(len=length) :: from_name)
                call get_command_argument(i, from_name)
                source_first_inference = .true.
            case ("-ext")
                ! Accept Tapenade's external-summary option for migration.
                ! FortAD derivative rules remain explicit via --rule or
                ! --call-rule; this option only avoids a parser-level failure.
                tapenade_compat = .true.
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
            case ("-multi", "-vector")
                ! Tapenade calls multidirectional mode `-vector`; keep
                ! FortAD's historical `-multi` spelling as an alias. The
                ! direction-count dummy is inferred as `nd`.
                tapenade_compat = .true.
                tapenade_multi = .true.
                if (len_trim(directions) == 0) directions = "nd"
            case ("-context", "-fixinterface", "-standalonediff")
                ! These Tapenade switches request behaviour that is already
                ! structural in FortAD: the whole source file is available
                ! to the lowering pass, generated products have explicit
                ! module interfaces, and the result is emitted as a
                ! standalone source unit.  Accept them so existing Options
                ! files can be reused without a flag-stripping wrapper.
                tapenade_compat = .true.
            case ("-m", "--mode")
                if ((compact_syntax .and. .not. bare_source_syntax) .or. &
                    check_syntax) then
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
                if (check_syntax .or. all_products) then
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
            case ("--head", "-head")
                ! Tapenade-compatible shorthand: HEAD names the selected
                ! procedure and, optionally, its active arguments, for
                ! example `-head square(x y)`.  Keep inference enabled so
                ! the ordinary generated name, module, and sibling output
                ! are selected just as they are for a source-first command.
                i = i + 1
                if (i > n) then
                    stat = 1
                    return
                end if
                call get_command_argument(i, length=length)
                block
                    character(len=:), allocatable :: head
                    allocate (character(len=length) :: head)
                    call get_command_argument(i, head)
                    call parse_head_spec(head, from_name, indep_list, stat)
                    if (stat /= 0) return
                end block
                source_first_inference = .true.
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

        if (.not. tapenade_compat .and. len_trim(output_path) == 0 .and. &
            len_trim(output_stem) > 0) output_path = output_stem
        if (len(input_path) == 0) stat = 1
        if (.not. roundtrip_only .and. len(indep_list) == 0 .and. &
            .not. ((compact_syntax .and. source_first_syntax) .or. &
            (tapenade_compat .and. source_first_inference))) stat = 1
    end subroutine parse_arguments

    subroutine finalize_tapenade_output(input_path, mode, output_path, &
            output_directory, output_stem, tapenade_compat, stat)
        !! Map Tapenade's `-O DIR -o STEM` naming to a free-form output file.
        character(len=*), intent(in) :: input_path, mode
        character(len=:), allocatable, intent(inout) :: output_path
        character(len=:), allocatable, intent(in) :: output_directory, output_stem
        logical, intent(in) :: tapenade_compat
        integer, intent(out) :: stat
        character(len=:), allocatable :: stem, basename, suffix
        integer :: separator, dot

        stat = 0
        if (.not. tapenade_compat .or. len_trim(output_path) > 0) return

        if (len_trim(output_stem) > 0) then
            stem = trim(output_stem)
        else
            separator = max(scan(input_path, "/", back=.true.), &
                scan(input_path, achar(92), back=.true.))
            dot = scan(input_path, ".", back=.true.)
            if (dot <= separator) dot = len_trim(input_path) + 1
            if (dot > separator + 1) then
                stem = input_path(separator + 1:dot - 1)
            else
                stat = 1
                return
            end if
        end if

        separator = max(scan(stem, "/", back=.true.), &
            scan(stem, achar(92), back=.true.))
        dot = scan(stem, ".", back=.true.)
        if (dot > separator) stem = stem(:dot - 1)
        select case (trim(mode))
        case ("parser")
            suffix = "p"
        case ("reverse")
            suffix = "b"
        case default
            suffix = "d"
        end select

        if (len_trim(output_directory) > 0) then
            if (separator > 0) then
                basename = stem(separator + 1:)
            else
                basename = stem
            end if
            output_path = trim(output_directory)//"/"//trim(basename)//"_"// &
                trim(suffix)//".f90"
        else
            output_path = trim(stem)//"_"//trim(suffix)//".f90"
        end if
    end subroutine finalize_tapenade_output

    subroutine parse_head_spec(spec, from_name, indep_list, stat)
        !! Parse Tapenade's `-head name(arg1 arg2)` shorthand.
        character(len=*), intent(in) :: spec
        character(len=:), allocatable, intent(inout) :: from_name, indep_list
        integer, intent(out) :: stat
        character(len=:), allocatable :: body, token
        integer :: open, close, i, start, body_length
        logical :: separator

        stat = 0
        open = index(trim(spec), "(")
        if (open == 0) then
            if (len_trim(spec) == 0) then
                stat = 1
            else
                from_name = trim(spec)
            end if
            return
        end if
        close = index(trim(spec), ")", back=.true.)
        if (open <= 1 .or. close /= len_trim(spec) .or. close <= open + 1) then
            stat = 1
            return
        end if

        from_name = trim(spec(:open - 1))
        body = spec(open + 1:close - 1)
        body_length = len_trim(body)
        indep_list = ""
        start = 0
        do i = 1, body_length + 1
            if (i > body_length) then
                separator = .true.
            else
                select case (body(i:i))
                case (",", " ", achar(9))
                    separator = .true.
                case default
                    separator = .false.
                end select
            end if
            if (.not. separator) then
                if (start == 0) start = i
            else if (start > 0) then
                token = trim(body(start:i - 1))
                if (len_trim(indep_list) == 0) then
                    indep_list = token
                else
                    indep_list = trim(indep_list)//","//token
                end if
                start = 0
            end if
        end do
        if (len_trim(indep_list) == 0) stat = 1
    end subroutine parse_head_spec

    function procedure_hint_from_path(path, mode) result(candidate)
        !! Extract a procedure hint from an explicit derivative output path.
        !!
        !! `--output selected_vjp.f90` is a useful zero-configuration spelling
        !! for a multi-procedure source.  Only the basename is considered, and
        !! callers still fall back to the first procedure when it is not found.
        character(len=*), intent(in) :: path, mode
        character(len=:), allocatable :: candidate, stem, suffix
        integer :: separator, dot, first

        candidate = ""
        separator = max(scan(path, "/", back=.true.), &
            scan(path, achar(92), back=.true.))
        dot = scan(path, ".", back=.true.)
        if (dot <= separator) dot = len_trim(path) + 1
        if (dot <= separator + 1) return
        stem = trim(path(separator + 1:dot - 1))
        select case (trim(mode))
        case ("forward")
            suffix = "_jvp"
        case ("reverse")
            suffix = "_vjp"
        case ("hessian")
            suffix = "_hvp"
        case ("parser")
            suffix = "_p"
        case default
            suffix = ""
        end select
        if (len_trim(suffix) > 0 .and. len_trim(stem) > len_trim(suffix)) then
            first = len_trim(stem) - len_trim(suffix) + 1
            if (same_cli_name(stem(first:), suffix)) stem = trim(stem(:first - 1))
        end if
        candidate = trim(stem)
    end function procedure_hint_from_path

    subroutine infer_cli_defaults(source, input_path, mode, from_name, proc_name, &
            output_path, module_name, indep_list, message, verbose, stat, legacy_compat, &
            root_hint, root_selection)
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
        logical, intent(in), optional :: legacy_compat
        character(len=*), intent(in), optional :: root_hint
        logical, intent(in), optional :: root_selection
        type(fad_proc_t) :: primal
        type(lower_status_t) :: lower_status
        character(len=:), allocatable :: product, generated_name, candidate
        logical :: use_legacy_compat

        stat = 0
        indep_list = ""
        message = ""
        use_legacy_compat = .false.
        if (present(legacy_compat)) use_legacy_compat = legacy_compat
        if (len_trim(from_name) == 0 .and. present(root_hint)) then
            if (present(root_selection)) then
                if (root_selection .and. len_trim(root_hint) > 0) then
                    candidate = procedure_hint_from_path(root_hint, mode)
                    if (len_trim(candidate) > 0) then
                        call lower_source(source, primal, lower_status, candidate)
                        if (lower_status%ok) from_name = candidate
                    end if
                end if
            end if
        end if
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
        call infer_independent_names(primal, indep_list, stat, &
            legacy_outputs=use_legacy_compat)
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

    subroutine infer_independent_names(primal, indep_list, stat, legacy_outputs)
        !! Select dummy arguments that can carry an incoming derivative.
        !!
        !! An absent INTENT is treated as INOUT, which is the useful default
        !! for legacy Fortran. Explicit INTENT(OUT) dummies are outputs, not
        !! independent variables.
        type(fad_proc_t), intent(in) :: primal
        character(len=:), allocatable, intent(out) :: indep_list
        integer, intent(out) :: stat
        logical, intent(in), optional :: legacy_outputs
        integer :: i, decl_index, n_components
        character(len=:), allocatable :: name
        logical :: derived_decl
        logical :: use_legacy_outputs

        indep_list = ""
        stat = 0
        use_legacy_outputs = .false.
        if (present(legacy_outputs)) use_legacy_outputs = legacy_outputs
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
            derived_decl = is_derived_declaration(primal, decl_index)
            if (derived_decl) then
                n_components = append_read_component_paths(primal, name, &
                    indep_list)
                if (n_components > 0) cycle
            end if
            if (use_legacy_outputs) then
                if (legacy_output_candidate(primal, name)) cycle
            end if
            if (len_trim(indep_list) == 0) then
                indep_list = trim(name)
            else
                indep_list = trim(indep_list)//","//trim(name)
            end if
        end do
        if (len_trim(indep_list) == 0) stat = 1
    end subroutine infer_independent_names

    integer function append_read_component_paths(primal, base_name, list) &
            result(appended)
        !! Prefer concrete REAL component paths over a whole derived dummy.
        !! The latter would perturb the object's storage/type identity, while
        !! the former is exactly the bounded component-derivative contract.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: base_name
        character(len=:), allocatable, intent(inout) :: list
        integer :: i
        character(len=:), allocatable :: path

        appended = 0
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (.not. primal%exprs(i)%component_is_real) cycle
            path = trim(primal%exprs(i)%text)
            if (allocated(primal%exprs(i)%component_original_path)) then
                path = trim(primal%exprs(i)%component_original_path)
            end if
            if (fad_base_name(path) /= trim(base_name)) cycle
            if (.not. component_path_is_read(primal, path)) cycle
            if (independent_list_contains(list, path)) cycle
            call append_independent_name(list, path)
            appended = appended + 1
        end do
    end function append_read_component_paths

    logical function component_path_is_read(primal, path) result(found)
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: path
        integer :: i, j

        found = .false.
        do i = 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                if (expression_has_component(primal, primal%stmts(i)%value, &
                    path)) then
                    found = .true.
                    return
                end if
            case (FAD_CALL_STMT)
                if (.not. allocated(primal%stmts(i)%call_args)) cycle
                do j = 1, size(primal%stmts(i)%call_args)
                    if (expression_has_component(primal, &
                        primal%stmts(i)%call_args(j), path)) then
                        found = .true.
                        return
                    end if
                end do
            end select
        end do
    end function component_path_is_read

    recursive logical function expression_has_component(primal, index, path) &
            result(found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: index
        character(len=*), intent(in) :: path
        integer :: i
        character(len=:), allocatable :: text

        found = .false.
        if (index < 1 .or. index > primal%n_exprs) return
        if (primal%exprs(index)%is_component_path) then
            text = trim(primal%exprs(index)%text)
            if (allocated(primal%exprs(index)%component_original_path)) then
                text = trim(primal%exprs(index)%component_original_path)
            end if
            if (same_cli_name(text, path)) then
                found = .true.
                return
            end if
        end if
        if (.not. allocated(primal%exprs(index)%args)) return
        do i = 1, size(primal%exprs(index)%args)
            if (expression_has_component(primal, primal%exprs(index)%args(i), &
                path)) then
                found = .true.
                return
            end if
        end do
    end function expression_has_component

    logical function is_derived_declaration(primal, decl_index) result(found)
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: decl_index
        character(len=:), allocatable :: compact
        integer :: i

        found = .false.
        if (decl_index <= 0 .or. decl_index > primal%n_decls) return
        if (.not. allocated(primal%decls(decl_index)%type_name)) return
        compact = ""
        do i = 1, len_trim(primal%decls(decl_index)%type_name)
            if (primal%decls(decl_index)%type_name(i:i) == " " .or. &
                primal%decls(decl_index)%type_name(i:i) == achar(9)) cycle
            compact = compact//primal%decls(decl_index)%type_name(i:i)
        end do
        found = index(compact, "type(") == 1 .or. index(compact, "TYPE(") == 1 .or. &
            index(compact, "class(") == 1 .or. index(compact, "CLASS(") == 1
    end function is_derived_declaration

    subroutine append_independent_name(list, name)
        character(len=:), allocatable, intent(inout) :: list
        character(len=*), intent(in) :: name

        if (len_trim(list) == 0) then
            list = trim(name)
        else
            list = trim(list)//","//trim(name)
        end if
    end subroutine append_independent_name

    logical function independent_list_contains(list, name) result(found)
        character(len=*), intent(in) :: list, name
        character(len=:), allocatable :: remaining, item
        integer :: comma

        found = .false.
        remaining = trim(list)
        do while (len_trim(remaining) > 0)
            comma = index(remaining, ",")
            if (comma > 0) then
                item = trim(remaining(:comma - 1))
                remaining = adjustl(remaining(comma + 1:))
            else
                item = trim(remaining)
                remaining = ""
            end if
            if (same_cli_name(item, name)) then
                found = .true.
                return
            end if
        end do
    end function independent_list_contains

    logical function same_cli_name(lhs, rhs) result(equal)
        character(len=*), intent(in) :: lhs, rhs
        integer :: i

        equal = len_trim(lhs) == len_trim(rhs)
        if (.not. equal) return
        do i = 1, len_trim(lhs)
            if (lower_cli_char(lhs(i:i)) /= lower_cli_char(rhs(i:i))) then
                equal = .false.
                return
            end if
        end do
    end function same_cli_name

    character function lower_cli_char(value) result(lowered)
        character, intent(in) :: value

        if (iachar(value) >= iachar("A") .and. iachar(value) <= iachar("Z")) then
            lowered = achar(iachar(value) + iachar("a") - iachar("A"))
        else
            lowered = value
        end if
    end function lower_cli_char

    subroutine infer_tapenade_dependent(source, from_name, dependent, candidates, stat)
        !! Infer a legacy subroutine's output from its first write.
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: from_name
        character(len=:), allocatable, intent(out) :: dependent, candidates
        integer, intent(out) :: stat
        type(fad_proc_t) :: primal
        type(lower_status_t) :: lower_status
        integer :: i, n_outputs
        character(len=:), allocatable :: name

        dependent = ""
        candidates = ""
        stat = 0
        if (len_trim(from_name) > 0) then
            call lower_source(source, primal, lower_status, trim(from_name))
        else
            call lower_source(source, primal, lower_status)
        end if
        if (.not. lower_status%ok) then
            stat = 1
            return
        end if
        if (primal%is_function) then
            dependent = primal%result_name
            return
        end if
        n_outputs = 0
        if (.not. allocated(primal%params)) then
            stat = 1
            return
        end if
        do i = 1, size(primal%params)
            name = dummy_name(primal%params(i))
            if (len_trim(name) == 0) cycle
            if (legacy_output_candidate(primal, name)) then
                n_outputs = n_outputs + 1
                dependent = name
                if (.not. independent_list_contains(candidates, name)) then
                    call append_independent_name(candidates, name)
                end if
            end if
        end do
        if (n_outputs == 0) then
            ! Modern legacy-style subroutines often write a concrete component
            ! of an otherwise input derived object.  The containing object is
            ! not an output candidate: the component path is the dependent.
            ! Keep this inference deliberately narrow and refuse ambiguity.
            do i = 1, primal%n_stmts
                if (primal%stmts(i)%kind /= FAD_ASSIGN) cycle
                if (.not. component_output_candidate(primal, &
                    primal%stmts(i)%target)) cycle
                if (n_outputs == 0) then
                    dependent = trim(primal%stmts(i)%target)
                    n_outputs = 1
                else if (.not. same_cli_name(dependent, &
                        primal%stmts(i)%target)) then
                    n_outputs = n_outputs + 1
                end if
                if (.not. independent_list_contains(candidates, &
                    primal%stmts(i)%target)) then
                    call append_independent_name(candidates, &
                        primal%stmts(i)%target)
                end if
            end do
        end if
        if (n_outputs /= 1) then
            dependent = ""
            stat = 1
        end if
    end subroutine infer_tapenade_dependent

    logical function component_output_candidate(primal, target) result(found)
        !! Whether TARGET is a bounded concrete REAL component output.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: target
        integer :: i

        found = .false.
        if (index(trim(target), "%") == 0) return
        do i = 1, primal%n_exprs
            if (.not. primal%exprs(i)%is_component_path) cycle
            if (.not. same_cli_name(primal%exprs(i)%text, target)) cycle
            if (.not. primal%exprs(i)%component_is_real) return
            if (primal%exprs(i)%component_is_allocatable) return
            if (primal%exprs(i)%component_is_pointer) return
            if (primal%exprs(i)%component_is_target) return
            if (primal%exprs(i)%component_is_polymorphic) return
            if (primal%exprs(i)%component_is_global) return
            if (primal%exprs(i)%component_rank > 4) return
            found = .true.
            return
        end do
        do i = 1, primal%n_stmts
            if (.not. primal%stmts(i)%target_is_component_path) cycle
            if (.not. allocated(primal%stmts(i)%target)) cycle
            if (.not. same_cli_name(primal%stmts(i)%target, target)) cycle
            if (.not. primal%stmts(i)%target_component_is_real) return
            if (primal%stmts(i)%target_component_is_allocatable .or. &
                primal%stmts(i)%target_component_is_pointer .or. &
                primal%stmts(i)%target_component_is_target .or. &
                primal%stmts(i)%target_component_is_polymorphic .or. &
                primal%stmts(i)%target_component_is_global) return
            if (primal%stmts(i)%target_component_rank > 4) return
            found = .true.
            return
        end do
    end function component_output_candidate

    logical function legacy_output_candidate(primal, name) result(is_output)
        !! Identify a legacy dummy written before it is read.
        type(fad_proc_t), intent(in) :: primal
        character(len=*), intent(in) :: name
        logical :: written
        integer :: i, j

        is_output = .false.
        written = .false.
        do i = 1, primal%n_stmts
            select case (primal%stmts(i)%kind)
            case (FAD_ASSIGN)
                if (.not. written) then
                    if (expression_mentions(primal, primal%stmts(i)%value, name)) return
                end if
                if (allocated(primal%stmts(i)%target)) then
                    if (trim(fad_base_name(primal%stmts(i)%target)) == trim(name)) then
                        written = .true.
                    end if
                end if
            case (FAD_CALL_STMT)
                if (.not. written) then
                    if (allocated(primal%stmts(i)%call_args)) then
                        do j = 1, size(primal%stmts(i)%call_args)
                            if (expression_mentions(primal, &
                                primal%stmts(i)%call_args(j), name)) return
                        end do
                    end if
                end if
            end select
        end do
        is_output = written
    end function legacy_output_candidate

    recursive logical function expression_mentions(primal, index, name) result(found)
        !! Return whether an IR expression mentions NAME as a variable.
        type(fad_proc_t), intent(in) :: primal
        integer, intent(in) :: index
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        if (index < 1) return
        if (index > primal%n_exprs) return
        select case (primal%exprs(index)%kind)
        case (FAD_VAR, FAD_INDEX)
            if (trim(fad_base_name(primal%exprs(index)%text)) == trim(name)) then
                found = .true.
                return
            end if
        end select
        if (.not. allocated(primal%exprs(index)%args)) return
        do i = 1, size(primal%exprs(index)%args)
            if (expression_mentions(primal, primal%exprs(index)%args(i), name)) then
                found = .true.
                return
            end if
        end do
    end function expression_mentions

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
        write (*, '(a)') "       fortad <file.f90> [names] [options] (inferred)"
        write (*, '(a)') "usage: fortad PRODUCT <file.f90> [names] [options]"
        write (*, '(a)') "       fortad PRODUCT <names> [options] <file.f90>"
        write (*, '(a)') "       fortad all <file.f90> [names]"
        write (*, '(a)') "       fortad check [--proc NAME] [-o PATH] <file.f90>"
        write (*, '(a)') "       fortad --indep <names> [options] <file.f90>"
        write (*, '(a)') ""
        write (*, '(a)') "  PRODUCT               jvp, vjp, or hvp"
        write (*, '(a)') "  all                   write inferred JVP and VJP siblings"
        write (*, '(a)') "  check                 parse and re-emit without differentiating"
        write (*, '(a)') "  -i, --indep a,b       independent variables (legacy form)"
        write (*, '(a)') "  -m, --mode MODE       forward (default), reverse, "// &
            "or hessian"
        write (*, '(a)') "      --module NAME     wrap the result in a module, "// &
            "for a checked interface"
        write (*, '(a)') "      --directions nd   vector mode: name of the "// &
            "direction-count argument"
        write (*, '(a)') "      --name f_jvp      name of the generated procedure"
        write (*, '(a)') "      --proc NAME       target procedure in the input"
        write (*, '(a)') "      --head SPEC       Tapenade form: NAME(arg1 arg2)"
        write (*, '(a)') "  -o STEM               Tapenade output basename; adds _p/_d/_b.f90"
        write (*, '(a)') "      --output PATH     write here instead of stdout"
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
        write (*, '(a)') "  Tapenade: -p/-d/-b -root NAME -O DIR -o STEM"
        write (*, '(a)') "      -vector/-multi   forward vector mode (directions nd)"
        write (*, '(a)') "      -context          whole-file context (already automatic)"
        write (*, '(a)') "      -fixinterface     checked interfaces (already automatic)"
        write (*, '(a)') "      -standalonediff   standalone source (already automatic)"
        write (*, '(a)') "      -ext FILE         accepted for migration; use --rule instead"
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
