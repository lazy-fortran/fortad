module fortad_emit
    !! IR to standard Fortran.
    !!
    !! The emitter is the product. It writes code a good Fortran programmer
    !! would have written: scalar temporaries, no allocation on the hot path,
    !! attributes preserved so the user's compiler can still optimise. House
    !! style is 4-space indent and a single space before an inline comment.
    !!
    !! Expressions are written into a `buffer_t` by recursive subroutines. A
    !! recursive function returning a deferred-length allocatable would be the
    !! obvious shape and is the one that silently corrupted output here, so the
    !! append form is deliberate, not incidental.
    use fortad_ir, only: fad_proc_t, fad_stmt_t, fad_decl_t, &
        FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, FAD_CALL, &
        FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, &
        FAD_ELSE, FAD_END_IF, FAD_CALL_STMT, &
        FAD_DIRECTIVE, FAD_SELECT_TYPE, FAD_TYPE_IS, &
        FAD_CLASS_IS, FAD_CLASS_DEFAULT, FAD_END_SELECT, &
        FAD_ALLOCATE, FAD_DEALLOCATE, FAD_MOVE_ALLOC, &
        FAD_INTENT_IN, FAD_INTENT_OUT, &
        FAD_INTENT_INOUT
    use fortgen_buffer, only: buffer_t
    use fortgen_layout, only: put_wrapped, indent_of, DEFAULT_LINE_LIMIT
    use fortgen_banner, only: put_banner
    implicit none
    private

    public :: emit_proc, emit_expr, emit_decl_line, write_expr, emit_module

    !! fortad claims no copyright in what it emits, and the banner says so
    !! where a reader of the generated file will actually see it.
    character(len=*), parameter :: OUTPUT_LICENCE_NOTE = &
        "This is a derivative of your program, under your own licence."
    character(len=*), parameter :: ACC_MARKER = "!"//achar(36)//"acc"


    !! Unary minus binds tighter than the binary arithmetic operators but
    !! looser than exponentiation.
    integer, parameter :: PREC_UNARY = 5

contains

    function emit_module(p, module_name, generator) result(text)
        !! Wrap the generated procedure in a module.
        !!
        !! A module gives the consumer an explicit interface without anyone
        !! maintaining a second, hand-written one. An argument-count mismatch
        !! then fails the build instead of corrupting a result at run time,
        !! which is exactly the bug this project's own test suite hit before
        !! the wrapper existed.
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: generator
        character(len=:), allocatable :: text
        character(len=:), allocatable :: wrapper_name
        type(buffer_t) :: b

        wrapper_name = trim(module_name)
        if (same_fortran_name(wrapper_name, p%name)) then
            wrapper_name = wrapper_name//"_module"
        end if

        if (present(generator)) then
            call put_banner(b, generator, note=OUTPUT_LICENCE_NOTE)
        else
            call put_banner(b, "fortad", note=OUTPUT_LICENCE_NOTE)
        end if
        call b%line("module "//wrapper_name)
        call b%line("    implicit none")
        call b%line("    private")
        call b%line("")
        call b%line("    public :: "//p%name)
        call b%line("")
        call b%line("contains")
        call b%line("")
        call b%put(indent_block(emit_proc(p, nested=4)))
        call b%line("")
        call b%line("end module "//wrapper_name)
        text = b%str()
    end function emit_module

    logical function same_fortran_name(a, b) result(equal)
        !! Fortran identifiers compare without regard to ASCII letter case.
        character(len=*), intent(in) :: a, b
        integer :: i

        equal = .false.
        if (len_trim(a) /= len_trim(b)) return
        do i = 1, len_trim(a)
            if (lower_ascii(a(i:i)) /= lower_ascii(b(i:i))) return
        end do
        equal = .true.
    end function same_fortran_name

    character function lower_ascii(c)
        character, intent(in) :: c

        lower_ascii = c
        if (c >= "A" .and. c <= "Z") lower_ascii = achar(iachar(c) + 32)
    end function lower_ascii


    function indent_block(text) result(out)
        !! Indent every non-empty line by one level, for module nesting.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i, start, used, nonempty_lines, output_length

        nonempty_lines = 0
        output_length = 0
        start = 1
        do i = 1, len(text)
            if (text(i:i) /= new_line('a')) cycle
            if (i > start) then
                nonempty_lines = nonempty_lines + 1
                output_length = output_length + i - start
            end if
            output_length = output_length + 1
            start = i + 1
        end do
        output_length = output_length + 4*nonempty_lines
        if (output_length == 0) then
            out = ""
            return
        end if
        allocate (character(len=output_length) :: out)
        used = 0
        start = 1
        do i = 1, len(text)
            if (text(i:i) /= new_line('a')) cycle
            if (i > start) then
                out(used + 1:used + 4) = "    "
                used = used + 4
                out(used + 1:used + i - start) = text(start:i - 1)
                used = used + i - start
            end if
            ! Empty lines have no indentation, matching the old emitter.
            out(used + 1:used + 1) = new_line('a')
            used = used + 1
            start = i + 1
        end do
    end function indent_block

    function emit_proc(p, nested) result(text)
        !! Emit a complete procedure as Fortran source text.
        type(fad_proc_t), intent(in) :: p
        !! Columns the caller will add in front of every line. Wrapping has to
        !! know: a module wrapper indents the whole procedure by four, and a
        !! line wrapped to exactly the limit then sits four columns past it.
        integer, intent(in), optional :: nested
        character(len=:), allocatable :: text
        type(buffer_t) :: b
        type(fad_stmt_t) :: pending_directive
        logical :: has_pending_directive, directive_for_loop, buffered
        integer :: i, indent, limit, loop_number, end_stmt
        character(len=128), allocatable :: accumulator_names(:)
        integer, allocatable :: accumulator_terms(:)

        limit = DEFAULT_LINE_LIMIT
        if (present(nested)) limit = limit - nested
        has_pending_directive = .false.

        call write_header(b, p, limit)
        do i = 1, p%n_uses
            call b%line(indent_of(1)//trim(p%uses(i)))
        end do
        call b%line("    implicit none")
        do i = 1, p%n_decls
            if (p%decls(i)%is_select_alias) cycle
            call b%line(indent_of(1)//emit_decl_line(p%decls(i)))
        end do
        call write_buffer_decls(b, p)
        call b%line("")

        indent = 1
        loop_number = 0
        i = 1
        do while (i <= p%n_stmts)
            directive_for_loop = .false.
            if (p%stmts(i)%kind == FAD_DIRECTIVE) then
                pending_directive = p%stmts(i)
                has_pending_directive = .true.
                i = i + 1
                cycle
            end if
            if (p%stmts(i)%kind == FAD_DO) loop_number = loop_number + 1
            if (has_pending_directive .and. p%stmts(i)%kind == FAD_DO) then
                call write_omp_directive(b, p, pending_directive, i, &
                    indent_of(indent), limit)
                directive_for_loop = .true.
                has_pending_directive = .false.
            end if
            if (p%stmts(i)%kind == FAD_DO .and. .not. directive_for_loop) then
                end_stmt = matching_end_do(p, i)
                call analyze_buffered_loop(p, i, end_stmt, buffered, &
                    accumulator_names, accumulator_terms)
                if (buffered) then
                    call write_buffered_loop(b, p, i, end_stmt, indent, limit, &
                        loop_number, accumulator_names)
                    i = end_stmt + 1
                    cycle
                end if
            end if
            select case (p%stmts(i)%kind)
            case (FAD_END_DO, FAD_END_IF, FAD_ELSE, FAD_TYPE_IS, &
                    FAD_CLASS_IS, FAD_CLASS_DEFAULT, FAD_END_SELECT)
                indent = max(1, indent - 1)
            end select
            block
                type(buffer_t) :: line
                call write_stmt(line, p, p%stmts(i))
                call put_wrapped(b, indent_of(indent), line%str(), limit)
            end block
            select case (p%stmts(i)%kind)
            case (FAD_DO, FAD_IF, FAD_ELSE, FAD_SELECT_TYPE, FAD_TYPE_IS, &
                    FAD_CLASS_IS, FAD_CLASS_DEFAULT)
                indent = indent + 1
            end select
            i = i + 1
        end do

        if (p%is_function) then
            call b%line("end function "//p%name)
        else
            call b%line("end subroutine "//p%name)
        end if
        text = b%str()
    end function emit_proc

    subroutine write_buffer_decls(b, p)
        !! Declare automatic arrays for loops whose reduction body is pure.
        !!
        !! The arrays preserve the loop's iteration order.  The first loop is
        !! therefore free of a loop-carried dependency, while the second loop
        !! performs the original ordered accumulation.  This is deliberately
        !! a narrow emitter optimization: an unproved dependency stays in the
        !! ordinary one-loop form.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        logical :: buffered
        integer :: i, end_stmt, loop_number, k, di
        character(len=128), allocatable :: names(:), accumulators(:)
        integer, allocatable :: terms(:)
        character(len=:), allocatable :: bounds

        loop_number = 0
        do i = 1, p%n_stmts
            if (p%stmts(i)%kind /= FAD_DO) cycle
            loop_number = loop_number + 1
            end_stmt = matching_end_do(p, i)
            call analyze_buffered_loop(p, i, end_stmt, buffered, accumulators, terms)
            if (.not. buffered) cycle
            do k = 1, size(accumulators)
                di = p%decl_index(trim(accumulators(k)))
                bounds = emit_expr(p, p%stmts(i)%lo)//":"// &
                    emit_expr(p, p%stmts(i)%hi)
                call b%line(indent_of(1)//trim(p%decls(di)%type_name)//" :: "// &
                    trim(buffer_name(p, loop_number, k))//"("//trim(bounds)//")")
            end do
        end do
    end subroutine write_buffer_decls

    integer function matching_end_do(p, first) result(last)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first
        integer :: i, depth

        last = first
        depth = 0
        do i = first, p%n_stmts
            select case (p%stmts(i)%kind)
            case (FAD_DO)
                depth = depth + 1
            case (FAD_END_DO)
                depth = depth - 1
                if (depth == 0) then
                    last = i
                    return
                end if
            end select
        end do
    end function matching_end_do

    subroutine analyze_buffered_loop(p, first, last, ok, accumulators, terms)
        !! Prove that a loop can be split without changing its arithmetic.
        !!
        !! Only scalar real reductions with a direct ``acc + term`` shape are
        !! accepted.  Every other body statement must be a scalar assignment,
        !! and no contribution may read a reduction variable.  In particular,
        !! array writes, calls, nested loops, and control flow are refused
        !! here because their independence needs richer alias information than
        !! the emitter has.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        logical, intent(out) :: ok
        character(len=128), allocatable, intent(out) :: accumulators(:)
        integer, allocatable, intent(out) :: terms(:)
        character(len=128), allocatable :: found_names(:)
        integer, allocatable :: found_terms(:)
        character(len=:), allocatable :: target
        integer :: i, k, n, di
        logical :: is_reduction

        allocate (found_names(max(0, last - first - 1)))
        allocate (found_terms(max(0, last - first - 1)))
        n = 0
        ok = .false.

        if (last <= first + 1) then
            allocate (accumulators(0), terms(0))
            return
        end if
        if (.not. unit_stride(p, first)) then
            allocate (accumulators(0), terms(0))
            return
        end if
        if (.not. stable_loop_bounds(p, first)) then
            allocate (accumulators(0), terms(0))
            return
        end if

        do i = first + 1, last - 1
            if (p%stmts(i)%kind /= FAD_ASSIGN) then
                allocate (accumulators(0), terms(0))
                return
            end if
            target = trim(p%stmts(i)%target)
            if (target == "!fad_raw" .or. index(target, "(") > 0 .or. &
                index(target, "%") > 0) then
                allocate (accumulators(0), terms(0))
                return
            end if
            di = p%decl_index(target)
            if (di <= 0 .or. .not. scalar_safe_decl(p%decls(di))) then
                allocate (accumulators(0), terms(0))
                return
            end if
            if (.not. safe_expression(p, p%stmts(i)%value)) then
                allocate (accumulators(0), terms(0))
                return
            end if
            call reduction_term(p, p%stmts(i), target, is_reduction, k)
            if (.not. is_reduction) cycle
            if (name_in_list(found_names, n, target)) then
                allocate (accumulators(0), terms(0))
                return
            end if
            n = n + 1
            found_names(n) = target
            found_terms(n) = k
        end do

        if (n == 0) then
            allocate (accumulators(0), terms(0))
            return
        end if

        do k = 1, n
            di = p%decl_index(trim(found_names(k)))
            if (di <= 0) then
                allocate (accumulators(0), terms(0))
                return
            end if
            if (.not. scalar_real_decl(p%decls(di))) then
                allocate (accumulators(0), terms(0))
                return
            end if
        end do

        do i = first + 1, last - 1
            target = trim(p%stmts(i)%target)
            call reduction_term(p, p%stmts(i), target, is_reduction, k)
            if (is_reduction) then
                if (mentions_any(p, k, found_names, n)) then
                    allocate (accumulators(0), terms(0))
                    return
                end if
            else if (mentions_any(p, p%stmts(i)%value, found_names, n)) then
                allocate (accumulators(0), terms(0))
                return
            end if
        end do

        allocate (accumulators(n), terms(n))
        accumulators = found_names(:n)
        terms = found_terms(:n)
        ok = .true.
    end subroutine analyze_buffered_loop

    logical function unit_stride(p, first) result(ok)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first

        ok = p%stmts(first)%step == 0
        if (.not. ok) then
            ok = p%stmts(first)%step > 0 .and. is_one_expr(p, p%stmts(first)%step)
        end if
    end function unit_stride

    logical function is_one_expr(p, idx) result(ok)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx

        ok = idx > 0 .and. idx <= p%n_exprs
        if (.not. ok) return
        ok = p%exprs(idx)%kind == FAD_CONST .and. &
            (trim(p%exprs(idx)%text) == "1" .or. trim(p%exprs(idx)%text) == "1_")
    end function is_one_expr

    logical function stable_loop_bounds(p, first) result(ok)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first
        integer :: i
        character(len=:), allocatable :: target

        ok = .true.
        do i = 1, first - 1
            if (p%stmts(i)%kind /= FAD_ASSIGN) then
                ok = .false.
                return
            end if
            target = trim(p%stmts(i)%target)
            if (target == "!fad_raw") then
                ok = .false.
                return
            end if
            if (expression_mentions_ci(p, p%stmts(first)%lo, target) .or. &
                expression_mentions_ci(p, p%stmts(first)%hi, target)) then
                ok = .false.
                return
            end if
        end do
        do i = first + 1, matching_end_do(p, first) - 1
            if (p%stmts(i)%kind /= FAD_ASSIGN) cycle
            target = trim(p%stmts(i)%target)
            if (same_fortran_name(target, trim(p%stmts(first)%target))) then
                ok = .false.
                return
            end if
        end do
    end function stable_loop_bounds

    logical function scalar_real_decl(d) result(ok)
        type(fad_decl_t), intent(in) :: d
        character(len=:), allocatable :: type_name

        ok = .false.
        if (.not. allocated(d%type_name)) return
        type_name = lower_string(trim(d%type_name))
        ok = index(type_name, "real") == 1 .and. .not. d%is_array .and. &
            .not. d%is_allocatable
    end function scalar_real_decl

    logical function scalar_safe_decl(d) result(ok)
        !! Scalar intrinsic temporaries cannot carry storage or finalization.
        type(fad_decl_t), intent(in) :: d
        character(len=:), allocatable :: type_name

        ok = .false.
        if (d%is_array .or. d%is_allocatable) return
        if (.not. allocated(d%type_name)) return
        type_name = lower_string(trim(d%type_name))
        ok = index(type_name, "integer") == 1 .or. &
            index(type_name, "real") == 1 .or. &
            index(type_name, "logical") == 1
    end function scalar_safe_decl

    recursive logical function safe_expression(p, idx) result(ok)
        !! Permit only side-effect-free intrinsic arithmetic in the first pass.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: i

        ok = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        select case (p%exprs(idx)%kind)
        case (FAD_CONST, FAD_VAR)
            ok = .true.
        case (FAD_BINOP, FAD_UNOP, FAD_INDEX)
            ok = .true.
        case (FAD_CALL)
            ok = intrinsic_name(p, p%exprs(idx)%text)
        case default
            return
        end select
        if (.not. ok) return
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            if (.not. safe_expression(p, p%exprs(idx)%args(i))) then
                ok = .false.
                return
            end if
        end do
    end function safe_expression

    logical function intrinsic_name(p, raw) result(ok)
        type(fad_proc_t), intent(in) :: p
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: name

        name = lower_string(trim(raw))
        if (p%decl_index(name) > 0) then
            ok = .false.
            return
        end if
        select case (name)
        case ("abs", "acos", "asin", "atan", "atan2", "cos", "cosh", &
                "exp", "log", "log10", "sin", "sinh", "sqrt", "tan", "tanh")
            ok = .true.
        case default
            ok = .false.
        end select
    end function intrinsic_name

    subroutine reduction_term(p, s, target, found, term)
        type(fad_proc_t), intent(in) :: p
        type(fad_stmt_t), intent(in) :: s
        character(len=*), intent(in) :: target
        logical, intent(out) :: found
        integer, intent(out) :: term
        integer :: left, right

        found = .false.
        term = 0
        if (s%kind /= FAD_ASSIGN .or. index(target, "(") > 0) return
        if (s%value <= 0 .or. s%value > p%n_exprs) return
        if (p%exprs(s%value)%kind /= FAD_BINOP) return
        if (trim(p%exprs(s%value)%text) /= "+") return
        if (.not. allocated(p%exprs(s%value)%args)) return
        if (size(p%exprs(s%value)%args) /= 2) return
        left = p%exprs(s%value)%args(1)
        right = p%exprs(s%value)%args(2)
        if (is_named_expr(p, left, target)) then
            if (.not. expression_mentions_ci(p, right, target)) then
                found = .true.
                term = right
            end if
        else if (is_named_expr(p, right, target)) then
            if (.not. expression_mentions_ci(p, left, target)) then
                found = .true.
                term = left
            end if
        end if
    end subroutine reduction_term

    logical function is_named_expr(p, idx, name) result(found)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name

        found = idx > 0 .and. idx <= p%n_exprs
        if (.not. found) return
        found = p%exprs(idx)%kind == FAD_VAR .and. &
            same_fortran_name(trim(p%exprs(idx)%text), trim(name))
    end function is_named_expr

    logical function name_in_list(names, count, name) result(found)
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, count
            if (same_fortran_name(trim(names(i)), trim(name))) then
                found = .true.
                return
            end if
        end do
    end function name_in_list

    recursive logical function expression_mentions_ci(p, idx, name) result(found)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (same_fortran_name(trim(p%exprs(idx)%text), trim(name))) then
            found = .true.
            return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            if (expression_mentions_ci(p, p%exprs(idx)%args(i), name)) then
                found = .true.
                return
            end if
        end do
    end function expression_mentions_ci

    logical function mentions_any(p, idx, names, count) result(found)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, count
        character(len=128), intent(in) :: names(:)
        integer :: i

        found = .false.
        do i = 1, count
            if (expression_mentions_ci(p, idx, names(i))) then
                found = .true.
                return
            end if
        end do
    end function mentions_any

    function lower_string(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: i

        do i = 1, len(text)
            out(i:i) = lower_ascii(text(i:i))
        end do
    end function lower_string

    function buffer_name(p, loop_number, accumulator_number) result(name)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: loop_number, accumulator_number
        character(len=128) :: name
        character(len=32) :: suffix

        write (suffix, '(i0,"_",i0)') loop_number, accumulator_number
        name = "fad_buffer_"//trim(suffix)
        if (p%decl_index(trim(name)) > 0) name = trim(name)//"_x"
    end function buffer_name

    subroutine write_buffered_loop(b, p, first, last, indent, limit, loop_number, &
            accumulators)
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last, indent, limit, loop_number
        character(len=128), intent(in) :: accumulators(:)
        type(buffer_t) :: line
        character(len=128) :: target
        integer :: pass, i, k
        logical :: is_reduction
        integer :: term

        do pass = 1, 2
            call line%reset()
            call write_stmt(line, p, p%stmts(first))
            call put_wrapped(b, indent_of(indent), line%str(), limit)
            do i = first + 1, last - 1
                target = trim(p%stmts(i)%target)
                call reduction_term(p, p%stmts(i), target, is_reduction, term)
                if (pass == 1 .and. is_reduction) then
                    k = accumulator_number(accumulators, target)
                    call line%reset()
                    call line%put(trim(buffer_name(p, loop_number, k))//"("// &
                        trim(p%stmts(first)%target)//") = ")
                    call write_expr(line, p, term)
                else if (pass == 1) then
                    call line%reset()
                    call write_stmt(line, p, p%stmts(i))
                else
                    cycle
                end if
                call put_wrapped(b, indent_of(indent + 1), line%str(), limit)
            end do
            if (pass == 2) then
                do k = 1, size(accumulators)
                    call line%reset()
                    call line%put(trim(accumulators(k))//" = "// &
                        trim(accumulators(k))//" + "// &
                        trim(buffer_name(p, loop_number, k))//"("// &
                        trim(p%stmts(first)%target)//")")
                    call put_wrapped(b, indent_of(indent + 1), line%str(), limit)
                end do
            end if
            call b%line(indent_of(indent)//"end do")
        end do
    end subroutine write_buffered_loop

    integer function accumulator_number(names, name) result(number)
        character(len=128), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: i

        number = 0
        do i = 1, size(names)
            if (same_fortran_name(trim(names(i)), trim(name))) then
                number = i
                return
            end if
        end do
    end function accumulator_number

    subroutine write_header(b, p, limit)
        !! The `function`/`subroutine` statement with its dummy argument list.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: limit
        character(len=:), allocatable :: line
        integer :: i

        ! Code fortad writes itself does only arithmetic on its arguments -
        ! no I/O, no saved state - so saying `pure` lets the consumer's
        ! compiler hoist, vectorise, and parallelise calls to it. A procedure
        ! that calls something fortad cannot see gets no such claim.
        ! The dummy list is wrapped like any other statement. A procedure with
        ! thirteen arguments produced a 333-character line; free-form Fortran
        ! stops at 132, and gfortran accepting it quietly does not make it
        ! portable.
        line = ""
        if (p%is_elemental) line = "elemental "
        if (p%is_pure) line = line//"pure "
        if (p%is_function) then
            line = line//"function "//p%name//"("
        else
            line = line//"subroutine "//p%name//"("
        end if
        if (allocated(p%params)) then
            do i = 1, size(p%params)
                if (i > 1) line = line//", "
                line = line//trim(p%params(i))
            end do
        end if
        line = line//")"
        if (p%is_function .and. allocated(p%result_name)) then
            line = line//" result("//p%result_name//")"
        end if
        call put_wrapped(b, "", line, limit)
    end subroutine write_header

    function emit_decl_line(d) result(line)
        !! One declaration, attributes in a stable order.
        type(fad_decl_t), intent(in) :: d
        character(len=:), allocatable :: line

        line = d%type_name
        if (d%is_value) line = line//", value"
        if (d%is_optional) line = line//", optional"
        select case (d%intent)
        case (FAD_INTENT_IN)
            line = line//", intent(in)"
        case (FAD_INTENT_OUT)
            line = line//", intent(out)"
        case (FAD_INTENT_INOUT)
            line = line//", intent(inout)"
        end select
        if (d%is_allocatable) line = line//", allocatable"
        if (d%is_contiguous) line = line//", contiguous"
        if (d%is_array) then
            if (allocated(d%dims)) then
                line = line//", dimension("//d%dims//")"
            else
                line = line//", dimension(:)"
            end if
        end if
        line = line//" :: "//d%name
    end function emit_decl_line

    subroutine write_stmt(b, p, s)
        !! One statement.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        type(fad_stmt_t), intent(in) :: s

        select case (s%kind)
        case (FAD_ASSIGN)
            ! A registered rule supplies a whole statement, already written as
            ! Fortran. It is emitted verbatim rather than rebuilt.
            if (s%target == "!fad_raw") then
                call write_expr(b, p, s%value)
                return
            end if
            call b%put(s%target)
            call b%put(" = ")
            call write_expr(b, p, s%value)
        case (FAD_DO)
            call b%put("do "//s%target//" = ")
            call write_expr(b, p, s%lo)
            call b%put(", ")
            call write_expr(b, p, s%hi)
            if (s%step /= 0) then
                call b%put(", ")
                call write_expr(b, p, s%step)
            end if
        case (FAD_CALL_STMT)
            call b%put("call "//s%target//"(")
            block
                integer :: k
                do k = 1, size(s%call_args)
                    if (k > 1) call b%put(", ")
                    if (allocated(s%call_arg_names)) then
                        if (len_trim(s%call_arg_names(k)) > 0) then
                            call b%put(trim(s%call_arg_names(k))//"=")
                        end if
                    end if
                    call write_expr(b, p, s%call_args(k))
                end do
            end block
            call b%put(")")
        case (FAD_END_DO)
            call b%put("end do")
        case (FAD_IF)
            call b%put("if (")
            call write_expr(b, p, s%value)
            call b%put(") then")
        case (FAD_ELSE)
            call b%put("else")
        case (FAD_END_IF)
            call b%put("end if")
        case (FAD_SELECT_TYPE)
            call b%put("select type (")
            if (allocated(s%target)) call b%put(trim(s%target)//" => ")
            call write_expr(b, p, s%value)
            call b%put(")")
        case (FAD_TYPE_IS)
            call b%put("type is ("//s%target//")")
        case (FAD_CLASS_IS)
            call b%put("class is ("//s%target//")")
        case (FAD_CLASS_DEFAULT)
            call b%put("class default")
        case (FAD_END_SELECT)
            call b%put("end select")
        case (FAD_DIRECTIVE)
            call b%put(s%target)
        case (FAD_ALLOCATE)
            call write_allocate_stmt(b, p, s)
        case (FAD_DEALLOCATE)
            call b%put("deallocate(")
            call write_expr(b, p, s%allocation_args(1))
            call b%put(")")
        case (FAD_MOVE_ALLOC)
            call b%put("call move_alloc(")
            call write_expr(b, p, s%call_args(1))
            call b%put(", ")
            call write_expr(b, p, s%call_args(2))
            call b%put(")")
        case default
            call b%put("! unsupported statement")
        end select
    end subroutine write_stmt

    subroutine write_allocate_stmt(b, p, s)
        !! Emit the bounded one-owner allocation form represented by the IR.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        type(fad_stmt_t), intent(in) :: s
        integer :: i

        call b%put("allocate(")
        call write_expr(b, p, s%allocation_args(1))
        if (size(s%allocation_args) > 1) then
            call b%put("(")
            do i = 2, size(s%allocation_args)
                if (i > 2) call b%put(", ")
                call write_expr(b, p, s%allocation_args(i))
            end do
            call b%put(")")
        end if
        if (s%allocation_source > 0) then
            call b%put(", source=")
            call write_expr(b, p, s%allocation_source)
        else if (s%allocation_mold > 0) then
            call b%put(", mold=")
            call write_expr(b, p, s%allocation_mold)
        end if
        call b%put(")")
    end subroutine write_allocate_stmt

    subroutine write_omp_directive(b, p, s, directive_index, indent, limit)
        !! Add race-free data-sharing clauses from the final IR.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        type(fad_stmt_t), intent(in) :: s
        integer, intent(in) :: directive_index, limit
        character(len=*), intent(in) :: indent
        character(len=:), allocatable :: text, acc_text, candidates, reductions
        character(len=:), allocatable :: to_names, tofrom_names, name
        integer :: do_stmt, end_stmt, depth, i, pos

        text = "!$omp target teams distribute parallel do"
        acc_text = ACC_MARKER//" parallel loop"
        candidates = ""
        if (index(s%target, "|") > 0) then
            candidates = s%target(index(s%target, "|") + 1:)
        end if
        do_stmt = directive_index
        if (do_stmt <= 0 .or. do_stmt > p%n_stmts) then
            do_stmt = 0
        else if (p%stmts(do_stmt)%kind /= FAD_DO) then
            do_stmt = 0
        end if
        if (do_stmt == 0) then
            do i = directive_index + 1, p%n_stmts
                if (p%stmts(i)%kind == FAD_DO) then
                    do_stmt = i
                    exit
                end if
            end do
        end if
        if (do_stmt == 0) then
            call write_directive(b, text, indent, limit)
            return
        end if
        end_stmt = do_stmt
        depth = 0
        do i = do_stmt, p%n_stmts
            select case (p%stmts(i)%kind)
            case (FAD_DO)
                depth = depth + 1
            case (FAD_END_DO)
                depth = depth - 1
                if (depth == 0) then
                    end_stmt = i
                    exit
                end if
            end select
        end do

        reductions = ""
        pos = 1
        do while (next_name(candidates, pos, name))
            if (has_assignment_target(p, do_stmt, end_stmt, name)) then
                call append_name(reductions, name)
            end if
        end do
        if (len_trim(reductions) > 0) then
            pos = 1
            do while (next_name(reductions, pos, name))
                text = text//" reduction(+:"//trim(name)//")"
            end do
        end if

        to_names = ""
        tofrom_names = ""
        if (allocated(p%params)) then
            do i = 1, size(p%params)
                name = trim(p%params(i))
                if (loop_writes_param(p, do_stmt, end_stmt, name)) then
                    call append_name(tofrom_names, name)
                else if (loop_reads_param(p, do_stmt, end_stmt, name)) then
                    call append_name(to_names, name)
                end if
            end do
        end if
        if (len_trim(to_names) > 0) then
            text = text//" map(to:"//trim(to_names)//")"
            acc_text = acc_text//" copyin("//trim(to_names)//")"
        end if
        if (len_trim(tofrom_names) > 0) then
            text = text//" map(tofrom:"//trim(tofrom_names)//")"
            acc_text = acc_text//" copy("//trim(tofrom_names)//")"
        end if
        if (len_trim(reductions) > 0) then
            pos = 1
            do while (next_name(reductions, pos, name))
                acc_text = acc_text//" reduction(+:"//trim(name)//")"
            end do
        end if
        call write_directive(b, text, indent, limit)
        call write_directive(b, acc_text, indent, limit, ACC_MARKER)
    end subroutine write_omp_directive

    logical function next_name(list, pos, name) result(found)
        character(len=*), intent(in) :: list
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: name
        integer :: comma, start

        found = .false.
        name = ""
        do while (pos <= len_trim(list))
            if (list(pos:pos) == " ") then
                pos = pos + 1
                cycle
            end if
            if (pos > len_trim(list)) return
            start = pos
            comma = index(list(pos:), ",")
            if (comma == 0) then
                name = trim(list(start:))
                pos = len(list) + 1
            else
                name = trim(list(start:pos + comma - 2))
                pos = pos + comma
            end if
            if (len_trim(name) > 0) then
                found = .true.
                return
            end if
        end do
    end function next_name

    subroutine append_name(list, name)
        character(len=:), allocatable, intent(inout) :: list
        character(len=*), intent(in) :: name

        if (len_trim(list) > 0) list = list//","
        list = list//trim(name)
    end subroutine append_name

    logical function loop_reads_param(p, first, last, name) result(found)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: name
        integer :: i, k

        found = .false.
        do i = first + 1, last - 1
            select case (p%stmts(i)%kind)
            case (FAD_ASSIGN, FAD_IF)
                if (expression_mentions(p, p%stmts(i)%value, name)) then
                    found = .true.
                    return
                end if
            case (FAD_DO)
                if (expression_mentions(p, p%stmts(i)%lo, name) .or. &
                    expression_mentions(p, p%stmts(i)%hi, name) .or. &
                    expression_mentions(p, p%stmts(i)%step, name)) then
                    found = .true.
                    return
                end if
            case (FAD_CALL_STMT)
                if (allocated(p%stmts(i)%call_args)) then
                    do k = 1, size(p%stmts(i)%call_args)
                        if (expression_mentions(p, p%stmts(i)%call_args(k), name)) then
                            found = .true.
                            return
                        end if
                    end do
                end if
            end select
        end do
    end function loop_reads_param

    logical function loop_writes_param(p, first, last, name) result(found)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = first + 1, last - 1
            if (p%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (base_name(p%stmts(i)%target) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function loop_writes_param

    recursive logical function expression_mentions(p, idx, name) result(found)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        if (idx <= 0 .or. idx > p%n_exprs) return
        if (trim(p%exprs(idx)%text) == trim(name)) then
            found = .true.
            return
        end if
        if (.not. allocated(p%exprs(idx)%args)) return
        do i = 1, size(p%exprs(idx)%args)
            if (expression_mentions(p, p%exprs(idx)%args(i), name)) then
                found = .true.
                return
            end if
        end do
    end function expression_mentions

    logical function has_assignment_target(p, first, last, name) result(found)
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = first + 1, last - 1
            if (p%stmts(i)%kind /= FAD_ASSIGN) cycle
            if (base_name(p%stmts(i)%target) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function has_assignment_target

    function base_name(target) result(base)
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: base
        integer :: paren

        paren = index(target, "(")
        if (paren > 0) then
            base = trim(target(:paren - 1))
        else
            base = trim(target)
        end if
    end function base_name

    subroutine write_directive(b, line, indent, limit, prefix)
        !! Wrap a free-form directive with directive continuations.
        type(buffer_t), intent(inout) :: b
        character(len=*), intent(in) :: line, indent
        integer, intent(in) :: limit
        character(len=*), intent(in), optional :: prefix
        character(len=:), allocatable :: continuation
        integer :: start, remaining, room, cut

        continuation = "!$omp&"
        if (present(prefix)) continuation = trim(prefix)//"&"

        room = limit - len(indent)
        if (len(line) <= room) then
            call b%line(indent//line)
            return
        end if

        start = 1
        do
            remaining = len(line) - start + 1
            if (remaining <= room) then
                if (start == 1) then
                    call b%line(indent//line(start:))
                else
                    call b%line(indent//continuation//" "//line(start:))
                end if
                return
            end if
            cut = directive_break(line, start, start + room - 3)
            if (cut < start) then
                call b%line(indent//line(start:))
                return
            end if
            if (start == 1) then
                call b%line(indent//line(start:cut)//" &")
            else
                call b%line(indent//continuation//" "//line(start:cut)//" &")
            end if
            start = cut + 1
            do while (start <= len(line))
                if (line(start:start) /= " ") exit
                start = start + 1
            end do
        end do
    end subroutine write_directive

    integer function directive_break(line, lo, hi) result(cut)
        character(len=*), intent(in) :: line
        integer, intent(in) :: lo, hi
        integer :: i

        cut = lo - 1
        do i = min(hi, len(line)), lo + 1, -1
            if (line(i:i) == " ") then
                cut = i - 1
                return
            end if
        end do
    end function directive_break

    recursive subroutine write_expr(b, p, idx)
        !! One expression, parenthesised only where precedence requires it.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        integer :: i, k

        if (idx <= 0 .or. idx > p%n_exprs) then
            call b%put("!<invalid>")
            return
        end if

        k = p%exprs(idx)%kind
        select case (k)
        case (FAD_CONST, FAD_VAR)
            call b%put(p%exprs(idx)%text)
        case (FAD_BINOP)
            call write_operand(b, p, p%exprs(idx)%args(1), &
                prec(p%exprs(idx)%text), .false.)
            call b%put(" ")
            call b%put(trim(p%exprs(idx)%text))
            call b%put(" ")
            call write_operand(b, p, p%exprs(idx)%args(2), &
                prec(p%exprs(idx)%text), .true.)
        case (FAD_UNOP)
            call b%put(trim(p%exprs(idx)%text))
            call write_operand(b, p, p%exprs(idx)%args(1), PREC_UNARY, .false.)
        case (FAD_CALL, FAD_INDEX)
            call b%put(p%exprs(idx)%text)
            call b%put("(")
            do i = 1, size(p%exprs(idx)%args)
                if (i > 1) call b%put(", ")
                if (k == FAD_CALL) then
                    if (allocated(p%exprs(idx)%call_arg_names)) then
                        if (len_trim(p%exprs(idx)%call_arg_names(i)) > 0) then
                            call b%put(trim(p%exprs(idx)%call_arg_names(i))//"=")
                        end if
                    end if
                end if
                call write_expr(b, p, p%exprs(idx)%args(i))
            end do
            call b%put(")")
        case default
            call b%put("!<unknown>")
        end select
    end subroutine write_expr

    recursive subroutine write_operand(b, p, idx, outer, is_right)
        !! Write a child, parenthesised when the parent binds more tightly, or
        !! when it is the right operand of an equally binding operator.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx, outer
        logical, intent(in) :: is_right
        logical :: needs
        integer :: inner

        needs = .false.
        if (idx > 0 .and. idx <= p%n_exprs) then
            select case (p%exprs(idx)%kind)
            case (FAD_BINOP, FAD_UNOP)
                inner = prec(p%exprs(idx)%text)
                needs = inner < outer .or. (inner == outer .and. is_right)
            end select
        end if

        if (needs) call b%put("(")
        call write_expr(b, p, idx)
        if (needs) call b%put(")")
    end subroutine write_operand

    function emit_expr(p, idx) result(text)
        !! An expression as standalone text. Convenience for callers that need
        !! a string rather than an append; the emitter itself uses `write_expr`.
        type(fad_proc_t), intent(in) :: p
        integer, intent(in) :: idx
        character(len=:), allocatable :: text
        type(buffer_t) :: b

        call write_expr(b, p, idx)
        text = b%str()
    end function emit_expr

    integer function prec(op) result(level)
        !! Fortran operator precedence, higher binds tighter.
        character(len=*), intent(in) :: op

        select case (trim(op))
        case ("**")
            level = 6
        case ("*", "/")
            level = 4
        case ("+", "-")
            level = 3
        case ("==", "/=", "<", "<=", ">", ">=")
            level = 2
        case default
            level = 1
        end select
    end function prec

end module fortad_emit
