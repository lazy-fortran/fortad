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
    use fortad_ir, only: fad_proc_t, fad_expr_t, fad_stmt_t, fad_decl_t, &
                        FAD_CONST, FAD_VAR, FAD_BINOP, FAD_UNOP, FAD_CALL, &
                        FAD_INDEX, FAD_ASSIGN, FAD_DO, FAD_END_DO, FAD_IF, &
                        FAD_ELSE, FAD_END_IF, FAD_INTENT_IN, FAD_INTENT_OUT, &
                        FAD_INTENT_INOUT
    use fortad_text, only: buffer_t
    implicit none
    private

    public :: emit_proc, emit_expr, emit_decl_line, write_expr

    !! Unary minus binds tighter than the binary arithmetic operators but
    !! looser than exponentiation.
    integer, parameter :: PREC_UNARY = 5

contains

    function emit_proc(p) result(text)
        !! Emit a complete procedure as Fortran source text.
        type(fad_proc_t), intent(in) :: p
        character(len=:), allocatable :: text
        type(buffer_t) :: b
        integer :: i, indent

        call write_header(b, p)
        call b%line("    implicit none")
        do i = 1, p%n_decls
            call b%line("    "//emit_decl_line(p%decls(i)))
        end do
        call b%line("")

        indent = 1
        do i = 1, p%n_stmts
            select case (p%stmts(i)%kind)
            case (FAD_END_DO, FAD_END_IF, FAD_ELSE)
                indent = max(1, indent - 1)
            end select
            call b%put(spaces(indent))
            call write_stmt(b, p, p%stmts(i))
            call b%put(new_line('a'))
            select case (p%stmts(i)%kind)
            case (FAD_DO, FAD_IF, FAD_ELSE)
                indent = indent + 1
            end select
        end do

        if (p%is_function) then
            call b%line("end function "//p%name)
        else
            call b%line("end subroutine "//p%name)
        end if
        text = b%str()
    end function emit_proc

    subroutine write_header(b, p)
        !! The `function`/`subroutine` statement with its dummy argument list.
        type(buffer_t), intent(inout) :: b
        type(fad_proc_t), intent(in) :: p
        integer :: i

        if (p%is_function) then
            call b%put("function "//p%name//"(")
        else
            call b%put("subroutine "//p%name//"(")
        end if
        if (allocated(p%params)) then
            do i = 1, size(p%params)
                if (i > 1) call b%put(", ")
                call b%put(trim(p%params(i)))
            end do
        end if
        call b%put(")")
        if (p%is_function .and. allocated(p%result_name)) then
            call b%put(" result("//p%result_name//")")
        end if
        call b%put(new_line('a'))
    end subroutine write_header

    function emit_decl_line(d) result(line)
        !! One declaration, attributes in a stable order.
        type(fad_decl_t), intent(in) :: d
        character(len=:), allocatable :: line

        line = d%type_name
        select case (d%intent)
        case (FAD_INTENT_IN)
            line = line//", intent(in)"
        case (FAD_INTENT_OUT)
            line = line//", intent(out)"
        case (FAD_INTENT_INOUT)
            line = line//", intent(inout)"
        end select
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
        case default
            call b%put("! unsupported statement")
        end select
    end subroutine write_stmt

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

    pure function spaces(n) result(s)
        !! `n` levels of 4-space indentation.
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        integer :: i

        s = ""
        do i = 1, n
            s = s//"    "
        end do
    end function spaces

end module fortad_emit
