module fortad_ir
    !! fortad's differentiation IR.
    !!
    !! An expression arena plus a flat statement list per procedure. Expressions
    !! are referenced by integer index, exactly as fortfront's AST arena does, so
    !! lowering is index remapping rather than pointer surgery, and a derivative
    !! expression can reference a primal expression without copying it.
    !!
    !! The IR is deliberately smaller than Fortran. It carries what a derivative
    !! transformation needs and nothing else; anything outside the supported
    !! subset is refused by name at lowering time rather than mistranslated.
    use fortad_kinds, only: dp
    implicit none
    private

    ! Expression kinds.
    integer, parameter, public :: FAD_CONST = 1     !! literal, text in `text`
    integer, parameter, public :: FAD_VAR = 2       !! variable reference by name
    integer, parameter, public :: FAD_BINOP = 3     !! text = operator, 2 args
    integer, parameter, public :: FAD_UNOP = 4      !! text = operator, 1 arg
    integer, parameter, public :: FAD_CALL = 5      !! text = name, n args
    integer, parameter, public :: FAD_INDEX = 6     !! text = array name, n subscripts

    ! Statement kinds.
    integer, parameter, public :: FAD_ASSIGN = 1    !! target_text = value
    integer, parameter, public :: FAD_DO = 2        !! do var = a, b [, c]
    integer, parameter, public :: FAD_END_DO = 3
    integer, parameter, public :: FAD_IF = 4        !! if (cond) then
    integer, parameter, public :: FAD_ELSE = 5
    integer, parameter, public :: FAD_END_IF = 6
    !! `call name(args...)`. The arguments live in `call_args`; the name is in
    !! `target`. fortad never differentiates the body of such a call - it
    !! applies a registered rule or refuses.
    integer, parameter, public :: FAD_CALL_STMT = 7

    ! Argument intents, mirroring fortfront's constants.
    integer, parameter, public :: FAD_INTENT_NONE = 0
    integer, parameter, public :: FAD_INTENT_IN = 1
    integer, parameter, public :: FAD_INTENT_OUT = 2
    integer, parameter, public :: FAD_INTENT_INOUT = 3

    type, public :: fad_expr_t
        !! One node of the expression arena.
        integer :: kind = 0
        character(len=:), allocatable :: text
        integer, allocatable :: args(:)
        !! Index of the array subscript base for FAD_INDEX, else 0.
        integer :: rank = 0
    end type fad_expr_t

    type, public :: fad_stmt_t
        !! One statement. Structured statements (do, if) are bracketed by
        !! explicit end markers so the list stays flat and reversible.
        integer :: kind = 0
        character(len=:), allocatable :: target
        integer :: value = 0
        !! Loop bounds for FAD_DO, condition for FAD_IF.
        integer :: lo = 0, hi = 0, step = 0
        !! Source line in the primal, for provenance comments.
        integer :: line = 0
        !! Actual arguments of a FAD_CALL_STMT.
        integer, allocatable :: call_args(:)
    end type fad_stmt_t

    type, public :: fad_decl_t
        !! A declared entity: dummy argument, result, or local.
        character(len=:), allocatable :: name
        character(len=:), allocatable :: type_name   !! "real(dp)", "integer"
        integer :: intent = FAD_INTENT_NONE
        logical :: is_array = .false.
        logical :: is_contiguous = .false.
        logical :: is_result = .false.
        !! Verbatim dimension text, e.g. "n" or ":,:" - emitted unchanged.
        character(len=:), allocatable :: dims
    end type fad_decl_t

    type, public :: fad_proc_t
        !! One procedure in IR form.
        character(len=:), allocatable :: name
        character(len=:), allocatable :: result_name
        logical :: is_function = .false.
        !! Kind suffix for real literals fortad emits, taken from the primal's
        !! own declarations: "d0" for real(8), "e0" for real(4), "_wp" when the
        !! primal names a kind parameter. Emitting `1.0_dp` into a procedure
        !! that never defined `dp` is a compile error, so this is not cosmetic.
        character(len=:), allocatable :: real_suffix
        !! Whether the generated procedure may be declared `pure`. Code fortad
        !! writes itself always could be; a call to a procedure fortad cannot
        !! see might not be, and claiming purity it cannot verify would be a
        !! promise to the compiler that the compiler will act on.
        logical :: is_pure = .true.
        type(fad_decl_t), allocatable :: decls(:)
        character(len=:), allocatable :: params(:)
        type(fad_expr_t), allocatable :: exprs(:)
        type(fad_stmt_t), allocatable :: stmts(:)
        integer :: n_exprs = 0
        integer :: n_stmts = 0
        integer :: n_decls = 0
    contains
        procedure :: add_expr => proc_add_expr
        procedure :: add_stmt => proc_add_stmt
        procedure :: add_decl => proc_add_decl
        procedure :: decl_index => proc_decl_index
    end type fad_proc_t

    public :: expr_const, expr_var, expr_binop, expr_unop, expr_call
    public :: fad_expr_equal

contains

    integer function proc_add_expr(self, e) result(idx)
        !! Append an expression, growing the arena geometrically.
        class(fad_proc_t), intent(inout) :: self
        type(fad_expr_t), intent(in) :: e
        type(fad_expr_t), allocatable :: tmp(:)
        integer :: cap

        if (.not. allocated(self%exprs)) allocate (self%exprs(64))
        cap = size(self%exprs)
        if (self%n_exprs >= cap) then
            allocate (tmp(2*cap))
            tmp(1:cap) = self%exprs
            call move_alloc(tmp, self%exprs)
        end if
        self%n_exprs = self%n_exprs + 1
        self%exprs(self%n_exprs) = e
        idx = self%n_exprs
    end function proc_add_expr

    integer function proc_add_stmt(self, s) result(idx)
        !! Append a statement.
        class(fad_proc_t), intent(inout) :: self
        type(fad_stmt_t), intent(in) :: s
        type(fad_stmt_t), allocatable :: tmp(:)
        integer :: cap

        if (.not. allocated(self%stmts)) allocate (self%stmts(64))
        cap = size(self%stmts)
        if (self%n_stmts >= cap) then
            allocate (tmp(2*cap))
            tmp(1:cap) = self%stmts
            call move_alloc(tmp, self%stmts)
        end if
        self%n_stmts = self%n_stmts + 1
        self%stmts(self%n_stmts) = s
        idx = self%n_stmts
    end function proc_add_stmt

    integer function proc_add_decl(self, d) result(idx)
        !! Append a declaration, replacing any existing one of the same name.
        class(fad_proc_t), intent(inout) :: self
        type(fad_decl_t), intent(in) :: d
        type(fad_decl_t), allocatable :: tmp(:)
        integer :: cap, existing

        existing = self%decl_index(d%name)
        if (existing > 0) then
            self%decls(existing) = d
            idx = existing
            return
        end if
        if (.not. allocated(self%decls)) allocate (self%decls(32))
        cap = size(self%decls)
        if (self%n_decls >= cap) then
            allocate (tmp(2*cap))
            tmp(1:cap) = self%decls
            call move_alloc(tmp, self%decls)
        end if
        self%n_decls = self%n_decls + 1
        self%decls(self%n_decls) = d
        idx = self%n_decls
    end function proc_add_decl

    integer function proc_decl_index(self, name) result(idx)
        !! Index of the declaration for `name`, or 0.
        class(fad_proc_t), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: i

        idx = 0
        do i = 1, self%n_decls
            if (allocated(self%decls(i)%name)) then
                if (self%decls(i)%name == name) then
                    idx = i
                    return
                end if
            end if
        end do
    end function proc_decl_index

    type(fad_expr_t) function expr_const(text) result(e)
        !! A literal constant carrying its source text verbatim.
        character(len=*), intent(in) :: text

        e%kind = FAD_CONST
        e%text = text
        allocate (e%args(0))
    end function expr_const

    type(fad_expr_t) function expr_var(name) result(e)
        !! A read of a named variable.
        character(len=*), intent(in) :: name

        e%kind = FAD_VAR
        e%text = name
        allocate (e%args(0))
    end function expr_var

    type(fad_expr_t) function expr_binop(op, a, b) result(e)
        !! A binary operation over two expression indices.
        character(len=*), intent(in) :: op
        integer, intent(in) :: a, b

        e%kind = FAD_BINOP
        e%text = op
        e%args = [a, b]
    end function expr_binop

    type(fad_expr_t) function expr_unop(op, a) result(e)
        !! A unary operation over one expression index.
        character(len=*), intent(in) :: op
        integer, intent(in) :: a

        e%kind = FAD_UNOP
        e%text = op
        e%args = [a]
    end function expr_unop

    type(fad_expr_t) function expr_call(name, args) result(e)
        !! An intrinsic or procedure reference.
        character(len=*), intent(in) :: name
        integer, intent(in) :: args(:)

        e%kind = FAD_CALL
        e%text = name
        e%args = args
    end function expr_call

    logical function fad_expr_equal(a, b) result(same)
        !! Structural equality, used by constant folding and CSE.
        type(fad_expr_t), intent(in) :: a, b

        same = .false.
        if (a%kind /= b%kind) return
        if (allocated(a%text) .neqv. allocated(b%text)) return
        if (allocated(a%text)) then
            if (a%text /= b%text) return
        end if
        if (size(a%args) /= size(b%args)) return
        if (size(a%args) > 0) then
            if (any(a%args /= b%args)) return
        end if
        same = .true.
    end function fad_expr_equal

end module fortad_ir
