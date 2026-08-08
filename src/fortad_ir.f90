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

    !! Hash buckets for expression sharing. A power of two so the modulo is
    !! cheap, and large enough that a kernel's expression count stays well
    !! under it.
    integer, parameter :: BUCKETS = 4096

    ! Expression kinds.
    integer, parameter, public :: FAD_CONST = 1 !! literal, text in `text`
    integer, parameter, public :: FAD_VAR = 2 !! variable reference by name
    integer, parameter, public :: FAD_BINOP = 3 !! text = operator, 2 args
    integer, parameter, public :: FAD_UNOP = 4 !! text = operator, 1 arg
    integer, parameter, public :: FAD_CALL = 5 !! text = name, n args
    integer, parameter, public :: FAD_INDEX = 6 !! text = array name, n subscripts

    ! Statement kinds.
    integer, parameter, public :: FAD_ASSIGN = 1 !! target_text = value
    integer, parameter, public :: FAD_DO = 2 !! do var = a, b [, c]
    integer, parameter, public :: FAD_END_DO = 3
    integer, parameter, public :: FAD_IF = 4 !! if (cond) then
    integer, parameter, public :: FAD_ELSE = 5
    integer, parameter, public :: FAD_END_IF = 6
    !! `call name(args...)`. The arguments live in `call_args`; the name is in
    !! `target`. fortad never differentiates the body of such a call - it
    !! applies a registered rule or refuses.
    integer, parameter, public :: FAD_CALL_STMT = 7
    !! A compiler directive marker, verbatim in `target`.  The emitter expands
    !! the fused-loop marker to the OpenMP-target and OpenACC forms.
    integer, parameter, public :: FAD_DIRECTIVE = 8
    !! Runtime-polymorphic dispatch. The selector and guard arms remain
    !! structural statements so the generated derivative follows the same
    !! dynamic type as the primal.
    integer, parameter, public :: FAD_SELECT_TYPE = 9
    integer, parameter, public :: FAD_TYPE_IS = 10
    integer, parameter, public :: FAD_CLASS_IS = 11
    integer, parameter, public :: FAD_CLASS_DEFAULT = 12
    integer, parameter, public :: FAD_END_SELECT = 13
    !! Explicit ownership operations. Allocation state belongs to the object,
    !! not to one SSA value version.
    integer, parameter, public :: FAD_ALLOCATE = 14
    integer, parameter, public :: FAD_DEALLOCATE = 15
    integer, parameter, public :: FAD_MOVE_ALLOC = 16

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
        !! Keyword names for a procedure call, aligned with ``args``.  A
        !! blank entry is a positional actual.
        character(len=:), allocatable :: call_arg_names(:)
        !! Index of the array subscript base for FAD_INDEX, else 0.
        integer :: rank = 0
        !! FortFront's storage facts for a component path.  These facts are
        !! copied with the expression so derivative leaves can distinguish an
        !! active component from another component of the same derived object.
        logical :: is_component_path = .false.
        logical :: component_is_allocatable = .false.
        logical :: component_is_pointer = .false.
        logical :: component_is_target = .false.
        logical :: component_is_polymorphic = .false.
        logical :: component_is_global = .false.
        logical :: component_is_real = .false.
        integer :: component_rank = -1
        character(len=:), allocatable :: component_type_name
        character(len=:), allocatable :: component_original_path
    end type fad_expr_t

    type, public :: fad_stmt_t
        !! One statement. Structured statements (do, if) are bracketed by
        !! explicit end markers so the list stays flat and reversible.
        integer :: kind = 0
        character(len=:), allocatable :: target
        integer :: value = 0
        !! FortFront proved this whole assignment targets a concrete
        !! allocatable owner and may perform automatic reallocation.
        logical :: is_automatic_reallocation = .false.
        !! Loop bounds for FAD_DO; condition for FAD_IF; selector expression
        !! for FAD_SELECT_TYPE.
        integer :: lo = 0, hi = 0, step = 0
        !! Source line in the primal, for provenance comments.
        integer :: line = 0
        !! Actual arguments of a FAD_CALL_STMT.
        integer, allocatable :: call_args(:)
        !! Keyword names for ``call_args``; blank entries are positional.
        character(len=:), allocatable :: call_arg_names(:)
        !! For FAD_ALLOCATE/FAD_DEALLOCATE, the first entry is the owning
        !! object and the remaining entries are shape expressions.  The
        !! optional SOURCE= and MOLD= expressions use the fields below.
        integer, allocatable :: allocation_args(:)
        integer :: allocation_source = 0
        integer :: allocation_mold = 0
        logical :: allocation_target_polymorphic = .false.
        logical :: allocation_target_unlimited_polymorphic = .false.
    end type fad_stmt_t

    public :: fad_copy_stmt

    type, public :: fad_decl_t
        !! A declared entity: dummy argument, result, or local.
        character(len=:), allocatable :: name
        character(len=:), allocatable :: type_name !! "real(dp)", "integer"
        !! Source line for diagnostics at transformation boundaries.
        integer :: line = 0
        integer :: intent = FAD_INTENT_NONE
        logical :: is_value = .false.
        !! Whether this is an optional dummy argument.  Keeping this bit in
        !! the IR matters for generated interfaces: a copied `present(x)`
        !! guard is only valid when the generated dummy is optional too.
        logical :: is_optional = .false.
        logical :: is_array = .false.
        logical :: is_contiguous = .false.
        logical :: is_result = .false.
        !! Whether the entity owns deferred-shape storage.  This is distinct
        !! from ``is_array``: an allocatable tangent must carry the same
        !! descriptor and cannot be emitted as an ordinary assumed-shape
        !! array.
        logical :: is_allocatable = .false.
        !! Semantic type facts copied from fortfront.  These are deliberately
        !! not inferred from type_name by the differentiation passes: a
        !! polymorphic allocatable needs a dynamic-type ownership model, not a
        !! spelling heuristic.
        logical :: is_polymorphic = .false.
        logical :: is_unlimited_polymorphic = .false.
        logical :: is_select_alias = .false.
        logical :: is_associate_alias = .false.
        character(len=:), allocatable :: alias_target
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
        !! Whether the primal was declared ELEMENTAL. Keeping this bit in the
        !! procedure IR preserves elemental array semantics in its derivative.
        logical :: is_elemental = .false.
        !! `use` statements copied verbatim from the primal.
        !!
        !! The derivative names the same kinds and calls the same helpers as the
        !! primal, so it needs the same imports. Reproducing the text rather
        !! than resolving it keeps fortad out of the business of module lookup,
        !! which is the consumer's compiler's job.
        character(len=:), allocatable :: uses(:)
        integer :: n_uses = 0
        type(fad_decl_t), allocatable :: decls(:)
        character(len=:), allocatable :: params(:)
        type(fad_expr_t), allocatable :: exprs(:)
        type(fad_stmt_t), allocatable :: stmts(:)
        integer :: n_exprs = 0
        integer :: n_stmts = 0
        integer :: n_decls = 0
        !! Hash buckets over the expression arena, so an identical subtree is
        !! built once and shared. Children are already shared, so structural
        !! equality reduces to shallow equality on (kind, text, args) - which
        !! is what makes common-subexpression detection a lookup rather than a
        !! tree comparison.
        integer, allocatable :: bucket_head(:)
        integer, allocatable :: bucket_next(:)
    contains
        procedure :: add_expr => proc_add_expr
        procedure :: add_stmt => proc_add_stmt
        procedure :: add_decl => proc_add_decl
        procedure :: add_decl_fields => proc_add_decl_fields
        procedure :: decl_index => proc_decl_index
        procedure :: decl_index_of => proc_decl_index_of
    end type fad_proc_t

    public :: expr_const, expr_var, expr_binop, expr_unop, expr_call
    public :: fad_base_name, fad_suffix_name
    public :: fad_expr_equal, copy_decl

contains

    function fad_base_name(raw) result(base)
        !! Return the declared object behind an array or component reference.
        !!
        !! The expression arena deliberately keeps component accesses as text
        !! (for example ``state%position%x``).  Activity and SSA bookkeeping
        !! still need the declaration which owns that storage, namely
        !! ``state``.  This helper also handles ``a(i)`` and
        !! ``state%values(i)`` and ``state(i)%value`` without attempting to
        !! parse Fortran generally.
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: base
        integer :: cut, pos

        cut = len_trim(raw) + 1
        pos = index(raw, "%")
        if (pos > 0) cut = min(cut, pos)
        pos = index(raw, "(")
        if (pos > 0) cut = min(cut, pos)
        if (cut <= 1) then
            base = trim(raw)
        else
            base = trim(raw(:cut - 1))
        end if
    end function fad_base_name

    function fad_suffix_name(raw, suffix, vector) result(name)
        !! Put a derivative suffix on the owning object, preserving access.
        !!
        !! ``x(i)`` becomes ``x_d(i)`` and ``state%values(i)`` becomes
        !! ``state_d%values(i)``.  In vector mode the leading lane is inserted
        !! before the existing subscript, so the result is
        !! ``state_d%values(:, i)``.
        character(len=*), intent(in) :: raw, suffix
        logical, intent(in), optional :: vector
        character(len=:), allocatable :: name
        integer :: percent, open
        logical :: lanes

        lanes = .false.
        if (present(vector)) lanes = vector
        percent = index(raw, "%")
        open = index(raw, "(")
        if (percent > 0) then
            ! An array element can be the object that owns a component, as in
            ! ``holders(2)%payload``.  The subscript belongs before the
            ! derivative suffix: ``holders_d(2)%payload``.  The older branch
            ! below handles a subscript on the component itself, such as
            ! ``state%values(2)``.
            if (open > 0 .and. open < percent) then
                name = trim(raw(:open - 1))//trim(suffix)//raw(open:)
                return
            end if
            if (open > 0) then
                name = trim(raw(:percent - 1))//trim(suffix)// &
                    raw(percent:open - 1)
                if (lanes) then
                    name = name//"(:, "//raw(open + 1:)
                else
                    name = name//raw(open:)
                end if
            else
                name = trim(raw(:percent - 1))//trim(suffix)//raw(percent:)
            end if
        else if (open > 0) then
            if (lanes) then
                name = trim(raw(:open - 1))//trim(suffix)//"(:, "// &
                    raw(open + 1:)
            else
                name = trim(raw(:open - 1))//trim(suffix)//raw(open:)
            end if
        else
            name = trim(raw)//trim(suffix)
            if (lanes) name = name//"(:)"
        end if
    end function fad_suffix_name

    integer function proc_add_expr(self, e) result(idx)
        !! Append an expression, sharing it with an identical existing one.
        !!
        !! Hash-consing here is what later lets the emitter notice that
        !! `exp(0.01*a(i))` appears three times in a generated loop body: the
        !! three uses carry the same index, so counting uses is counting
        !! integers rather than comparing trees.
        class(fad_proc_t), intent(inout) :: self
        type(fad_expr_t), intent(in) :: e
        type(fad_expr_t), allocatable :: tmp(:)
        integer, allocatable :: itmp(:)
        integer :: cap, h, probe

        if (.not. allocated(self%exprs)) allocate (self%exprs(64))
        if (.not. allocated(self%bucket_head)) then
            allocate (self%bucket_head(0:BUCKETS - 1))
            self%bucket_head = 0
        end if
        if (.not. allocated(self%bucket_next)) then
            allocate (self%bucket_next(size(self%exprs)))
            self%bucket_next = 0
        end if

        h = expr_hash(e)
        probe = self%bucket_head(h)
        do while (probe /= 0)
            if (fad_expr_equal(self%exprs(probe), e)) then
                idx = probe
                return
            end if
            probe = self%bucket_next(probe)
        end do

        cap = size(self%exprs)
        if (self%n_exprs >= cap) then
            allocate (tmp(2*cap))
            tmp(1:cap) = self%exprs
            call move_alloc(tmp, self%exprs)
            allocate (itmp(2*cap))
            itmp = 0
            itmp(1:cap) = self%bucket_next(1:cap)
            call move_alloc(itmp, self%bucket_next)
        end if
        self%n_exprs = self%n_exprs + 1
        self%exprs(self%n_exprs) = e
        self%bucket_next(self%n_exprs) = self%bucket_head(h)
        self%bucket_head(h) = self%n_exprs
        idx = self%n_exprs
    end function proc_add_expr

    integer function expr_hash(e) result(h)
        !! Hash of (kind, text, args). Only shallow data is read, because
        !! children are already shared.
        type(fad_expr_t), intent(in) :: e
        integer :: i

        h = e%kind*31
        if (allocated(e%text)) then
            do i = 1, len(e%text)
                h = modulo(h*31 + iachar(e%text(i:i)), 1048576)
            end do
        end if
        if (allocated(e%args)) then
            do i = 1, size(e%args)
                h = modulo(h*31 + e%args(i), 1048576)
            end do
        end if
        h = modulo(abs(h), BUCKETS)
    end function expr_hash

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
        call fad_copy_stmt(self%stmts(self%n_stmts), s)
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
            call copy_decl(self%decls(existing), d)
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
        call copy_decl(self%decls(self%n_decls), d)
        idx = self%n_decls
    end function proc_add_decl

    subroutine copy_decl(out, source)
        !! Copy declaration metadata without compiler-generated assignment of
        !! a derived type carrying several deferred-length allocatables.
        !! nvfortran's older front end can corrupt an unrelated allocatable
        !! descriptor when that intrinsic assignment is repeated in a loop.
        type(fad_decl_t), intent(inout) :: out
        type(fad_decl_t), intent(in) :: source

        if (allocated(out%name)) deallocate (out%name)
        if (allocated(out%type_name)) deallocate (out%type_name)
        if (allocated(out%dims)) deallocate (out%dims)
        if (allocated(source%name)) out%name = source%name
        if (allocated(source%type_name)) out%type_name = source%type_name
        if (allocated(source%dims)) out%dims = source%dims
        out%intent = source%intent
        out%line = source%line
        out%is_value = source%is_value
        out%is_optional = source%is_optional
        out%is_array = source%is_array
        out%is_contiguous = source%is_contiguous
        out%is_allocatable = source%is_allocatable
        out%is_polymorphic = source%is_polymorphic
        out%is_unlimited_polymorphic = source%is_unlimited_polymorphic
        out%is_select_alias = source%is_select_alias
        out%is_associate_alias = source%is_associate_alias
        if (allocated(source%alias_target)) out%alias_target = source%alias_target
        out%is_result = source%is_result
    end subroutine copy_decl

    integer function proc_add_decl_fields(self, name, type_name, intent, &
            is_value, is_array, is_contiguous, &
            is_result, dims, is_optional, is_allocatable) result(idx)
        !! Append declaration scalars and strings directly. This is the
        !! compiler-neutral path for transformation code that repeatedly
        !! mirrors declarations; it avoids passing an allocatable-component
        !! derived value through nvfortran's argument machinery.
        class(fad_proc_t), intent(inout) :: self
        character(len=*), intent(in) :: name, type_name, dims
        integer, intent(in) :: intent
        logical, intent(in) :: is_value, is_array, is_contiguous, is_result
        logical, intent(in), optional :: is_optional
        logical, intent(in), optional :: is_allocatable
        type(fad_decl_t), allocatable :: tmp(:)
        integer :: cap, existing
        logical :: optional_arg
        logical :: allocatable_arg

        optional_arg = .false.
        if (present(is_optional)) optional_arg = is_optional
        allocatable_arg = .false.
        if (present(is_allocatable)) allocatable_arg = is_allocatable

        existing = self%decl_index(name)
        if (existing > 0) then
            call set_decl_fields(self%decls(existing), name, type_name, intent, &
                is_value, is_array, is_contiguous, is_result, dims, optional_arg, &
                allocatable_arg)
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
        call set_decl_fields(self%decls(self%n_decls), name, type_name, intent, &
            is_value, is_array, is_contiguous, is_result, dims, optional_arg, &
            allocatable_arg)
        idx = self%n_decls
    end function proc_add_decl_fields

    subroutine set_decl_fields(out, name, type_name, intent, is_value, &
            is_array, is_contiguous, is_result, dims, is_optional, is_allocatable)
        type(fad_decl_t), intent(inout) :: out
        character(len=*), intent(in) :: name, type_name, dims
        integer, intent(in) :: intent
        logical, intent(in) :: is_value, is_array, is_contiguous, is_result
        logical, intent(in) :: is_optional
        logical, intent(in) :: is_allocatable

        if (allocated(out%name)) deallocate (out%name)
        if (allocated(out%type_name)) deallocate (out%type_name)
        if (allocated(out%dims)) deallocate (out%dims)
        out%line = 0
        out%name = trim(name)
        if (len_trim(type_name) > 0) out%type_name = trim(type_name)
        if (len_trim(dims) > 0) out%dims = trim(dims)
        out%intent = intent
        out%is_value = is_value
        out%is_optional = is_optional
        out%is_array = is_array
        out%is_contiguous = is_contiguous
        out%is_result = is_result
        out%is_allocatable = is_allocatable
        out%is_polymorphic = .false.
        out%is_unlimited_polymorphic = .false.
        out%is_select_alias = .false.
        out%is_associate_alias = .false.
        if (allocated(out%alias_target)) deallocate (out%alias_target)
    end subroutine set_decl_fields

    !! Declaration index of the object behind a possibly-qualified reference.
    !!
    !! Callers used to write `decl_index_of(raw)`, passing a
    !! `character(len=:), allocatable` function result straight into a
    !! `character(len=*)` dummy. nvfortran rejects that -- "Argument number 2
    !! to proc_decl_index: type mismatch" -- at every one of the forty call
    !! sites, and that blocked the OpenACC build of the whole stack.
    !!
    !! Extracting the base name inside keeps the deferred-length string from
    !! crossing a procedure boundary, and is a better interface regardless:
    !! the two steps were always taken together.
    integer function proc_decl_index_of(self, raw) result(idx)
        class(fad_proc_t), intent(in) :: self
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: base

        base = fad_base_name(raw)
        idx = proc_decl_index(self, base)
    end function proc_decl_index_of

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
        allocate (e%args(2))
        e%args(1) = a
        e%args(2) = b
    end function expr_binop

    type(fad_expr_t) function expr_unop(op, a) result(e)
        !! A unary operation over one expression index.
        character(len=*), intent(in) :: op
        integer, intent(in) :: a

        e%kind = FAD_UNOP
        e%text = op
        allocate (e%args(1))
        e%args(1) = a
    end function expr_unop

    type(fad_expr_t) function expr_call(name, args, arg_names) result(e)
        !! An intrinsic or procedure reference.
        character(len=*), intent(in) :: name
        integer, intent(in) :: args(:)
        character(len=*), intent(in), optional :: arg_names(:)

        e%kind = FAD_CALL
        e%text = name
        e%args = args
        if (present(arg_names)) e%call_arg_names = arg_names
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
        if (allocated(a%args) .neqv. allocated(b%args)) return
        if (allocated(a%args)) then
            if (size(a%args) /= size(b%args)) return
        end if
        if (allocated(a%args)) then
            if (size(a%args) > 0) then
                if (any(a%args /= b%args)) return
            end if
        end if
        if (allocated(a%call_arg_names) .neqv. allocated(b%call_arg_names)) return
        if (allocated(a%call_arg_names)) then
            if (size(a%call_arg_names) /= size(b%call_arg_names)) return
            if (any(a%call_arg_names /= b%call_arg_names)) return
        end if
        if (a%rank /= b%rank) return
        if (a%is_component_path .neqv. b%is_component_path) return
        if (a%component_is_allocatable .neqv. b%component_is_allocatable) return
        if (a%component_is_pointer .neqv. b%component_is_pointer) return
        if (a%component_is_target .neqv. b%component_is_target) return
        if (a%component_is_polymorphic .neqv. b%component_is_polymorphic) return
        if (a%component_is_global .neqv. b%component_is_global) return
        if (a%component_is_real .neqv. b%component_is_real) return
        if (a%component_rank /= b%component_rank) return
        if (allocated(a%component_type_name) .neqv. &
            allocated(b%component_type_name)) return
        if (allocated(a%component_type_name)) then
            if (a%component_type_name /= b%component_type_name) return
        end if
        if (allocated(a%component_original_path) .neqv. &
            allocated(b%component_original_path)) return
        if (allocated(a%component_original_path)) then
            if (a%component_original_path /= b%component_original_path) return
        end if
        same = .true.
    end function fad_expr_equal

    !! Copy one statement onto another, component by component.
    !!
    !! Fortran's intrinsic derived-type assignment does exactly this and is
    !! what the code used. nvfortran 26.5 raises an internal compiler error on
    !! it -- "Deferred-length character symbol must have descriptor" -- because
    !! of the `character(len=:), allocatable` components, and that error
    !! aborted the whole OpenACC build of the stack, so no GPU target could be
    !! compiled at all.
    !!
    !! The workaround is mechanical and carries a cost worth naming: it must be
    !! kept in step with the type by hand, and a component added above without
    !! a line here would be silently dropped. `test_fortad_ir_copy` guards
    !! that by round-tripping a statement with every component set.
    subroutine fad_copy_stmt(destination, source)
        type(fad_stmt_t), intent(out) :: destination
        type(fad_stmt_t), intent(in) :: source

        destination%kind = source%kind
        destination%value = source%value
        destination%is_automatic_reallocation = source%is_automatic_reallocation
        destination%lo = source%lo
        destination%hi = source%hi
        destination%step = source%step
        destination%line = source%line
        destination%allocation_source = source%allocation_source
        destination%allocation_mold = source%allocation_mold
        destination%allocation_target_polymorphic = &
            source%allocation_target_polymorphic
        destination%allocation_target_unlimited_polymorphic = &
            source%allocation_target_unlimited_polymorphic
        if (allocated(source%target)) destination%target = source%target
        if (allocated(source%call_args)) &
            destination%call_args = source%call_args
        if (allocated(source%call_arg_names)) &
            destination%call_arg_names = source%call_arg_names
        if (allocated(source%allocation_args)) &
            destination%allocation_args = source%allocation_args
    end subroutine fad_copy_stmt

end module fortad_ir
