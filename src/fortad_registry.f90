module fortad_registry
    !! User-supplied derivative rules.
    !!
    !! fortad knows the derivatives of the Fortran intrinsics. It cannot know
    !! the derivative of *your* `bessel_j0_fast` or `equation_of_state`, and
    !! guessing would be worse than refusing. A rule registered here fills that
    !! gap, and doing so is usually the difference between a derivative that is
    !! merely correct and one that is fast: differentiating a linear solve
    !! through its iterations is asymptotically worse than applying the implicit
    !! function theorem once, and no amount of cleverness at the loop level
    !! recovers that.
    !!
    !! A rule states the partial derivative with respect to each argument, as
    !! Fortran text over the placeholders `$1`, `$2`, ... Reverse mode reads the
    !! same rule transposed, so one entry serves both modes.
    !!
    !!     call fad_add_rule("eos_pressure", ["deos_drho($1, $2)", &
    !!                                        "deos_dtemp($1, $2)"])
    !!
    !! The registry is deliberately dumb: it substitutes text and trusts you.
    !! It is the one place in fortad where a wrong answer is the user's to own,
    !! so the API says as much and the tests check the substitution, not the
    !! mathematics.
    implicit none
    private

    public :: fad_add_rule, fad_clear_rules, registry_has, registry_n_args, &
              registry_partial

    integer, parameter :: MAX_RULES = 256
    integer, parameter :: MAX_ARGS = 8
    integer, parameter :: NAME_LEN = 64
    integer, parameter :: EXPR_LEN = 512

    type :: rule_t
        character(len=NAME_LEN) :: name = ""
        integer :: n_args = 0
        character(len=EXPR_LEN) :: partials(MAX_ARGS) = ""
    end type rule_t

    type(rule_t), save :: rules(MAX_RULES)
    integer, save :: n_rules = 0

contains

    subroutine fad_add_rule(name, partials, stat)
        !! Register the partial derivatives of `name`.
        !!
        !! `partials(i)` is the derivative with respect to argument `i`, written
        !! as Fortran over `$1`, `$2`, ... A later registration of the same name
        !! replaces the earlier one, so a project can override a default.
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: partials(:)
        !! 0 on success; nonzero when the rule cannot be stored.
        integer, intent(out), optional :: stat
        integer :: i, slot

        if (present(stat)) stat = 0
        if (size(partials) > MAX_ARGS) then
            if (present(stat)) stat = 1
            return
        end if
        if (len_trim(name) > NAME_LEN) then
            if (present(stat)) stat = 2
            return
        end if
        do i = 1, size(partials)
            if (len_trim(partials(i)) > EXPR_LEN) then
                if (present(stat)) stat = 3
                return
            end if
        end do

        slot = find(name)
        if (slot == 0) then
            if (n_rules >= MAX_RULES) then
                if (present(stat)) stat = 4
                return
            end if
            n_rules = n_rules + 1
            slot = n_rules
        end if

        rules(slot)%name = lower(trim(name))
        rules(slot)%n_args = size(partials)
        rules(slot)%partials = ""
        do i = 1, size(partials)
            rules(slot)%partials(i) = trim(partials(i))
        end do
    end subroutine fad_add_rule

    subroutine fad_clear_rules()
        !! Forget every registered rule.
        n_rules = 0
    end subroutine fad_clear_rules

    logical function registry_has(name) result(yes)
        !! True when a rule is registered for `name`.
        character(len=*), intent(in) :: name

        yes = find(name) > 0
    end function registry_has

    integer function registry_n_args(name) result(n)
        !! Argument count the registered rule expects, or 0.
        character(len=*), intent(in) :: name
        integer :: slot

        n = 0
        slot = find(name)
        if (slot > 0) n = rules(slot)%n_args
    end function registry_n_args

    function registry_partial(name, which, arg_texts) result(text)
        !! The partial with respect to argument `which`, with `$1`, `$2`, ...
        !! replaced by the caller's actual argument text.
        character(len=*), intent(in) :: name
        integer, intent(in) :: which
        character(len=*), intent(in) :: arg_texts(:)
        character(len=:), allocatable :: text
        integer :: slot, i

        text = ""
        slot = find(name)
        if (slot == 0) return
        if (which < 1 .or. which > rules(slot)%n_args) return

        text = trim(rules(slot)%partials(which))
        ! Substitute from the highest index down, so `$1` does not match the
        ! first character of `$10`.
        do i = min(size(arg_texts), MAX_ARGS), 1, -1
            text = replace_all(text, "$"//itoa(i), "("//trim(arg_texts(i))//")")
        end do
    end function registry_partial

    integer function find(name) result(slot)
        !! Index of the rule for `name`, or 0.
        character(len=*), intent(in) :: name
        integer :: i
        character(len=:), allocatable :: key

        key = lower(trim(name))
        slot = 0
        do i = 1, n_rules
            if (trim(rules(i)%name) == key) then
                slot = i
                return
            end if
        end do
    end function find

    function replace_all(text, needle, replacement) result(out)
        !! Replace every occurrence of `needle`.
        character(len=*), intent(in) :: text, needle, replacement
        character(len=:), allocatable :: out
        integer :: pos, start

        out = ""
        start = 1
        do
            pos = index(text(start:), needle)
            if (pos == 0) exit
            out = out//text(start:start + pos - 2)//replacement
            start = start + pos - 1 + len(needle)
        end do
        out = out//text(start:)
    end function replace_all

    pure function lower(s) result(out)
        !! ASCII lowercase.
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, c

        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
                out(i:i) = achar(c + 32)
            else
                out(i:i) = s(i:i)
            end if
        end do
    end function lower

    function itoa(n) result(s)
        !! Integer to trimmed decimal text.
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=16) :: buf

        write (buf, '(i0)') n
        s = trim(buf)
    end function itoa

end module fortad_registry
