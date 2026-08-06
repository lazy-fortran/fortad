module fortad_boundaries
    !! Source-level refusal boundaries shared by the public transforms.
    !!
    !! The IR has no representation for storage ownership or allocation state.
    !! Detecting those constructs before parsing keeps an unsupported primal
    !! from reaching a partial lowering path that could emit an invalid
    !! derivative.
    implicit none
    private

    public :: find_allocation_construct

contains

    logical function find_allocation_construct(source, line, construct) result(found)
        !! Find the first allocation-lifetime construct in ``source``.
        character(len=*), intent(in) :: source
        integer, intent(out) :: line
        character(len=:), allocatable, intent(out) :: construct
        character(len=:), allocatable :: physical, code
        integer :: i, start, finish, n

        found = .false.
        line = 0
        construct = ""
        start = 1
        n = len(source)
        do i = 1, n + 1
            if (i <= n) then
                if (source(i:i) /= new_line('a')) cycle
            end if
            if (i > start) then
                finish = i - 1
                physical = source(start:finish)
            else
                physical = ""
            end if
            code = sanitise(physical)
            if (contains_word(code, "allocatable")) then
                found = .true.
                construct = "allocatable declaration/component"
            else if (contains_word(code, "move_alloc")) then
                found = .true.
                construct = "move_alloc"
            else if (contains_word(code, "deallocate")) then
                found = .true.
                construct = "deallocate"
            else if (contains_word(code, "allocate")) then
                found = .true.
                if (contains_keyword_assignment(code, "source")) then
                    construct = "allocate(source=...)"
                else if (contains_keyword_assignment(code, "mold")) then
                    construct = "allocate(mold=...)"
                else
                    construct = "allocate"
                end if
            end if
            if (found) then
                line = count_lines(source, start)
                return
            end if
            if (i > n) exit
            start = i + 1
        end do
    end function find_allocation_construct

    function sanitise(line) result(out)
        !! Remove comments and quoted text, then fold ASCII to lower case.
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: out
        character :: quote, c
        integer :: i
        logical :: quoted, escaped_quote

        out = ""
        quote = achar(0)
        quoted = .false.
        escaped_quote = .false.
        do i = 1, len(line)
            c = line(i:i)
            if (quoted) then
                if (escaped_quote) then
                    escaped_quote = .false.
                    out = out//" "
                    cycle
                end if
                if (c == quote) then
                    if (i < len(line)) then
                        if (line(i + 1:i + 1) == quote) then
                            escaped_quote = .true.
                            out = out//" "
                            cycle
                        end if
                    end if
                    quoted = .false.
                end if
                out = out//" "
            else if (c == "'" .or. c == '"') then
                quoted = .true.
                quote = c
                out = out//" "
            else if (c == "!") then
                exit
            else
                out = out//lower(c)
            end if
        end do
    end function sanitise

    logical function contains_word(text, word) result(found)
        character(len=*), intent(in) :: text, word
        integer :: i, last

        found = .false.
        last = len_trim(text) - len_trim(word) + 1
        if (last < 1) return
        do i = 1, last
            if (text(i:i + len_trim(word) - 1) /= trim(word)) cycle
            if (i > 1) then
                if (identifier_char(text(i - 1:i - 1))) cycle
            end if
            if (i + len_trim(word) <= len_trim(text)) then
                if (identifier_char(text(i + len_trim(word):i + len_trim(word)))) cycle
            end if
            found = .true.
            return
        end do
    end function contains_word

    logical function contains_keyword_assignment(text, keyword) result(found)
        character(len=*), intent(in) :: text, keyword
        integer :: i, j, k

        found = .false.
        do i = 1, len_trim(text) - len_trim(keyword) + 1
            if (text(i:i + len_trim(keyword) - 1) /= trim(keyword)) cycle
            if (i > 1) then
                if (identifier_char(text(i - 1:i - 1))) cycle
            end if
            j = i + len_trim(keyword)
            do while (j <= len_trim(text))
                if (text(j:j) /= " " .and. text(j:j) /= achar(9)) exit
                j = j + 1
            end do
            if (j > len_trim(text)) cycle
            if (text(j:j) /= "=") cycle
            k = j + 1
            if (k <= len_trim(text)) then
                found = .true.
                return
            end if
        end do
    end function contains_keyword_assignment

    logical function identifier_char(c) result(is_char)
        character, intent(in) :: c

        is_char = (c >= "a" .and. c <= "z") .or. &
            (c >= "0" .and. c <= "9") .or. c == "_"
    end function identifier_char

    character function lower(c) result(out)
        character, intent(in) :: c

        out = c
        if (c >= "A" .and. c <= "Z") out = achar(iachar(c) + 32)
    end function lower

    integer function count_lines(source, position) result(line)
        character(len=*), intent(in) :: source
        integer, intent(in) :: position
        integer :: i

        line = 1
        do i = 1, position - 1
            if (source(i:i) == new_line('a')) line = line + 1
        end do
    end function count_lines

end module fortad_boundaries
