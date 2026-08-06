module fortad_sparse
    !! Compressed Jacobians by column colouring.
    !!
    !! A Jacobian with `n` columns costs `n` tangent sweeps to build densely.
    !! When most entries are structurally zero that is nearly all wasted work:
    !! two columns whose nonzeros never share a row can be evaluated in the
    !! *same* sweep and separated afterwards, because in any given row at most
    !! one of them contributes.
    !!
    !! Grouping columns that way is graph colouring - Curtis, Powell and Reid
    !! (1974) for the idea, Gebremedhin, Manne and Pothen (2005) for the modern
    !! statement of it as distance-2 colouring of the bipartite graph. What is
    !! implemented here is the greedy algorithm, which is not optimal and does
    !! not need to be: it is within a small factor in practice and costs almost
    !! nothing next to the sweeps it saves.
    !!
    !! This is a runtime library, not code generation. fortad cannot know your
    !! sparsity pattern from the source in general, and inventing one would be
    !! worse than asking. Supply the pattern, get the seed matrix, run vector
    !! forward mode once, and recover the entries.
    use fortad_kinds, only: dp
    implicit none
    private

    public :: sparsity_t, colour_columns, seed_matrix, recover_entries
    public :: star_colour_columns, recover_symmetric

    type :: sparsity_t
        !! Structural nonzeros of an `n_rows` by `n_cols` Jacobian, by column.
        !!
        !! `rows(col_start(j) : col_start(j+1)-1)` lists the rows in which
        !! column `j` is structurally nonzero. A pattern that omits a genuine
        !! nonzero silently loses that derivative entry, so it is the caller's
        !! job to be conservative: an over-full pattern costs sweeps, an
        !! under-full one costs correctness.
        integer :: n_rows = 0
        integer :: n_cols = 0
        integer, allocatable :: col_start(:)
        integer, allocatable :: rows(:)
    end type sparsity_t

contains

    subroutine colour_columns(pattern, colour, n_colours, stat)
        !! Greedy distance-2 colouring: give each column the lowest colour not
        !! already used by a column sharing one of its rows.
        !!
        !! Columns are visited in decreasing order of nonzero count, the
        !! standard largest-first heuristic, which usually beats natural order
        !! by a wide margin on banded and arrowhead patterns.
        !!
        !! The conflict test needs, for each row, *every* colour already placed
        !! in it - not merely the last one, which would miss conflicts and
        !! silently produce a colouring that cannot be decompressed. That needs
        !! the row-wise view of the pattern, which is built here.
        type(sparsity_t), intent(in) :: pattern
        integer, allocatable, intent(out) :: colour(:)
        integer, intent(out) :: n_colours
        !! 0 on success, nonzero when the pattern is malformed.
        integer, intent(out), optional :: stat
        integer, allocatable :: order(:), forbidden(:)
        integer, allocatable :: row_start(:), row_cols(:)
        integer :: j, k, c, col, row, other

        if (present(stat)) stat = 0
        n_colours = 0
        if (.not. valid(pattern)) then
            if (present(stat)) stat = 1
            allocate (colour(0))
            return
        end if

        allocate (colour(pattern%n_cols))
        allocate (forbidden(pattern%n_cols + 1))
        colour = 0
        forbidden = 0

        call build_row_index(pattern, row_start, row_cols)
        call order_by_degree(pattern, order)

        do k = 1, pattern%n_cols
            col = order(k)
            ! Forbid every colour already present in any row this column
            ! touches, via every other column touching that row.
            do j = pattern%col_start(col), pattern%col_start(col + 1) - 1
                row = pattern%rows(j)
                do c = row_start(row), row_start(row + 1) - 1
                    other = row_cols(c)
                    if (other == col) cycle
                    if (colour(other) > 0) forbidden(colour(other)) = col
                end do
            end do
            c = 1
            do while (c <= pattern%n_cols)
                if (forbidden(c) /= col) exit
                c = c + 1
            end do
            colour(col) = c
            n_colours = max(n_colours, c)
        end do
    end subroutine colour_columns

    subroutine build_row_index(pattern, row_start, row_cols)
        !! The row-wise view: which columns are nonzero in each row.
        type(sparsity_t), intent(in) :: pattern
        integer, allocatable, intent(out) :: row_start(:), row_cols(:)
        integer, allocatable :: fill(:)
        integer :: i, j, row

        allocate (row_start(pattern%n_rows + 1))
        allocate (row_cols(size(pattern%rows)))
        allocate (fill(pattern%n_rows))
        row_start = 0
        fill = 0

        do j = 1, size(pattern%rows)
            row = pattern%rows(j)
            row_start(row + 1) = row_start(row + 1) + 1
        end do
        row_start(1) = 1
        do i = 1, pattern%n_rows
            row_start(i + 1) = row_start(i + 1) + row_start(i)
        end do

        do j = 1, pattern%n_cols
            do i = pattern%col_start(j), pattern%col_start(j + 1) - 1
                row = pattern%rows(i)
                row_cols(row_start(row) + fill(row)) = j
                fill(row) = fill(row) + 1
            end do
        end do
    end subroutine build_row_index

    subroutine star_colour_columns(pattern, colour, n_colours, stat)
        !! Star colouring of a **symmetric** pattern.
        !!
        !! For a Jacobian, two columns conflict when they share a row. For a
        !! symmetric matrix that test throws away what symmetry gives you:
        !! `H(i,j)` and `H(j,i)` are the same number, so an entry only has to be
        !! recovered from one of its two directions. Exploiting that is star
        !! colouring - a proper colouring of the adjacency graph in which no
        !! path on four vertices uses only two colours.
        !!
        !! The gain is not marginal. An arrowhead Hessian, one dense row and
        !! column, needs `n` colours by the Jacobian test because the dense
        !! column conflicts with every other. Its graph is a star, which has no
        !! four-vertex path at all, so star colouring needs **two**.
        !!
        !! Greedy, largest-first, same as the asymmetric case. The extra work is
        !! the two-coloured-path test, which is why this is not simply the other
        !! routine with a different comment.
        type(sparsity_t), intent(in) :: pattern
        integer, allocatable, intent(out) :: colour(:)
        integer, intent(out) :: n_colours
        !! 0 on success, 1 for a malformed pattern, 2 when it is not symmetric.
        integer, intent(out), optional :: stat
        integer, allocatable :: order(:), row_start(:), row_cols(:)
        integer :: k, c, col

        if (present(stat)) stat = 0
        n_colours = 0
        if (.not. valid(pattern)) then
            if (present(stat)) stat = 1
            allocate (colour(0))
            return
        end if
        if (pattern%n_rows /= pattern%n_cols) then
            if (present(stat)) stat = 2
            allocate (colour(0))
            return
        end if

        call build_row_index(pattern, row_start, row_cols)
        if (.not. symmetric(pattern, row_start, row_cols)) then
            if (present(stat)) stat = 2
            allocate (colour(0))
            return
        end if

        allocate (colour(pattern%n_cols))
        colour = 0
        call order_by_degree(pattern, order)

        do k = 1, pattern%n_cols
            col = order(k)
            c = 1
            do while (c <= pattern%n_cols)
                if (star_allows(row_start, row_cols, colour, col, c)) exit
                c = c + 1
            end do
            colour(col) = c
            n_colours = max(n_colours, c)
        end do
    end subroutine star_colour_columns

    logical function star_allows(row_start, row_cols, colour, v, c) &
        result(ok)
        !! Whether colour `c` may be given to column `v`.
        !!
        !! Two conditions. The colouring must stay proper - no neighbour of `v`
        !! already has `c`. And no path `v - w - x - y` may end up using only
        !! two colours, which happens when `c(x) = c` and `c(y) = c(w)`.
        integer, intent(in) :: row_start(:), row_cols(:), colour(:), v, c
        integer :: iw, w, ix, x, iy, y

        ok = .false.
        do iw = row_start(v), row_start(v + 1) - 1
            w = row_cols(iw)
            if (w == v) cycle
            if (colour(w) == c) return
        end do

        do iw = row_start(v), row_start(v + 1) - 1
            w = row_cols(iw)
            if (w == v .or. colour(w) == 0) cycle
            do ix = row_start(w), row_start(w + 1) - 1
                x = row_cols(ix)
                if (x == v .or. x == w) cycle
                if (colour(x) /= c) cycle
                do iy = row_start(x), row_start(x + 1) - 1
                    y = row_cols(iy)
                    if (y == w .or. y == x) cycle
                    if (colour(y) == colour(w)) return
                end do
            end do
        end do
        ok = .true.
    end function star_allows

    logical function symmetric(pattern, row_start, row_cols) result(ok)
        !! Whether the pattern is structurally symmetric.
        !!
        !! Star colouring is only valid on one, and a caller who passes an
        !! asymmetric pattern has made a mistake worth reporting rather than
        !! silently producing entries that cannot be recovered.
        type(sparsity_t), intent(in) :: pattern
        integer, intent(in) :: row_start(:), row_cols(:)
        integer :: j, k, i

        ok = .false.
        do j = 1, pattern%n_cols
            do k = pattern%col_start(j), pattern%col_start(j + 1) - 1
                i = pattern%rows(k)
                if (.not. in_row(row_start, row_cols, j, i)) return
            end do
        end do
        ok = .true.
    end function symmetric

    logical function in_row(row_start, row_cols, row, col) result(yes)
        !! Whether (row, col) is structurally nonzero.
        integer, intent(in) :: row_start(:), row_cols(:), row, col
        integer :: k

        yes = .false.
        do k = row_start(row), row_start(row + 1) - 1
            if (row_cols(k) == col) then
                yes = .true.
                return
            end if
        end do
    end function in_row

    subroutine recover_symmetric(pattern, colour, compressed, values, stat)
        !! Recover a symmetric matrix from a star-coloured compression.
        !!
        !! Entry `(i,j)` is read from whichever of its two directions is
        !! unambiguous: from row `i` if `j` is the only column of its colour
        !! with a nonzero there, otherwise from row `j`. A star colouring
        !! guarantees at least one of the two works, which is the property that
        !! distinguishes it from a merely proper colouring.
        type(sparsity_t), intent(in) :: pattern
        integer, intent(in) :: colour(:)
        real(dp), intent(in) :: compressed(:, :)
        real(dp), allocatable, intent(out) :: values(:)
        !! 0 on success; 1 when an entry was ambiguous from both directions,
        !! which means the colouring was not a star colouring.
        integer, intent(out), optional :: stat
        integer, allocatable :: row_start(:), row_cols(:)
        integer :: j, k, i

        if (present(stat)) stat = 0
        allocate (values(size(pattern%rows)))
        call build_row_index(pattern, row_start, row_cols)

        do j = 1, pattern%n_cols
            do k = pattern%col_start(j), pattern%col_start(j + 1) - 1
                i = pattern%rows(k)
                if (alone_in_row(row_start, row_cols, colour, i, j)) then
                    values(k) = compressed(colour(j), i)
                else if (alone_in_row(row_start, row_cols, colour, j, i)) then
                    values(k) = compressed(colour(i), j)
                else
                    values(k) = 0.0_dp
                    if (present(stat)) stat = 1
                end if
            end do
        end do
    end subroutine recover_symmetric

    logical function alone_in_row(row_start, row_cols, colour, row, col) &
        result(yes)
        !! Whether `col` is the only column of its colour nonzero in `row`.
        integer, intent(in) :: row_start(:), row_cols(:), colour(:), row, col
        integer :: k, other

        yes = .true.
        do k = row_start(row), row_start(row + 1) - 1
            other = row_cols(k)
            if (other == col) cycle
            if (colour(other) == colour(col)) then
                yes = .false.
                return
            end if
        end do
    end function alone_in_row

    subroutine seed_matrix(pattern, colour, n_colours, seeds)
        !! The tangent seeds: `seeds(c, j) = 1` when column `j` has colour `c`.
        !!
        !! Laid out with the direction index first, matching the layout fortad's
        !! vector forward mode expects, so this drops straight into the
        !! generated routine.
        type(sparsity_t), intent(in) :: pattern
        integer, intent(in) :: colour(:), n_colours
        real(dp), allocatable, intent(out) :: seeds(:, :)
        integer :: j

        allocate (seeds(n_colours, pattern%n_cols))
        seeds = 0.0_dp
        do j = 1, pattern%n_cols
            if (colour(j) >= 1 .and. colour(j) <= n_colours) then
                seeds(colour(j), j) = 1.0_dp
            end if
        end do
    end subroutine seed_matrix

    subroutine recover_entries(pattern, colour, compressed, values)
        !! Read the Jacobian entries back out of the compressed result.
        !!
        !! `compressed(c, i)` is row `i` of `J S` for colour `c`. Because no two
        !! columns of one colour share a row, that number is exactly the entry
        !! of whichever of them is nonzero in row `i`.
        !!
        !! `values` is returned in the same order as `pattern%rows`, so it pairs
        !! directly with the pattern the caller supplied.
        type(sparsity_t), intent(in) :: pattern
        integer, intent(in) :: colour(:)
        real(dp), intent(in) :: compressed(:, :)
        real(dp), allocatable, intent(out) :: values(:)
        integer :: j, k

        allocate (values(size(pattern%rows)))
        do j = 1, pattern%n_cols
            do k = pattern%col_start(j), pattern%col_start(j + 1) - 1
                values(k) = compressed(colour(j), pattern%rows(k))
            end do
        end do
    end subroutine recover_entries

    subroutine order_by_degree(pattern, order)
        !! Column indices sorted by decreasing nonzero count.
        type(sparsity_t), intent(in) :: pattern
        integer, allocatable, intent(out) :: order(:)
        integer, allocatable :: degree(:)
        integer :: i, j, tmp

        allocate (order(pattern%n_cols), degree(pattern%n_cols))
        do j = 1, pattern%n_cols
            order(j) = j
            degree(j) = pattern%col_start(j + 1) - pattern%col_start(j)
        end do

        ! Insertion sort: the column count is small next to the derivative
        ! sweeps this saves, so clarity beats asymptotics here.
        do i = 2, pattern%n_cols
            j = i
            do while (j > 1)
                if (degree(order(j)) <= degree(order(j - 1))) exit
                tmp = order(j)
                order(j) = order(j - 1)
                order(j - 1) = tmp
                j = j - 1
            end do
        end do
    end subroutine order_by_degree

    logical function valid(pattern) result(ok)
        !! Structural checks on a supplied pattern.
        type(sparsity_t), intent(in) :: pattern
        integer :: j

        ok = .false.
        if (pattern%n_rows < 1 .or. pattern%n_cols < 1) return
        if (.not. allocated(pattern%col_start)) return
        if (.not. allocated(pattern%rows)) return
        if (size(pattern%col_start) /= pattern%n_cols + 1) return
        if (pattern%col_start(1) /= 1) return
        if (pattern%col_start(pattern%n_cols + 1) /= size(pattern%rows) + 1) return
        do j = 1, size(pattern%rows)
            if (pattern%rows(j) < 1 .or. pattern%rows(j) > pattern%n_rows) return
        end do
        ok = .true.
    end function valid

end module fortad_sparse
