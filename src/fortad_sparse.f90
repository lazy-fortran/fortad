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
