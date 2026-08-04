program test_sparse_oracle
    !! Independent behavioural oracle for compressed Jacobians.
    !!
    !! Two properties, checked separately because they fail separately:
    !!
    !! 1. **The colouring is valid.** No two columns of the same colour share a
    !!    row. This is checked directly against the pattern, not inferred from
    !!    the recovered values, because an invalid colouring can still produce
    !!    plausible-looking numbers.
    !! 2. **Recovery is exact.** The entries read back out of the compressed
    !!    product equal the dense Jacobian entries, to the last bit. Compression
    !!    is a rearrangement, not an approximation, so anything short of exact
    !!    equality is a bug.
    !!
    !! The reference Jacobians here are written out explicitly rather than
    !! generated, so the compression is checked against a matrix the colouring
    !! code had no part in producing. The colour counts are asserted too - one
    !! colour for a diagonal, three for a tridiagonal, n for a dense matrix -
    !! because a colouring that is merely valid but uses n colours everywhere
    !! would pass every correctness check and deliver no speedup at all.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortad_sparse, only: sparsity_t, colour_columns, seed_matrix, &
                             recover_entries
    implicit none

    integer :: failures

    failures = 0

    call test_banded(failures)
    call test_arrowhead(failures)
    call test_diagonal_is_one_colour(failures)
    call test_dense_column_forces_many(failures)
    call test_symmetric_hessian(failures)
    call test_malformed_pattern_is_refused(failures)

    if (failures == 0) then
        print *, "test_sparse_oracle: all cases passed"
    else
        print *, "test_sparse_oracle: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine test_banded(failures)
        !! A tridiagonal Jacobian. Three colours suffice and the greedy
        !! algorithm should find that.
        integer, intent(inout) :: failures
        type(sparsity_t) :: pattern
        real(dp), allocatable :: dense(:, :)
        integer, parameter :: n = 12

        call build_banded(n, 1, pattern, dense)
        call check_pattern("banded", pattern, dense, failures, expect_max=3)
    end subroutine test_banded

    subroutine test_arrowhead(failures)
        !! One dense row and one dense column. The dense column conflicts with
        !! every other, so it needs a colour of its own - the case where a naive
        !! colouring silently produces something undecompressable.
        integer, intent(inout) :: failures
        type(sparsity_t) :: pattern
        real(dp), allocatable :: dense(:, :)
        integer, parameter :: n = 10
        integer :: j, k, nnz

        nnz = 0
        do j = 1, n
            if (j == 1) then
                nnz = nnz + n
            else
                nnz = nnz + 2
            end if
        end do

        pattern%n_rows = n
        pattern%n_cols = n
        allocate (pattern%col_start(n + 1), pattern%rows(nnz))
        allocate (dense(n, n))
        dense = 0.0_dp
        k = 0
        do j = 1, n
            pattern%col_start(j) = k + 1
            if (j == 1) then
                block
                    integer :: i
                    do i = 1, n
                        k = k + 1
                        pattern%rows(k) = i
                        dense(i, j) = 1.0_dp + 0.1_dp*i
                    end do
                end block
            else
                k = k + 1
                pattern%rows(k) = 1
                dense(1, j) = 0.5_dp*j
                k = k + 1
                pattern%rows(k) = j
                dense(j, j) = 2.0_dp + 0.3_dp*j
            end if
        end do
        pattern%col_start(n + 1) = k + 1

        call check_pattern("arrowhead", pattern, dense, failures)
    end subroutine test_arrowhead

    subroutine test_diagonal_is_one_colour(failures)
        !! A diagonal Jacobian has no conflicts at all, so the whole thing must
        !! come out of a single tangent sweep. If this needs more than one
        !! colour, the conflict test is over-eager and every sparse case pays.
        integer, intent(inout) :: failures
        type(sparsity_t) :: pattern
        real(dp), allocatable :: dense(:, :)
        integer, parameter :: n = 8

        call build_banded(n, 0, pattern, dense)
        call check_pattern("diagonal", pattern, dense, failures, expect_max=1)
    end subroutine test_diagonal_is_one_colour

    subroutine test_dense_column_forces_many(failures)
        !! A fully dense Jacobian cannot be compressed: every column conflicts
        !! with every other, so the colouring must use n colours and gain
        !! nothing. A method that claimed fewer would be losing entries.
        integer, intent(inout) :: failures
        type(sparsity_t) :: pattern
        real(dp), allocatable :: dense(:, :)
        integer, parameter :: n = 5
        integer :: i, j, k

        pattern%n_rows = n
        pattern%n_cols = n
        allocate (pattern%col_start(n + 1), pattern%rows(n*n))
        allocate (dense(n, n))
        k = 0
        do j = 1, n
            pattern%col_start(j) = k + 1
            do i = 1, n
                k = k + 1
                pattern%rows(k) = i
                dense(i, j) = real(i, dp) + 0.25_dp*j
            end do
        end do
        pattern%col_start(n + 1) = k + 1

        call check_pattern("dense", pattern, dense, failures, expect_max=n, &
                           expect_min=n)
    end subroutine test_dense_column_forces_many

    subroutine test_symmetric_hessian(failures)
        !! A Hessian is symmetric, and column colouring works on it unchanged:
        !! the compression only needs "no two columns of a colour share a row",
        !! which knows nothing about symmetry.
        !!
        !! It is not *optimal* for a symmetric matrix - star colouring exploits
        !! symmetry to use fewer colours - and this test pins the current cost
        !! so that a later star colouring can be shown to beat it rather than
        !! merely claimed to.
        integer, intent(inout) :: failures
        type(sparsity_t) :: pattern
        real(dp), allocatable :: dense(:, :)
        integer, parameter :: n = 9
        integer :: i, j, k, nnz

        ! A symmetric tridiagonal Hessian, as a separable-plus-coupling
        ! objective would produce.
        nnz = 0
        do j = 1, n
            do i = max(1, j - 1), min(n, j + 1)
                nnz = nnz + 1
            end do
        end do

        pattern%n_rows = n
        pattern%n_cols = n
        allocate (pattern%col_start(n + 1), pattern%rows(nnz))
        allocate (dense(n, n))
        dense = 0.0_dp
        k = 0
        do j = 1, n
            pattern%col_start(j) = k + 1
            do i = max(1, j - 1), min(n, j + 1)
                k = k + 1
                pattern%rows(k) = i
                ! Symmetric by construction: the value depends only on the
                ! unordered pair.
                dense(i, j) = 1.0_dp + 0.5_dp*(i + j) - 0.25_dp*abs(i - j)
            end do
        end do
        pattern%col_start(n + 1) = k + 1

        do j = 1, n
            do i = 1, n
                if (dense(i, j) /= dense(j, i)) then
                    print *, "FAIL symmetric_hessian: test matrix is not symmetric"
                    failures = failures + 1
                    return
                end if
            end do
        end do

        call check_pattern("symmetric_hessian", pattern, dense, failures, &
                           expect_max=3)
    end subroutine test_symmetric_hessian

    subroutine test_malformed_pattern_is_refused(failures)
        !! A pattern whose indices do not line up must be reported, not used.
        integer, intent(inout) :: failures
        type(sparsity_t) :: pattern
        integer, allocatable :: colour(:)
        integer :: n_colours, stat

        pattern%n_rows = 3
        pattern%n_cols = 3
        allocate (pattern%col_start(4), pattern%rows(3))
        pattern%col_start = [1, 2, 3, 9]        ! inconsistent with rows
        pattern%rows = [1, 2, 3]

        call colour_columns(pattern, colour, n_colours, stat)
        if (stat == 0) then
            print *, "FAIL malformed_pattern: accepted an inconsistent pattern"
            failures = failures + 1
        else
            print *, "pass malformed_pattern"
        end if
    end subroutine test_malformed_pattern_is_refused

    subroutine build_banded(n, half, pattern, dense)
        !! A banded pattern with the given half-bandwidth.
        integer, intent(in) :: n, half
        type(sparsity_t), intent(out) :: pattern
        real(dp), allocatable, intent(out) :: dense(:, :)
        integer :: i, j, k, nnz

        nnz = 0
        do j = 1, n
            do i = max(1, j - half), min(n, j + half)
                nnz = nnz + 1
            end do
        end do

        pattern%n_rows = n
        pattern%n_cols = n
        allocate (pattern%col_start(n + 1), pattern%rows(nnz))
        allocate (dense(n, n))
        dense = 0.0_dp
        k = 0
        do j = 1, n
            pattern%col_start(j) = k + 1
            do i = max(1, j - half), min(n, j + half)
                k = k + 1
                pattern%rows(k) = i
                dense(i, j) = 1.0_dp + 0.5_dp*i - 0.25_dp*j
            end do
        end do
        pattern%col_start(n + 1) = k + 1
    end subroutine build_banded

    subroutine check_pattern(label, pattern, dense, failures, expect_max, &
                             expect_min)
        !! Colour, compress, recover, and check both properties.
        character(len=*), intent(in) :: label
        type(sparsity_t), intent(in) :: pattern
        real(dp), intent(in) :: dense(:, :)
        integer, intent(inout) :: failures
        integer, intent(in), optional :: expect_max, expect_min
        integer, allocatable :: colour(:)
        real(dp), allocatable :: seeds(:, :), compressed(:, :), values(:)
        integer :: n_colours, stat, i, j, k, c
        logical :: bad

        bad = .false.
        call colour_columns(pattern, colour, n_colours, stat)
        if (stat /= 0) then
            print *, "FAIL ", label, ": colouring refused a valid pattern"
            failures = failures + 1
            return
        end if

        ! Property 1: no two columns of a colour share a row.
        do i = 1, pattern%n_rows
            do c = 1, n_colours
                k = 0
                do j = 1, pattern%n_cols
                    if (colour(j) /= c) cycle
                    if (has_entry(pattern, i, j)) k = k + 1
                end do
                if (k > 1) then
                    print *, "FAIL ", label, ": colour", c, "collides in row", i
                    bad = .true.
                end if
            end do
        end do

        if (present(expect_max)) then
            if (n_colours > expect_max) then
                print *, "FAIL ", label, ": used", n_colours, &
                    "colours, expected at most", expect_max
                bad = .true.
            end if
        end if
        if (present(expect_min)) then
            if (n_colours < expect_min) then
                print *, "FAIL ", label, ": used", n_colours, &
                    "colours, expected at least", expect_min
                bad = .true.
            end if
        end if

        ! Property 2: recovery is exact. compressed = J S, seeds laid out with
        ! the direction index first, as vector forward mode produces.
        call seed_matrix(pattern, colour, n_colours, seeds)
        allocate (compressed(n_colours, pattern%n_rows))
        compressed = 0.0_dp
        do i = 1, pattern%n_rows
            do c = 1, n_colours
                do j = 1, pattern%n_cols
                    compressed(c, i) = compressed(c, i) + dense(i, j)*seeds(c, j)
                end do
            end do
        end do

        call recover_entries(pattern, colour, compressed, values)
        do j = 1, pattern%n_cols
            do k = pattern%col_start(j), pattern%col_start(j + 1) - 1
                if (values(k) /= dense(pattern%rows(k), j)) then
                    print *, "FAIL ", label, ": entry (", pattern%rows(k), ",", &
                        j, ") recovered as", values(k), "not", &
                        dense(pattern%rows(k), j)
                    bad = .true.
                end if
            end do
        end do

        if (bad) then
            failures = failures + 1
        else
            print *, "pass ", label, " (", n_colours, "colours for", &
                pattern%n_cols, "columns)"
        end if
    end subroutine check_pattern

    logical function has_entry(pattern, row, col) result(yes)
        !! Whether the pattern declares (row, col) structurally nonzero.
        type(sparsity_t), intent(in) :: pattern
        integer, intent(in) :: row, col
        integer :: k

        yes = .false.
        do k = pattern%col_start(col), pattern%col_start(col + 1) - 1
            if (pattern%rows(k) == row) then
                yes = .true.
                return
            end if
        end do
    end function has_entry

end program test_sparse_oracle
