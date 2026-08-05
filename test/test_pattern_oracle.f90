program test_pattern_oracle
    !! Independent oracle for static structural dependency propagation.
    !!
    !! The expected pattern is written from the kernel's mathematical data
    !! flow, not derived from the implementation's internal dependency sets.
    use fortad, only: fad_static_pattern, sparsity_t
    implicit none

    character(len=*), parameter :: source = &
        "subroutine kernel(x1, x2, x3, y1, y2, y3)"//new_line('a')// &
        "  real(8), intent(in) :: x1, x2, x3"//new_line('a')// &
        "  real(8), intent(out) :: y1, y2, y3"//new_line('a')// &
        "  real(8) :: t"//new_line('a')// &
        "  t = x1 + x2"//new_line('a')// &
        "  if (x3 > 0.0d0) then"//new_line('a')// &
        "    y1 = t * x3"//new_line('a')// &
        "  else"//new_line('a')// &
        "    y1 = x1 * x3"//new_line('a')// &
        "  end if"//new_line('a')// &
        "  y2 = x2"//new_line('a')// &
        "  y3 = 7.0d0"//new_line('a')// &
        "end subroutine kernel"
    type(sparsity_t) :: pattern
    integer :: stat, failures
    integer, parameter :: expected_start(4) = [1, 2, 4, 5]
    integer, parameter :: expected_rows(4) = [1, 1, 2, 1]

    failures = 0
    call fad_static_pattern(source, ["x1", "x2", "x3"], &
                            ["y1", "y2", "y3"], pattern, stat)
    if (stat /= 0) then
        print *, "FAIL static pattern status", stat
        failures = failures + 1
    else if (pattern%n_rows /= 3 .or. pattern%n_cols /= 3) then
        print *, "FAIL static pattern shape"
        failures = failures + 1
    else if (any(pattern%col_start /= expected_start)) then
        print *, "FAIL static pattern column pointers:", pattern%col_start
        failures = failures + 1
    else if (any(pattern%rows /= expected_rows)) then
        print *, "FAIL static pattern rows:", pattern%rows
        failures = failures + 1
    else
        print *, "pass static pattern propagation"
    end if

    if (failures == 0) then
        print *, "test_pattern_oracle: all cases passed"
    else
        error stop 1
    end if
end program test_pattern_oracle
