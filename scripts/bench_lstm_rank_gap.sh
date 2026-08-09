#!/usr/bin/env bash
set -eu

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
flang=${FLANG:-flang}
tmp=$(mktemp -d "${TMPDIR:-/tmp}/fortad-lstm-rank-gap.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

cat >"$tmp/lstm.f90" <<'EOF'
subroutine lstm(n, z, y)
    integer, intent(in) :: n
    real(8), intent(in) :: z(n)
    real(8), intent(out) :: y
    real(8) :: cell, change, forget, hidden, ingate, outgate
    integer :: i
    cell = 0.2d0
    hidden = -0.1d0
    y = 0.0d0
    do i = 1, n
        forget = 1.0d0/(1.0d0 + exp(-(0.7d0*z(i) + 0.2d0)))
        ingate = 1.0d0/(1.0d0 + exp(-(-0.4d0*hidden + 0.1d0)))
        outgate = 1.0d0/(1.0d0 + exp(-(0.5d0*z(i) - 0.3d0)))
        change = tanh(0.8d0*hidden + 0.6d0*z(i))
        cell = cell*forget + ingate*change
        hidden = outgate*tanh(cell)
        y = y + log(2.0d0 + exp(hidden)) - 0.1d0*hidden
    end do
    y = y/real(n, 8)
end subroutine lstm
EOF

fo exec fortad --mode reverse --indep z --name lstm_vjp \
    --output "$tmp/lstm_vjp.f90" "$tmp/lstm.f90" >/dev/null
fo exec --no-build fortad --mode reverse --indep z --no-primal \
    --name lstm_grad --output "$tmp/lstm_grad.f90" "$tmp/lstm.f90" >/dev/null

"$flang" -O3 -c "$tmp/lstm_vjp.f90" -o "$tmp/lstm_vjp.o" \
    -module-dir "$tmp"
"$flang" -O3 -c "$tmp/lstm_grad.f90" -o "$tmp/lstm_grad.o" \
    -module-dir "$tmp"

cat >"$tmp/bench.f90" <<'EOF'
program bench_lstm_rank_gap
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none

    interface
        subroutine lstm_vjp(n, z, y, y_b, z_b)
            integer, intent(in) :: n
            real(8), intent(in) :: z(n)
            real(8), intent(out) :: y
            real(8), intent(in) :: y_b
            real(8), intent(out) :: z_b(n)
        end subroutine lstm_vjp
        subroutine lstm_grad(n, z, y_b, z_b)
            integer, intent(in) :: n
            real(8), intent(in) :: z(n)
            real(8), intent(in) :: y_b
            real(8), intent(out) :: z_b(n)
        end subroutine lstm_grad
    end interface

    integer, parameter :: sizes(5) = [100, 1000, 10000, 100000, 1000000]
    integer, parameter :: trials = 7
    real(real64), volatile :: checksum
    real(real64), allocatable :: z(:), z_b(:)
    real(real64) :: y, elapsed
    integer(int64) :: start, finish, rate
    integer :: i, j, n, rep, repetitions, trial

    call system_clock(count_rate=rate)
    checksum = 0.0_real64
    print '(a)', 'mode,size,repetitions,trial,seconds'
    do i = 1, size(sizes)
        n = sizes(i)
        repetitions = max(1, 1000000/n)
        allocate (z(n), z_b(n))
        z = [(0.17_real64*sin(0.31_real64*j) - &
            0.03_real64*j, j=1,n)]
        do trial = 1, trials
            call system_clock(start)
            do rep = 1, repetitions
                call lstm_vjp(n, z, y, 1.0d0, z_b)
                checksum = checksum + y
            end do
            call system_clock(finish)
            elapsed = real(finish-start, real64)/real(rate, real64)
            print '(a,1x,i0,1x,i0,1x,i0,1x,es16.8)', &
                'vjp', n, repetitions, trial, elapsed
        end do
        do trial = 1, trials
            call system_clock(start)
            do rep = 1, repetitions
                call lstm_grad(n, z, 1.0d0, z_b)
                checksum = checksum + z_b(1)
            end do
            call system_clock(finish)
            elapsed = real(finish-start, real64)/real(rate, real64)
            print '(a,1x,i0,1x,i0,1x,i0,1x,es16.8)', &
                'grad', n, repetitions, trial, elapsed
        end do
        deallocate (z, z_b)
    end do
    print '(a,1x,es16.8)', 'checksum', checksum

end program bench_lstm_rank_gap
EOF

"$flang" -O3 -o "$tmp/bench" "$tmp/bench.f90" \
    "$tmp/lstm_vjp.o" "$tmp/lstm_grad.o"

printf 'fortad_commit: %s\n' "$(git -C "$root" rev-parse --short HEAD)"
printf 'compiler: %s\n' "$($flang --version | head -1)"
printf 'generated_source_bytes: %s\n' "$(( $(wc -c <"$tmp/lstm_vjp.f90") + $(wc -c <"$tmp/lstm_grad.f90") ))"
printf 'object_bytes: %s\n' "$(( $(wc -c <"$tmp/lstm_vjp.o") + $(wc -c <"$tmp/lstm_grad.o") ))"
printf 'executable_bytes: %s\n' "$(wc -c <"$tmp/bench")"
printf 'sizes: 100,1000,10000,100000,1000000\n'
ulimit -s unlimited 2>/dev/null || true
"$tmp/bench"
