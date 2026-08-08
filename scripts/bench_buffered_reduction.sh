#!/usr/bin/env bash
set -eu

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
fortad_bin=${FORTAD_BIN:-"$root/build/fo/app/fortad"}
flang=${FLANG:-flang-new}

test -x "$fortad_bin" || {
    echo "fortad executable not found: $fortad_bin (run fo build first)" >&2
    exit 2
}
command -v "$flang" >/dev/null || {
    echo "Flang compiler not found: $flang" >&2
    exit 2
}

tmp=$(mktemp -d "${TMPDIR:-/tmp}/fortad-buffered-reduction.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

"$fortad_bin" jvp "$root/bench/buffered-reduction/kernel.f90" z \
    --proc buffered_reduction_kernel \
    --name buffered_reduction_kernel_jvp \
    --module buffered_generated \
    --output "$tmp/generated.f90"

flang_flags=(-std=f2018 -O3 -Rpass=loop-vectorize -Rpass-missed=loop-vectorize)
"$flang" "${flang_flags[@]}" -c "$tmp/generated.f90" -o "$tmp/generated.o" \
    >"$tmp/generated.vec.log" 2>&1
"$flang" "${flang_flags[@]}" -c "$root/bench/buffered-reduction/baseline.f90" \
    -o "$tmp/baseline.o" >"$tmp/baseline.vec.log" 2>&1
"$flang" -std=f2018 -O3 "$root/bench/buffered-reduction/driver.f90" \
    "$tmp/generated.o" "$tmp/baseline.o" -o "$tmp/benchmark"

grep -q 'vectorized loop' "$tmp/generated.vec.log" || {
    echo 'buffered contribution loop was not vectorized' >&2
    cat "$tmp/generated.vec.log" >&2
    exit 1
}
grep -q 'loop not vectorized' "$tmp/baseline.vec.log" || {
    echo 'baseline reduction unexpectedly vectorized' >&2
    cat "$tmp/baseline.vec.log" >&2
    exit 1
}

for run in 1 2 3 4 5; do
    "$tmp/benchmark"
done | tee "$tmp/runs.log"
median=$(awk '/buffered_over_baseline/ {print $2}' "$tmp/runs.log" | \
    sort -n | awk '{values[NR] = $1} END {if (NR == 0) exit 1; print values[int((NR + 1) / 2)]}')
awk -v ratio="$median" 'BEGIN {if (ratio >= 0.90) exit 1}' || {
    echo "buffered reduction did not clear the 10% improvement gate: $median" >&2
    exit 1
}

echo "buffered_vectorization: pass"
echo "baseline_vectorization: pass (not vectorized)"
echo "median_buffered_over_baseline: $median"
