#!/usr/bin/env bash
set -eu

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
fortad_bin=${FORTAD_BIN:-"$root/build/fo/app/fortad"}
fc=${FC:-gfortran}

test -x "$fortad_bin" || {
    echo "fortad executable not found: $fortad_bin (run fo build first)" >&2
    exit 2
}
command -v "$fc" >/dev/null || {
    echo "Fortran compiler not found: $fc" >&2
    exit 2
}

tmp=$(mktemp -d "${TMPDIR:-/tmp}/fortad-vector-jvp.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

"$fortad_bin" jvp "$root/bench/vector-jvp/kernel.f90" x \
    --proc vector_jvp_kernel \
    --name vector_jvp_scalar \
    --module vector_scalar \
    --output "$tmp/scalar.f90"
"$fortad_bin" jvp "$root/bench/vector-jvp/kernel.f90" x \
    --proc vector_jvp_kernel \
    --directions nd \
    --name vector_jvp_batch \
    --module vector_batch \
    --output "$tmp/batch.f90"

flags=(-std=f2018 -O3)
"$fc" "${flags[@]}" -J "$tmp" -c "$tmp/scalar.f90" -o "$tmp/scalar.o"
"$fc" "${flags[@]}" -J "$tmp" -c "$tmp/batch.f90" -o "$tmp/batch.o"
"$fc" "${flags[@]}" -J "$tmp" -I "$tmp" \
    "$root/bench/vector-jvp/driver.f90" "$tmp/scalar.o" "$tmp/batch.o" \
    -o "$tmp/benchmark"

runs=()
for run in 1 2 3 4 5; do
    output=$("$tmp/benchmark")
    printf '%s\n' "$output"
    ratio=$(awk '/batch_over_scalar/ {print $2}' <<<"$output")
    test -n "$ratio"
    runs+=("$ratio")
done

median=$(printf '%s\n' "${runs[@]}" | sort -n | awk '{values[NR] = $1} END {print values[int((NR + 1) / 2)]}')
awk -v ratio="$median" 'BEGIN {if (ratio >= 1.0) exit 1}' || {
    echo "batched vector JVP did not beat repeated scalar JVP: $median" >&2
    exit 1
}

echo "vector_jvp_oracle: pass"
echo "median_batch_over_scalar: $median"
