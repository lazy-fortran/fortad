#!/usr/bin/env bash
set -eu

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
fortad_bin=${FORTAD_BIN:-"$root/build/fo/app/fortad"}

# This measures source transformation and module emission, not generated-code
# runtime.  The temporary kernel is deliberately large enough to expose the
# emitter's indentation/materialisation path.

test -x "$fortad_bin" || {
    echo "fortad executable not found: $fortad_bin (run fo build first)" >&2
    exit 2
}

tmp=$(mktemp -d "${TMPDIR:-/tmp}/fortad-emitter-codegen.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

source="$tmp/emitter_codegen_kernel.f90"
{
    printf '%s\n' 'subroutine emitter_codegen_kernel(z, y)' \
        '    real(8), intent(in) :: z(2048)' \
        '    real(8), intent(out) :: y' \
        '    real(8) :: x' \
        '    y = 0.0d0'
    i=1
    while [ "$i" -le 2048 ]; do
        printf '    x = z(%d) + %dd-4\n' "$i" "$i"
        printf '    y = y + x*x\n'
        i=$((i + 1))
    done
    printf '%s\n' 'end subroutine emitter_codegen_kernel'
} >"$source"

printf 'fortad_commit: %s\n' "$(git rev-parse --short HEAD)"
printf 'fo_version: %s\n' "$(fo version 2>/dev/null | tail -1)"
printf 'gfortran: %s\n' "$(gfortran --version | head -1)"
printf 'kernel_assignments: 4096\n'
printf 'samples: 3\n'
printf 'seconds:\n'
for run in 1 2 3; do
    /usr/bin/time -f '%e' -o "$tmp/time" "$fortad_bin" jvp "$source" z \
        --proc emitter_codegen_kernel --name emitter_codegen_kernel_jvp \
        --module emitter_codegen --output "$tmp/generated.f90" >/dev/null
    cat "$tmp/time"
done | tee "$tmp/runs.log"

median=$(sort -n "$tmp/runs.log" | awk '{values[NR] = $1} END {print values[int((NR + 1) / 2)]}')
printf 'median_seconds: %s\n' "$median"
