# Standards-conforming buffered reductions

FortAD has one deliberately narrow emitter optimization for a loop whose body
is a scalar positive reduction. The emitter accepts only unit-stride loops with
stable bounds, scalar intrinsic reduction variables, scalar intrinsic
assignments, and expressions made from arithmetic, array indexing, and known
pure intrinsics. Calls, raw statements, array/component targets, nested control
flow, and bounds that can be changed before the loop stay in the original form.

The accepted loop is emitted as two loops. The first computes one contribution
per iteration into an automatic local array. The second adds those contributions
in the original iteration order. Thus the transformation does not use
`-ffast-math`, OpenMP, or an unsafe compiler directive, and it preserves the
serial floating-point accumulation order. The trade-off is one temporary array
per reduction and the corresponding stack/storage cost.

## Independent correctness gate

`test/test_buffered_reduction_oracle.f90` generates the derivative, compiles it
with GFortran, and compares both the primal reduction and its tangent against
the independent closed form for

`f(z) = sum_i exp(a_i*b_i)` and
`df = sum_i exp(a_i*b_i)*(da_i*b_i + a_i*db_i)`.

Run it with:

```text
fo exec test_buffered_reduction_oracle
```

## Reproducible performance gate

The isolated benchmark compares the generated one-pass derivative with a
hand-written one-pass reference. It uses Flang `-O3`, without fast-math or
directives, requires the contribution loop to vectorize and the baseline
reduction not to vectorize, and runs five timed samples. The median must be at
least 10% faster:

```text
fo build
scripts/bench_buffered_reduction.sh
```

On the recorded Linux toolchain (Flang 22.1.8, GCC 16.1.1), the buffered form
measured roughly 0.70--0.73 of the baseline time. GFortran already vectorizes
the one-pass reduction on this workload, so this is a strict-Flang result, not
a universal compiler claim.
