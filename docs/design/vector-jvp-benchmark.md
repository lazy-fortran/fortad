# Batched vector-JVP benchmark

FortAD's vector forward mode carries several tangent directions through one
primal sweep. This benchmark measures that mode on a common modern-Fortran
pattern: an elementwise loop over a large rank-one array with intrinsic
`sin` and `exp` calls. The comparison is repeated scalar JVP calls generated
from the same source, so it measures the batching benefit without changing
the mathematical workload or relying on another engine's output format.

The forward rule also recognises the provable left-associated form `c*x*x`
when `c` is a literal and the inner tangent is exactly `c*dx`. It emits the
equivalent `2*c*x*dx` contribution. Active or nonlinear coefficients do not
match this rule and retain the ordinary product rule. This keeps the
optimisation local and evidence-based while removing redundant arithmetic from
the common elementwise path.

The benchmark driver independently evaluates

```text
y(i) = sin(x(i)) + 0.25*x(i)**2 + exp(-0.1*x(i))
dy(i) = (cos(x(i)) + 0.5*x(i) - 0.1*exp(-0.1*x(i)))*dx(i)
```

for every element and direction before timing. It rejects a generated result
that differs from that closed form, and also checks each scalar JVP against
the same oracle. Unsafe aliasing, global mutable state, and unsupported data
flow are outside this benchmark and remain named FortAD refusal boundaries.

Run the reproducible harness with:

```text
fo build
scripts/bench_vector_jvp.sh
```

The script generates scalar and batched routines from the same kernel, builds
both with the selected Fortran compiler (`FC`, default `gfortran`), checks five
timed samples, and reports the median `batch_over_scalar` ratio. A ratio below
`1.0` is the performance gate: batched forward mode must beat replaying one
scalar JVP for each direction. This is a FortAD feature measurement rather
than a universal claim about Tapenade or any other engine; the same kernel and
direction count can be used for an external engine comparison.

One pre-optimisation local run used GNU Fortran 16.1.1, eight directions,
131,072 elements, and 24 timed repetitions per sample. Its five ratios were
`0.228245052`, `0.229601616`, `0.235766604`, `0.235866386`, and `0.225098682`;
the median was `0.229601616`. The same harness after the `c*x*x` rule change
reported `0.213896324`, `0.213876228`, `0.220190588`, `0.227145068`, and
`0.232270783`; the median was `0.220190588`, a 4.10% lower
batch-over-scalar ratio. Both runs used the same source, compiler, direction
count, and repetition count. The independent analytic oracle passed in both
runs. The exact ratio is hardware- and load-dependent; rerun the script for a
current measurement.

For a second compiler on the same host, Flang 22.1.8 reported post-change
ratios `0.193212809`, `0.221924471`, `0.184794953`, `0.145450956`, and
`0.195044870`; the median was `0.193212809`. Its independent analytic oracle
also passed.
