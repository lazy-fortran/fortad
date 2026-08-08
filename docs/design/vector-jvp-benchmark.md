# Batched vector-JVP benchmark

FortAD's vector forward mode carries several tangent directions through one
primal sweep. This benchmark measures that mode on a common modern-Fortran
pattern: an elementwise loop over a large rank-one array with intrinsic
`sin` and `exp` calls. The comparison is repeated scalar JVP calls generated
from the same source, so it measures the batching benefit without changing
the mathematical workload or relying on another engine's output format.

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

The recorded local run used GNU Fortran 16.1.1, eight directions, 131,072
elements, and 24 timed repetitions per sample. Its five ratios were
`0.2280`, `0.2212`, `0.1487`, `0.2259`, and `0.2236`; the median was `0.2236`.
