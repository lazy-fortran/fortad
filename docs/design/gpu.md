# GPU derivative emission

P6.2 applies to the fused, one-level positive reduction loops that already
have a race-free reverse transformation.  The emitter writes two adjacent
directives before the same loop:

```fortran
!$omp target teams distribute parallel do ...
!$acc parallel loop ...
do i = 1, n
```

Each compiler consumes its own directive and treats the other one as a
comment.  The loop IR supplies the data clauses: procedure variables read by
the loop are `map(to:)` / `copyin(...)`, and variables written by it are
`map(tofrom:)` / `copy(...)`.  Reduction locals are left implicit and appear
only in the corresponding reduction clause.  This avoids mapping procedure
arguments that are merely bounds or values used before the loop.

The ordinary compiler path remains valid. Without OpenMP or OpenACC enabled,
both lines are comments and the generated routine is serial standard
Fortran.  A GPU validation is not allowed to use that path as evidence.  The
OpenMP oracle sets `OMP_TARGET_OFFLOAD=MANDATORY`. Both the OpenMP and OpenACC
drivers independently query device execution and compare the generated VJP
against the analytic gradient of a dot-product-with-sine kernel.  The timing
wraps the complete call, including host/device transfers.

The committed validation uses TU Graz host `acluster` and its Tesla T4 with a
user-local NVIDIA HPC SDK 26.5 installation. CUDA 12.9 is selected because
the host driver is 535.261.03. The compiler targets the native T4 `cc75`
architecture. The P6.2 result file in `fortad-bench` records the exact flags,
device oracle, timings, memory use, and compiler reports.
The GCC NVPTX path remains a separately validated fallback on `faepop31`, not
the production GPU toolchain.
