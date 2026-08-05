# P0.8 decision gate

Date: 2026-08-05

## Evidence

| gate | evidence | result |
| --- | --- | --- |
| Frontend coverage | 161 of 161 `fortnum/src` files parsed and passed semantic analysis. See [coverage.md](coverage.md). | pass |
| Type and binding information | Focused fortfront query tests cover resolved types, scope bindings, derived types, and type-bound procedures. | pass |
| Derivative correctness | The VMEC++ hand-written JVP and VJP pass the central-difference sweep, scalar loss agreement, and arbitrary-cotangent adjoint identity. | pass |
| Runtime performance | On one pinned AMD EPYC 7282 core, hand Fortran takes 16.50 us per JVP and 20.50 us per VJP. C++/Enzyme takes 8.66 us forward and 13.68 us reverse. | fail |

The hand-written Fortran JVP is 1.90 times slower than the C++/Enzyme forward
reference. The hand-written VJP is 1.50 times slower than the reverse
reference. Peak RSS is 3348 kB for hand Fortran and 3280 kB for the reference.
The complete validation record is in fortad-bench commit `eafcd1c`.

## Decision

The original universal performance thesis is rejected by its own gate. The
frontend and correctness parts of the project are viable. The VMEC++
performance comparison does not support a claim that a hand-written Fortran
derivative matches C++/Enzyme.

P0.8 is therefore a no-go for the original thesis and a trigger to rewrite the
roadmap before treating Phase 1 as evidence for that thesis. The revised
roadmap targets a portable, correctness-checked source-transformation AD
engine with performance claims stated per workload and backed by measurements.
Every result keeps its compiler, hardware, build time, code size, runtime, and
memory record. A competitor win remains an optimization target when the
evidence justifies the work. It is not silently reported as a fortad win.

The VMEC++ result remains the first performance challenge for generated code.
The next work must address the measured code-generation gap before making a
broader runtime claim.
