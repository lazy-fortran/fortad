# Hascoet, Naumann, and Pascual 2005: To Be Recorded Analysis

The paper isolates the storage decision that makes reverse-mode source
transformation practical. A reverse sweep needs local derivative factors in
the opposite order from the forward computation. When the forward program
overwrites a value before the reverse sweep uses it, the old value must either
be recorded or recomputed. Recording every left-hand-side value is safe, but
it wastes memory and can make the tape the dominant cost.

## The analysis

The paper first identifies active variables. A variable is active when it can be
reached from an independent input through differentiable dependencies and can
reach a dependent output. This removes derivative work for values that cannot
affect the requested result.

TBR analysis then works in two passes. The bottom-up pass summarizes each
instruction or structured block with two sets:

- `Kill` contains variables whose incoming values are completely overwritten.
- `AdjU` contains variables used by the adjoint of the block.

For sequences, the required set is propagated across the first block after values
killed by the second block are removed. Branches union the requirements and
intersect the values guaranteed to be killed. Loops are handled by fixed-point
equations for general flow graphs and explicit equations for structured loops.
The summaries are synthesized through the call graph and then combined with
the calling context.

The top-down pass propagates the values required at each point in the forward
program. When an instruction overwrites a required value, the generated code
gets a push before the overwrite and a pop before the reverse statement that
needs the old value. A variable that is repeatedly overwritten but never used
in an active local derivative is absent from `AdjU` and needs no tape entry.

The analysis is conservative because static information about arrays, aliases,
and control flow is incomplete. A coarse array approximation can record an
entire array when only one element is active. Array-region analysis improves
the result. The paper proves termination by monotone growth of finite sets and
uses the same data-flow structure for dependency, activity, and TBR analyses.

## Evidence and limits

The Bratu case study reports 377 seconds with TBR and 466 seconds without TBR
on a 233 MHz Pentium II. In a Navier-Stokes residual, the three strategies
reported 2.09 MB and 1.01 seconds without TBR, 0.38 MB and 0.91 seconds when
only initialized values were recorded, and 0.12 MB and 0.77 seconds with TBR.
The paper also reports a factor-of-five tape reduction in a 70,000-line
industrial thermal-hydraulic code. These results show that the analysis affects
both memory and runtime.

## Consequences for fortad

Fortad should make TBR a named analysis between activity analysis and reverse
emission. The analysis should produce an auditable set of record or recompute
decisions rather than burying them in code generation. Its first behavioral
oracle can be a small hand-derived program with an overwrite and a nonlinear
use. The expected tape set must exclude overwritten passive values and retain
the old active values that local adjoints read.

The paper also sets a precision boundary. A safe coarse array approximation is
better than a missing tape entry, but it can erase the memory advantage. Array
regions, alias information, and control-flow joins are therefore optimization
work with a direct product metric. The implementation should report both the
number of recorded values and the resulting runtime and peak memory.

## Source

[INRIA preprint, HNP04](https://www-sop.inria.fr/tropics/papers/HNP04.pdf)

[Published paper, DOI 10.1016/j.future.2004.11.009](https://doi.org/10.1016/j.future.2004.11.009)
