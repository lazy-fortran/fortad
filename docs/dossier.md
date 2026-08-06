# Pre-gate research dossier

Status: historical research record, frozen after the P0.8 decision on
2026-08-05.

This dossier records the hypotheses used to start fortad. It does not describe
the current API or claim current benchmark results. The
[P0.8 decision](design/go-no-go.md) rejected the original universal runtime
thesis after the hand-written VMEC++ comparison. The project continued with a
narrower goal: emit portable, correctness-checked derivative source and state
performance claims for the workload and toolchain that produced them.

Use the [main README](../README.md) for current capabilities, the
[product guide](products.md) for derivative layouts, and
[`fortad-bench`](https://github.com/lazy-fortran/fortad-bench) for measurements.
The [roadmap](../ROADMAP.md) records what was implemented after this analysis.

## Research question

The original question was whether a modern Fortran source transformer could
beat Enzyme while producing standard Fortran. The proposed mechanism was to
differentiate before Fortran array semantics, dummy-argument contracts, shapes,
and purity had been lowered to scalar compiler IR. That information could
support transformations that an LLVM-level tool would have to reconstruct or
could no longer express at the same level.

The question mixed four kinds of AD system:

| Level | Representation | Examples studied | Main tradeoff |
| --- | --- | --- | --- |
| surface syntax | parse tree close to source | Clad, Tangent, psyad | readable output and language types, with substantial normalization work |
| semantic IR | typed and name-resolved program | Tapenade IL, OpenAD/XAIF, LFortran ASR | tractable program analysis while source emission remains possible |
| compiler IR | optimized scalar SSA | Enzyme on LLVM or MLIR | compiler optimization before AD, with source-language facts lowered away |
| runtime trace | operations executed by one run | ADOL-C, CoDiPack, PyTorch, Adept | direct handling of dynamic control flow, with runtime recording costs |

fortad was placed at the semantic-IR level. The proposal combined fortfront's
typed frontend with a differentiation-specific IR owned by fortad.

## References considered

Tapenade supplied the principal source-transformation model. Its activity,
to-be-recorded, differentiable-liveness, and call-graph analyses defined the
minimum credible reverse pipeline. OpenAD supplied detailed public accounts of
similar analyses. PSyclone showed a maintained Fortran transformation project
with a typed IR and executable adjoint checks.

Enzyme was the runtime reference. Its strongest argument was optimization
order: compiler simplification before AD can expose a much smaller program than
source transformation over an unnormalized tree. Its LLVM placement also gives
it broad language coverage, parallel support, and mature handling of optimized
code. Any source-level competitor therefore needs its own inlining,
normalization, activity analysis, and generated-code optimization.

The Julia tools separated several design questions. Zygote illustrated the
difficulty of mutation in source-level reverse mode. Mooncake motivated typed,
pre-sized storage for mutation-heavy adjoints. ForwardDiff motivated batched
directions. ChainRules provided a model for explicit derivative rules.
SparseMatrixColorings provided a permissively licensed reference for sparse
recovery algorithms.

Clad supplied the closest C++ source-transformation comparison. Adept and
CoDiPack informed statement-level accumulation and tape costs. ADOL-C supplied
the familiar product names and the sparse-driver lineage. JAX supplied the
idea of defining local JVP rules and transposing their linear part for VJPs.

The Phase 0 reading notes contain the paper-level summaries:

- [Giering and Kaminski 1998](notes/giering-kaminski-1998-recipes.md)
- [Hascoet, Naumann, and Pascual 2005](notes/hascoet-naumann-pascual-2005-tbr.md)
- [Hascoet and Pascual 2013](notes/hascoet-pascual-2013-tapenade.md)
- [Moses and Churavy 2020](notes/moses-churavy-2020-enzyme.md)

## Original hypotheses

### Fortran semantics

The source-level argument depended on facts still visible in Fortran:

| Fortran fact | Proposed use |
| --- | --- |
| dummy-argument alias restrictions | dependence and mutation analysis |
| `intent(in)` | omit impossible writes |
| rank, shape, and `contiguous` | seed layout and loop generation |
| `pure` and `elemental` | effect analysis and recomputation decisions |
| whole-array expressions | retain array operations before scalar lowering |

These facts did not guarantee faster code. They identified transformations to
test. The resulting derivative still had to compile, vectorize, and win a
complete-workload measurement.

### Structured operations

Predicted gains also came from differentiating an operation's mathematics. For
`AX=B`, the derivative can reuse the primal factorization.
For a converged nonlinear root, the implicit-function theorem removes the
iteration history. A converged fixed point admits a second adjoint phase on the
linearized map. Selected numerical-library operations also have derivative
identities at the operation level.

This required an explicit rule registry. The application or library owns the
operation, its interface, and its validity conditions. fortad substitutes the
registered tangent or adjoint statements. The registrant must verify the rule
against an independent result.

### Batched directions

Many applications need several columns of a Jacobian or a covariance factor.
The proposal propagated several tangent directions through one primal traversal
so the primal work was shared. The initial design placed the direction index last.
The implementation places it first, which is the contiguous Fortran dimension
and matches `seed_matrix(color, column)`.

The dossier also proposed vector reverse mode. Version 0.1.0 has vector forward
mode and scalar-seed reverse mode. A Jacobian with several output seeds still
uses one VJP per seed.

### Build and portability

Generated `.f90` source removes the Enzyme plug-in and LLVM-version coupling
from the consumer build. That is a concrete dependency difference. It did not
justify the original claim of a categorical build-time win, so build time and
generated size became measured fields for each workload.

The implemented compiler matrix covers gfortran, ifx, flang-new, nvfortran,
and LFortran. The original dossier also named Cray and NAG without a compiler
gate. They are not part of the verified 0.1.0 claim.

## Algorithm choices

### First-order and higher-order modes

Scalar JVP and VJP were the baseline products. Vector forward mode was selected
for multiple directions. Forward-over-reverse was selected for
Hessian-vector products. Dense Hessians use repeated HVPs, and sparse Hessians
use coloring and recovery. Direct edge pushing remained conditional on an
independent implementation and a measured workload win. The P4.3 gate later
rejected it for lack of both.

Univariate Taylor propagation was selected for order three and above. Its
coefficient recurrences cost `O(d^2)` per operation and return all directional
derivatives through order `d`. The implemented source transformation is scoped
to straight-line scalar kernels.

### Analysis and reverse storage

The proposed analysis order was:

1. Normalize and inline supported same-file procedures.
2. Find values reachable from requested independents and useful to requested
   dependents.
3. Determine which primal values a reverse statement needs.
4. Choose recomputation or typed storage for each required value.
5. Emit the derivative and remove dead derivative work.

Reverse storage was ranked by cost. Values that no local derivative reads need
no storage. Cheap values can be recomputed. Scalars may remain live in local
variables. Loop histories use typed arrays sized from the loop bounds.
Long-running time integrations use checkpoint and replay schedules. A generic
dynamic tape was retained only as a possible fallback, not as the normal
representation.

### Sparsity and checkpointing

Structural propagation supplies a conservative Jacobian pattern for supported
source. Column coloring compresses compatible Jacobian directions. Star
coloring exploits Hessian symmetry. Pattern recovery must be exact. A missing
structural nonzero is a correctness failure, while an extra nonzero costs a
direction.

Revolve was selected for a known time-step count and fixed checkpoint budget.
fortad returns a schedule because the application owns its state, time-step
routine, checkpoint storage, and I/O policy.

### Emitted source

The generated program was required to preserve useful interfaces, avoid
allocation in differentiated loops, fuse derivative work where dependencies
permit, and expose loops a Fortran compiler can optimize. Common subexpression
elimination, dead-store removal, invariant hoisting, factoring, and recurrence
rewrites were proposed as machine-independent passes.

The implementation uses fortfront for parsing and semantic facts, a dedicated
fortad IR for transformation, and fortgen for text generation conventions.
The original proposal to emit through fortfront's AST emitter was not retained.

## P0.8 gate

The first decision gate used the VMEC++ `ComputeHalfGridJacobian` kernel. The
Fortran port and hand derivatives passed a central finite-difference sweep and
the arbitrary-cotangent adjoint identity. On one pinned AMD EPYC 7282 core, the
hand-written Fortran JVP took 16.50 microseconds and the VJP took 20.50
microseconds. The C++ and Enzyme reference took 8.66 microseconds forward and
13.68 microseconds reverse. Peak RSS was 3348 kB for the Fortran fixture and
3280 kB for the reference.

The frontend and correctness parts passed. The runtime comparison failed the
universal thesis. The [decision record](design/go-no-go.md) changed the project
goal before Phase 1 implementation continued.

## Results that survived implementation

Several architectural choices held up:

- fortfront supplied the typed facts needed at the lowering boundary, while
  fortad's own IR supported renaming and statement reordering;
- forward rules supplied partials used by reverse mode, so the modes did not
  require independent intrinsic rule tables;
- vector forward mode shared primal work across directions;
- fused reverse reduction loops removed a second array traversal;
- typed loop storage handled nonlinear recurrences, while affine recurrence
  analysis removed storage where the adjoint did not need it;
- structured rules covered `dgesv`, nonlinear roots, fixed points, and selected
  library operations;
- Revolve schedules, static patterns, sparse recovery, HVPs, and Taylor mode
  reached independently checked implementations.

The taped recurrence remains an open store-versus-recompute problem. A fortfem
forward kernel remains slower because the generated expression and compiler
vectorization do not match Enzyme's result. These cases are listed in the
[roadmap's open defects](../ROADMAP.md#open-defects).

## Current architecture

```text
Fortran source
    -> fortfront parsing and semantic analysis
    -> fortad lowering, normalization, analysis, and differentiation
    -> fortad optimization
    -> fortgen-backed Fortran text emission
    -> gfortran | ifx | flang-new | nvfortran | LFortran
```

fortsym remains an optional symbolic oracle and expression simplifier.
fortnum is the downstream numerical testbed. Expensive comparisons and the
third-party study corpus remain in `fortad-bench`, outside this repository.

## Reading this record

Statements above the P0.8 section describe the pre-gate reasoning. The results
sections state what later work established. Current callable behavior belongs
in the [public API](design/public-api.md), and current work belongs in the
[roadmap](../ROADMAP.md).
