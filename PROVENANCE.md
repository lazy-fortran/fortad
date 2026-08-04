# Algorithm and benchmark provenance

Every algorithm fortad implements has a row here before it has an
implementation. A row distinguishes a method implemented from a **publication**
from code **adapted** under an upstream licence. No row currently records
adapted code, and none may without the review described in
[LEGAL.md](LEGAL.md) §3.

The machine-readable inspection baseline is
[`docs/upstreams.toml`](docs/upstreams.toml). It pins the revision, licence,
inspected paths, and intended use of every project fortad studies.

Inspection baseline established: 2026-08-04. Revisions are recorded by
`scripts/fetch_upstreams.py --licenses` into the gitignored
`docs/generated/license-inventory.md`; that file, not this one, is the record of
what was actually on disk.

## Core transformation

| Area | Mathematical source | Upstream code inspected | Use in fortad | Adapted |
|---|---|---|---|---|
| Tangent (forward) mode statement rules | Griewank & Walther 2008, ch. 3 | Tapenade docs; Tangent `grads.py` (Apache-2.0) | rule table for intrinsics and operators | no |
| Adjoint (reverse) statement construction | Giering & Kaminski, TOMS 24(4), 1998 | Tapenade docs; OpenAD/xaifBooster docs | per-statement adjoint templates | no |
| Reverse mode as JVP + transposition | JAX `interpreters/ad.py` design (Apache-2.0), Frostig et al. | JAX source read for design only | single linear-rule table serving both modes | no |
| SSA-form adjoint with control flow | Innes 2019, *Don't Unroll Adjoint* | Zygote.jl (MIT) read for design | block-argument stacking for reversed control flow | no |
| Activity analysis | Hascoët, Naumann & Pascual, FGCS 21, 2005 | xaifBooster, Enzyme `ActivityAnalysis.cpp` | forward/backward dataflow over fortfront AST | no |
| To-be-recorded (TBR) analysis | Hascoët, Naumann & Pascual, FGCS 21, 2005 | xaifBooster `TBR` docs | decides which values the forward sweep stores | no |
| Linearity analysis | Naumann 2012, ch. 4 | xaifBooster | avoids storing operands of linear operations | no |
| Cross-country elimination | Naumann, Math. Prog. 112, 2008 (NP-completeness); Griewank & Walther ch. 9 | xaifBooster `BasicBlockPreaccumulation` | heuristic vertex/edge elimination for local Jacobians | no |
| Statement-level preaccumulation | Hogan, TOMS 40(4), 2014 | Adept (Apache-2.0), CoDiPack docs (GPL, docs only) | local Jacobian per statement before accumulation | no |

## Storage, checkpointing, and control flow

| Area | Mathematical source | Upstream code inspected | Use in fortad | Adapted |
|---|---|---|---|---|
| Binomial checkpointing | Griewank & Walther, TOMS 26(1), 2000 (Revolve) | none | time-step adjoint schedule | no |
| Online / adaptive checkpointing | Stumm & Walther, SISC 2010 | none | schedule when step count is unknown | no |
| Multi-level checkpointing to disk | Heimbach, Hill & Giering, FGCS 21, 2005 | MITgcm `pkg/autodiff` (MIT) read for design | out-of-core adjoints | no |
| Store-versus-recompute policy | Griewank & Walther 2008, ch. 12 | Enzyme `CacheUtility.cpp` | cost model choosing between tape and recomputation | no |
| Fixed-point iteration adjoint | Christianson, OMS 3, 1994 | SU2 driver (LGPL, read for design only) | two-phase adjoint of converged iterations | no |
| Loop transposition preserving parallelism | Paszke et al., ICFP 2021 | JAX (Apache-2.0) | adjoint of indexed array loops without serialising | no |
| OpenMP parallel-loop adjoints | Hückelheim & Hascoët, TOMS 2022 | none | reduction-safe adjoints of parallel loops | no |

## Matrix-level and structured rules

| Area | Mathematical source | Upstream code inspected | Use in fortad | Adapted |
|---|---|---|---|---|
| LU, Cholesky, QR, eigen derivative rules | Giles, *Collected Matrix Derivative Results*, 2008 | Enzyme `BlasDerivatives.td`; Stan Math (BSD-3) | custom rules for LAPACK calls in fortnum | no |
| BLAS level-1/2/3 derivative rules | Giles 2008; standard | Enzyme `BlasDerivatives.td` for rule-table *format* | rule registry entries | no |
| Implicit differentiation of solves and roots | Blondel et al., NeurIPS 2022; standard IFT | pyadjoint (LGPL, design only) | custom rules for nonlinear solves, ODE/BVP, optimisers | no |
| Special-function derivatives | NIST DLMF; existing fortnum analytical kernels | Stan Math `rev/fun/` (BSD-3) as a cross-check | rule entries for fortnum special functions | no |
| Symbolic simplification of emitted derivative expressions | Caviness JACM 17, 1970; Moses CACM 14, 1971 | fortsym (our own, MIT) | optional fortsym pass over emitted expressions | in-project reuse |

## Sparsity and higher order

| Area | Mathematical source | Upstream code inspected | Use in fortad | Adapted |
|---|---|---|---|---|
| Sparse Jacobian compression | Curtis, Powell & Reid, IMA J. Appl. Math. 13, 1974 | none | seed-matrix construction | no |
| Coloring for Jacobians and Hessians | Gebremedhin, Manne & Pothen, SIAM Review 47(4), 2005 | ColPack (LGPL, algorithms from the paper); SparseMatrixColorings.jl (MIT) | distance-2, star, and acyclic coloring | no |
| Sparsity pattern propagation | CasADi sparsity algebra (LGPL, design only); Griewank & Walther ch. 7 | — | static pattern inference over the AST | no |
| Forward-over-reverse Hessians | Griewank & Walther 2008, ch. 5 | Sacado (BSD-3) | default Hessian path | no |
| Direct second-order reverse (edge pushing) | Gower & Mello, OMS 2012 | ADOL-C (EPL/GPL, paper only) | sparse-Hessian candidate | no |
| Higher-order univariate Taylor propagation | Griewank & Walther 2008, ch. 13 | Rapsodia | generated fixed-order Taylor kernels | no |
| Chunked vector forward mode | ForwardDiff.jl (MIT) design; ADIFOR seed matrices | ForwardDiff.jl | tangent-block width tuned to SIMD width | no |

## Uncertainty quantification and optimisation products

| Area | Mathematical source | Upstream code inspected | Use in fortad | Adapted |
|---|---|---|---|---|
| First-order second-moment propagation | Smith, *Uncertainty Quantification*, 2013 | none | `cov(y) = J cov(x) Jᵀ` product built on vector forward mode | no |
| Local sensitivity indices and their limits | Saltelli et al., 2008 | none | documented scope boundary for UQ claims | no |
| Gauss-Newton and HVP consumers | Nocedal & Wright, 2006 | none | selects which derivative object each optimiser gets | no |

## Benchmarks and corpora

| Corpus | Licence | Use | Ported |
|---|---|---|---|
| ADBench (GMM, BA, hand, LSTM) | MIT | cross-tool comparability with published Enzyme numbers | planned, with attribution |
| VMEC++ half-grid Jacobian kernel | MIT (Proxima Fusion) | Enzyme baseline we already measured; Fortran port is the head-to-head case | planned, with attribution |
| `differentiable-fortran` 1D heat step | MIT (this project) | fixed benchmark protocol and contract | reused |
| fortnum kernels | MIT (this project) | primary testbed | reused |
| fortsym-bench derivations | MIT (this project) | symbolic cross-check of emitted derivative expressions | reused |
| MITgcm verification cases | MIT | large-scale adjoint validation | metadata only for now |
| SU2 cases | LGPL-2.1 | black-box baseline, separate build | never ported |

## Independent oracles

No fortad result is accepted because another AD tool agrees with it. Each
derivative product is checked against at least one of:

1. a hand-derived analytical derivative in the test file,
2. central finite differences with a Taylor / step-size convergence test,
3. the adjoint identity `⟨u, Jv⟩ = ⟨Jᵀu, v⟩` on random `u`, `v`,
4. a symbolic derivative from fortsym,
5. complex-step differentiation where the kernel is analytic.

Agreement with Enzyme, Tapenade, or JAX is recorded as corroboration, never as
the oracle.
