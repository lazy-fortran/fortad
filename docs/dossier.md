# Dossier: source-level automatic differentiation, and how to beat Enzyme

Status: research dossier, 2026-08-04. This is the reasoning behind
[`../ROADMAP.md`](../ROADMAP.md). It is opinionated on purpose: its job is to
name the best-of-breed algorithm for each part of the problem and say why the
alternatives lose, so that implementation work never revisits a settled
question.

Nothing here is a benchmark result. Every performance claim about fortad in this
document is a hypothesis with a stated test. The only measured numbers we own
today are the VMEC++ Enzyme kernel timings and the `differentiable-fortran`
heat-equation study, and both are cited as such.

---

## 1. What fortad is

fortad is a source-transformation automatic differentiation engine for Fortran.
It reads Fortran through [fortfront](#82-fortfront-as-the-differentiation-ir),
differentiates a semantically normalised form of the program, and emits
**standard Fortran** that any conforming compiler builds — gfortran, ifx,
flang-new, nvfortran, Cray, NAG, LFortran.

The three commitments that follow from that, in order of importance:

1. **No compiler plugin, no LLVM version lock.** Enzyme is an LLVM pass. Using
   it means matching a `ClangEnzyme-NN.so` to an exact LLVM, and for Fortran it
   means flang-new or LFortran specifically. Our own VMEC++ integration says so
   in its header: the intrinsics "do not link" without the plugin, and the CMake
   option is off by default. fortad's output is a `.f90` file.
2. **The derivative is readable, reviewable, and checkable in.** A generated
   adjoint that a domain scientist can read is a different product from an
   opaque IR transformation. It can be diffed, profiled line by line, patched by
   hand in an emergency, and cited in a paper.
3. **Everything, in every mode.** Forward (JVP), reverse (VJP), vector/batched
   forms of both, forward-over-forward, forward-over-reverse (HVP and Hessians),
   reverse-over-forward, sparse compressed Jacobians and Hessians, and
   higher-order Taylor coefficients. The products are linear UQ, sensitivity
   analysis, optimiser gradients, Gauss-Newton, and Hessians.

The target application is [fortnum](#83-fortnum-as-the-testbed), where fortad
becomes the second `autodiff` backend alongside Enzyme, competing against
`analytical` and `hybrid` candidates under fortnum's existing selection rules.

---

## 2. The landscape

### 2.1 What "source level" actually means

The phrase covers four genuinely different things, and conflating them is the
most common error in AD discussions.

| Level | Representation differentiated | Examples | Key consequence |
|---|---|---|---|
| **Surface AST** | the parse tree, near the tokens | Clad (Clang AST), Tangent (Python AST), psyad (PSyIR) | Output is source. Sees types, intent, shapes. Blind to the effects of optimisation. |
| **Semantic IR** | typed, name-resolved, control-flow-normalised program | Tapenade IL, OpenAD/XAIF, LFortran ASR, HLFIR, Julia typed SSA | Output can still be source. Analyses are tractable. **This is where fortad belongs.** |
| **Scalar compiler IR** | post-optimisation SSA over machine-ish types | Enzyme (LLVM IR), Enzyme-MLIR | Differentiates what will actually run. Loses language types, must rediscover them. |
| **Runtime trace** | the operations one execution performed | ADOL-C, CoDiPack, PyTorch, autograd, Adept | Trivially correct, control-flow-agnostic, pays tape cost at run time. |

Enzyme's published advantage — the "4.2×" over AD placed before optimisation —
is an argument for level 3 over levels 1 and 2 *as implemented in the tools it
compared against*. It is not an argument that levels 1 and 2 are intrinsically
slower, and §7 is the case that they are not.

### 2.2 Fortran

| Tool | Level | Modes | Licence | Verdict |
|---|---|---|---|---|
| **Tapenade** | semantic IR (Java) | tangent, adjoint, tangent-of-adjoint, checkpointing | Inria, non-commercial terms | **Best of breed in this class.** The analyses (activity, TBR, diff-liveness) are the ones to reimplement. Fortran 95-era front end; modern Fortran support is the weak point, and its output is not always idiomatic. |
| **OpenAD/F** | XAIF | adjoint, checkpointing | open | **Best documented internals.** xaifBooster is the canonical open implementation of TBR, activity and cross-country elimination. The Open64/mfef90 front end is effectively unmaintained. Study the algorithms, not the plumbing. |
| ADIFOR 2.0/3.0 | F77 source | forward, SparsLinC sparse | registration-gated, restrictive | Historical. SparsLinC's runtime sparse derivative vectors and the seed-matrix API are the durable ideas. **Papers only** (LEGAL.md §5). |
| TAF / TAC++ | F95 source | tangent, adjoint | commercial | The MITgcm adjoint's real engine. `Recipes for Adjoint Code Construction` is the citable content. Closed. |
| **PSyclone / psyad** | PSyIR | tangent-linear, adjoint | BSD-3 | **Best modern engineering.** A maintained Fortran source-to-source framework with a typed IR and a real test methodology for adjoint correctness. The closest structural analogue to fortfront + fortad, and a BSD licence means we may learn openly. |
| DNAD, AUTO_DERIV, Fazang | derived types | forward; Fazang also reverse tape | various | Correctness oracles and the naive baseline. Operator overloading in Fortran defeats vectorisation and inlining across the board. |
| Flang + Enzyme | LLVM IR | all | Apache-2.0 w/ LLVM exception | Works. Measured in `differentiable-fortran`. Requires flang-new plus a matching plugin. |
| LFortran + Enzyme | LLVM IR | all | BSD-3 + Enzyme | Same, through LFortran's LLVM backend. Also measured. |

**The gap fortad fills:** there is currently no maintained, permissively
licensed, modern-Fortran source-transformation AD tool. Tapenade is the closest
and it is neither modern-Fortran-complete nor permissively licensed. That is the
whole opportunity.

### 2.3 Julia

Julia is the most instructive ecosystem because it ran the experiment fortad is
about to run: source-level AD on a typed IR, against an LLVM-level tool, in the
same language, on the same programs.

| Tool | Level | Verdict |
|---|---|---|
| **Zygote.jl** | typed SSA IR, source-to-source | The published design (`Don't Unroll Adjoint`). Elegant for pure functional code. **Its failure mode is the lesson:** it handles mutation badly, and Fortran is mutation-first. Do not copy its pure-SSA adjoint. |
| **Mooncake.jl** | typed SSA IR | Current generation. Handles mutation with a typed, pre-allocated tape. **Best-of-breed reference for mutation-heavy adjoints** — the exact problem Fortran arrays pose. |
| **Enzyme.jl** | LLVM IR | Fastest in most Julia benchmarks. `BatchDuplicated` (vector mode) and `EnzymeRules` (custom rules) are the two features fortad must match. |
| ForwardDiff.jl | operator overloading | **Best-of-breed chunked vector forward mode.** The chunk-size/SIMD-width interaction is a directly transferable design. |
| **ChainRules.jl** | rule registry | **Best rule-registry design in existence.** `frule`/`rrule` split, thunks for lazy adjoints, projection back to the primal type, explicit opt-out. fortad's registry is judged against this. |
| Diffractor.jl | typed SSA, higher order | Ambitious ∞-order design; mostly a source of ideas about higher-order structure. |
| SparseMatrixColorings.jl | coloring | Clean modern implementation of star/acyclic coloring. Read instead of ColPack (MIT vs LGPL). |

**The finding that matters:** Julia's source-level tools generally lose to
Enzyme on scalar-heavy code and win nowhere structurally. But they lose to
Enzyme *inside the same compiler*, where Enzyme sees everything they see plus
optimisation. Fortran is different: Enzyme sees Fortran through flang's lowering
and has already lost `intent`, contiguity, shape, `pure`, and Fortran's aliasing
rules by the time it looks. That asymmetry is fortad's opening (§7.2).

### 2.4 Python

| Tool | Level | Verdict |
|---|---|---|
| **Tangent** (Google, archived) | Python AST → readable Python | **The closest product precedent to fortad.** Declarative rule table, per-statement adjoint templates, explicit refusal boundary (`fence.py`), readable output. Apache-2.0. Archived because Python is the wrong language for it, not because the design was wrong. |
| **JAX** | jaxpr → StableHLO | **The single most valuable architectural idea available:** define JVP only, then obtain VJP by *transposing the linear part*. `linearize = jvp + partial evaluation`. This halves the rule table and makes forward/reverse consistency structural rather than tested. |
| PyTorch | runtime tape / dynamo | `tools/autograd/derivatives.yaml` is the mature answer to "what does a declarative derivative-rule table look like at scale". |
| autograd | tracing | Minimal executable specification of reverse mode. Good for writing semantics tests against. |
| CasADi | symbolic graph → generated C | Sparsity-pattern algebra and the SX/MX split (scalar graph vs matrix graph). LGPL: design only. |
| pyadjoint / dolfin-adjoint | operator-level tape over PDE solves | **The structural-win exemplar.** Differentiating the *mathematics* of a solve instead of the *iterations* of a solve is orders of magnitude, not percent. §7.3. |

### 2.5 C/C++

| Tool | Level | Verdict |
|---|---|---|
| **Enzyme** | post-opt LLVM IR | The target. §3. |
| **Clad** | Clang AST → C++ | **The existence proof.** AST-level AD publishing competitive numbers against Enzyme and ADOL-C. Its reverse-mode visitor, tape-vs-recompute policy, and `custom_derivatives` are the closest thing to a blueprint. LGPL: clean-room. |
| Adept | expression templates + tape | Statement-level preaccumulation. Apache-2.0, well-benchmarked, citable. |
| CoDiPack | expression templates + tape | Fastest overloading tape (SU2). GPL-3: papers only, never link. |
| ADOL-C | tape | Reference semantics; home of `edge_pushing` Hessians and the ColPack sparse drivers. Its **driver API is the model for fortad's public surface**. |
| Sacado | templates | Nested forward-over-reverse in a production HPC stack. BSD-3. |
| Stan Math | arena tape | **Best available reference for special-function adjoints** — directly relevant to fortnum's `special/`. BSD-3. |
| dco/c++ | overloading | Commercial, NAG. Papers only. |

### 2.6 Rust and elsewhere

`std::autodiff` in nightly Rust routes a typed front end into Enzyme. It is
worth reading precisely because it shows what information a typed language
*has* to hand down to Enzyme by hand — activity, mutability, width — and that is
the information fortad simply keeps.

Swift for TensorFlow is dead, but its differentiable-programming design document
remains the best writeup of how a differentiability *type system* (differentiable
protocols, `@differentiable` function types, tangent-vector associated types)
constrains a source-level AD tool. Relevant to fortad's interface design.

---

## 3. Enzyme, in detail, as the thing to beat

### 3.1 Why it is good

1. **It differentiates optimised IR.** Inlining, SROA, LICM, GVN, dead-code
   elimination and loop simplification have already run. Activity analysis on
   optimised IR sees fewer, larger, more explicit operations.
2. **Type analysis.** LLVM IR has no types worth the name, so Enzyme rebuilds
   them with a dataflow analysis. This is impressive engineering *forced by the
   choice of level*.
3. **Language-agnostic.** One implementation serves C, C++, Fortran, Julia,
   Rust, Swift. Enormous leverage.
4. **Real parallel support.** OpenMP, MPI, and CUDA/ROCm kernels, published at
   SC'21 and SC'22.
5. **Mature.** Years of hardening on real codes.

### 3.2 Where it is structurally weak, and what we measured

From our own VMEC++ integration (`upstream/vmecpp`, header comments quoted in
fortad-bench's `docs/upstreams.toml`):

- **It forces the code to be written for it.** Enzyme's allocation analysis does
  not track Eigen's aligned allocator, so any heap temporary crossing the
  differentiated call aborts with *"freeing without malloc"*. Our differentiable
  kernels are therefore written allocation-free over flat, caller-owned buffers.
  That is a real, documented, permanent constraint on how the *primal* is
  allowed to be written. **A tool that constrains the primal has already lost
  part of the argument.**
- **It is a build-system dependency with a version lock.** `-fplugin=ClangEnzyme-NN.so`,
  matched to an exact LLVM. Our CMake option is off by default for that reason.
- **It differentiates the loop, not the mathematics.** Given a Newton solve or a
  fixed-point iteration, Enzyme faithfully adjoints every iteration and stores
  every intermediate. The Christianson two-phase adjoint (§5.6) is asymptotically
  cheaper and Enzyme cannot find it.
- **For Fortran specifically, it sees flang's output, not Fortran.** `intent(in)`
  has become a pointer. Contiguity has become a descriptor or a guess. Shapes
  have become runtime values. `pure` and `elemental` are gone. Enzyme must
  recover with alias analysis what the Fortran standard already guaranteed.

### 3.3 The honest scoreboard

Enzyme wins today on: scalar-heavy C/C++ code with irregular control flow;
codebases nobody will annotate; GPU kernels; anything where the primal was
written without AD in mind and cannot be touched.

Enzyme is beatable on: array-level Fortran with static shapes; anything with a
solve, an iteration, or a library call at its heart; anything where many
derivative directions are wanted at once; anything where build simplicity,
compiler portability, or reviewability has value.

fortnum is squarely in the second column. So is most of computational physics.

---

## 4. The algorithm catalogue

This is the full space. Best-of-breed choices are marked **[PICK]**, with build
time and generated-code performance both weighted, per the brief.

### 4.1 Modes

| Algorithm | Cost | When it wins | Verdict |
|---|---|---|---|
| Forward / tangent (JVP) | `O(n_dir · primal)` | few inputs, many outputs; UQ; sensitivity | **[PICK]** baseline. Cheap, no storage, trivially parallel. |
| Vector / batched forward | `O(primal + n_dir · flops_active)` | any time `n_dir > 1` | **[PICK]** The single biggest forward-mode win. One primal traversal, a contiguous tangent block per active variable, SIMD across directions. Enzyme's `BatchDuplicated` exists but is under-used; in Fortran a trailing tangent dimension is idiomatic and vectorises. |
| Reverse / adjoint (VJP) | `O(primal)` time, storage is the problem | many inputs, few outputs; gradients | **[PICK]** mandatory. |
| Vector reverse | amortises the forward sweep over several seeds | Jacobians of a few outputs | **[PICK]** |
| Cross-country / mixed | between the two, provably optimal is NP-hard | narrow "bottleneck" regions in the graph | **[PICK, scoped]** apply within basic blocks only (preaccumulation), never globally. Naumann 2008. |
| Forward-over-forward | second-order, dense | tiny problems, Hessian columns | secondary |
| **Forward-over-reverse** | `O(primal)` per HVP | HVPs, Newton-Krylov, `n` HVPs for a dense Hessian | **[PICK]** the default Hessian route. |
| Reverse-over-forward | equivalent HVP, different storage profile | when the reverse tape is the binding constraint | keep as an alternative candidate. |
| edge_pushing | direct second-order reverse sweep | *sparse* Hessians | **[PICK, phase 3]** genuinely better than `n` HVPs when the Hessian is sparse. Gower & Mello 2012. |
| Univariate Taylor propagation | `O(d²)` per operation for order `d` | order ≥ 3, Taylor-mode ODE integrators, singularity detection | **[PICK, phase 4]** via generated fixed-order kernels, Rapsodia-style. Never by nesting AD `d` times. |

### 4.2 Static analyses — where source level actually pays

These are the passes that decide whether generated code is fast or embarrassing.
Every one of them is easier on fortfront's typed AST than on LLVM IR.

| Analysis | What it does | Verdict |
|---|---|---|
| **Activity analysis** | which variables are on a path from an independent to a dependent | **[PICK]** mandatory. Forward "varied" ∧ backward "useful". Typically removes 40–80% of candidate derivative statements on real codes. |
| **To-be-recorded (TBR)** | which values the forward sweep must save for the reverse sweep | **[PICK]** mandatory. Hascoët et al. 2005. This is the difference between a tape and a few scalars. |
| **Linearity analysis** | operations whose partials are constants need no operand saved | **[PICK]** cheap, large effect on linear-algebra-heavy code. |
| **Differentiable liveness** | adjoint variables that are dead before use | **[PICK]** |
| **Alias / dependence analysis** | can two names touch the same storage | **[PICK]** and this is where Fortran hands us a gift: argument aliasing is *prohibited by the standard* unless declared. Enzyme must prove what we may assume. |
| **Shape and contiguity** | static extents, strides, `contiguous` attribute | **[PICK]** Fortran-only advantage; enables blocking and vectorisation decisions the IR level cannot make. |
| **Sparsity pattern propagation** | structural nonzeros of `J` and `H` before any evaluation | **[PICK, phase 3]** static beats CasADi's dynamic propagation where it applies. |
| **Purity / effect analysis** | `pure`, `elemental`, no I/O, no save | **[PICK]** free from the language; unlocks recompute-instead-of-store. |

### 4.3 Storage strategy for reverse mode

The whole difficulty of reverse mode. Options, in the order fortad should try
them per value:

1. **Don't need it** (activity / TBR / linearity says so). Free. **[PICK]**
2. **Recompute it** from live inputs. Costs flops, saves memory and bandwidth.
   **[PICK]** — and on modern hardware, recomputation usually beats a store,
   which is a bet Enzyme's `CacheUtility` also makes but with less information.
3. **Keep it in a register / scalar local.** Statement-level preaccumulation
   (Hogan 2014) turns most statements into this case. **[PICK]**
4. **Store to a typed, pre-sized stack.** Not a generic tape: a per-loop, typed,
   contiguous array whose size is known from the loop bounds. **[PICK]** This is
   the Mooncake lesson and it is what makes Fortran adjoints fast.
5. **Checkpoint and re-run.** Revolve for time stepping. **[PICK]**
6. **Generic dynamic tape.** The overloading tools' only option. **[REJECT]** as
   a default; keep as a fallback for constructs fortad refuses to analyse
   statically, and count every use as a defect.

### 4.4 Checkpointing

| Scheme | Verdict |
|---|---|
| **Revolve (binomial)** | **[PICK]** Optimal for known step count and fixed memory. Griewank & Walther 2000. Non-negotiable for time-stepping adjoints. |
| Online / adaptive (a-revolve) | **[PICK, phase 3]** when the step count is not known ahead. |
| Multi-level (RAM → disk) | **[PICK, phase 4]** MITgcm-scale problems. |
| Uniform / naive | reject except as a debugging mode. |

### 4.5 Structured rules — the largest single lever

An AD tool that differentiates *through* `dgesv` is doing arithmetic nobody
wants. The rules:

| Construct | Rule | Gain over differentiating through it |
|---|---|---|
| Linear solve `Ax = b` | forward: `A ẋ = ḃ - Ȧ x`; reverse: solve `Aᵀ λ = x̄`, then `Ā = -λ xᵀ`, `b̄ = λ` | reuses the *existing factorisation*: one triangular solve instead of adjointing all of LU. Order-of-magnitude. |
| Cholesky, QR, eigen, SVD | Giles 2008, closed forms | same |
| BLAS-3 | transposed BLAS-3 calls | keeps the derivative in optimised BLAS instead of in generated loops. Decisive. |
| FFT | derivative of a linear map is the map; adjoint is the conjugate transform | exact, free |
| Nonlinear solve / root find | implicit function theorem, one linear solve at the converged point | asymptotic — removes the entire iteration history from the tape |
| Fixed-point iteration | Christianson two-phase | asymptotic |
| ODE integration | discrete adjoint of the scheme, or continuous adjoint | large, and controls memory |
| Quadrature | differentiate the integrand, reuse nodes and weights | large |
| Special functions | closed-form derivative identities (DLMF recurrences) | exact, and avoids differentiating a series or a rational approximation — which is both slower *and* numerically worse |
| Interpolation / splines | derivative of the basis | exact |

**[PICK]** A ChainRules-style registry (`frule`/`rrule`, projection, opt-out) is
the mechanism. **This is where fortad beats Enzyme by construction**, because
fortnum's kernels already carry `analytical` derivative candidates and a
`fortnum_derivative_registry`. Enzyme has `BlasDerivatives.td` and gets some of
this; it does not and cannot get the implicit-solve and special-function layers
for an arbitrary Fortran library.

### 4.6 Sparsity

For `J ∈ ℝ^{m×n}` with a known pattern, evaluate `p ≪ n` compressed directions
instead of `n`.

- Pattern: **[PICK]** static propagation over the AST; fall back to probing.
- Compression: **[PICK]** distance-2 coloring (Jacobians), star coloring
  (symmetric Hessians, direct recovery), acyclic coloring (substitution
  recovery). Gebremedhin, Manne & Pothen 2005.
- Implementation: algorithms from the paper; read SparseMatrixColorings.jl (MIT)
  rather than ColPack (LGPL).
- Bidirectional (row+column) compression for Jacobians with both dense rows and
  dense columns: **[PICK, phase 3]**.

### 4.7 Emitted-code quality

Source-level AD lives or dies here. The generated Fortran must be code a good
programmer would have written.

**[PICK]** in the emitter:
- Common subexpression elimination on the derivative expressions, optionally via
  fortsym for the hard cases.
- Scalar replacement: derivative temporaries as scalar locals, never array
  temporaries.
- Loop fusion of the tangent update into the primal loop when TBR permits.
- Tangent-block dimension **last** (leftmost index innermost, per house style),
  contiguous, width a multiple of the SIMD width.
- Preserve `pure`, `elemental`, `contiguous`, and `intent` on generated
  procedures so the *user's* compiler can optimise them.
- Emit `!$omp` / `!$acc` directives mirroring the primal's, with reductions where
  the adjoint of a parallel loop needs them (Hückelheim & Hascoët 2022).
- Never emit an allocation inside a differentiated loop.
- Emit `-O3`-friendly straight-line code and then *let gfortran/ifx/flang do the
  optimising*. This is the point of the whole design: we do not need to
  reimplement LICM, we need to not obstruct it.

**[REJECT]:** emitting derived types with overloaded operators; emitting
allocatable temporaries per statement; emitting a generic tape module.

---

## 5. The thesis: how fortad beats Enzyme

Six mechanisms. The first three are structural and are where the win comes from;
the last three are the ones that stop us losing.

### 5.1 Differentiate above scalar IR, and keep the array

Enzyme sees `tau[ih] = tau1 + dSHalfDsInterp*tau2` as scalar loads and stores
after flang has lowered the array expression. fortad sees a Fortran array
assignment with known rank, known extents, and a `contiguous` guarantee. The
adjoint of an array expression is an array expression; the adjoint of its
scalarised form is a loop nest that some other pass then has to re-vectorise.

Enzyme-JAX exists because the Enzyme authors reached the same conclusion:
AD interleaved with high-level tensor optimisation beats AD at scalar level. We
get that level for free from Fortran.

### 5.2 Use the semantics Fortran gives and LLVM has thrown away

| Fortran guarantees | Enzyme must | fortad may assume |
|---|---|---|
| dummy arguments do not alias unless declared | prove non-aliasing | assume it |
| `intent(in)` is not modified | prove it | read it |
| `contiguous` | infer strides | read it |
| shapes and ranks are static or descriptor-carried | recover with type analysis | read them |
| `pure`, `elemental` | infer effects | read them |
| `real(dp)` vs `integer` distinction | type analysis pass | read it |

Enzyme's whole `TypeAnalysis/` directory exists to recover information the
Fortran standard states outright. That is not a criticism of Enzyme — it is the
price of language-agnosticism, and fortad is not paying it.

### 5.3 Differentiate the mathematics, not the iterations

The largest wins are asymptotic, not constant-factor, and they all come from
custom rules (§4.5): implicit differentiation of solves, Christianson's
two-phase fixed-point adjoint, reuse of factorisations, adjoints of BLAS as
BLAS, exact derivatives of special functions. Enzyme differentiates the
implementation because it can see nothing else. fortad differentiates the
*declared operation* because fortnum tells it what the operation is.

On a code whose inner loop is a Newton solve — which is most of computational
physics — this alone decides the benchmark.

### 5.4 Vector mode as the default, not an option

Linear UQ wants `J` applied to a covariance factor. Sensitivity analysis wants
many directions. Gauss-Newton wants many JVPs. Hessians want `n` HVPs. Almost no
real workload wants exactly one direction.

A single primal traversal carrying a contiguous block of `k` tangents costs
`primal + k·active_flops`, not `k·(primal + active_flops)`. For `k` = 8–32 with
SIMD, this is a several-fold win that a scalar-shadow tool never gets.
ForwardDiff.jl's chunking is the design; a trailing contiguous tangent dimension
is the Fortran realisation.

### 5.5 Build time as a first-class metric

Enzyme costs: build or install a matching LLVM, build the plugin, keep them in
sync, and pay whole-program LLVM optimisation plus the AD pass on every build.
fortad costs: run a Fortran-speed AST transformation, then compile the generated
`.f90` — and the generated file only changes when the primal changes, so it
caches under `fo` like any other source.

The brief asks for "reasonable build time". fortad's build-time story is not
reasonable, it is *categorically better*, and that is a product advantage worth
as much as the runtime one in practice.

### 5.6 Not losing: where source level historically failed

Honest accounting of why Tapenade-class tools lost to Enzyme, and the answer in
each case:

| Historical failure | fortad's answer |
|---|---|
| AD before inlining, so activity analysis is coarse and cross-procedure derivative traffic is large | Inline aggressively **in fortfront's IR** before differentiating. We control the pipeline; we can run our own inliner, constant propagation, and loop normalisation first. This removes Enzyme's main structural advantage. |
| Generated code the compiler cannot optimise (array temporaries, aliasing it can't rule out) | Emit with `contiguous`, `intent`, `pure`, scalar temporaries, no allocation on the hot path. Measure the emitted code's vectorisation reports as a gate. |
| Conservative analyses giving up on real code | Refuse loudly and specifically instead of degrading silently (Tangent's `fence` model). A named refusal is a bug report; a silent slow path is a lost benchmark. |
| Incomplete language coverage | fortfront's coverage is the bound, and it is a project we own and can extend. |
| Adjoints of parallel loops serialising | Paszke et al. 2021 for index-set transposition; Hückelheim & Hascoët 2022 for OpenMP reductions. Both are known-solved. |

---

## 6. Best of breed — the short list

If only these are implemented, fortad is competitive:

1. **JVP-plus-transposition architecture** (JAX). One linear rule table, reverse
   mode derived from it. Halves the work and makes the modes consistent by
   construction.
2. **Activity + TBR + linearity analysis** (Tapenade, xaifBooster). The three
   passes that decide whether the output is fast.
3. **Statement-level preaccumulation** (Adept, Clad). Local Jacobian per
   statement, in registers.
4. **Typed, pre-sized, per-loop storage instead of a tape** (Mooncake).
5. **A ChainRules-style custom-rule registry** with implicit differentiation for
   solves and BLAS/LAPACK/FFT/special-function rules (Giles 2008, Blondel 2022,
   fortnum's existing analytical kernels). Largest single lever.
6. **Chunked vector mode with a contiguous trailing tangent dimension**
   (ForwardDiff.jl, ADIFOR seed matrices).
7. **Revolve checkpointing** for time stepping.
8. **Forward-over-reverse HVPs**, then star-coloring-compressed Hessians, then
   edge_pushing for sparse cases.
9. **An emitter that produces code gfortran vectorises**, verified by reading
   vectorisation reports in CI.
10. **Inline and normalise before differentiating.** The cheapest way to buy back
    most of Enzyme's post-optimisation advantage.

Deliberately **not** on the list: globally optimal cross-country elimination
(NP-hard, marginal); a general dynamic tape; operator overloading of any kind;
GPU codegen before the CPU story is measured and won.

---

## 7. Products delivered

The brief names four. Each maps to a specific derivative object, and the mapping
is what stops fortad building things nobody needs.

| Product | Object | Mode | Cost |
|---|---|---|---|
| **Linear UQ** | `cov(y) = J cov(x) Jᵀ`, or `J L` for a covariance factor `L` | vector forward, `k` = rank of `L` | one primal + `k` tangent sweeps |
| **Sensitivity analysis** | `∂y_i/∂x_j`, often a few rows or columns | forward if few inputs, reverse if few outputs, compressed if sparse | `min(n, m, p_colors)` sweeps |
| **Optimiser gradients** | `∇f` | reverse, one sweep | `O(primal)` — the cheap gradient principle |
| **Gauss-Newton** | `J` of residuals, `JᵀJ v` | vector forward + one reverse, matrix-free | `k` sweeps |
| **Newton-Krylov** | `H v` | forward-over-reverse | `O(primal)` per product |
| **Full Hessian** | `H` | `n` HVPs, or star-coloring-compressed, or edge_pushing if sparse | problem-dependent |

`fortnum`'s existing vocabulary — `autodiff`, `analytical`, `hybrid`, competing
candidates selected by measured complete-workload wall clock — is the right
frame and fortad adopts it unchanged.

---

## 8. Integration

### 8.1 Where fortad sits

```
   user's .f90 / .lf
          |
      fortfront            lex, parse, semantic analysis, typed AST
          |
   fortad normalise        inline, SSA-ish rename, loop normalise, canonicalise
          |
   fortad analyse          activity, TBR, linearity, aliasing, shapes, sparsity
          |
   fortad differentiate    JVP rules; transpose for VJP; checkpoint placement
          |
   fortad emit             standard Fortran, via fortfront's code generator
          |
   any Fortran compiler    gfortran | ifx | flang-new | nvfortran | LFortran
```

fortsym is an optional pass on the emitted expressions (CSE, simplification,
and an independent symbolic oracle for the tests). Enzyme remains a *benchmark
baseline* reached through `differentiable-fortran`, never a dependency.

### 8.2 fortfront as the differentiation IR

fortfront already exposes what fortad needs. From `src/fortfront.f90`:

- **Arena AST with stable indices** — `ast_arena_t`, `create_ast_arena`,
  `visit_node_at`, `get_node_type_id_from_arena`. An arena with integer handles
  is exactly right for building derivative graphs: adjoint nodes can reference
  primal nodes by index without pointer chasing.
- **Typed node set** — `assignment_node`, `binary_op_node`, `call_or_subscript_node`,
  `do_loop_node`, `if_node`, `where_node`, `forall_node`, `declaration_node`,
  `function_def_node`, `subroutine_def_node`, `derived_type_node`.
- **Semantic queries** — `query_resolved_type`, `resolve_name_in_scope`,
  `resolve_identifier_binding`, `get_scope_bindings`, the `BINDING_*` kinds
  (dummy argument, named constant, function result, associate name). This is the
  name resolution an AD tool otherwise has to build itself.
- **Structural queries** — `query_declarations`, `get_function_body_info`,
  `get_dummy_allocatable_attribute`, `get_derived_type_components`.
- **Emission** — `emit_fortran`, plus `format_options_t`. The output side is
  solved.
- **Subtree cloning** — `ast_subtree_clone`, needed constantly when the forward
  sweep is duplicated for recomputation.

**Gaps fortad must close, in fortfront or beside it:**

1. A **mutable transformation API**: today the surface is query-heavy and
   read-oriented. fortad needs to build and splice nodes. `transformation_api.f90`
   and the `ast/factory` directory are the starting point; this likely becomes an
   upstream fortfront contribution.
2. **`intent`, `contiguous`, `pure`, `elemental` attribute queries** — check what
   `declaration_query_t` already carries and extend it.
3. **A normalisation pass set** — inlining, loop normalisation, three-address
   canonicalisation. This belongs in fortad, not fortfront, because only fortad
   needs it. It is the piece that recovers Enzyme's post-optimisation advantage
   and it is where the real engineering is.
4. **Provenance mapping** from generated lines back to primal lines, for
   debuggable output.

Whether to differentiate fortfront's AST directly or a dedicated fortad IR
lowered from it is the **first architectural decision** the roadmap forces, and
the answer is almost certainly a dedicated IR: the transformations wanted
(normalisation, SSA renaming, graph elimination) are not AST edits.

LFortran's ASR is the documented fallback if fortfront's coverage of modern
Fortran proves insufficient for real fortnum kernels.

### 8.3 fortnum as the testbed

fortnum is already built for this, which is why it is the testbed rather than a
later adopter:

- `src/ad/fortnum_derivative_registry.f90` — a derivative registry exists.
- `docs/design/ad.md` — `autodiff` / `analytical` / `hybrid` candidate
  vocabulary is already public API language.
- `ROADMAP.md` rule 12: *"Keep one mathematical source of truth for value, JVP,
  VJP, and fused products. Generate mechanical derivatives and wrappers instead
  of manually duplicating expressions."* fortad is the tool that rule is waiting
  for.
- `ROADMAP.md` target matrix already lists Enzyme as the `autodiff` mechanism for
  CPU, forward and reverse. **fortad enters as a second `autodiff` candidate and
  is selected only if it wins on measured complete-workload wall clock**, exactly
  like every other candidate. No special pleading.
- `test/ad/`, `benchmark/`, `benchmark/reference/*.json`, and
  `docs/design/differentiation_benchmarks.md` — the measurement apparatus and
  committed baselines already exist.
- `tools/codegen/app/gen_stable_sqrt_difference.f90` — fortnum already generates
  Fortran. The generated-code review culture is in place.

The first fortad target inside fortnum should be a kernel with an existing
`analytical` candidate and committed benchmark numbers, so the first result is a
three-way comparison — fortad vs analytical vs Enzyme — with no new
infrastructure. The stable-sqrt-difference and the frozen-autodiff-JVP integrate
benchmarks both qualify.

### 8.4 fortsym as the speed and correctness reference

fortsym provides three distinct things:

1. **A symbolic oracle.** Differentiate a kernel symbolically, evaluate, compare
   against fortad's generated code. Independent of any other AD tool.
2. **A codegen quality bar.** fortsym generates Fortran kernels today and keeps
   the smallest kernel across engines. fortad's emitted expressions should be
   compared against fortsym's for the same expression, and where fortsym wins,
   fortad should call fortsym as an optional simplification pass.
3. **A CSE and simplification engine** that already exists, is MIT, and is ours.
   Reusing it is strictly better than writing a second expression simplifier.

The limit is scope: fortsym handles expressions, not programs. Loops, control
flow, mutation, and storage are fortad's problem alone. Symbolic differentiation
is not an alternative to AD, it is a peephole optimiser for AD's output.

### 8.5 VMEC++ as the head-to-head case

We have a measured Enzyme baseline on a real nonlinear kernel:
`ComputeHalfGridJacobian` (`vmecpp/vmec/ideal_mhd_model/jacobian_kernel.h`),
with `__enzyme_fwddiff` JVP, `__enzyme_autodiff` VJP, finite-difference checks,
and timings, in `vmecpp/common/enzyme/jacobian_kernel_autodiff_test.cc`.

Porting that kernel to Fortran gives the cleanest possible comparison: identical
mathematics, identical buffer layout, one C++/Enzyme number and one
Fortran/fortad number. It also directly tests §5.1 and §5.2, because the Fortran
version can be written *idiomatically* — with array expressions, `intent`, and
`contiguous` — rather than in the flat-buffer allocation-free form Enzyme forced
on the C++.

If fortad cannot win or draw on this kernel, the thesis is wrong and the roadmap
should be stopped and rewritten. That is what makes it the right first
benchmark.

---

## 9. Risks, stated plainly

| Risk | Severity | Mitigation |
|---|---|---|
| fortfront's modern-Fortran coverage is insufficient for real kernels | **high** | Measure coverage on fortnum's actual source before committing to the IR. LFortran ASR is the fallback. This is roadmap item 1 for a reason. |
| The emitted Fortran does not vectorise, and hand-written analytical wins anyway | high | Gate on compiler vectorisation reports in CI from the first kernel, not at the end. |
| Enzyme's post-optimisation advantage is larger than the normalisation pass recovers | medium | The VMEC++ head-to-head answers this early and cheaply. Do it before building the general machinery. |
| Reverse mode over mutation-heavy Fortran is harder than the Julia precedent suggests | medium | Mooncake is the reference; scope phase 1 to forward mode and prove the pipeline first. |
| Scope explosion — everything, every mode, every compiler | **high** | The roadmap is ordered by measured win per unit of work, not by taxonomic completeness. Forward-only on a single kernel is a shippable milestone. |
| Licence contamination from studying LGPL/GPL tools | medium | LEGAL.md §3–§4, `adapt = "none"` by default, PROVENANCE row before code. |
