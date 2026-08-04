# fortad roadmap

The target is a source-transformation AD engine for Fortran that **beats Enzyme
on measured complete-workload wall clock** on fortnum's kernels, emits standard
Fortran that every conforming compiler builds, and does so with a build-time
cost far below Enzyme's.

Ordering principle: **measured win per unit of work**, not taxonomic
completeness. Forward mode on one kernel that beats Enzyme is worth more than a
complete but unmeasured mode matrix. The reasoning behind every choice below is
in [docs/dossier.md](docs/dossier.md); this file does not re-argue it.

## Hard execution rules

Work through this file one checkbox at a time.

1. Select exactly one unchecked item.
2. Keep the implementation as small as possible while fully satisfying it.
3. Avoid overengineering. No speculative abstraction, no unused flexibility. Add
   a shared abstraction only after two real kernels need the same behaviour.
4. Add an **independent behavioural oracle**. A test that reproduces the
   implementation's own output is not a test. Acceptable oracles are listed in
   [PROVENANCE.md](PROVENANCE.md): hand-derived analytical derivatives, finite
   differences with a convergence test, the adjoint identity
   `⟨u, Jv⟩ = ⟨Jᵀu, v⟩`, fortsym, or complex-step. Agreement with Enzyme,
   Tapenade or JAX is corroboration, never the oracle.
5. Measure and report for every item: runtime and peak memory, before/after or
   candidate-vs-candidate, with workload, compiler, hardware and method
   recorded. Complete-workload wall clock is the primary metric.
6. Report **build time and generated-code size** alongside runtime. They are
   product requirements, not footnotes.
7. Measure scaling in active-input count, output count, and direction count
   wherever those can flip the forward-vs-reverse verdict.
8. Read the compiler's vectorisation report for every emitted kernel. A kernel
   the compiler refuses to vectorise is a failed item even if it is correct.
9. Any algorithm gets its [PROVENANCE.md](PROVENANCE.md) row **before** its
   implementation.
10. `git status --porcelain upstream literature docs/generated` must be empty.
11. Update committed benchmark evidence when an item affects performance.
12. Check off the item, commit implementation + tests + evidence + this file
    together, push, and only then select the next item.

A mechanism name never selects a winner. Production selection requires an
independent oracle plus measured application runtime and peak memory — the same
rule fortnum applies to every candidate, applied to fortad without exception.

Do not combine checklist items. If an item contains several independent changes,
split it into smaller checkboxes before writing code.

---

## Phase 0 — Establish the ground truth before building anything

The purpose of this phase is to find out early whether the thesis is wrong. Every
item is cheap and every item can kill or redirect the project.

- [ ] **P0.1 Fetch and licence-verify the study corpus.** Run
      `scripts/fetch_upstreams.py` and `--licenses`. Resolve every `VERIFY` in
      `docs/upstreams.toml` against the actual checkout. Any entry with no
      discoverable licence drops to metadata-only. Record the revisions.
- [ ] **P0.2 Resolve the bibliography.** `scripts/fetch_literature.py --resolve`,
      then `--fetch`. Fix titles that Crossref cannot match. Record which papers
      are open access and which need institutional retrieval.
- [ ] **P0.3 Read the four primary sources.** Hascoët & Pascual 2013 (Tapenade
      specification), Hascoët et al. 2005 (TBR), Giering & Kaminski 1998 (adjoint
      recipes), Moses & Churavy 2020 (Enzyme). Write a one-page note per paper
      into `docs/notes/`. This is a deliverable, not preparation.
- [ ] **P0.4 fortfront coverage measurement.** Parse every file in fortnum's
      `src/` through fortfront. Report: files parsed, files failed, constructs
      unsupported, and whether the resolved-type and binding queries return
      usable information for a representative kernel. **This decides whether
      fortfront or LFortran ASR is the front end**, and no IR work starts before
      it.
- [ ] **P0.5 fortfront transformation-API gap analysis.** Enumerate exactly what
      is missing to build and splice nodes (see dossier §8.2). Decide: extend
      `transformation_api.f90` upstream, or build a fortad IR lowered from the
      AST. Write the decision and its reasoning into `docs/design/ir.md`.
- [ ] **P0.6 VMEC++ Jacobian kernel, Fortran port, hand-differentiated.** Port
      `ComputeHalfGridJacobian` to idiomatic Fortran. Hand-write its JVP and VJP.
      Validate against finite differences and the adjoint identity. Benchmark
      against the existing C++/Enzyme numbers on the same machine. **This is the
      number fortad must beat, and the hand-written version is the ceiling.**
- [ ] **P0.7 Establish the benchmark harness.** Add fortad as a `solutions/`
      entry in `differentiable-fortran` so the protocol, contract and plots are
      inherited rather than reinvented. Baseline row for Enzyme and analytical.
- [ ] **P0.8 Decision gate.** Write `docs/design/go-no-go.md`: given P0.4 and
      P0.6, is the thesis intact? If fortfront cannot parse real kernels, or if
      the hand-written Fortran JVP does not at least match C++/Enzyme, stop and
      rewrite this roadmap before Phase 1.

## Phase 1 — Forward mode, one kernel, end to end

Smallest thing that is genuinely useful and genuinely measurable.

- [ ] **P1.1 fortad IR.** Whatever P0.5 decided. Typed, name-resolved,
      three-address, explicit control flow, arena-indexed. Round-trips to Fortran
      through fortfront's emitter with no semantic change — tested by running the
      round-tripped primal against the original on fortnum's test suite.
- [ ] **P1.2 Normalisation passes.** Inlining, loop normalisation, canonical
      three-address form, constant propagation. This is the pass set that buys
      back Enzyme's post-optimisation advantage (dossier §5.6) and it is worth
      doing well.
- [ ] **P1.3 Activity analysis.** Forward "varied" ∧ backward "useful". Report
      the fraction of statements eliminated on the VMEC++ kernel.
- [ ] **P1.4 JVP rule table.** Operators, intrinsics, `real(dp)` arithmetic. The
      table is declarative and separate from the transformation, in the shape of
      ChainRules' `frule` (dossier §6.1). Rules for array expressions, not only
      scalars.
- [ ] **P1.5 Scalar forward-mode transformation.** One tangent direction. Emit
      standard Fortran. Correct on the VMEC++ kernel against P0.6's hand-written
      JVP.
- [ ] **P1.6 Emitter quality pass.** CSE, scalar replacement, tangent update
      fused into the primal loop, `intent`/`contiguous`/`pure` preserved, no
      allocation in the loop. Gate: gfortran vectorises the emitted kernel.
- [ ] **P1.7 Benchmark: fortad JVP vs Enzyme vs analytical vs hand-written.**
      Runtime, peak memory, build time, generated-code size. Publish into
      `differentiable-fortran`. **This is the first result that means anything.**
- [ ] **P1.8 Vector forward mode.** Contiguous trailing tangent dimension, chunk
      width tuned to SIMD width. Measure the scaling in direction count against
      Enzyme's `BatchDuplicated` and against `k` separate scalar JVPs. Dossier
      §5.4 predicts this is the largest forward-mode win; verify or retract.
- [ ] **P1.9 Second and third kernels.** One from fortnum `special/`, one from
      `quadrature/` or `interp/`. Fix whatever breaks. Do not generalise before
      three kernels have demanded the same generalisation.

## Phase 2 — Reverse mode

- [ ] **P2.1 TBR analysis.** Hascoët et al. 2005. Report bytes stored with and
      without it on each Phase 1 kernel.
- [ ] **P2.2 Linearity analysis.** Report additional bytes saved.
- [ ] **P2.3 Transposition of the linear part.** Derive VJP from the JVP rules by
      transposition (dossier §6.1) rather than writing a second rule table. If
      this proves impractical for Fortran's mutation, record why in
      `docs/design/` and fall back to explicit adjoint rules — but try it first,
      because it is the difference between one rule table and two.
- [ ] **P2.4 Data-flow reversal for control flow.** Loops, branches, `where`,
      `forall`. Typed, pre-sized, per-loop storage — never a generic tape
      (dossier §4.3). Mooncake is the reference for mutation.
- [ ] **P2.5 Statement-level preaccumulation.** Local Jacobian per statement in
      registers. Hogan 2014. Measure against the non-preaccumulated adjoint.
- [ ] **P2.6 Reverse-mode benchmark vs Enzyme.** Gradient of a fortnum workload
      with many inputs. Runtime, peak memory, build time.
- [ ] **P2.7 Adjoint of parallel loops.** OpenMP reductions, race-free
      accumulation, index-set transposition that does not serialise. Hückelheim &
      Hascoët 2022; Paszke et al. 2021.

## Phase 3 — The structural wins

This phase is where the dossier claims fortad becomes uncatchable, because these
are asymptotic advantages Enzyme cannot obtain by seeing more IR.

- [ ] **P3.1 Custom-rule registry.** `frule`/`rrule`, projection, explicit
      opt-out. Wire it to fortnum's existing `fortnum_derivative_registry` so a
      kernel's `analytical` candidate becomes fortad's rule automatically.
- [ ] **P3.2 BLAS and LAPACK rules.** Giles 2008. Reverse of a linear solve
      reuses the existing factorisation. Measure against Enzyme differentiating
      through `dgesv`.
- [ ] **P3.3 Implicit differentiation of nonlinear solves and roots.** IFT at the
      converged point. Measure against Enzyme adjointing the iteration.
- [ ] **P3.4 Fixed-point adjoints.** Christianson two-phase.
- [ ] **P3.5 FFT, quadrature, interpolation, special-function rules.** Sourced
      from fortnum's analytical kernels and DLMF identities.
- [ ] **P3.6 Revolve checkpointing** for time-stepping adjoints. Griewank &
      Walther 2000.
- [ ] **P3.7 Sparsity: static pattern propagation, distance-2 and star coloring,
      compressed Jacobians and Hessians.**

## Phase 4 — Second order and higher

- [ ] **P4.1 Forward-over-reverse HVPs.** Default Hessian route.
- [ ] **P4.2 Dense Hessians** by `n` HVPs; **sparse Hessians** by star-coloring
      compression.
- [ ] **P4.3 edge_pushing** as a competing sparse-Hessian candidate. Keep only if
      it wins measurably.
- [ ] **P4.4 Higher-order Taylor kernels**, generated at fixed order, Rapsodia
      style. Never by nesting AD.

## Phase 5 — Products

- [ ] **P5.1 Linear UQ.** `cov(y) = J cov(x) Jᵀ` on the vector forward mode.
      Document precisely where first-order propagation stops being valid
      (Saltelli et al. 2008).
- [ ] **P5.2 Sensitivity analysis driver.** Mode selected automatically from
      input count, output count, and sparsity.
- [ ] **P5.3 Optimiser integration.** Gradients, Gauss-Newton `JᵀJv`,
      Newton-Krylov `Hv`, full Hessians, wired to fortnum's optimisers.
- [ ] **P5.4 Public API freeze.** ADOL-C's driver set is the model for the
      surface: `jacobian`, `hessian`, `jac_vec`, `vec_jac`, `hess_vec`, and the
      sparse variants.

## Phase 6 — Reach

- [ ] **P6.1 Compiler matrix.** gfortran, ifx, flang-new, nvfortran, LFortran,
      NAG. Emitted code builds and vectorises on all of them, in CI.
- [ ] **P6.2 GPU.** OpenMP target and OpenACC directives on emitted derivative
      code. Only after the CPU story is measured and won. Transfer-inclusive
      wall clock is the metric; silent host fallback is a failure.
- [ ] **P6.3 Standalone CLI.** `fortad --mode=reverse --dep=f --indep=x kernel.f90`
      so fortad is usable outside the lazy-fortran stack.

---

## Explicitly out of scope

Recorded so they are not rediscovered as good ideas:

- Globally optimal cross-country elimination. NP-hard (Naumann 2008), marginal
  gain. Basic-block preaccumulation only.
- A general dynamic tape as the default storage strategy. Fallback only, and
  every use counts as a defect.
- Operator overloading of any kind. It defeats vectorisation and inlining.
- Symbolic differentiation as the engine. fortsym is a peephole optimiser for
  fortad's output, not an alternative to it.
- Replacing Enzyme. Enzyme stays in fortnum as a competing `autodiff` candidate
  and as the benchmark baseline. If Enzyme wins a workload, Enzyme is selected.
