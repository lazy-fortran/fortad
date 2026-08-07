# fortad roadmap

The P0.8 decision gate rejected the original universal performance thesis. The
current target is a portable source-transformation AD engine for Fortran that
emits standard Fortran, passes independent derivative checks, and reports
performance per workload with build time, code size, runtime, and memory.

The engine matrix remains Enzyme/Tapenade/Clad/CoDiPack/ADOL-C/Adept/Sacado/JAX/
PyTorch/Zygote/Mooncake/Enzyme.jl. The mode matrix remains
forward, reverse, vector forms of both, forward-over-reverse,
sparse-compressed, and higher-order Taylor. A performance win is claimed only
when the complete workload measurement supports it. A loss is recorded as a
named optimization target or limitation.

The decision record is [docs/design/go-no-go.md](docs/design/go-no-go.md), and
the research reasoning remains in [docs/dossier.md](docs/dossier.md).

Ordering principle: **independent correctness and measured value per unit of
work**. A complete but unmeasured mode matrix is unfinished, and a benchmark
win on one kernel is not generalized beyond its evidence. The reasoning behind
every choice below is in [docs/dossier.md](docs/dossier.md).

## Hard execution rules

Work through this file one checkbox at a time.

1. Select exactly one unchecked item.
2. Keep the implementation as small as possible while fully satisfying it.
3. Avoid overengineering. No speculative abstraction, no unused flexibility. Add
   a shared abstraction only after two real kernels need the same behavior.
4. Add an **independent behavioral oracle**. A test that reproduces the
   implementation's own output is not a test. Acceptable oracles are listed in
   [PROVENANCE.md](PROVENANCE.md): hand-derived analytical derivatives, finite
   differences with a convergence test, the adjoint identity
   `⟨u, Jv⟩ = ⟨Jᵀu, v⟩`, fortsym, or complex-step. Agreement with Enzyme,
   Tapenade or JAX is corroboration, never the oracle.
5. Measure and report for every item: runtime and peak memory, before/after or
   candidate-vs-candidate. Record the workload and method, together with the
   compiler and hardware. Complete-workload wall clock is the primary metric.
6. Report **build time and generated-code size** alongside runtime. They are
   product requirements, not footnotes.
7. Measure scaling in active-input count, output count, and direction count
   wherever those can flip the forward-vs-reverse verdict.
8. Read the compiler's vectorization report for every emitted kernel. A kernel
   the compiler refuses to vectorize is a failed item even if it is correct.
9. Any algorithm gets its [PROVENANCE.md](PROVENANCE.md) row **before** its
   implementation.
10. No third-party code or literature enters this repository. The study
    corpus and the expensive benchmark corpus live in fortad-bench.
11. Update committed benchmark evidence when an item affects performance.
12. Check off the item, commit implementation + tests + evidence + this file
    together, push, and only then select the next item.
13. **Report to Zulip whenever something is finished.** DM krystophny with the
    updated results for *all* benchmarks, not only the ones the item touched:
    the full fortnum table, the full fortfem table, Enzyme's own suite, and
    the worst ratio in each. A finished item that has not been reported is
    not finished. Use the plot URLs from the upload script rather than local
    paths, and post the numbers even when they did not move. An unchanged table
    is itself the result.

Do not combine checklist items. If an item contains several independent changes,
split it into smaller checkboxes before writing code.

---

## Status on 2026-08-07

Phases 0 through 6 contain 42 completed items. The arithmetic core works, but
the current integration gate is still open:

- [x] `fo` retains sources that FortFront cannot parse and sends them to the
      compiler. Commit `f1a8e56` fixed the source loss. Commit `15e95f6`
      independently checks the compiler boundary and fails on the old code.
- [x] FortFront parses the former 15-source blocker set. Commit `c40ce77e`
      also fixes continued character literals, including
      `iga_polar_feec.f90` lines 328 and 344. Commit `11da10a4` gives
      nvfortran 26.5 a cold 381-target build after fixes to path handling, JSON
      escaping, and the CLI. Commit `ac02b4d0` exports the component-access
      query used by FortAD commit `155bf0e`. FortFront `main` now also contains
      `5b60c777`, which copies explicit `CALL` nodes through defined assignment
      and checks that their allocatable name and argument list survive arena
      growth under GNU and nvfortran.
- [ ] FortFront `main` is green on Windows. The six failures and their current
      diagnosis are recorded under Repository state.
- [ ] fortfem PR 63 is merged with green CI. All 733 local tests pass and
      `fo lint` is clean, but the GitHub jobs remain unstable.
- [ ] The current FortAD head passes GNU/Flang/ifx/nvfortran/LFortran.
      GNU is current: `fo check` builds 408 targets, checks 407 derivative
      targets, and runs 35 tests.
      The cheap lint rules report zero unused imports and zero short-circuit
      hazards. 108 `-Warray-temporaries` diagnostics still keep
      `fo lint` nonzero. The other four lanes still rely on a run that
      predates the latest lowering work. `fo fmt --check` still reports
      formatting debt in legacy files. The files touched by the current slices
      pass the formatter check.
- [ ] Every operator shared by fortnum and fortfem has same-machine FortAD and
      Enzyme measurements. The three vector-Newton routines have a FortAD-only
      record, and the tangent gaps below remain open.
- [ ] Complete result tables and plot links have been posted to Zulip.

The work after this gate is Phases 7 through 12. It extends FortAD from the
current arithmetic subset to the program semantics used by the pinned
itpplasma applications.

The implementation snapshot is `8718ec0`. Its GNU behavioral gate is green
(408 build targets, 407 derivative targets, 35/35 tests); `fo lint` still has
108 array-temporary warnings. Feature scope is recorded in the Phase 7 and 8
checklists below. The three previously failing nvfortran rule oracles now pass
after `a85aab9` moves lowering to FortFront's parse/query boundary and adds a
scalar external-CALL refusal oracle. The complete multi-compiler gate remains
open until the remaining lanes are rerun.

## Current integration gate

This gate integrates the current feature set. New modes and derivative rules
belong to the phase checklist.

Completion requires all of the following:

1. `fortnum` and `fortfem` build, test, and lint on `main` with
   `AD_ENGINE = FORTAD` and green CI.
2. Enzyme remains a test oracle and benchmark competitor, not a user-facing
   fallback. Unsupported FortAD input must fail explicitly.
3. Every operator supported by both engines has forward, reverse, and
   gradient-only results in `fortad-bench`. A result outside 30% of Enzyme is
   named and explained.
4. `fo` cold-builds every repository without losing a source.
5. The full results are reported under hard execution rule 13.

### Feature surface to preserve

The implemented modes are scalar and vector forward, reverse, gradient-only
reverse through `--no-primal`, forward-over-reverse HVP, sparse-compressed,
and higher-order Taylor. Batched reverse is P7.0, not a current CLI feature.

The CLI surface is `--mode`, `--indep`, `--dep`, `--name`, `--output`,
`--directions`, `--no-primal`, `--module`, `--proc`, `--roundtrip`, `--rule`,
`--call-rule`, `--version`, and `--help`.

- `--proc NAME` lowers one target and follows its call graph. Lowering the
  whole file crashes `collect_params` on a benchmark driver.
- `--rule NAME:partial;partial` registers scalar partials.
- `--call-rule NAME:n_args:tangent;...|adjoint;...` registers statement-level
  rules used by the linear-solve and IFT cases.

`fortad_opt` provides `propagate_copies`, `substitute_temps`,
`propagate_loop_zeros`, `coalesce_element_updates`, `rename_bodies`,
`factor_self_update`, `rotate_carried`, `hoist_invariants`,
`hoist_subexpressions`, `share_subexpressions`, `pack_adjacent_reads`,
`canonicalise_division_signs`, `reciprocate_divisions`, `regroup_products`,
`fold_identities`, `fold_negations`, `balance_sums`,
`drop_self_assignments`, and `eliminate_dead_stores`. These transformations
may reassociate arithmetic, which an ordinary Fortran compile cannot assume.

Same-file IR inlining in `fortad_inline.f90` is bounded by
`MAX_SPLICES = 256`, and a registered rule takes precedence. Affine recurrence
collapse removes the tape from a linear ODE adjoint. It is not an `rk4`
special case. Tapenade obtains the same result through TBR analysis.

### Repository state

**fortnum is integrated.** PR 63 merged with five green checks, and
`FORTNUM_AD_ENGINE` is `FORTNUM_AD_FORTAD`. `inv2` must differentiate the
closed-form inverse in terms of `a`, not `ainv`, because the fortsym rule takes
the inverse entries as input while FortAD differentiates the closed form. fpm
and CMake have separate source lists, so a new source must be added in four
places. Vector-Newton evidence is under P1.2. The cluster has no
compatible Enzyme toolchain, so those rows do not establish the 30% target.

**fortfem is not integrated.** PR 63 is open. One CI path builds all 733
targets, then gfortran 13.3 reports a truncated
`build/fo/mod/fortfem_feec.mod` at line 181, column 54 even with `FO_JOBS: 1`.
The fpm retry reaches one failure, `test_equation_objective_registry`, exit 2.
Neither failure reproduces in Ubuntu 24.04 with gfortran 13.3.0, where that
test passes 18 of 18, or locally with gfortran 16.1.1, where all 733 pass.
`main` has a separate line-truncation failure already fixed on the branch.

**The upstream source-loss and parser blockers are closed.** Before
`f1a8e56`, a scan failure left `ierr` at zero, removed the source from the DAG,
and surfaced later as an undefined reference. A warm cache hid the defect. The
fix recovers an unscannable unit into the build so legal syntax reaches the
compiler and invalid syntax fails at its own file. The regression uses a
module containing `integer :: value =` and requires the compiler log to name
that source. Missing `app/` and `example/` directories remain routine rather
than fatal. The independent `fo` backend tests and cold FortFront and fortfem
builds pass this boundary.

FortFront's former 15-source blocker list, 25-test GNU failure list, continued
character literals, and nvfortran cold build are closed. GNU now builds 381
targets and 378 test programs containing 483 tests. The remaining Windows
tests are `test_compiler_facing_queries`, `test_reject_bind_02_diagnostics`,
`test_reject_placement_01_diagnostics`,
`test_reject_value_scope_01_diagnostics`, `test_all_examples_slow`, and
`test_elemental_validation`. `test_module_distribution` also remains
parallel-fragile because it invokes the repository Makefile and cleans shared
artifacts, although it passes alone and in the final bare gate.

`fo` pins FortFront `main`, and fpm caches its dependency clone. After a
FortFront change, remove `build/dependencies` and `build/cache.toml` before
reinstalling `fo`, or the old revision will be reused.

### Benchmark records

`fortad-bench` currently records 59 operators: 17 from fortnum and 42 from
fortfem, plus Enzyme's own suite. Each new case needs the plain Fortran kernel,
the `_c.f90` variant used by Enzyme, a batch loop, `ARITY` and dispatcher
registration, the `NW` count, and engine wrapper macros. Both derivative
programs use the same flang compiler. Record forward, reverse, gradient-only,
build time, generated source and object size, runtime, peak memory, and the
vectorization report, then regenerate the plots and README tables.

[cases/fortfem/kernels/](https://github.com/lazy-fortran/fortad-bench/tree/797d20514f410c18c122d342b288e09ec68ed7f3/cases/fortfem/kernels)
contains 43 sources while
the harness says `NW = 42`. Tapenade is not wired in, although it is the fair
comparison for the `rk4` reverse margin. Build time is still unmeasured, and
three result caveats remain in
[fortad-bench/ROADMAP.md](https://github.com/lazy-fortran/fortad-bench/blob/797d20514f410c18c122d342b288e09ec68ed7f3/ROADMAP.md).

### Verification conventions

- Compare failing sets, not counts. Equal counts have hidden swaps.
- Establish a clean baseline on the tree being measured. Stale artifacts have
  produced false improvements and regressions.
- Reproduce runner-only failures in Ubuntu 24.04 with gfortran 13.3. This
  machine uses 16.1.1.
- Check FortFront's parse result, not only the process exit status.
- Preserve the original error signature while reducing a failure.
- `gh pr checks` reporting `MERGEABLE` means no conflicts, not green checks.

## Implementation decisions that constrain later phases

FortFront's arena AST supplies typed, name-resolved nodes and dimensions. It
also supplies intent, contiguity, and a call graph. Dummy arguments are
`parameter_declaration_node` values, not `identifier_node` values. FortAD
lowers this AST to its own IR because SSA renaming, expression-zero
propagation, and statement reordering are not practical AST edits.

The emitter writes standard Fortran through recursive subroutines into a
growable buffer. A recursive function returning
`character(len=:), allocatable` corrupted its own output under gfortran.
Arena-mutating calls must not be nested, and an arena child must be copied
before another call can reallocate the arena. Fortran does not order argument
evaluation. Both errors produced wrong derivatives or a segmentation fault.

A tangent index of zero represents structural zero. Rule builders propagate
it, while declaration activity uses a fixed-point analysis. Reverse partials
come from seeding the forward rule one argument at a time, so intrinsics need
one rule table. Linear reductions need no tape. Fusing a reduction's reverse
sweep into its primal loop changed the measured result from parity with Enzyme
to 1.7 to 1.8 times faster and moved it from 85% behind a handwritten adjoint
to within 5% to 9%.

fortsym and FortAD independently needed the same text buffer, continuation
breaker, and provenance banner before those pieces became `fortgen`. Generated
adjoint, tangent, and vector-tangent routines compile with
gfortran/flang/nvfortran/LFortran without compiler-specific output. gfortran
vectorizes the fused adjoint loop.

The first `dot_sin` measurement found FortAD about 8% faster than Enzyme per
element for one direction and equal to the analytical tangent. At 16
directions, vector mode reaches about 10 times the throughput per direction of
one-call-per-direction engines. Build time is a tie on this small case, so it
does not support a general build-time advantage.

## Phase 0: Establish the ground truth before building anything

- [x] **P0.1 Fetch and license-verify the study corpus.** Run fortad-bench's
      [scripts/fetch_upstreams.py](https://github.com/lazy-fortran/fortad-bench/blob/4f663ac3408683188e54424f2910332d14acb979/scripts/fetch_upstreams.py)
      and `--licenses`. Resolve every `VERIFY` in
      [docs/upstreams.toml](https://github.com/lazy-fortran/fortad-bench/blob/4f663ac3408683188e54424f2910332d14acb979/docs/upstreams.toml)
      against the actual checkout. Any entry with no
      discoverable license drops to metadata-only. Record the revisions. Completed
      2026-08-05: 33 reachable checkouts have recorded revisions and license files.
      Six historical or unavailable sources are explicit metadata-only entries.
      The inventory has no unresolved `VERIFY`, `NOT FETCHED`, or `NONE FOUND` rows.
- [x] **P0.2 Resolve the bibliography.** Run
      [scripts/fetch_literature.py](https://github.com/lazy-fortran/fortad-bench/blob/47a332d672200df021eb8f5aabb9ad50bc463d0f/scripts/fetch_literature.py)
      with `--resolve`, then `--fetch`. Fix titles that Crossref cannot match. Record which papers
      are open access and which need institutional retrieval. Completed
      2026-08-05: all 33 entries were resolved remotely. Nine arXiv PDFs were
      validated, three additional open-access landing pages were recorded but
      rejected as HTML, and the remaining 21 entries are marked for
      institutional/library retrieval. No title correction was needed.
- [x] **P0.3 Read the four primary sources.** Hascoët & Pascual 2013 (Tapenade
      specification), Hascoët et al. 2005 (TBR), Giering & Kaminski 1998 (adjoint
      recipes), Moses & Churavy 2020 (Enzyme). Write a one-page note per paper
      into `docs/notes/`. Completed 2026-08-05: [Tapenade](docs/notes/hascoet-pascual-2013-tapenade.md),
      [TBR](docs/notes/hascoet-naumann-pascual-2005-tbr.md),
      [adjoint recipes](docs/notes/giering-kaminski-1998-recipes.md), and
      [Enzyme](docs/notes/moses-churavy-2020-enzyme.md) were read from source
      PDFs fetched and inspected on sCluster.
- [x] **P0.4 fortfront coverage measurement.** Parse every file in fortnum's
      `src/` through fortfront. Report: files parsed, files failed, constructs
      unsupported, and whether the resolved-type and binding queries return
      usable information for a representative kernel. **This decides whether
      fortfront or LFortran ASR is the front end**, and no IR work starts before
      it.
- [x] **P0.5 fortfront transformation-API gap analysis.** Enumerate exactly what
      is missing to build and splice nodes (see dossier §8.2). Decide: extend
      `transformation_api.f90` upstream, or build a fortad IR lowered from the
      AST. Write the decision and its reasoning into `docs/design/ir.md`.
- [x] **P0.6 VMEC++ Jacobian kernel, Fortran port, hand-differentiated.** Ported
      `ComputeHalfGridJacobian` to idiomatic Fortran and hand-wrote its JVP and
      VJP. The central-difference step sweep and arbitrary-cotangent adjoint
      identity both pass. On the same pinned AMD EPYC 7282 core, hand Fortran
      takes 16.50 us/JVP and 20.50 us/VJP versus the existing C++/Enzyme
      reference at 8.66 us/forward and 13.68 us/reverse. Peak RSS is 3348 kB
      versus 3280 kB. The hand version is therefore the measured ceiling that
      fortad must beat, not a claimed win. FortAD-bench commit
      [`eafcd1c`](https://github.com/lazy-fortran/fortad-bench/commit/eafcd1c40df0e8bef0344e4a8a17b283f1456f35)
      contains the full source and
      [validation](https://github.com/lazy-fortran/fortad-bench/blob/eafcd1c40df0e8bef0344e4a8a17b283f1456f35/results/vmec_jacobian_validation.txt),
      [runtime and memory](https://github.com/lazy-fortran/fortad-bench/blob/eafcd1c40df0e8bef0344e4a8a17b283f1456f35/results/vmec_jacobian.csv), and
      [build and size](https://github.com/lazy-fortran/fortad-bench/blob/eafcd1c40df0e8bef0344e4a8a17b283f1456f35/results/vmec_jacobian_build.csv).
- [x] **P0.7 Establish the benchmark harness in fortad-bench.** Stand up
      the [harness](https://github.com/lazy-fortran/fortad-bench/tree/7dd8f0714ae989caa1537c2eee0aea538ad93cee/harness)
      and the `analytical`, finite-difference and Enzyme adapters,
      inheriting `differentiable-fortran`'s protocol and contract rather than
      reinventing them. Record build time alongside runtime from the first row.
- [x] **P0.8 Decision gate.** The frontend and independent correctness gates
      pass. The hand-written Fortran JVP and VJP are slower than C++/Enzyme on
      the VMEC++ kernel, so the original universal performance thesis fails.
      `docs/design/go-no-go.md` records the no-go result and this roadmap was
      rewritten before continuing Phase 1 work.

## Phase 1: Forward mode, one kernel, end to end

- [x] **P1.1 fortad IR.** Whatever P0.5 decided. Typed, name-resolved,
      three-address, explicit control flow, arena-indexed. Round-trips to Fortran
      through fortfront's emitter with no semantic change. The test runs the
      round-tripped primal against the original on fortnum's suite.
- [x] **P1.2 Normalization passes.** Inlining, loop normalization, canonical
      three-address form, constant propagation. This is the pass set that buys
      back Enzyme's post-optimization advantage (dossier §5.6). The three
      previously blocked vector-Newton routines now pass independent central-FD
      checks. The [measurement](https://github.com/lazy-fortran/fortad-bench/blob/ae594189d7060036c9b31649fdabbc2f7b41f908/results/vector_newton_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/ae594189d7060036c9b31649fdabbc2f7b41f908/results/vector_newton_validation.txt)
      records contain runtime and memory. Build time and generated sizes are
      included.
- [x] **P1.3 Activity analysis.** Forward "varied" ∧ backward "useful". On the
      plain-array arithmetic form of the VMEC++ half-grid kernel, activity
      removes 1 of 9 candidate derivative sites (11.11%). The directional
      central-FD oracle, build time, generated size, memory and vectorization
      record are in the [P1.3 evidence](https://github.com/lazy-fortran/fortad-bench/blob/2546adeedacef953c4bace44f4b6a239ae2354ba/results/p13_activity_validation.txt).
      The derived-type allocation wrapper remains outside fortad's supported
      source subset, so this measures the exact kernel arithmetic, not the
      wrapper.
- [x] **P1.4 JVP rule table.** Operators, intrinsics, `real(dp)` arithmetic. The
      table is declarative and separate from the transformation, in the shape of
      ChainRules' `frule` (dossier §6.1). Rules for array expressions, not only
      scalars.
- [x] **P1.5 Scalar forward-mode transformation.** One tangent direction. Emit
      standard Fortran. Correct on the VMEC++ kernel against P0.6's hand-written
      JVP.
- [x] **P1.6 Emitter quality pass.** CSE, scalar replacement, tangent update
      fused into the primal loop, `intent`/`contiguous`/`pure` preserved, no
      allocation in the loop. Gate: gfortran vectorizes the emitted kernel.
- [x] **P1.7 Benchmark: fortad JVP vs Enzyme vs analytical vs hand-written.**
      The item called for runtime, peak memory, build time, generated-code size,
      and publication into `differentiable-fortran`. Its surviving historical
      `dot_sin` records contain the [runtime comparison](https://github.com/lazy-fortran/fortad-bench/blob/7dd8f0714ae989caa1537c2eee0aea538ad93cee/results/dot_sin_raw.csv)
      and [build-stage timings](https://github.com/lazy-fortran/fortad-bench/blob/7dd8f0714ae989caa1537c2eee0aea538ad93cee/results/dot_sin_build.csv),
      not peak memory or generated-code size.
- [x] **P1.8 Vector forward mode.** Contiguous trailing tangent dimension, chunk
      width tuned to SIMD width. Measure the scaling in direction count against
      Enzyme's `BatchDuplicated` and against `k` separate scalar JVPs. Dossier
      §5.4 predicts this is the largest forward-mode win. Verify or retract it
      against P1.7's linked direction-count record.
- [x] **P1.9 Second and third kernels.** One from fortnum `special/`, one from
      `quadrature/` or `interp/`. Fix whatever breaks. Do not generalize before
      three kernels have demanded the same generalization. `erfsum` and
      `fixed_quadrature_integrand` now pass independent central-FD checks and
      have complete [runtime and memory records](https://github.com/lazy-fortran/fortad-bench/blob/f98bfe9f9036152d8dc767c16d3e6127f42e9df1/results/p19_kernels_fortad.csv).
      The same records include build time and generated sizes. The
      [validation record](https://github.com/lazy-fortran/fortad-bench/blob/f98bfe9f9036152d8dc767c16d3e6127f42e9df1/results/p19_kernels_validation.txt)
      supplies the oracle and method.
      The wide quadrature access pattern remains the separately named
      slice-packing limitation.

## Phase 2: Reverse mode

- [x] **P2.1 TBR analysis.** The loop analysis now makes the recording choice
      explicit: reduction accumulators and recomputable temporaries are not
      stored, while nonlinear loop state gets typed per-loop storage. The
      Phase 1 byte comparison, including the nonlinear recurrence boundary
      case, is in the [comparison](https://github.com/lazy-fortran/fortad-bench/blob/de5219fa80518e99627e90e43287f9c7e6d916eb/results/p21_tbr_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/de5219fa80518e99627e90e43287f9c7e6d916eb/results/p21_tbr_validation.txt)
      records. Hascoët et al. 2005.
- [x] **P2.2 Linearity analysis.** The strict loop-state test removes
      the state tape from the affine RK4 recurrence: 8000 bytes at `n=1000`,
      with an independent directional finite-difference check. The
      counterfactual and emitted-source record are in the
      [comparison](https://github.com/lazy-fortran/fortad-bench/blob/e899fbe61b7b6d3a20547d20b18f3a97e7fc3d9e/results/p22_linearity_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/e899fbe61b7b6d3a20547d20b18f3a97e7fc3d9e/results/p22_linearity_validation.txt)
      records.
- [x] **P2.3 Transposition of the linear part.** Derive VJP from the JVP rules by
      transposition (dossier §6.1) rather than writing a second rule table. If
      this proves impractical for Fortran's mutation, record why in
      `docs/design/` and fall back to explicit adjoint rules. Try it first,
      because it is the difference between one rule table and two.
- [x] **P2.4 Data-flow reversal for control flow.** Branches and reduction
      loops done. Nonlinear loop-state recurrences remain refused. Loops,
      branches, `where`, and `forall` use typed, pre-sized, per-loop storage,
      never a generic tape
      (dossier §4.3). Mooncake is the reference for mutation.
- [x] **P2.5 Statement-level preaccumulation.** Local Jacobian per statement in
      registers. Hogan 2014. Measure against the non-preaccumulated adjoint.
- [x] **P2.6 Reverse-mode benchmark vs Enzyme.** Gradient of a fortnum workload
      with many inputs. The historical `dot_sin` files record
      [reverse runtime by input count](https://github.com/lazy-fortran/fortad-bench/blob/4491b2cc508164b70088f240ea7b4752ef8cc18d/results/dot_sin_grad.csv)
      and [build-stage timings](https://github.com/lazy-fortran/fortad-bench/blob/7dd8f0714ae989caa1537c2eee0aea538ad93cee/results/dot_sin_build.csv).
      Neither file contains a peak-memory or generated-size field.
- [x] **P2.7 Adjoint of parallel loops.** One-level fused positive reduction
      loops now emit `parallel do` with explicit reduction,
      `default(firstprivate)` scalar scope, and shared procedure dummies. The
      generated form passed serial/eight-thread independent directional-FD
      checks for `erfsum`. The [result](https://github.com/lazy-fortran/fortad-bench/blob/ad519cf62bd76df3ca6c8eb2653e1a760e66c816/results/p27_openmp_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/ad519cf62bd76df3ca6c8eb2653e1a760e66c816/results/p27_openmp_validation.txt)
      records cover this path. Negative accumulations, stateful and nested
      loops, and general index-set transposition remain outside it. Hückelheim
      & Hascoët 2022 and Paszke et al. 2021.

## Phase 3: Structural wins

- [x] **P3.1 Custom-rule registry.** `frule`/`rrule`, projection, explicit
      opt-out. Wire it to fortnum's existing `fortnum_derivative_registry` so a
      kernel's `analytical` candidate becomes fortad's rule automatically.
- [x] **P3.2 BLAS and LAPACK rules.** The registry now exposes an explicit
      `dgesv` rule table: the tangent applies `dB-dA*X` with `dgemm` and reuses
      the primal LU factorization with `dgetrs`. The reverse applies the
      transposed solve and `A_b -= lambda*X^T`. The generated forward and
      reverse routines pass complete-solve finite-difference and adjoint-
      identity checks against real LAPACK/BLAS on the TU Graz `acluster`.
      The existing TU Graz direct-solve fixture records the Enzyme comparison
      in the [result](https://github.com/lazy-fortran/fortad-bench/blob/4fab9f33c9d119ca6a3783c0124c49dda4cfc6ad/results/p32_blas_lapack_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/4fab9f33c9d119ca6a3783c0124c49dda4cfc6ad/results/p32_blas_lapack_validation.txt)
      records. The cross-record limitation is documented in
      `docs/design/blas-lapack-rules.md`. The built-in rule is explicit opt-in.
      Other mutating interfaces and calls inside loops remain outside this
      scoped path. Giles 2008.
- [x] **P3.3 Implicit differentiation of nonlinear solves and roots.** The
      structured registry now has an explicit IFT contract for a converged
      root: caller-supplied residual tangent and adjoint products are evaluated
      at the root, with no Newton-iteration tape. A cubic scalar-root JVP/VJP
      oracle passes complete-root finite differences and the adjoint identity
      on the TU Graz `acluster`. The [benchmark](https://github.com/lazy-fortran/fortad-bench/blob/149962ffafbabbf50f3a9af8941decbb7a8cf053/results/p33_implicit_root_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/ace350ca7fe647ccd3ff703eb63f01994602c7ca/results/p33_implicit_root_validation.txt)
      records compare the implicit products with Enzyme differentiating the
      fixed Newton iteration. The rule remains opt-in and does not infer
      residuals or certify convergence. See
      `docs/design/implicit-root-rules.md`.
- [x] **P3.4 Fixed-point adjoints.** The structured registry now covers the
      Christianson two-phase boundary: a converged fixed-point tangent and a
      transpose linearized-map adjoint phase use the converged state without
      recording the forward iteration history. The two-state tanh-map JVP/VJP
      oracle passes complete-resolve finite differences and the adjoint
      identity on the TU Graz `acluster`. The [timing](https://github.com/lazy-fortran/fortad-bench/blob/d502e6dc9d40166b7e506c76ea7a2990aa72036d/results/p34_fixed_point_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/d502e6dc9d40166b7e506c76ea7a2990aa72036d/results/p34_fixed_point_validation.txt)
      records cover this boundary. The rule is caller-supplied and requires a
      convergent map. Automatic map extraction and general shaped interfaces
      remain outside this scope. See `docs/design/fixed-point-rules.md`.
- [x] **P3.5 FFT, quadrature, interpolation, special-function rules.** The
      structured rule table is exercised across a real FFT pair, fixed
      quadrature, four-node Lagrange interpolation, and `erf`, with tangent
      and adjoint callbacks. A composite generated JVP/VJP passes complete
      central finite differences, the adjoint identity, and component checks
      on the TU Graz `acluster`. The [result](https://github.com/lazy-fortran/fortad-bench/blob/005063b43678159fbe073867bf959ca643769696/results/p35_library_rules_fortad.csv)
      and [validation](https://github.com/lazy-fortran/fortad-bench/blob/005063b43678159fbe073867bf959ca643769696/results/p35_library_rules_validation.txt)
      records accompany the existing fortnum performance context. The generic
      rule remains explicit and does not infer external ABIs. Complex FFT
      interfaces, adaptive quadrature, general spline state, and the remaining
      special-function catalog remain caller-supplied extensions. See
      `docs/design/library-rules.md`.
- [x] **P3.6 Revolve checkpointing** for time-stepping adjoints. Griewank &
      Walther 2000. Schedules are executed against a simulated integration in
      the tests, not inspected, and the forward-step count is checked against
      the binomial bound.
- [x] **P3.7 Sparsity.** Static structural dependency propagation now supplies
      conservative patterns from lowered procedures. Distance-2 column
      coloring, compressed Jacobian recovery, and star-colored symmetric
      Hessian recovery are independently tested. Opaque calls are propagated
      conservatively across their actual arguments. Callers may still supply a
      more precise pattern when external state or an unmodeled interface is
      involved. See the [correctness record](https://github.com/lazy-fortran/fortad-bench/blob/abc1b815bc47d8ed2b20f6a98339035e2f0d63f3/results/p37_sparse_validation.txt).

## Phase 4: Second order and higher

- [x] **P4.1 Forward-over-reverse HVPs.** Default Hessian route. Correctness is
      checked by the local [independent Hessian oracle](test/test_hessian_oracle.f90).
- [x] **P4.2 Dense Hessians** by `n` HVPs, and **sparse Hessians** by star
      coloring with direct recovery. On a symmetric arrowhead it uses 2
      colors where the asymmetric test needs 10, and the test asserts the
      improvement rather than merely that both are valid.
- [x] **P4.3 edge_pushing** decision gate. Rejected for now: no compatible
      independent implementation is available on the TU Graz hosts, and no
      complete-workload measurement demonstrates a win over the existing
      star-colored HVP route. Reopen only with both. The
      [decision record](https://github.com/lazy-fortran/fortad-bench/blob/4feb951de79410964daa2fe903ed2d784fac084c/results/p43_edge_pushing_decision.txt)
      gives the evidence boundary.
- [x] **P4.4 Higher-order Taylor kernels.** The arithmetic and source
      transformation are built and pinned against closed-form series: exp, log,
      sqrt, sin/cos, the Cauchy product, division, and integer powers, all
      `O(d^2)` per operation rather than the `O(2^d)` of nesting a first-order
      tool. The generated mode is deliberately limited to straight-line scalar
      kernels. Arrays, loops, and branches are refused by name. The
      [validation record](https://github.com/lazy-fortran/fortad-bench/blob/bc59332e2f25b9d69a9ffd53902fd50b88859564/results/p44_taylor_validation.txt)
      covers the generated transformation.

## Phase 5: Products

- [x] **P5.1 Linear UQ.** `cov(y) = J cov(x) Jᵀ` on the vector forward mode.
      Document precisely where first-order propagation stops being valid
      (Saltelli et al. 2008).
- [x] **P5.2 Sensitivity analysis driver.** Documented and tested as a
      shape rule rather than a wrapper. See docs/products.md. Mode is selected from
      input count, output count, and sparsity.
- [x] **P5.3 Optimizer integration boundary.** FortAD's gradients, JVPs,
      VJPs, and HVPs are wired to fortnum's backend-opaque optimizer contract:
      flat named active vectors, value/product callbacks, provenance and
      quality status, and validated candidate selection. Gauss-Newton `JᵀJv`
      is the matrix-free JVP-then-VJP composition. Newton-Krylov consumes
      `Hv`. A full Hessian is obtained from repeated HVPs or the existing
      star-colored recovery. No generic optimizer loop is added. Storage,
      constraints, line search, convergence, and calling conventions belong
      to the downstream application. The boundary and its independent
      gradient, JVP/VJP, layout, and registry checks pass remotely in the
      [P5.3 evidence](https://github.com/lazy-fortran/fortad-bench/blob/14f83918277ccff3823b8380ea13dcfaebef969a/results/p53_optimizer_validation.txt).
- [x] **P5.4 Public API freeze.** The source-transforming `fortad` surface is
      frozen at `0.1.0`: `fad_jvp`, `fad_vjp`, `fad_hvp`, `fad_taylor`, the
      round-trip and rule-registration calls, static-pattern and sparse
      recovery helpers, and the Revolve/Taylor runtime helpers. ADOL-C's
      `jacobian`, `hessian`, `jac_vec`, `vec_jac`, `hess_vec`, and sparse names
      are the product taxonomy and are mapped to these calls. Literal runtime
      tape aliases are not exposed. See docs/design/public-api.md and the
      [P5.4 evidence](https://github.com/lazy-fortran/fortad-bench/blob/4c444b5a7b2322783a1500cfc2f918786299932c/results/p54_public_api_validation.txt).

## Phase 6: Reach

- [x] **P6.1 Compiler matrix, historical acceptance at the 2026-08-05
      baseline.** All three generated procedures compiled with
      `gfortran 12.2.0`, `flang-new 22.1.8`, `ifx 2026.1.0`, `nvfortran 26.5`, and
      `lfortran 0.64.0`. The generated adjoint loop vectorized in every lane.
      The small affine reduction isolates emitted loop structure from
      transcendental-library profitability. The five compiler gates passed in
      CI run 31029227263. The [acceptance record](https://github.com/lazy-fortran/fortad-bench/blob/25f4134a86e27f038c83152a8d6f0728909ebdf0/results/p61_compiler_matrix_validation.txt)
      contains the exact reports. The Status section governs the current-head
      rerun: its bare `fo` GNU result is current, and the other four lanes remain
      open.
- [x] **P6.2 GPU.** Fused one-level positive reduction loops now emit adjacent
      OpenMP target and OpenACC loop directives with IR-derived data clauses.
      On TU Graz `acluster` (Tesla T4), NVIDIA HPC SDK 26.5 with CUDA 12.9
      compiles both forms for `cc75`. Mandatory device oracles and the
      analytic VJP check pass for both, with transfer-inclusive timings and
      memory measurements in the [P6.2 evidence](https://github.com/lazy-fortran/fortad-bench/blob/ace80f67f664cdf426555cc66c7c2669219bb3d5/results/p62_gpu_validation.txt).
- [x] **P6.3 Standalone CLI.** `fortad --mode reverse --dep f --indep x kernel.f90`
      makes fortad usable outside the lazy-fortran stack. The
      [CLI evidence](https://github.com/lazy-fortran/fortad-bench/blob/4c444b5a7b2322783a1500cfc2f918786299932c/results/p54_public_api_validation.txt)
      contains direct `--help` and `--version` checks. It does not validate the
      example reverse invocation.

## End-to-end contract

An end-to-end target names continuous inputs, one scalar objective, and the
computational path between them. Configuration, file I/O, mesh selection,
runtime type tags, branch outcomes, iteration stopping decisions, and event
topology are passive unless an item gives them an explicit derivative rule.
FortAD differentiates the executed smooth path. It reports a boundary when a
discrete choice changes under perturbation.

An application is green only when all of these hold:

1. FortAD transforms the named path without hand editing generated source.
2. The generated primal agrees with the application within its stated
   tolerance.
3. JVP and VJP pass an analytical, CAS, finite-difference convergence,
   residual, or adjoint-identity oracle.
4. Passive choices and invalid derivative regions are recorded with the case.
5. Build time, generated source and object size, runtime, peak memory, and the
   compiler's vectorization report are committed.
6. Every unsupported active construct reports its repository and file, plus
   the line and construct. No fallback may change the differentiated program.

"All itpplasma code" means the pinned manifest in Phase 12. A discovery job
finds maintained, non-fork repositories containing production Fortran and
fails when one has no manifest entry. Archived upstream mirrors and papers
stay outside the product gate, as do data and historical forks.

For example, the P8.1 oracle passes one `class(model_t)` argument to a single
generated JVP. A `linear_t` child returns `scale`, while a `quadratic_t` child
returns `2*scale*x`. The runtime type is passive, `x` is active, and a change
of selected child ends the fixed-path derivative contract.

## Phase 7: Data and call semantics

- [ ] **P7.0 Batched reverse.** Add an explicit cotangent-count API and a
      leading contiguous lane dimension. Check it against repeated scalar VJPs
      and the adjoint identity, then measure scaling in seed count. The forward
      `--directions` option must not silently acquire reverse semantics.
- [ ] **P7.1 Derived values.** Differentiate reads and writes of scalar, array,
      nested, and inherited real or complex components. Shadow values retain
      the primal layout needed by callees. Integer and logical components
      remain passive. Character and procedure components are passive.
      - [x] **P7.1a bounded concrete value slice.** Commits `dcf40a8` and
        `2aa8e32` support component-named independents on a concrete `type(t)`
        value, preserving
        scalar, inherited, nested, and array component paths in JVP/VJP shadows.
        The independent object itself is refused. Allocation, alias, character,
        logical, and procedure-component activity remain open.
- [ ] **P7.2 Allocation lifetime.** Cover allocatable components,
      `allocate`, `deallocate`, `source=`, `mold=`, automatic reallocation,
      deep assignment, and `move_alloc`. Reverse mode must reproduce the
      lifetime of every active allocation without leaking or reading a dead
      object.
      - [x] **P7.2a executable refusal boundary.** The public transforms now
        reject the first allocatable declaration/component or lifetime-changing
        statement with its source line before lowering. The independent
        [`test_allocation_lifetime_oracle.f90`](test/test_allocation_lifetime_oracle.f90)
        compiles and runs a primal using `mold=`, `source=`, deep assignment,
        automatic reallocation, `deallocate`, and `move_alloc`, then checks
        named JVP and VJP refusals. Shadow allocation state and reverse replay
        remain open P7.2 work.
- [ ] **P7.3 Aliasing and sections.** Track `pointer`, `target`, association,
      overlapping actual arguments, noncontiguous sections, and component
      aliases by storage identity. Test aliases that share a target and aliases
      that do not.
      - [x] **P7.3a explicit refusal boundary.** The lowering pass now refuses
        `POINTER` and `TARGET` declarations, pointer association, strided and
        vector-subscript array sections before derivative emission with named
        diagnostics because the IR does not track storage identity. The
        independent
        [`test_alias_boundary_oracle.f90`](test/test_alias_boundary_oracle.f90)
        checks both JVP and VJP paths. Positive element writes are separate:
        [`test_element_target_oracle.f90`](test/test_element_target_oracle.f90)
        checks them against central differences. Shared-target, disjoint,
        overlapping, and component-alias differentiation remain open.
- [ ] **P7.4 Procedure interfaces.** Preserve optional and keyword arguments,
      `present` branches, generic resolution by type, kind, and rank, elemental
      calls, and user-defined operators. FortAD remains a source transformer.
      Commit `0209b3a` now preserves optional dummies and `present` branches in
      generated JVP/VJP interfaces. Keyword mapping across inlined siblings,
      generic resolution, and operator-overloaded inputs remain open. Commit
      `08201bf` rejects active optional tangents/adjoints explicitly and keeps
      optional metadata off generated locals and SSA shadows.
      - [x] **P7.4a elemental procedure preservation.** The selected same-file
        procedure's standalone `ELEMENTAL` prefix is carried into generated
        JVP and VJP headers. The independent
        [`test_elemental_interface_oracle.f90`](test/test_elemental_interface_oracle.f90)
        compiles scalar and conformable rank-one calls, checks a central finite
        difference, and verifies the componentwise reverse product. Generic
        resolution and user-defined operators remain open.
- [ ] **P7.5 Complex values.** Define the real-Jacobian contract for complex
      inputs and outputs. Cover multiplication, division, `conjg`, `abs`,
      `real`, `aimag`, complex BLAS, and non-holomorphic refusal boundaries.
      - [x] **P7.5a real-coordinate forward slice.** Complex JVPs cover
            multiplication, division, `conjg`, `real`, `aimag`, `cmplx`, and
            `abs`, with a compiled hand/finite-difference oracle in
            [`test_complex_intrinsic_oracle.f90`](test/test_complex_intrinsic_oracle.f90).
            Active complex reverse paths now refuse before emission with a
            named diagnostic rather than producing invalid Fortran.
      - [x] **P7.5b bounded real-objective projection VJP.** A real-valued
            objective may depend on an active complex input through direct
            `real(z)` or `dble(z)` projections and ordinary real arithmetic.
            The generated complex adjoint stores the two coordinate gradients;
            [`test_complex_reverse_oracle.f90`](test/test_complex_reverse_oracle.f90)
            checks hand values, central differences, and the real adjoint
            identity. Complex arithmetic, `aimag`, `conjg`, `abs`, complex
            outputs, and BLAS remain named refusal boundaries.
      Complex reverse rules beyond this projection, complex BLAS, and
      non-holomorphic objective conventions remain open.
- [ ] **P7.6 Source forms.** Accept fixed form, CPP and includes,
      semicolon-separated statements, mixed legacy modules, and generated
      interfaces on the same path used by the production build.
      - [x] **P7.6a fixed-form CLI slice.** The production CLI recognizes
            `.f`, `.for`, `.ftn`, and `.f77`, normalizes column-1 legacy
            comments through FortFront, and does not label an ordinary legacy
            procedure derivative `PURE`. The compiled
            [`test_tapenade_fixed_form_oracle.f90`](test/test_tapenade_fixed_form_oracle.f90)
            transforms a Tapenade-style `DOUBLE PRECISION` kernel, checks the
            hand JVP and a central finite difference, and exercises the real
            CLI. Library source strings, CPP, include expansion, mixed forms,
            semicolon parity, and broad legacy syntax remain open.
- [ ] **P7.7 Language-completeness lane.** Add assumed-rank and `select rank`,
      submodules, `do concurrent`, coarrays, parameterized derived types, and
      finalizers after the application blockers above. The 2026-08-06
      itpplasma census found no production dependency on the first three.

## Phase 8: Runtime polymorphism and callbacks

The type tag and binding choice are passive. A child selected at runtime uses
the derivative of that child's implementation. The derivative at a point where
the selected type changes is undefined unless the application supplies a
problem-specific rule.

- [x] **P8.1 `select type` JVP closeout.** Commit `7766fa1` preserves a
      `class(base)` selector, concrete guards, and component reads in forward
      mode. One generated routine returns the analytical derivative for two
      different child types at runtime. FortAD main `d77a8a3` contains the
      measured runtime implementation and
      verified in fortad-bench commits `58a9a49` and `60da134`. The pinned
      [evidence](https://github.com/lazy-fortran/fortad-bench/blob/60da1343531522116d0a1563b2512968b3c700af/results/itpplasma_polymorphic_select_type_validation.txt)
      contains the hand-derived child JVP oracle, transform and build timings,
      generated source and object sizes, runtime and RSS, and the compiler
      optimization report.
- [x] **P8.2 `select type` VJP.** Replay the selected guard in reverse, merge
      n-way SSA values, and propagate one seed through the selected guard only.
      The compiled [`test_runtime_select_type_oracle.f90`](test/test_runtime_select_type_oracle.f90)
      exercises four runtime arms (three named children plus `class default`),
      checks hand gradients, two-step central differences of the untouched
      primal, and the reverse adjoint identity for every arm. The verified
      scope is fixed-shape arms with matching writes and an active scalar input.
      Broader active derived components remain P7.1 work, while dynamic ownership and
      dispatch-boundary diagnostics remain P8.5 and P8.7 work.
- [ ] **P8.3 Concrete type-bound calls.** Resolve and transform `pass`, named
      `pass(arg)`, `nopass`, inherited bindings, overrides, and type-bound
      generics. A same-file implementation is inlined or emitted as a separate
      derivative procedure according to the existing call policy.
      - [x] **P8.3a bounded concrete call.** A statically declared `type(t)`
            receiver with the default implicit PASS or `NOPASS` and a same-file
            function is normalized to an ordinary call before JVP/VJP
            generation. Named PASS, inherited-only, generic, deferred, and
            ambiguous type/implementation names remain named refusals. The
            compiled oracle is
            [`test_type_bound_oracle.f90`](test/test_type_bound_oracle.f90). It
            checks generated JVP/VJP values for both binding forms. It uses central
            finite differences, the adjoint seed, and all five binding-refusal
            cases in both JVP and VJP. Its second NOPASS case uses same-named
            locals in two procedures to verify scope-correct binding resolution.
            An active whole-receiver case verifies the named boundary. Local
            overrides on an abstract/deferred hierarchy are covered by P8.4a.
            Active receiver cotangents and runtime dispatch remain open.
- [ ] **P8.4 Abstract deferred bindings.** Generate a derivative binding for
      each reachable override and a parallel derivative hierarchy. Forward and
      reverse calls preserve the primal object's dynamic type through
      multi-level inheritance.
      - [x] **P8.4a fixed-dispatch override slice.** A statically declared
            concrete child may override an abstract deferred binding through
            one intermediate level. FortAD resolves each local override and
            generates its JVP and VJP. The independent
            [`test_abstract_hierarchy_oracle.f90`](test/test_abstract_hierarchy_oracle.f90)
            checks both levels with hand values, central finite differences,
            and the adjoint identity. Direct `class(base)` dispatch,
            inherited-only bindings, and unresolved deferred bindings remain
            named refusals. Runtime type-tag preservation and a derivative
            hierarchy remain open P8.4 work.
- [ ] **P8.5 Polymorphic ownership.** Cover allocatable base-class components,
      factories, `allocate(source=child)`, nested field/coordinate objects,
      arrays of polymorphic holders, assignment, and destruction.
- [ ] **P8.6 Procedure pointers and callbacks.** Treat callback identity as a
      passive runtime choice and pair each active callback with its JVP and VJP.
      Cover pointer reassignment, `associated`, null callbacks, passed
      procedures, and `class(*)` context objects.
- [ ] **P8.7 Dispatch diagnostics.** Detect perturbations that cross a type or
      callback boundary. Also detect branch and clamp boundaries, along with
      event or convergence boundaries. Report the choice and source line. A
      fixed-trace derivative remains available when the application accepts
      that contract.

## Phase 9: Numerical application rules

- [ ] **P9.1 Dense and sparse solves.** Extend the existing structured solve
      rule to real and complex dense, banded, and sparse interfaces. Reuse
      factors where the ABI permits it. Check residual derivatives and
      transpose identities.
- [ ] **P9.2 Generalized eigenproblems.** Implement the simple-eigenvalue rule
      and check
      `v^T(dK - lambda*dM)v / (v^T M v)`. Detect clustered or repeated
      eigenvalues and require a projector or subspace objective there.
- [ ] **P9.3 Adaptive ODEs and events.** Differentiate a fixed controller and
      event trace, add the analytical event-time term, and test refinement
      convergence. Refuse changed event ordering or classification.
- [ ] **P9.4 Fixed points and Anderson acceleration.** Apply the existing IFT
      boundary to converged Anderson in B15 and the application residual in A9.
      Apply it to converged Newton and Arnoldi. Compare against fresh converged
      finite differences.
- [ ] **P9.5 Time stepping.** Connect nonlinear state storage to Revolve and
      multi-level checkpoints. Verify the adjoint and the predicted memory and
      recomputation scaling.
- [ ] **P9.6 Library catalog.** Finish complex FFT, interpolation and spline
      state, adaptive quadrature, special functions, and matrix-free operator
      rules required by the manifest through A14.
- [ ] **P9.7 Stochastic paths.** Support frozen random draws and explicit
      reparameterization rules. Score-function estimators need their own API
      and variance oracle. A changed random path is outside a pathwise
      derivative.

## Phase 10: Parallelism, accelerators, and foreign code

- [ ] **P10.1 OpenMP.** Cover reductions, threadprivate state, nested regions,
      one/many-thread agreement, and the application-kernel tasks scoped by
      B19 and A0.
- [ ] **P10.2 OpenACC and devices.** Preserve data regions and routine clauses,
      then compare host and device derivatives. Record transfer-inclusive and
      resident-data timings.
- [ ] **P10.3 MPI.** Add rules for broadcast, reduce, allreduce, gather/scatter,
      and point-to-point calls. Check one, two, and four ranks plus rank
      invariance of the global objective.
- [ ] **P10.4 ISO C binding.** Transform interoperable wrappers and require a
      registered derivative for an active foreign call. An opaque active call
      fails at its call site.
- [ ] **P10.5 Python and C APIs.** Expose generated JVP/VJP entry points and
      compare native, C, and Python calls on identical inputs.
- [ ] **P10.6 I/O boundary.** Preserve passive NetCDF, HDF5, namelist, and
      diagnostic I/O outside derivative kernels. Active data movement through
      an unknown file or library interface requires a rule.

## Phase 11: Advanced fortad-bench corpus

Each item is one executable case with an independent oracle and the six
measurements in the end-to-end contract. Another AD engine is corroboration.
An unsupported result is recorded as such and never counted as a runtime win.

- [ ] **B0 Tapenade corpus closeout.** The companion manifest
      (`fortad-bench/docs/corpora/tapenade.toml`) pins the upstream tree and
      inventories 2,014 candidate cases. Classify
      every candidate, port runnable Fortran cases with an independent oracle,
      and record transform, compile, runtime, memory, and generated-source
      measurements. Invalid, non-Fortran, and dependency-blocked entries stay
      in the ledger with a reproducible refusal. They do not count as wins.

- [ ] **B1** abstract base with two runtime `select type` children, JVP and VJP
- [ ] **B2** multi-level inheritance with several deferred bindings
- [ ] **B3** polymorphic factory using `allocate(source=...)`
- [ ] **B4** nested polymorphic field and coordinate components
- [ ] **B5** `class(*)` context plus runtime procedure-pointer callback
- [ ] **B6** deep assignment, reallocation, `move_alloc`, and reverse lifetime
- [ ] **B7** same-target, different-target, overlapping, and strided aliases
- [ ] **B8** optional arguments, keyword reordering, and `present` branches
- [ ] **B9** generic selection by type, kind, and rank
- [ ] **B10** complex real-Jacobian intrinsic and BLAS rules
- [ ] **B11** fixed/free/CPP/include/semicolon form parity
- [ ] **B12** fixed dispatch trace plus switch-boundary diagnostics
- [ ] **B13** callback-driven RK and adaptive controller trace
- [ ] **B14** analytical event-time sensitivity and changed-event refusal
- [ ] **B15** Anderson fixed point through a residual IFT
- [ ] **B16** simple and clustered generalized eigenvalues
- [ ] **B17** complex sparse-solve adjoint from residual identities
- [ ] **B18** checkpointed nonlinear time stepping and memory scaling
- [ ] **B19** OpenMP at one and several threads
- [ ] **B20** OpenACC host/device agreement
- [ ] **B21** MPI collectives at one, two, and four ranks
- [ ] **B22** registered ISO C rule and opaque-call refusal
- [ ] **B23** passive NetCDF/HDF5 boundary
- [ ] **B24** native, C, and Python JVP/VJP agreement
- [ ] **B25** every Phase 12 application slice at its pinned revision

## Phase 12: itpplasma applications

The required manifest is KAMEL, rabe, NEO-RT, MEPHIT, SIMPLE, GORILLA,
GORILLA_APPLETS, NEO-2, GLISS, mhd1d, closure1d, KiLCA-FLR2, tiago, and
libneo. `spline`, `vode`, `quadpack`, and BOOZER_MAGFIE form a dependency-rule
lane. `fortedge` had a README and no Fortran source at the audited revision, so
it remains pending rather than passing.

- [ ] **A0 Manifest and discovery.** Pin commit, role, source roots, objective,
      active inputs, passive boundaries, oracle, compiler lanes, and status for
      every repository. Fail nightly discovery on an unclassified maintained
      production repository.
- [ ] **A1 Frontend census.** Parse every required source and produce a report
      of active-path support. Compiler-accepted source must reach the compiler
      even when FortFront cannot analyze it.
- [ ] **A2 mhd1d and closure1d.** Profile parameters to converged
      equilibrium/closure output, checked by CAS identities, residual IFT, and
      a finite-difference ladder.
- [ ] **A3 rabe.** Runtime-selected anti-sigma field to a field-line
      coefficient, checked by the repository's analytical fixtures.
- [ ] **A4 tiago.** Coil currents to flux-loop signals, checked by exact
      linearity and unit-current superposition.
- [ ] **A5 GLISS.** Equilibrium coefficients to a simple fixed-boundary
      eigenvalue, checked by the generalized eigenvalue formula and finite
      differences.
- [ ] **A6 SIMPLE and libneo.** Fixed field and event trace to orbit endpoint,
      checked by finite-difference convergence, the adjoint identity, and
      reversibility.
- [ ] **A7 NEO-RT.** Fixed-topology bounce/transport objective, checked across
      timestep refinement.
- [ ] **A8 KAMEL.** Runtime KIM child to a small Fourier response, checked by
      an analytical Fourier fixture or a finite-difference ladder.
- [ ] **A9 MEPHIT.** Sparse fixed-point slice to a converged scalar, checked by
      residual IFT and fresh converged finite differences.
- [ ] **A10 NEO-2.** Single-rank QL slice followed by MPI execution, checked by
      finite differences, the adjoint identity, and rank invariance.
- [ ] **A11 GORILLA.** Fixed tetrahedron trace to orbit endpoint, with event
      time and changed-cell cases separated.
- [ ] **A12 KiLCA-FLR2.** Input perturbation to selected wave response, checked
      by finite-difference convergence.
- [ ] **A13 GORILLA_APPLETS.** Fixed-seed particle aggregation, checked on the
      frozen random path.
- [ ] **A14 Dependency rules.** Cover the used spline, VODE, QUADPACK, and
      BOOZER_MAGFIE interfaces without importing their source into FortAD.
- [ ] **A15 Completion.** Every required manifest entry is green, every
      exception has a file-and-line diagnostic, and discovery finds no
      unclassified maintained computational Fortran repository.

---

## Optimizer and reverse-mode contracts

Tapenade measurements showed that emitted code shape, not differentiation,
was often the gap. `fortad_opt` therefore runs source-level transformations
that may reassociate exact arithmetic. The complete pass list is in the
feature surface above.

Some passes only pay off as a sequence. `substitute_temps` may duplicate a
single operation so `factor_self_update` can expose a coefficient and
`hoist_subexpressions` can compute it once. `propagate_loop_zeros` removes a
clear that protects an accumulation in the next reverse iteration, which
ordinary dead-store elimination cannot see. `rotate_carried` adds a snapshot
but shortens the loop-state dependency path. It improved Euler by 15%.

The optimizer tries conservative and duplicating substitution once per
top-level loop. For `n` loops this costs `n + 1` pass sequences. It keeps a
candidate only when arithmetic operation count falls, and a tie keeps the
conservative form. Variables and literals cost zero. Dead-store elimination
must run before scoring, or replaced statements make every candidate appear
to tie. Each pass stays inside a straight-line assignment window, and
substitution checks that every operand is unchanged between definition and
use. The oracle suite caught the missing check with `t = u` followed by a
write to `u`.

`fad_vjp(..., with_primal=.false.)` implements the gradient-only
contract. Tapenade omits the primal output while Enzyme computes it because
the seed uses a duplicated output. When no adjoint coefficient needs a primal
value, dead-loop elimination removes FortAD's forward sweep. Comparisons must
use this contract explicitly.

Branches inside loops and recurrences inside nested loops are still rejected
by name. An array-element assignment outside a loop is supported. Its adjoint
scatters into the corresponding element. If the array is the dependent value,
the reverse sweep copies the caller's `intent(in)` seed and clears each element
after reading it so the store's adjoint stops at that write.

## Open defects and measurements

A competitor winning a benchmark is an owned defect. Keep the failed
hypotheses with the measurements so they are not repeated.

### Forward tangent layout

`fci_polygon_edge_area` is 1.24 times Enzyme after improving from 1.40 times.
Its original FortAD loop used four scalar `mulsd`, three `subsd`, and two
packed `mulpd` instructions while Enzyme used four `mulpd` and two `addpd`.
The operation count matched, but FortAD was 35% slower. LLVM vectorizes both
loops into a `<2 x double>` accumulator. FortAD then swaps, extracts, and
continues the subtraction in scalar lanes because the primal and tangent terms
do not form adjacent input pairs.

In the later packed comparison per two inputs, Enzyme used three `mulpd`, one
`addpd`, and one `subpd`, while FortAD used four `mulpd`, three `subpd`, and two
`addpd`. Total instruction counts were 38 and 39. Enzyme formed strided lanes
with `movsd` plus `movhpd` instead of reading the adjacent inputs as `movupd`.
That layout lets its primal and tangent share partial products.

Six explanations are eliminated: reassociation cost, instruction scheduling,
accumulator pairing, the compile pipeline, term ordering, and reduction
vectorization. The records behind that conclusion are:

- A two-element value/tangent accumulator measured 0.359 ns/input against
  0.356 for the current form and 0.257 for Enzyme.
- Sending generated Fortran through Enzyme's flang, `opt -O3`, and
  `clang -O3` pipeline retained FortAD's four `mulsd` and three `subsd`.
- `-ffast-math` produced 19 `mulpd`, 17 packed loads, and no scalar multiply.
  Explicit four-way strip mining also packed the loop, but measured 0.365
  ns/input against 0.373 unvectorized and 0.271 for Enzyme.
- A contiguous four-input slice measured 0.333 ns/input against 0.356. Two
  batch elements with partial accumulators measured 0.347 against 0.380.
  Combining them measured 0.335 against Enzyme's 0.258, so the changes did not
  compose and remained at 1.30 times. A separate run placed Enzyme at 0.284,
  making strip mining worth about 9% and leaving an estimated 1.22 times gap.
- Grouping positive and negative terms reduced vector arithmetic from nine
  operations to six without changing runtime. It also cost the degree-eleven
  Bezier adjoint 11 percentage points, so it was reverted.

The remaining difference is about four operations and 0.1 ns per element.
Loads, shuffles, and shared products are better leads than another reduction
reassociation experiment.

The curved quadrilateral cell-area tangent is a separate wide-input case. It
takes 16 inputs, runs at 1.63 times Enzyme, and emits 444 instructions against
305. FortAD copies a contiguous 16-element slice and then extracts every
element into a scalar, paying for both representations. Propagate element
reads into their uses so the named scalars disappear, or retain direct reads
when propagation is impossible. A width threshold would only hide the cause.

### Reverse performance

- Taped recurrences are about 3.5% slower than Enzyme on
  [fortad-bench/cases/recurrence](https://github.com/lazy-fortran/fortad-bench/tree/481f4843c80711be0e526fac88751274305080a2/cases/recurrence),
  down from a pre-square-rule gap of 10% to 12%.
  The remaining store-or-recompute choice has no cost model. FortAD tapes an
  input and recomputes its statement, while Enzyme appears to store more. See
  dossier section 4.3.
- `rk4` with its primal is 10% behind Enzyme but within tolerance: 21.15
  ns/input against 19.13, with a 13.44 primal and reverse sweeps of 7.71 and
  5.69. Gradient-only FortAD leads Tapenade, 7.68 against 8.71. The step is
  linear in `state`, but its scatter is a chain of updates to `z_b(i)`.
  Combine that chain into one expression before factoring it into a scaling
  and scatter. The same generalized factoring already collapses `bruss` and
  `ba`.

### Correctness and output

- An assumed-size `real(dp) :: z(*)` becomes `dimension(:)` in the derivative.
  The mismatched explicit interface caused a segmentation fault in the fortnum
  driver. Preserve `(*)` and zero by element, or reject it with a diagnostic.
- A wrapped continuation can reach columns 89 to 91 because the continuation
  indent is not subtracted from the 88-column target. This remains legal under
  the 132-column standard limit.
- `fortad_cse` remains disabled. It silently gives a plausible but
  nonsymmetric Hessian through `fad_hvp`. `share_subexpressions` replaces its
  useful case inside one straight-line loop-body window and derives a
  temporary's type from the expression. It moved `lagrange4` from 21% to 18%
  behind Enzyme and `det3` from 11% to 5%.

## Exclusions

Recorded so they are not rediscovered as good ideas:

- Globally optimal cross-country elimination. NP-hard (Naumann 2008), marginal
  gain. Basic-block preaccumulation only.
- A general dynamic tape as the default storage strategy. Fallback only, and
  every use counts as a defect.
- Implementing FortAD itself through operator overloading. P7.4 still accepts
  input programs that use overloaded operators. An overloaded AD engine would
  defeat vectorization and inlining.
- Symbolic differentiation as the engine. fortsym is a peephole optimizer for
  FortAD's output, not an alternative to it.
- Replacing Enzyme. Enzyme stays in fortnum as a competing `autodiff` candidate
  and as the benchmark baseline. If Enzyme wins a workload, Enzyme is selected.

## Closed optimizer history

Keep these results because they define the performance floor for future
changes.

fortfem's degree-eleven Bezier edge area once took 20.5 seconds to transform
and now takes 3.2 seconds. `substitute_temps` accounted for part of the 18
seconds spent in two passes because it searched from each definition to the
end of the procedure. It now stops at the recorded last mention.
`hoist_subexpressions` now uses a bottom-up invariant flag and an O(1) hash set
of written names. Every identifier in text such as `u_tape(i)` counts, and a
child added during an arena extension is included. Three forward oracles
caught the high-water-mark mistake. Passes that mutate a body refresh the
cache rather than making millions of queries rebuild it.

Use counting and escape analysis were also made linear per round.
`share_in_body` counts uses once, and `escapes` reads a precomputed mention
span. Quintic Lagrange weights fell from 11.0 to 0.40 seconds and quartic
Bezier from 3.2 to 0.37 seconds.

The separate nontermination in `hoist_subexpressions` came from
`insert_before(p, first, s)`: insertion displaced the current statement, then
the old cursor visited it again. Advancing the cursor with `first` and `last`
fixed the loop, and two redundant `invalidate_invariance()` calls were removed.
The failing `fixed_newton_solve` transformation ran for more than two minutes,
although `--roundtrip --proc fixed_newton_solve` with both residuals inlined
finished in under a second. The three vector-Newton routines blocked by this
bug now differentiate end to end and have the FortAD record named above.

## Compiler and measurement handoff

Intel LLVM `ifx` is supported. Legacy `ifort` is intentionally outside the
matrix. GNU/Flang/nvfortran/ifx/LFortran remain the five lanes.

An earlier nvfortran 26.5 FortAD run compiled and linked all 406 targets. Its
declaration smoke covered `real(dp)`, a renamed `iso_fortran_env` import, five
dummy arguments, and five declarations. Lowering now reads parameters from the
source header and copies deferred-length `uses(:)` and `params(:)` arrays
element by element. Declaration mirroring uses scalar metadata. IR declaration
copying and DCE compaction use the same explicit ownership boundary to avoid
nvfortran allocatable-descriptor damage.

That run did not sign off the optimized tangent. A debug build with the
post-differentiation optimizer disabled passed the finite-difference oracle,
but the optimized run was interrupted. The current branch also contains later
lowering and runtime-dispatch work, so the whole matrix must be rerun.

1. Run nvfortran with the tangent finite-difference oracle, VJP oracle,
   generated-source compile checks, and all tests. If the optimized lane still
   damages the heap, inspect remaining `fad_stmt_t` and `fad_expr_t` intrinsic
   assignments and reverse declaration copies at the same ownership boundary.
2. Run ifx, Flang, and LFortran and record the pass counts.
3. Repeat operation and scaling benchmarks against PyTorch, GPyTorch, and
   KeOps at identical precision and parameters, on the same hardware and with
   identical flags. Record stable slopes and all six measurements. The 30%
   target needs same-machine
   CPU and GPU evidence.
4. Complete the CUDA-first, KeOps-style matrix-free lane while retaining CPU
   Fortran. ROCm and SYCL remain extension points. Add compact-support regular
   grids and tensor-product grids to the corpus.
5. Upload every regenerated plot and send Zulip the complete table, workload
   metadata, independent oracle, and plot URL.
