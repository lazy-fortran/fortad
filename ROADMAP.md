# fortad roadmap

The P0.8 decision gate rejected the original universal performance thesis. The
current target is a portable source-transformation AD engine for Fortran that
emits standard Fortran, passes independent derivative checks, and reports
performance per workload with build time, code size, runtime, and memory.

The engine matrix remains Enzyme, Tapenade, Clad, CoDiPack, ADOL-C, Adept,
Sacado, JAX, PyTorch, Zygote, Mooncake, and Enzyme.jl. The mode matrix remains
forward, reverse, vector forms of both, forward-over-reverse,
sparse-compressed, and higher-order Taylor. A performance win is claimed only
when the complete workload measurement supports it. A loss is recorded as a
named optimization target or limitation.

The decision record is [docs/design/go-no-go.md](docs/design/go-no-go.md), and
the research reasoning remains in [docs/dossier.md](docs/dossier.md).

Ordering principle: **independent correctness and measured value per unit of
work**. A complete but unmeasured mode matrix is unfinished, and a benchmark
win on one kernel is not generalized beyond its evidence. The reasoning behind
every choice below is in [docs/dossier.md](docs/dossier.md); this file records
the work order and its results.

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
    paths, and post the numbers even when they did not move — an unchanged
    table is itself the result.

A mechanism name never selects a winner. Production selection requires an
independent oracle plus measured application runtime and peak memory — the same
rule fortnum applies to every candidate, applied to fortad without exception.

Do not combine checklist items. If an item contains several independent changes,
split it into smaller checkboxes before writing code.

---

## Current work order: fortad integrated everywhere, at the current feature set

This section is self-contained and actionable. It is the whole task: fortad
is the derivative engine for every repository that needs one, everything
downstream builds and tests cleanly on it, every benchmark is recorded, and
the upstream defects that stop any of that are fixed. An agent should be able
to work from this section alone.

Scope note: this is about integrating the **current** feature set flawlessly,
not about extending it. New modes and new rules are out of scope here — they
belong in the phase sections below. If a downstream operator needs a rule
fortad does not have, that is a new item, not part of this one.

### Definition of done

1. `fortnum` and `fortfem` both build, test, and lint clean with
   `AD_ENGINE = FORTAD`, on `main`, with green CI.
2. Enzyme is reachable only as the correctness oracle in tests. It is not a
   user-facing backend anywhere, and anything it covers that fortad does not
   is marked unsupported rather than quietly routed to Enzyme.
3. Every operator both engines can differentiate has forward, reverse and
   gradient numbers committed in `fortad-bench`, all within 30% of Enzyme,
   with the exceptions named and explained rather than omitted.
4. `fo` builds and tests every one of these repositories from a cold cache
   without silently dropping a source.
5. The results are posted to Zulip per hard execution rule 13.

### Current feature set — what must keep working

This is the surface the integration has to hold up. Any change here is a
regression, not a refactor.

Modes: forward (tangent), reverse (adjoint), reverse with `--no-primal` for
the gradient-only contract Tapenade also offers, vector forms of both via
`--directions`, forward-over-reverse for Hessian-vector products,
sparse-compressed, and higher-order Taylor.

CLI: `--mode`, `--indep`, `--dep`, `--name`, `--output`, `--directions`,
`--no-primal`, `--module`, `--proc`, `--roundtrip`, `--rule`, `--call-rule`,
`--version`, `--help`.

- `--proc NAME` differentiates one procedure inside a module. `lower_source`
  lowers the target first and then follows the call graph, deliberately:
  lowering everything segfaults `collect_params` on a benchmark driver.
- `--rule NAME:partial;partial` registers a scalar derivative rule.
- `--call-rule NAME:n_args:tangent;...|adjoint;...` registers a structured
  rule that emits statements — this is what the linear-solve and IFT cases
  use, and it is the mechanism the dossier's largest predicted lever needs.

Structural passes in `fortad_opt`, all of which a Fortran compiler is not
permitted to run because reassociation changes rounding: `propagate_copies`,
`substitute_temps`, `propagate_loop_zeros`, `coalesce_element_updates`,
`rename_bodies`, `factor_self_update`, `rotate_carried`, `hoist_invariants`,
`hoist_subexpressions`, `share_subexpressions`, `pack_adjacent_reads`,
`canonicalise_division_signs`, `reciprocate_divisions`, `regroup_products`,
`fold_identities`, `fold_negations`, `balance_sums`, `drop_self_assignments`,
`eliminate_dead_stores`.

IR-level inlining of same-file callees (`fortad_inline.f90`), bounded at
`MAX_SPLICES = 256`, with registered rules winning over inlining.

Affine recurrence collapse, which is what makes a linear ODE's adjoint
tape-free. This is a general transformation, not a special case fitted to
`rk4`; Tapenade reaches the same result through its to-be-recorded analysis.

### Downstream integration

**fortnum — done.** PR #63 merged, five checks green. `FORTNUM_AD_ENGINE` is
`FORTNUM_AD_FORTAD`. Two traps worth not rediscovering: `inv2` must
differentiate the closed-form inverse in terms of `a`, not `ainv`, because
the fortsym rule takes the inverse entries as its input while fortad
differentiates the closed form; and fpm and CMake keep separate source
lists, so a new source needs adding in four places or CI fails on a missing
module while the local fpm build stays green.

Remaining: three vector-Newton routines were the last of Enzyme's fortnum
corpus fortad could not do, blocked by the `hoist_subexpressions`
non-termination. That defect is fixed and they have not been re-measured
since. Do that, and add them to the corpus.

**fortfem — PR #63 open, CI red.** Green locally: 733 passed, 0 failed,
`fo lint` clean. Both CI failures are described under fortfem below.

### Upstream blockers

These are not fortad bugs, but fortad's integration cannot be called done
while they stand. Fix them in this order — the first one makes the rest fail
loudly instead of silently, which is worth more than it sounds.

#### 1. fo silently drops sources it cannot scan

The highest-value fix in the whole set, and a correctness bug rather than a
convenience one.

`scan_dir` calls `scan_file` per source. On failure it prints the diagnostic,
removes the unit from the list, and **leaves `ierr` at zero**. The file then
has no module name and no program name, so `build_dag_from_units` gives it no
node, so it is never compiled. The compiler never sees it, never reports its
syntax error, and the build is declared successful with the file missing.

Reproduce with two files:

```
fpm.toml      name = "p"
src/ok.f90    any valid module
src/broken.f90  a module whose body contains the statement `x =`
```

`fo check` prints a parse diagnostic, then reports
`Build: OK (1 modules, 0 cached, 1 changed, 1 affected)` and exits 0. Only
`ok.f90` was compiled.

How it actually shows up: as an undefined reference at link time to a symbol
the dropped file defined, never as a parse error. Cold-building fortfront
with `fo` produced `parse_range`, then
`keyword_should_parse_as_identifier`, then `get_standardizer_input_mode` —
one per file, each revealed only after the previous was fixed. `fo`'s own
`test_backend` and `test_backend_gfortran` fail from a cold cache for the
same reason, confirmed present before any of the recent work, so it is not a
regression.

A warm scan cache hides all of it. **Always `rm -rf build` before trusting a
result here**, in fo and in whatever it is building.

What was tried and reverted: making a scan failure a hard build error. It is
the obvious fix and it is wrong as stated, because `fo` then cannot build
itself — fortfront cannot yet parse some of fo's own sources. A subtlety any
fix must preserve: a missing `app/` or `example/` directory *also* returns a
nonzero status from `scan_dir` and is entirely routine, so a fix cannot treat
any nonzero status as fatal. The attempt introduced a distinct
`SCAN_ERR_UNSCANNABLE` code for exactly this reason; that part was sound.

Likely correct fix: keep the unscannable unit in the build with a node of its
own so it still reaches the compiler and the compiler reports the real error.
That also degrades gracefully while fortfront has parser gaps — a file `fo`
cannot scan but gfortran can compile still builds.

Related and already fixed, recorded because the lesson generalises:
`parse_test_results` filled a fixed 256-entry array and stopped, dropping
every later result **including failures**. `fo test` on a 738-target project
printed `Tests: 256 passed`, exited 1, and showed nothing about what failed.
A cap that silently discards results is worse than no cap, because the output
still looks like a complete answer.

#### 2. fortfront: 15 sources still do not parse

These are what `fo` drops, so they are the direct cause of blocker 1's
visible symptoms. Full list with line numbers is in `fortfront/ROADMAP.md`.
Grouped:

- Ten report a bare `end` or `end block`, meaning a construct slice still
  ends before its terminator on some path: `parser_array_constructs.f90:426`,
  `parser_result_types.f90:253`, `parser_statement_data_module.f90:605`,
  `parser_statement_utilities.f90:564`, `semantic_binary_operations.f90:145`,
  `semantic_procedure_signature.f90:164`, `type_hierarchy.f90:205`,
  `standardizer_declarations_inference.f90:180`,
  `standardizer_parameter.f90:128`, `fortfront_utils.f90:454`.
- `app/fortfront.f90:377` — `flush (output_unit)` unrecognized.
- `frontend_compiler_queries.f90:565` and
  `frontend_compiler_type_queries.f90:1008` — a statement beginning
  `operator` unrecognized.
- `semantic_external_declaration_names.f90:138` — an identifier beginning
  `block_` mistaken for the `block` keyword.
- `path_validation.f90:296` — reported as an IF construct missing its `then`.

**Critical caveat.** Minimal reproductions of the obvious shapes all pass: a
`block` holding a `do` inside a `do`, and an `if` inside a `select type` arm
both parse. These are *not* one shared cause and will not be closed by
another fallback registration. Each needs per-file bisection down to the
construct that actually fails. Do not assume otherwise — that assumption cost
real time already.

Also open, both legal Fortran that gfortran accepts:

- A full-line comment between continuation lines of one statement. Reported
  as "Unexpected token newline in expression".
- A character literal continued across lines. The continued part comes back
  as code tokens; hits fortfem's
  `example/iga_polar_feec/iga_polar_feec.f90:328` and `:344`, where a `write`
  format string is split across a continuation. Predates the recent parser
  work.

And 25 failing tests on `main`, pre-existing and unrelated, grouped by theme
in `fortfront/ROADMAP.md`. `test_variable_usage_block_construct` is worth
trying first now that BLOCK reaches a parser from nested bodies.

Already fixed and not to be re-done: `select type` / `select case`, `block`,
`use` inside `block`, and F2018 `stop ..., quiet=` were all unreachable from
nested bodies. That took fortfront's own unparseable sources from about 100
to 15.

#### 3. fortfem CI

Two failures, neither reproducible:

- `fo` builds all 733 targets, then every test fails to compile with
  `f951: Fatal Error: Reading module build/fo/mod/fortfem_feec.mod at line
  181 column 54: Expected right parenthesis` — a truncated module file
  written by gfortran 13.3 under fo's build. The workflow already sets
  `FO_JOBS: 1` for exactly this class of problem and it still happens.
- The workflow's fpm retry path then gets through the whole suite with one
  failure: `test_equation_objective_registry`, exit 2.

Neither reproduces in an `ubuntu:24.04` container with the runner's exact
gfortran 13.3.0: the project builds and that test passes 18/18. All 733 pass
locally through `fo`. **Local gfortran here is 16.1.1**, which is why the
container check was necessary and should be the first step for anyone
continuing. Note `main` is separately red on a line-truncation error that
this branch already fixes.

#### Dependency order

`fo` pins fortfront `main`, so a fortfront gap becomes an `fo` scan failure,
and an `fo` scan failure becomes a silently missing object downstream. fpm
caches the dependency clone: `rm -rf build/dependencies build/cache.toml`
before reinstalling `fo` after a fortfront change, or the old fortfront is
silently reused and the fix appears not to work.

### What belongs in fortad-bench

The corpus repository, never this one — hard execution rule 10. Currently 59
operators: 17 fortnum, 42 fortfem, plus Enzyme's own suite.

Add for every item:

- The kernel as plain Fortran in `cases/<suite>/kernels/`, plus the `_c.f90`
  C-bound variant Enzyme differentiates. Both engines are compiled by the
  same flang deliberately, so the comparison is of derivative code and not of
  two compilers.
- A batch loop. One evaluation of these kernels is far below timer
  resolution, and batching is how fortfem and fortnum actually call them —
  once per cell or per quadrature point.
- Registration in the harness `ARITY` table and dispatcher, the `NW` count,
  and the engine wrapper macros.
- Forward, reverse and gradient-only numbers, plus build time and generated
  code size per rule 6, and the vectorisation report per rule 8.
- Regenerated plots, and the results tables in `README.md`.

Open there: `cases/fortfem/kernels/` holds 43 sources against the harness's
`NW = 42` — reconcile. Tapenade is not wired in and is the honest comparison
for the `rk4` reverse margin. Build time is unmeasured although the stated
goal covers it. Three result caveats stand unresolved and are named in
`fortad-bench/ROADMAP.md`.

### Verification conventions

Learned the hard way in this work; each one caught a wrong conclusion:

- Compare failing **sets**, not counts, before and after a change. Equal
  counts have hidden a swap here more than once.
- Establish the baseline on the same clean tree you will measure. Stale
  artifacts have produced both a false improvement and a false regression.
- Reproduce CI in an `ubuntu:24.04` container when the runner disagrees with
  local. Runner is gfortran 13.3, this box is 16.1, and defects exist that
  appear on only one.
- Check the frontend's parse result, not the process exit status.
- When reducing a file, preserve the original error signature or the
  reduction converges on a different bug.
- `gh pr checks` reporting `MERGEABLE` means "no conflicts", not green.

## What the implementation has established so far

Findings from building Phase 0 and Phase 1, recorded here because they change
the plan rather than merely reporting on it.

**fortfront is the right front end, and it needed no changes.** Its arena AST
already carries everything a differentiation pass needs: typed nodes with
integer-index children, `intent`, `is_contiguous`, `is_array` with dimension
expressions, name resolution, and a call graph. `compile_frontend_from_string`
with `INPUT_MODE_STANDARD` parses ordinary Fortran directly. Dummy arguments
arrive as `parameter_declaration_node`, not `identifier_node` - the one
non-obvious detail. **P0.5 is therefore settled: fortad lowers to its own IR
rather than differentiating the AST**, because the transformations it needs
(SSA renaming, expression-level zero propagation, statement reordering) are not
AST edits.

**Emitting text beats building AST nodes.** The emitter writes Fortran into a
growable buffer through recursive subroutines. The obvious alternative - a
recursive function returning `character(len=:), allocatable` - silently
corrupted its own output under gfortran and cost real debugging time.

**Two Fortran-specific traps produced silently wrong derivative code**, and
both are worth a linting rule:
- Nesting an arena-mutating call inside another call that also takes the arena.
  Fortran does not order argument evaluation and the arena reallocates
  underneath the outer call. Every mutating call now gets its own temporary.
- Reading `arena%exprs(i)%args(j)` as an actual argument to a call that also
  takes `arena`. Same cause, found by segfault. Nodes are snapshotted first.

**Zero-aware rule builders replace a separate activity pass at expression
level.** A tangent index of 0 means a structural zero, and propagating it
through the builders is why the emitted code has no `+ 0.0` terms and no dead
tangent statements. A declaration-level fixed-point activity analysis is still
needed and is implemented; the expression-level part came free.

**Reverse mode needs no second rule table.** Partials come from seeding the
forward rule with one for the argument of interest and zero for the rest. Modes
cannot drift apart, and a new intrinsic needs one rule rather than two. This
was the highest-leverage architectural decision in the dossier and it held up
in practice.

**Reduction loops need no tape.** The adjoint of a linear accumulation does not
read the accumulator, and per-iteration temporaries are cheaper to recompute
than to store. The emitted reverse loop therefore has no loop-carried
dependence and stays parallelisable - a property a taped adjoint does not have.

**Loop fusion was worth more than everything else combined.** The reverse
sweep was emitted as a second loop over the same arrays, so a bandwidth-bound
kernel streamed its inputs twice. Fusing the adjoint into the primal loop -
valid because the reduction's adjoint seed is loop-invariant - took reverse
mode from tied with Enzyme to 1.7-1.8x faster, and from 85% behind a
hand-written adjoint to within 5-9% of it. This is the clearest evidence for
the dossier's thesis: a source-level tool can restructure the derivative
program, and an IR-level tool that must run forward then reverse cannot.

**A shared abstraction earned its extraction.** fortsym and fortad had
independently written the same text buffer, line-continuation breaker, and
provenance banner, with the same rationale in their comments. That is the bar
for `fortgen`; one implementation would have been a guess.

**The portability claim now has a check.** gfortran, flang, nvfortran and
LFortran all compile the generated adjoint, tangent, and vector tangent with no
compiler-specific handling, and gfortran vectorises the fused adjoint loop.

**First measured result** (`fortad-bench`, dot_sin, this machine): fortad is
~8% faster than Enzyme per element at one direction and matches the
hand-written analytical tangent; vector mode reaches ~10x per direction at 16
directions while any one-call-per-direction engine stays flat. Build time is
currently a wash on a case this small, and the claim that fortad's build story
is categorically better is **not yet supported by measurement** - it rests on
needing no matching LLVM and no plugin, which this benchmark does not capture.

## Phase 0 — Establish the ground truth before building anything

The purpose of this phase is to find out early whether the thesis is wrong. Every
item is cheap and every item can kill or redirect the project.

- [x] **P0.1 Fetch and licence-verify the study corpus.** Run
      fortad-bench's `scripts/fetch_upstreams.py` and `--licenses`. Resolve every `VERIFY` in
      fortad-bench's `docs/upstreams.toml` against the actual checkout. Any entry with no
      discoverable licence drops to metadata-only. Record the revisions. Completed
      2026-08-05: 33 reachable checkouts have recorded revisions and licence files;
      six historical or unavailable sources are explicit metadata-only entries;
      the inventory has no unresolved `VERIFY`, `NOT FETCHED`, or `NONE FOUND` rows.
- [x] **P0.2 Resolve the bibliography.** `scripts/fetch_literature.py --resolve`,
      then `--fetch`. Fix titles that Crossref cannot match. Record which papers
      are open access and which need institutional retrieval. Completed
      2026-08-05: all 33 entries were resolved remotely; nine arXiv PDFs were
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
      reference at 8.66 us/forward and 13.68 us/reverse; peak RSS is 3348 kB
      versus 3280 kB. The hand version is therefore the measured ceiling that
      fortad must beat, not a claimed win. Full source, validation, compiler
      diagnostics, build timing and size records are in fortad-bench commit
      `eafcd1c`.
- [x] **P0.7 Establish the benchmark harness in fortad-bench.** Stand up
      `harness/` and the `analytical`, finite-difference and Enzyme adapters,
      inheriting `differentiable-fortran`'s protocol and contract rather than
      reinventing them. Record build time alongside runtime from the first row.
- [x] **P0.8 Decision gate.** The frontend and independent correctness gates
      pass. The hand-written Fortran JVP and VJP are slower than C++/Enzyme on
      the VMEC++ kernel, so the original universal performance thesis fails.
      `docs/design/go-no-go.md` records the no-go result and this roadmap was
      rewritten before continuing Phase 1 work.

## Phase 1 — Forward mode, one kernel, end to end

Smallest thing that is genuinely useful and genuinely measurable.

- [x] **P1.1 fortad IR.** Whatever P0.5 decided. Typed, name-resolved,
      three-address, explicit control flow, arena-indexed. Round-trips to Fortran
      through fortfront's emitter with no semantic change — tested by running the
      round-tripped primal against the original on fortnum's test suite.
- [x] **P1.2 Normalisation passes.** Inlining, loop normalisation, canonical
      three-address form, constant propagation. This is the pass set that buys
      back Enzyme's post-optimisation advantage (dossier §5.6) and it is worth
      doing well. The three previously blocked vector-Newton routines now pass
      independent central-FD checks; focused runtime, build, size and memory
      records are in `fortad-bench/results/vector_newton_fortad.csv`.
- [x] **P1.3 Activity analysis.** Forward "varied" ∧ backward "useful". Report
      the fraction of statements eliminated on the VMEC++ kernel.
- [x] **P1.4 JVP rule table.** Operators, intrinsics, `real(dp)` arithmetic. The
      table is declarative and separate from the transformation, in the shape of
      ChainRules' `frule` (dossier §6.1). Rules for array expressions, not only
      scalars.
- [x] **P1.5 Scalar forward-mode transformation.** One tangent direction. Emit
      standard Fortran. Correct on the VMEC++ kernel against P0.6's hand-written
      JVP.
- [x] **P1.6 Emitter quality pass.** CSE, scalar replacement, tangent update
      fused into the primal loop, `intent`/`contiguous`/`pure` preserved, no
      allocation in the loop. Gate: gfortran vectorises the emitted kernel.
- [x] **P1.7 Benchmark: fortad JVP vs Enzyme vs analytical vs hand-written.**
      Runtime, peak memory, build time, generated-code size. Publish into
      `differentiable-fortran`. **This is the first result that means anything.**
- [x] **P1.8 Vector forward mode.** Contiguous trailing tangent dimension, chunk
      width tuned to SIMD width. Measure the scaling in direction count against
      Enzyme's `BatchDuplicated` and against `k` separate scalar JVPs. Dossier
      §5.4 predicts this is the largest forward-mode win; verify or retract.
- [x] **P1.9 Second and third kernels.** One from fortnum `special/`, one from
      `quadrature/` or `interp/`. Fix whatever breaks. Do not generalise before
      three kernels have demanded the same generalisation. `erfsum` and
      `fixed_quadrature_integrand` now pass independent central-FD checks and
      have complete runtime, memory, build and size records in
      `fortad-bench/results/p19_kernels_fortad.csv`. The wide quadrature
      access pattern remains the separately named slice-packing limitation.

## Phase 2 — Reverse mode

- [x] **P2.1 TBR analysis.** The loop analysis now makes the recording choice
      explicit: reduction accumulators and recomputable temporaries are not
      stored, while nonlinear carried states get typed per-loop storage. The
      Phase 1 byte comparison, including the nonlinear recurrence boundary
      case, is recorded in
      `fortad-bench/results/p21_tbr_fortad.csv` and
      `fortad-bench/results/p21_tbr_validation.txt`. Hascoët et al. 2005.
- [x] **P2.2 Linearity analysis.** The strict carried-variable test removes
      the state tape from the affine RK4 recurrence: 8000 bytes at `n=1000`,
      with an independent directional finite-difference check. The
      counterfactual and emitted-source record are in
      `fortad-bench/results/p22_linearity_fortad.csv` and
      `fortad-bench/results/p22_linearity_validation.txt`.
- [x] **P2.3 Transposition of the linear part.** Derive VJP from the JVP rules by
      transposition (dossier §6.1) rather than writing a second rule table. If
      this proves impractical for Fortran's mutation, record why in
      `docs/design/` and fall back to explicit adjoint rules — but try it first,
      because it is the difference between one rule table and two.
- [x] **P2.4 Data-flow reversal for control flow.** Branches and reduction
      loops done; nonlinear loop-carried recurrences still refused. Loops, branches, `where`,
      `forall`. Typed, pre-sized, per-loop storage — never a generic tape
      (dossier §4.3). Mooncake is the reference for mutation.
- [x] **P2.5 Statement-level preaccumulation.** Local Jacobian per statement in
      registers. Hogan 2014. Measure against the non-preaccumulated adjoint.
- [x] **P2.6 Reverse-mode benchmark vs Enzyme.** Gradient of a fortnum workload
      with many inputs. Runtime, peak memory, build time.
- [x] **P2.7 Adjoint of parallel loops.** One-level fused positive reduction
      loops now emit `parallel do` with explicit reduction,
      `default(firstprivate)` scalar scope, and shared procedure dummies. The
      generated form passed serial/eight-thread independent directional-FD
      checks for `erfsum`; records are in
      `fortad-bench/results/p27_openmp_fortad.csv` and
      `fortad-bench/results/p27_openmp_validation.txt`. Negative accumulations,
      carried and nested loops, and general index-set transposition remain
      outside this scoped path. Hückelheim & Hascoët 2022; Paszke et al. 2021.

## Phase 3 — The structural wins

This phase is where the dossier claims fortad becomes uncatchable, because these
are asymptotic advantages Enzyme cannot obtain by seeing more IR.

- [x] **P3.1 Custom-rule registry.** `frule`/`rrule`, projection, explicit
      opt-out. Wire it to fortnum's existing `fortnum_derivative_registry` so a
      kernel's `analytical` candidate becomes fortad's rule automatically.
- [~] **P3.2 BLAS and LAPACK rules.** The mechanism exists and is tested on a
      linear solve; the rule table itself is not yet written. Giles 2008. Reverse of a linear solve
      reuses the existing factorisation. Measure against Enzyme differentiating
      through `dgesv`.
- [~] **P3.3 Implicit differentiation of nonlinear solves and roots.** Same
      mechanism; the linear case is done and tested. IFT at the
      converged point. Measure against Enzyme adjointing the iteration.
- [ ] **P3.4 Fixed-point adjoints.** Christianson two-phase.
- [ ] **P3.5 FFT, quadrature, interpolation, special-function rules.** Sourced
      from fortnum's analytical kernels and DLMF identities.
- [x] **P3.6 Revolve checkpointing** for time-stepping adjoints. Griewank &
      Walther 2000. Schedules are executed against a simulated integration in
      the tests, not inspected, and the forward-step count is checked against
      the binomial bound.
- [~] **P3.7 Sparsity.** Distance-2 column colouring and compressed Jacobians
      are done and tested. Static pattern propagation and star colouring for
      Hessians are not; the pattern is supplied by the caller.

## Phase 4 — Second order and higher

- [x] **P4.1 Forward-over-reverse HVPs.** Default Hessian route.
- [x] **P4.2 Dense Hessians** by `n` HVPs, and **sparse Hessians** by star
      colouring with direct recovery. On a symmetric arrowhead it uses 2
      colours where the asymmetric test needs 10, and the test asserts the
      improvement rather than merely that both are valid.
- [ ] **P4.3 edge_pushing** as a competing sparse-Hessian candidate. Keep only if
      it wins measurably.
- [~] **P4.4 Higher-order Taylor kernels.** The arithmetic is built and
      pinned against closed-form series: exp, log, sqrt, sin/cos, the Cauchy
      product, division, and integer powers, all `O(d^2)` per operation rather
      than the `O(2^d)` of nesting a first-order tool. The **transformation**
      that rewrites a kernel into calls to these routines is not built; what
      exists is the piece whose correctness can be pinned exactly.

## Phase 5 — Products

- [x] **P5.1 Linear UQ.** `cov(y) = J cov(x) Jᵀ` on the vector forward mode.
      Document precisely where first-order propagation stops being valid
      (Saltelli et al. 2008).
- [x] **P5.2 Sensitivity analysis driver.** Documented and tested as a
      shape rule rather than a wrapper; see docs/products.md. Mode selected automatically from
      input count, output count, and sparsity.
- [~] **P5.3 Optimiser integration.** Gradients and HVPs exist and are
      tested; the driver layer does not. Gradients, Gauss-Newton `JᵀJv`,
      Newton-Krylov `Hv`, full Hessians, wired to fortnum's optimisers.
- [ ] **P5.4 Public API freeze.** ADOL-C's driver set is the model for the
      surface: `jacobian`, `hessian`, `jac_vec`, `vec_jac`, `hess_vec`, and the
      sparse variants.

## Phase 6 — Reach

- [x] **P6.1 Compiler matrix.** gfortran, ifx, flang-new, nvfortran, LFortran,
      NAG. Emitted code builds and vectorises on all of them, in CI.
- [ ] **P6.2 GPU.** OpenMP target and OpenACC directives on emitted derivative
      code. Only after the CPU story is measured and won. Transfer-inclusive
      wall clock is the metric; silent host fallback is a failure.
- [x] **P6.3 Standalone CLI.** `fortad --mode=reverse --dep=f --indep=x kernel.f90`
      so fortad is usable outside the lazy-fortran stack.

---

## Where the work stands

Done and measured: forward mode in every shape the IR supports, reverse mode
over straight-line code, branches, reduction loops and array-element writes,
Hessian-vector products, a user-rule registry, the compiler matrix, and both
halves of the performance goal on one kernel.

**The largest predicted lever is still unbuilt.** The dossier argues that
differentiating a solve through the implicit function theorem, rather than
through its iterations, is an asymptotic win that no loop-level cleverness
recovers. The registry is the mechanism for it, but the registry currently
carries scalar partials only. Structured rules that emit *statements* - solve
with the existing factorisation, transpose a BLAS call, apply the IFT at a
converged point - are the next substantial piece of work, and the one most
likely to change the shape of the benchmark results.

Per-iteration storage, nested loops, and the product documentation are done.
Structured rules that emit statements are built: `fad_add_call_rule` takes
Fortran statement templates, and the linear-solve case is tested against a real
solver. That closes the dossier's largest predicted lever.

### The machine-independent optimiser

Measuring against Tapenade on the Enzyme suite showed that the emitted code, not
the differentiation, was the gap. `fortad_opt` now runs eight passes that a
Fortran compiler is not permitted to run, because reassociating floating-point
arithmetic changes rounding and the caller's build will not have `-ffast-math`.
An AD tool may: the forms agree in exact arithmetic.

| pass | what it does |
| --- | --- |
| `propagate_copies` | `a = b` makes later reads of `a` read `b` |
| `substitute_temps` | inline a definition into its use |
| `propagate_loop_zeros` | the first accumulation onto a cleared adjoint is an assignment |
| `coalesce_element_updates` | repeated `z_b(i)` updates become one load and one store |
| `rename_bodies` | one assignment per scalar per iteration, so substitution can see through an accumulation |
| `factor_self_update` | `x = x*c1 + x*c2` becomes `x = x*(c1 + c2)`, around the target or around any variable the terms share |
| `rotate_carried` | issue the loop-carried update first, behind a snapshot |
| `distribute_products` | expand `(a + b)*c` so factoring sees a sum of products |
| `balance_sums` | a long sum becomes a balanced tree, not a left-leaning chain |
| `regroup_products` | reassociate so invariant factors group together |
| `hoist_invariants` | lift a wholly invariant statement out of the loop |
| `hoist_subexpressions` | name the invariant coefficient, compute it once |

Two of these are worth spelling out because they look wrong in isolation.

`substitute_temps` inlines a definition read *more than once* when it is a
single arithmetic operation. That duplicates work. It is right because it is
what exposes the coefficient of a self-update to `factor_self_update`, after
which `hoist_subexpressions` lifts it out of the loop and both copies vanish.
Judged one pass at a time it is a pessimisation.

`propagate_loop_zeros` deletes the clear at the end of a reverse-sweep body.
Dead-store elimination cannot: the read it appears to protect is the
accumulation earlier in the body, which belongs to the *next* iteration, and
that pass does not model iterations.

`rotate_carried` runs last and looks like a pessimisation too: it adds a
register move. The loop-carried dependence is the critical path, and anything
scheduled before the update lengthens the wait for the next iteration.
Snapshotting the incoming value buys a decoupled scatter for one move, and on
euler it is worth 15%.

Whether to substitute a definition read more than once cannot be decided in
advance: duplicating it is right when factoring then folds the whole chain into
constants and wrong when it does not. So both are tried, and the choice is made
**per loop**, greedily - each top-level loop is offered the bold form in turn
and keeps it only if the total operation count in loop bodies falls. That is
`n + 1` runs of the pass sequence for `n` loops, cheap next to compiling the
result. Ties go to the conservative form, which has fewer duplicated
subexpressions and so fewer live values than the cost model can see.

Two details of the cost model were each worth a large fraction of the benefit
and each looked like a detail:

- It counts arithmetic **operations**, not arena nodes. A variable or a literal
  is free, and counting leaves made a form with fewer multiplies but more
  operands score worse than one with more multiplies.
- The pass sequence ends with dead-store elimination. Without it a collapsed
  loop body still carried the statements it replaced, and scored exactly what it
  replaced - so every bold trial tied with its conservative twin and none was
  ever selected. The whole per-loop mechanism was inert until this was fixed.

Every pass reasons only within a straight-line run of assignments; a definition
whose reads are not all inside that window is left alone. The passes run on both
the forward and the reverse pipeline.

Substitution must also check that the definition's operands are unchanged
between definition and use. That check was missing at first, and the oracle
suite caught it as soon as `rename_bodies` made `t = u` followed by a write to
`u` reachable - the inlined copy silently read the new `u`.

### Contracts

`fad_vjp(..., with_primal=.false.)` returns the gradient without the primal
value. This is not a convenience: Tapenade's reverse routine never assigns the
primal output, Enzyme's always does because its seed rides on a duplicated
output, and comparing across that difference credits Tapenade with a forward
loop it does not run. With the primal dropped, dead-loop elimination removes the
entire forward sweep whenever no adjoint coefficient needs a primal value.

Remaining, in rough order of expected value: collapsing a loop body that is
wholly linear in its carried variable into a single scaling (this is the rk4
gap), star colouring for sparse Hessians, static sparsity-pattern propagation,
and reverse-mode rules applied automatically where fortnum already declares an
`analytical` candidate.
Branches inside loops and recurrences inside nests are still refused by name.
Assignment to an array element outside a loop is no longer among them: an
element write names a storage location rather than a variable, so there is
nothing to give an SSA version to, and its adjoint is a scatter into the
matching element of the array's adjoint. Where that array is the dependent, the
sweep works on a local copy of the seed - clearing an element's adjoint after
reading it is what makes a store's adjoint stop there, and the seed is the
caller's `intent(in)` argument.

## Open defects

- **Forward mode does not vectorise where Enzyme's does.** Measured rather than
  guessed: disassembling both entry points for fortfem's polygon edge area,
  Enzyme's loop is four `mulpd` and two `addpd` - packed double throughout -
  and fortad's is four `mulsd`, three `subsd` and two `mulpd`, mostly scalar.
  Same operation count, 35% slower.

  Enzyme computes the value and the tangent as a two-lane vector. fortad emits
  them as two independent scalar chains - `y_d = y_d + ...` and
  `y = y + ...` - which the compiler does not fuse. That is the shape of the
  emitted code rather than anything about the derivative, and it is the best
  explanation on hand for why fortad's reverse numbers lead Enzyme's while its
  forward numbers sit at parity: reverse has one accumulation to make, forward
  has two that want to be one vector.

  Two explanations have been tested and are wrong:

  - **Not accumulator pairing.** Writing the value and tangent accumulators as
    a two-element array and accumulating `acc = acc + step*half` measures
    0.359 ns/input against the current 0.356. Enzyme is at 0.257. The pair is
    not what Enzyme is packing.
  - **Not the compile pipeline.** The Enzyme object goes flang to IR, then
    `opt -O3`, then `clang -O3`, while fortad's is `flang -O3` on Fortran.
    Putting fortad's generated Fortran through the identical pipeline gives the
    same instruction mix, four `mulsd` and three `subsd`. The scalar code is
    what fortad emits, not what a compiler made of it.

  Reading the IR rather than guessing a third time: **both loops vectorise**.
  Each carries a `<2 x double>` accumulator. The difference is that fortad's
  packs a pair, then takes it apart again -

      %27 = shufflevector <2 x double> %26, ..., <i32 1, i32 0>
      %29 = fmul contract <2 x double> %27, %28
      %30 = extractelement <2 x double> %29, i64 0
      %32 = extractelement <2 x double> %29, i64 1
      %31 = fsub contract double %24, %30

  - one packed multiply whose lanes are immediately extracted to do the
  subtractions in scalar. The `shufflevector` is a lane swap, which says the
  pairing LLVM found does not match the order the terms are written in.

  The terms of this tangent are `z(1)*z_d(4)`, `z_d(1)*z(4)`, `z(3)*z_d(2)`,
  `z_d(3)*z(2)`: no two of them share an adjacent pair of array elements, so
  there is no term order that makes the loads line up. Whatever Enzyme is doing
  is not a reordering of the same four products.

  Two further measurements, and they do not agree with each other:

  - **Reassociation of the reduction is what blocks vectorisation.** Compiling
    fortad's own output with `-ffast-math` turns it fully packed - nineteen
    `mulpd`, seventeen packed loads, no scalar multiplies - which is Enzyme's
    shape exactly. flang will not reorder `y = y + ...` across iterations
    without permission, and Enzyme's IR carries that permission.
  - **But vectorising it does not close the gap.** Strip-mining the reduction
    four ways reassociates it explicitly and portably, with no flags, and the
    result is packed. It measures 0.365 ns/input against 0.373 unvectorised,
    with Enzyme at 0.271. Marginal, nowhere near.

  **The achievable ceiling has now been measured, and it does not reach the
  target.** Two transformations are available and both were hand-written
  against fortad's current output:

  - reading the four adjacent inputs as one contiguous slice, so the loads pack:
    0.333 ns/input against 0.356
  - two batch elements an iteration with two partial accumulators, so the
    reduction is reassociated: 0.347 against 0.380

  Together they give 0.335 - no better than the slice alone, so they do not
  compose - against Enzyme's 0.258. That is 1.30x, still outside 20%. Whatever
  accounts for the remainder is not either of these and has survived seven
  investigations.

  A seventh measurement bounds what is left. Enzyme processes two batch
  elements per iteration with packed loads where fortad processes one scalar.
  Hand-writing that - two partial accumulators, two elements an iteration -
  measures 0.347 ns/input against fortad's 0.380, with Enzyme at 0.284. So
  reduction strip-mining is worth about 9% here and would leave this kernel at
  roughly 1.22x, still outside the target: it is part of the answer and not the
  whole of it.

  So the scalar code is a real difference and is not the reason Enzyme is
  faster here. Six explanations have now been tested on this kernel -
  reassociation cost, instruction scheduling, accumulator pairing, compile
  pipeline, term ordering, reduction vectorisation - and all six are wrong or
  insufficient. It is four operations and 0.1 ns per element, and the next
  person to look at it should start from the fact that six plausible answers
  are already eliminated.

- **Generation cost is no longer a defect, and here is what it took.**
  fortfem's degree-eleven Bezier edge area took 20.5s to differentiate. It
  takes 3.2s. Two passes were 18 of those 20 seconds, which timing every pass
  showed in one command after several rounds of bisecting by hand.

  `substitute_temps` scanned from each definition to the end of the procedure
  looking for reads. The span table already knows where a name is last
  mentioned, so the walk stops there.

  `hoist_subexpressions` asked whether an expression was loop-invariant once
  per node of every statement, and each question walked the body. It reads a
  flag now, held per arena node and computed bottom-up. Three things had to be
  right at once, and each was found by getting it wrong:

  - **Membership must be O(1).** Scanning the written names per node is what
    made the first two attempts slower rather than faster - a body writes as
    many names as it has statements. It is a hash set.
  - **Every identifier in a node's text counts, not the leading one.** A tape
    restore arrives as `u_tape(i)`; taking the part before the parenthesis
    calls it invariant when it depends on the loop variable.
  - **A child created during the same extension still counts.** Skipping
    children above the previous high-water mark ignored their dependence and
    called the parent invariant. Three forward-mode oracles caught it.

  And the refresh has to be driven by the passes that change the body, not by
  the question: rebuilding the written set costs a pass over the body, and the
  question is asked millions of times.

- **`hoist_subexpressions` was quadratic on a large loop body.** Two of
  the three passes that were have been fixed: `share_in_body` counts uses once
  per round instead of per candidate, and `escapes` answers from a precomputed
  span of where each name is mentioned instead of scanning the procedure.
  fortfem's quintic Lagrange weights went from 11.0s to 0.40s and its quartic
  Bezier from 3.2s to 0.37s.

  Fixed, above.

A competitor winning a benchmark is a defect with an owner, not a limitation to
document. Currently open:

- **Taped recurrences are about 3.5% slower than Enzyme.** Measured on
  `fortad-bench/cases/recurrence`; was 10-12% before the square rule. The TBR
  boundary is now explicit, but the remaining store-versus-recompute choice
  inside a taped recurrence has no cost model: fortad recomputes the statement
  whose input it taped, where Enzyme appears to store more. That is right for
  the bandwidth-bound cases it was chosen for and wrong here. Dossier section
  4.3.

- **rk4 with the primal is 10% behind Enzyme, which is within tolerance.** 21.15 ns/input against 19.13,
  on a primal costing 13.44; the reverse sweeps are 7.71 and 5.69. The whole rk4
  step is linear in `state`, so the adjoint collapses in principle to one
  scaling and one scatter. `rename_bodies` removed the multiple-definition
  barrier and generalised factoring collapsed the same shape in bruss and ba,
  but rk4's scatter is built as a chain of accumulations onto `z_b(i)`, and
  factoring works within one statement. Merging an accumulation chain into a
  single expression before factoring is the remaining step. Gradient-only rk4
  already leads Tapenade, 7.68 against 8.71.

- **An assumed-size dummy silently becomes assumed-shape.** A primal declaring
  `real(dp), intent(in) :: z(*)` produces a derivative declaring
  `dimension(:)`. The generated routine then no longer matches the caller's
  explicit interface, which is undefined behaviour rather than an error - it
  segfaulted the fortnum benchmark driver. fortad needs a shape to emit
  `z_b = 0`, so the fix is either to carry `(*)` through and zero it by
  element, or to refuse the declaration and say why.

- **A wrapped continuation can still overrun the 88-column target by a few
  columns.** The limit is respected for the line being wrapped, but the
  continuation's own indent is not subtracted, so a body expression inside a
  module lands at 89 to 91. Well inside the standard's 132; a style defect, not
  a portability one.

- **The whole-procedure CSE pass is written and stays disabled.** `fortad_cse`
  produces wrong Hessians through the `fad_hvp` composition path, silently - a
  plausible but non-symmetric result. `share_subexpressions` in `fortad_opt`
  replaces it for the case that mattered: it is confined to a straight-line run
  of assignments inside one loop body, runs after every other pass, and takes
  the temporary's type from the subexpression rather than from the statement
  around it. On the fortnum operators it took lagrange4's tangent from 21% to
  18% behind Enzyme and det3's from 11% to 5%. The older pass has no remaining
  workload arguing for it.

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

## fci_polygon_edge_area tangent sits at 1.24x Enzyme

The only measurement above 20% of Enzyme across all three suites. The
cause is measured, not guessed.

Per two inputs, Enzyme's loop does three `mulpd`, one `addpd` and one
`subpd` - five vector ALU ops. fortad's does four `mulpd`, three
`subpd` and two `addpd` - nine. Total instruction counts are almost
equal (38 against 39), so this is not extra work in the usual sense.

Enzyme gathers strided lanes with `movsd` plus `movhpd` rather than
reading the four adjacent inputs as one `movupd` pair. That lane
pairing lets the primal and the tangent share partial products. flang
does not find the same pairing from the expression shape fortad emits,
and no algebraic identity is missing on fortad's side - the arithmetic
is already minimal as written.

Closing this needs fortad to emit the tangent with an operand order
that makes the shared products adjacent in a lane, which is a
vectorisation concern rather than a differentiation one. The contiguous
slice read brought it from 1.40x to 1.24x.

Grouping the sum into added terms minus subtracted terms was tried and
reverted. It produced exactly Enzyme's expression shape and cut the
loop from nine vector arithmetic ops to six, and the measurement did
not move - so this loop is bound by its loads and shuffles, not by its
arithmetic. It also unbalanced sums whose signs are lopsided and cost
the degree-eleven Bezier adjoint eleven points. Both results are worth
keeping in mind before anyone tries the same thing again.

## hoist_subexpressions does not terminate on a large inlined body (fixed)

Fixed. `insert_before(p, first, s)` shifts every following body statement
down by one, and the loop kept scanning from the old index, so it re-visited
the statement it had just displaced and hoisted forever. The cursor now
advances alongside `first` and `last`. Two needless `invalidate_invariance()`
calls were removed at the same time, which is what made the re-sweep
described below expensive rather than merely wasteful.

The analysis kept below is still accurate about the cache behaviour and is
worth reading before changing the invariance cache again.

## Original analysis: hoist_subexpressions on a large inlined body

Differentiating fortnum's `fixed_newton_solve` after inlining its two
residuals runs for over two minutes and was killed. Sampling the stack
puts it in

    hoist_subexpressions -> maximal_invariant -> loop_invariant
    -> ensure_invariance -> node_invariant -> text_touches

Inlining is not the problem: `--roundtrip --proc fixed_newton_solve`
lowers the same body in well under a second, so lowering and inlining
both finish. Everything after that is differentiation and optimisation.

The invariance cache is keyed on the set of names a loop writes. When
that set changes the whole arena is re-swept, and hoisting itself adds
nodes to the arena as it goes. On the small kernels the corpus had
before inlining this never showed; a body with several inlined callees
is the first thing large enough to expose it.

What this blocked: three of fortnum's vector-Newton routines, which were
the last of Enzyme's corpus that fortad could not do for a reason that
was fortad's own rather than a missing rule. Re-check whether those three
now differentiate end to end, and whether their numbers are within the
30% band; that verification has not been done since the fix.

## Outstanding (2026-08-05)

- The three vector-Newton routines are verified and recorded in
  `fortad-bench/results/vector_newton_fortad.csv`. This is a focused fortad
  record because the acluster has no compatible Enzyme toolchain; no
  same-machine 30% comparison is claimed.
- Forward-mode vectorisation, below, remains the largest systematic gap
  against Enzyme and is the one worth solving properly rather than
  case by case.
- Slice packing on wide operators, below, is a narrower instance of the
  same theme: the emitted shape rather than the derivative.
- Coverage in `fortad-bench` now stands at 59 operators across fortnum and
  fortfem. Every kernel Enzyme covers that fortad also covers has numbers
  recorded there; keep new rules paired with a benchmark case.

## Slice packing pays twice on a wide operator

fortfem's curved quadrilateral cell area takes sixteen inputs and its
tangent runs 1.63x Enzyme, the widest gap in that suite. fortad emits
444 instructions against Enzyme's 305.

The emitted loop reads the whole run as a slice, into a sixteen-element
temporary, and then copies each element out into a named scalar. Both
costs are paid: the copy of the run, and the extraction of every
element. Enzyme reads `z(base + k)` where it needs it.

Packing is right when a run is read once and narrow - fortfem's polygon
edge area, four inputs, went from 1.40x to 1.24x on it. It is wrong
when the elements are each bound to a name that is then used
separately, because the slice copy buys nothing the individual reads
did not already have.

The fix is not a width threshold, which would be arbitrary. It is to
propagate the element reads into their uses so the named temporaries
disappear, leaving the slice as the only load - or, where that is not
possible, to leave the reads alone.
