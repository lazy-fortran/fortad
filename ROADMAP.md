# fortad roadmap

The target is a source-transformation AD engine for Fortran that is **faster
than every existing AD engine, at both build time and runtime, in every mode**.

Every engine means Enzyme, Tapenade, Clad, CoDiPack, ADOL-C, Adept, Sacado,
JAX, PyTorch, Zygote, Mooncake and Enzyme.jl — not just Enzyme. Every mode means
forward, reverse, vector forms of both, forward-over-reverse, sparse-compressed,
and higher-order Taylor. Both metrics means the emitted derivative runs faster
*and* the toolchain producing it builds faster.

There is no workload class conceded in advance. Where a competing engine wins a
benchmark, that is an open defect with an owner, not a documented limitation.
Reasoning in [docs/dossier.md](docs/dossier.md).

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
10. No third-party code or literature enters this repository. The study
    corpus and the expensive benchmark corpus live in fortad-bench.
11. Update committed benchmark evidence when an item affects performance.
12. Check off the item, commit implementation + tests + evidence + this file
    together, push, and only then select the next item.

A mechanism name never selects a winner. Production selection requires an
independent oracle plus measured application runtime and peak memory — the same
rule fortnum applies to every candidate, applied to fortad without exception.

Do not combine checklist items. If an item contains several independent changes,
split it into smaller checkboxes before writing code.

---

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

- [ ] **P0.1 Fetch and licence-verify the study corpus.** Run
      fortad-bench's `scripts/fetch_upstreams.py` and `--licenses`. Resolve every `VERIFY` in
      fortad-bench's `docs/upstreams.toml` against the actual checkout. Any entry with no
      discoverable licence drops to metadata-only. Record the revisions.
- [ ] **P0.2 Resolve the bibliography.** `scripts/fetch_literature.py --resolve`,
      then `--fetch`. Fix titles that Crossref cannot match. Record which papers
      are open access and which need institutional retrieval.
- [ ] **P0.3 Read the four primary sources.** Hascoët & Pascual 2013 (Tapenade
      specification), Hascoët et al. 2005 (TBR), Giering & Kaminski 1998 (adjoint
      recipes), Moses & Churavy 2020 (Enzyme). Write a one-page note per paper
      into `docs/notes/`. This is a deliverable, not preparation.
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
- [ ] **P0.6 VMEC++ Jacobian kernel, Fortran port, hand-differentiated.** Port
      `ComputeHalfGridJacobian` to idiomatic Fortran. Hand-write its JVP and VJP.
      Validate against finite differences and the adjoint identity. Benchmark
      against the existing C++/Enzyme numbers on the same machine. **This is the
      number fortad must beat, and the hand-written version is the ceiling.**
- [x] **P0.7 Establish the benchmark harness in fortad-bench.** Stand up
      `harness/` and the `analytical`, finite-difference and Enzyme adapters,
      inheriting `differentiable-fortran`'s protocol and contract rather than
      reinventing them. Record build time alongside runtime from the first row.
- [ ] **P0.8 Decision gate.** Write `docs/design/go-no-go.md`: given P0.4 and
      P0.6, is the thesis intact? If fortfront cannot parse real kernels, or if
      the hand-written Fortran JVP does not at least match C++/Enzyme, stop and
      rewrite this roadmap before Phase 1.

## Phase 1 — Forward mode, one kernel, end to end

Smallest thing that is genuinely useful and genuinely measurable.

- [x] **P1.1 fortad IR.** Whatever P0.5 decided. Typed, name-resolved,
      three-address, explicit control flow, arena-indexed. Round-trips to Fortran
      through fortfront's emitter with no semantic change — tested by running the
      round-tripped primal against the original on fortnum's test suite.
- [~] **P1.2 Normalisation passes.** Inlining, loop normalisation, canonical
      three-address form, constant propagation. This is the pass set that buys
      back Enzyme's post-optimisation advantage (dossier §5.6) and it is worth
      doing well.
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
- [~] **P1.9 Second and third kernels.** One from fortnum `special/`, one from
      `quadrature/` or `interp/`. Fix whatever breaks. Do not generalise before
      three kernels have demanded the same generalisation.

## Phase 2 — Reverse mode

- [~] **P2.1 TBR analysis.** Not needed yet: SSA, recomputation and loop
      fusion have kept every case so far tape-free, so there is nothing to
      decide about recording. Revisit with per-iteration storage. Hascoët et al. 2005. Report bytes stored with and
      without it on each Phase 1 kernel.
- [ ] **P2.2 Linearity analysis.** Report additional bytes saved.
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
- [~] **P2.7 Adjoint of parallel loops.** The generated reduction adjoint
      already carries no loop-carried dependence and measures 7.4x on 8
      threads; explicit OpenMP directives are not yet emitted. OpenMP reductions, race-free
      accumulation, index-set transposition that does not serialise. Hückelheim &
      Hascoët 2022; Paszke et al. 2021.

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

- **`hoist_subexpressions` is still quadratic on a large loop body.** Two of
  the three passes that were have been fixed: `share_in_body` counts uses once
  per round instead of per candidate, and `escapes` answers from a precomputed
  span of where each name is mentioned instead of scanning the procedure.
  fortfem's quintic Lagrange weights went from 11.0s to 0.40s and its quartic
  Bezier from 3.2s to 0.37s.

  Its degree-eleven Bezier is still 20s, and `hoist_subexpressions` is about
  half of that: `loop_invariant` asks the body statement by statement whether
  an expression mentions its target, once per candidate node. Inverting it -
  collecting the names the body writes and asking the expression whether it
  reads any - is the obvious shape and measured three and a half times *slower*,
  because a body writes as many names as it has statements and the inner loop
  moves rather than disappearing. What it needs is a membership test that is
  not a linear scan, which means giving the pass a real set rather than an
  array of names.

A competitor winning a benchmark is a defect with an owner, not a limitation to
document. Currently open:

- **Taped recurrences are about 3.5% slower than Enzyme.** Measured on
  `fortad-bench/cases/recurrence`; was 10-12% before the square rule. What
  remains is store-versus-recompute: fortad recomputes the statement it taped
  the input of, where Enzyme appears to store more. fortad has no cost model
  for that choice and always prefers recomputation, which is right for the
  bandwidth-bound cases it was chosen for and wrong here. Dossier section 4.3.

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
