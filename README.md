# fortad

Source-transformation automatic differentiation for Fortran. Reads Fortran,
emits **standard Fortran** derivative code that any conforming compiler builds —
no plugin, no LLVM version lock, no constraint on how the primal is written.

This repository is currently **research and design**. There is no implementation
yet. What exists is the dossier, the roadmap, and the tooling to study the field
without redistributing anyone else's work.

## Why

The Fortran options today are Enzyme (an LLVM pass, requiring flang-new or
LFortran plus a matching `ClangEnzyme-NN.so`), Tapenade (Fortran 95-era, Inria
non-commercial terms), OpenAD (effectively unmaintained), or operator
overloading (which defeats vectorisation). There is no maintained, permissively
licensed, modern-Fortran source-transformation AD tool. fortad is that.

## The goal

**Faster than every engine in the field — Enzyme, Tapenade, Clad, CoDiPack,
ADOL-C, Adept, JAX, Zygote, Mooncake, Enzyme.jl — at both build time and
runtime, in every mode.**

Not parity. Not "competitive on our workloads". Not "wins on solver codes and
loses on scalar code". Every mode means forward, reverse, vector forms of both,
forward-over-reverse, sparse-compressed, and higher-order Taylor. Both metrics
means the generated derivative runs faster *and* the toolchain that produced it
builds faster.

Enzyme is the hardest of those targets at runtime and is therefore the one the
dossier argues against in detail; the overloading tools (CoDiPack, ADOL-C,
Adept) and the tracing tools (JAX, PyTorch) are separate targets with different
weak points. Build time is a target in its own right, measured and reported on
every roadmap item, not a footnote — a source transformation that emits cached
`.f90` should beat an LLVM plugin pass by a wide margin, and if it does not,
that is a defect.

The mechanism is differentiating **above** scalar IR, where Fortran's array
semantics, `intent`, contiguity, shapes and purity are still visible, and
differentiating the *mathematics* of a solve rather than its iterations.

The full argument, the algorithm catalogue, and the honest accounting of where
Enzyme wins are in **[docs/dossier.md](docs/dossier.md)**.

## What it will provide

| Product | Object | Mode |
|---|---|---|
| Linear UQ | `cov(y) = J cov(x) Jᵀ` | vector forward |
| Sensitivity analysis | rows or columns of `J`, sparse-compressed | forward, reverse, or compressed |
| Optimiser gradients | `∇f` | reverse |
| Gauss-Newton | `J`, `JᵀJ v` | vector forward + reverse |
| Newton-Krylov | `H v` | forward-over-reverse |
| Hessians | `H`, dense or sparse | HVPs, star coloring, or edge_pushing |
| Higher order | Taylor coefficients to order `d` | generated Taylor kernels |

## Where it fits

```
user Fortran → fortfront (AST) → fortad (normalise, analyse, differentiate)
             → standard Fortran → gfortran | ifx | flang-new | nvfortran | LFortran
```

- **[fortfront](https://github.com/lazy-fortran/fortfront)** provides the typed
  AST, name resolution, and Fortran emission.
- **[fortnum](https://github.com/lazy-fortran/fortnum)** is the testbed. fortad
  enters as a second `autodiff` candidate beside Enzyme and is selected only if
  it wins on measured complete-workload wall clock.
- **[fortsym](https://github.com/lazy-fortran/fortsym)** is the symbolic oracle
  and an optional CSE pass over emitted expressions.
- **[differentiable-fortran](https://github.com/lazy-fortran/differentiable-fortran)**
  supplies the fixed benchmark protocol and the Enzyme baselines.

## Benchmarks and the study corpus

**[fortad-bench](https://github.com/lazy-fortran/fortad-bench)** holds everything
expensive: the workloads, the adapters for every competing engine, the
measurement harness, and the committed results. It also holds the field survey —
the pinned manifest of 39 third-party AD projects
([`docs/upstreams.toml`](https://github.com/lazy-fortran/fortad-bench/blob/main/docs/upstreams.toml))
and the curated literature
([`docs/reading-list.md`](https://github.com/lazy-fortran/fortad-bench/blob/main/docs/reading-list.md)).

This repository keeps only what must run on every change: unit tests and
microbenchmarks that finish in seconds. **No third-party code or literature
lives here at all.** A fortad change claiming a performance result cites a run
recorded in fortad-bench; a change there never gates a commit here.

Read **[LEGAL.md](LEGAL.md)** before adapting anything from the study corpus —
for most entries the answer is that you may not. **[PROVENANCE.md](PROVENANCE.md)**
records the publication behind every algorithm fortad implements.

## Status

See **[ROADMAP.md](ROADMAP.md)**. The next action is not writing an AD engine —
it is measuring whether fortfront can parse fortnum's real kernels, and porting
the VMEC++ Jacobian kernel to Fortran so there is a head-to-head Enzyme number
to beat before any general machinery is built.

## Licence

MIT. Derivative code fortad emits from your program is your program's
derivative: fortad claims no copyright in its output and imposes no licence on
it.
