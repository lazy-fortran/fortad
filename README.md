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

The goal is not parity with Enzyme. It is to beat it on the workloads that
matter for computational physics, by differentiating **above** scalar IR where
Fortran's array semantics, `intent`, contiguity, shapes and purity are still
visible — and by differentiating the *mathematics* of a solve rather than its
iterations.

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

## Studying the field

Third-party code and papers are fetched locally and **never committed**:

```bash
scripts/fetch_upstreams.py --list      # what will be fetched
scripts/fetch_upstreams.py             # clone into gitignored upstream/
scripts/fetch_upstreams.py --licenses  # verify declared vs actual licences
scripts/fetch_literature.py --fetch    # openly licensed papers only
```

`docs/upstreams.toml` pins every project with its licence, the paths worth
reading, and what fortad wants to learn from it. `docs/bibliography.bib` carries
the literature as metadata. Both are committed; the checkouts and the PDFs are
not.

Read **[LEGAL.md](LEGAL.md)** before copying anything out of `upstream/`. For
most entries the answer is that you may not. **[PROVENANCE.md](PROVENANCE.md)**
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
