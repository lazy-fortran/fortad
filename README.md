# fortad

Source-transformation automatic differentiation for Fortran. Reads Fortran,
emits **standard Fortran** derivative code that any conforming compiler builds —
no plugin, no LLVM version lock, no constraint on how the primal is written.

```console
$ fortad --mode reverse --indep a,b --module k_adjoint kernel.f90
```

Working today: forward mode (scalar, array, loop, branch, and vector/batched),
reverse mode (straight-line, branches, and loops - reductions, element writes,
and taped recurrences), Hessian-vector products, and a registry for your own
procedures' derivatives. 48 oracle cases
pass locally, each checked against finite differences, the adjoint identity, or
Hessian symmetry - never against another AD tool.

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

## Measured against Enzyme

On the `dot_sin` reduction kernel, this machine, all Fortran compiled with the
same flang, correctness gating every timing. Full method and raw data in
[fortad-bench](https://github.com/lazy-fortran/fortad-bench).

| Mode | fortad vs Enzyme |
|---|---|
| Forward, one direction | ~8% faster, and matches a hand-written tangent |
| Forward, 16 directions | ~10x faster per direction |
| Reverse (gradient) | 1.7-1.8x faster, within 5-9% of a hand-written adjoint |
| Build time | 2.7x faster cold and incremental; 4x smaller object |
| Threads | bit-identical results, 7.4x on 8 cores |

The reverse-mode margin comes from fusing the adjoint into the primal loop, so
the arrays are streamed once rather than twice. Enzyme differentiates a program
it must run forward and then reverse; a source-level tool can restructure the
derivative program itself.

The build-time figure excludes building or installing a matching LLVM and the
Enzyme plugin. That is a cost fortad does not have at all, but it is paid once,
so folding it in would flatter fortad. The kernel is also small, so build time
here measures toolchain overhead rather than compiling a large body of code; on
a big file the Fortran compiler dominates both paths and the ratio narrows.

These are numbers from one machine on one kernel, not a promise about another.

## Status by capability

| Capability | Forward | Reverse |
|---|---|---|
| Straight-line expressions | yes | yes |
| Arrays and subscripts | yes | yes, including element writes in loops |
| `do` loops, including nests | yes | reductions, element writes, recurrences |
| `if`/`else` | yes | yes |
| Nonlinear loop-carried recurrence | yes | yes, taped |
| Vector / batched directions | yes | no |
| Hessian-vector products | forward-over-reverse | - |
| Calls to your own procedures | via a registered rule | via a registered rule |

What reverse mode cannot do it **refuses by name**, with a message saying which
mode does handle it. A named refusal is a bug report; a silent fallback is a
wrong derivative.

## Products

| Product | Object | Mode | State |
|---|---|---|---|
| Optimiser gradients | grad f | reverse | working |
| Newton-Krylov | H v | forward-over-reverse | working |
| Sensitivity analysis | rows or columns of J | forward or reverse | working, [documented](docs/products.md) |
| Linear UQ | cov(y) = J cov(x) J^T | vector forward | working, [documented](docs/products.md) |
| Gauss-Newton | J, J^T J v | vector forward + reverse | working, [documented](docs/products.md) |
| Sparse Jacobians | compressed | column colouring | working, [documented](docs/products.md) |
| Sparse Hessians | compressed | star colouring | planned |
| Higher order | Taylor coefficients | generated kernels | planned |

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

## Using it

```fortran
use fortad, only: fad_vjp, fad_result_t

type(fad_result_t) :: res

res = fad_vjp(source, independents=["x", "y"], module_name="my_adjoint")
if (res%ok) then
    write (unit, '(a)') res%code
else
    write (error_unit, '(a)') res%message
end if
```

`fad_jvp`, `fad_vjp`, and `fad_hvp` all take Fortran source and return Fortran
source. **[docs/products.md](docs/products.md)** works back from the product you
want - gradient, Hessian-vector product, linear UQ, sensitivity, Gauss-Newton -
to the call that gets it, with the cost of each. Passing `module_name` is recommended: the consumer then gets a
compiler-checked interface rather than an external declaration nobody verifies.

Generated procedures are `pure` and hold no state, so they are thread-safe by
construction, and the reduction adjoint loop carries no loop-carried dependence
and parallelises directly.

## Next

See **[ROADMAP.md](ROADMAP.md)**, which also records what building the engine
established. Next up: per-iteration storage for nonlinear recurrences in
reverse mode, then BLAS/LAPACK and implicit-solve rules built on the registry,
so a linear solve is differentiated as the operation it is rather than as the
iterations that implement it.

## Licence

MIT. Derivative code fortad emits from your program is your program's
derivative: fortad claims no copyright in its output and imposes no licence on
it.
