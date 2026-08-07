# fortad

`fortad` 0.1.0 transforms a supported subset of Fortran into standard Fortran
procedures for automatic differentiation. The generated source has no runtime
tape library or compiler plug-in dependency. The compiler matrix currently
checks gfortran, ifx, flang-new, nvfortran, and LFortran.

```console
$ fo exec fortad vjp example/rosenbrock.f90 --module rosenbrock_ad \
    --output build/rosenbrock_ad.f90
```

That command differentiates the checked-in
[Rosenbrock example](example/README.md) and writes a module containing
`rosenbrock_vjp`. The example page compiles the generated module and checks its
gradient against the analytic result.

## Build

The current manifest uses sibling path dependencies for `fortfront` and
`fortgen`. Clone all three repositories into the same parent directory:

```console
$ mkdir lazy-fortran
$ cd lazy-fortran
$ git clone https://github.com/lazy-fortran/fortfront.git
$ git clone https://github.com/lazy-fortran/fortgen.git
$ git clone https://github.com/lazy-fortran/fortad.git
$ cd fortad
$ fo check
```

The build requires a Fortran 2018 compiler, `fpm`, and the local
[`fo`](https://github.com/lazy-fortran/fo) build driver. `fo check` builds the
library and command-line application, then runs the oracle suite. Run bare
`fo` for the complete contributor gate, including lint and formatting checks.

## Current support

| Capability | Status |
| --- | --- |
| Scalar JVP and VJP | expressions, intrinsic calls, branches, and supported procedure calls |
| Array JVP | whole arrays and subscripts, including loops and batched directions |
| Array VJP | reductions, element writes, nested loops, affine recurrences, and taped nonlinear recurrences |
| Derivative-only output | forward and reverse modes with `--no-primal` |
| Hessian-vector product | scalar-direction forward-over-reverse |
| Sparse Jacobian | static conservative patterns, column coloring, seeding, and recovery |
| Sparse Hessian | repeated HVPs with star coloring and symmetric recovery |
| Higher derivatives | univariate Taylor transformation for straight-line scalar kernels |
| Long integrations | Revolve checkpoint schedules supplied to a caller-owned time loop |
| Opaque procedures | scalar partial rules and statement-based tangent/adjoint rules |
| Elemental procedures | same-file elemental functions retain elemental JVP/VJP array calls |
| Fixed-form input | CLI `.f`, `.for`, `.ftn`, and `.f77` files with legacy comments and declarations |
| Derived components | bounded concrete scalar, nested, inherited, and array component paths |
| Abstract/deferred hierarchy | fixed-dispatch JVP/VJP for statically known child overrides |
| Complex JVP/VJP | real-coordinate `conjg`, `real`, `aimag`, `cmplx`, `abs`, multiplication, and division; bounded real-objective VJP through `real(z)`/`dble(z)` |
| Aliasing and sections | named refusal for `pointer`, `target`, pointer association, and noncontiguous sections |
| GPU directives | one-level fused positive reduction adjoints through OpenMP target and OpenACC |

Forward vector mode places the direction index first. For an array `x(n)`, a
seed block has shape `(n_directions, n)`. Reverse vector mode is not
implemented. Sparse Hessians therefore run one scalar HVP for each color.

Complex forward examples and the real-coordinate contract are in
[the complex-values design note](docs/design/complex-values.md).
The bounded real-objective complex VJP and its hand/finite-difference/adjoint
oracle are covered by [`test_complex_reverse_oracle.f90`](test/test_complex_reverse_oracle.f90).
The bounded derived-component contract and its JVP/VJP oracle are in
[the derived-components design note](docs/design/derived-components.md).
Allocatable components and lifetime-changing statements are currently refused
with a source-line diagnostic. The boundary and primal oracle are in
[the allocation-lifetime design note](docs/design/allocation-lifetime.md).
The storage-identity boundary and its executable refusal oracle are in
[the aliasing and sections design note](docs/design/aliasing.md).
The fixed-dispatch abstract/deferred hierarchy slice and its multi-level
oracle are in [the abstract hierarchy design note](docs/design/abstract-hierarchy.md).

Same-file callees can be inlined before differentiation. A call whose body is
unavailable needs a registered derivative rule. The built-in structured rules
cover an opt-in `dgesv` path. The same interface supports caller-supplied rules
for nonlinear roots and fixed points, as well as selected numerical-library
operations.

Elemental procedures are differentiated as elemental procedures too. The
generated JVP and VJP can therefore be called with scalar or conformable array
actuals. See [`test_elemental_interface_oracle.f90`](test/test_elemental_interface_oracle.f90)
for the finite-difference and compiled array oracle. Generic resolution by
type, kind, or rank and user-defined operators still need explicit support.

The CLI recognizes fixed form from the input filename and emits free-form
derivative source. A minimal legacy example and the remaining source-form
limits are in [the source-forms note](docs/design/source-forms.md).

Unsupported constructs return `fad_result_t%ok = .false.` and name the
refused construct in `message`. This release is not a complete Fortran
language implementation. In particular:

- vector reverse mode is absent;
- Taylor transformation refuses arrays, loops, and branches;
- GPU emission is limited to the reduction shape described above;
- assumed-size dummy arrays currently emerge as assumed-shape arrays, so do
  not differentiate a procedure that declares an active dummy with `(*)`;
- registered statement rules may introduce impure calls into generated code.

The [open defects](ROADMAP.md#open-defects) section tracks implementation limits
and measured performance losses.

## Derivative products

Choose a product from the shape of the problem:

| Need | fortad route | Guide |
| --- | --- | --- |
| Gradient of a scalar objective | one VJP | [Gradients](docs/products.md#gradients) |
| Jacobian-vector product | one JVP | [Sensitivity analysis](docs/products.md#sensitivity-analysis) |
| Hessian-vector product | `fad_hvp` | [Hessian-vector products](docs/products.md#hessian-vector-products) |
| Linear covariance propagation | vector JVP seeded by a covariance factor | [Linear uncertainty propagation](docs/products.md#linear-uncertainty-propagation) |
| Matrix-free Gauss-Newton product | JVP followed by VJP | [Gauss-Newton](docs/products.md#gauss-newton) |
| Sparse Jacobian or Hessian | coloring, compressed products, recovery | [Sparse derivatives](docs/products.md#sparse-derivatives) |
| Time-integration adjoint | caller executes a Revolve schedule | [Checkpointing](docs/products.md#checkpointing) |
| Derivatives above second order | `fad_taylor` or `tay_*` arithmetic | [Taylor mode](docs/products.md#taylor-mode) |

The [product guide](docs/products.md) includes layouts, costs, and validity
boundaries. [`test/test_products_oracle.f90`](test/test_products_oracle.f90)
executes its gradient, Jacobian, and linear-UQ constructions and compares them
with independent results.

## Command line

The command reads one Fortran source file. Explicit forms write generated
Fortran to standard output; the source-only compact form writes a predictable
derivative file beside the input. Start with one of these forms:

```console
$ fortad kernel.f90                       # infer inputs; write kernel_jvp.f90
$ fortad kernel.f90 --mode reverse         # same inference; write kernel_vjp.f90
$ fortad jvp kernel.f90                    # infer inputs; write kernel_jvp.f90
$ fortad vjp kernel.f90                    # infer inputs and the sole output
$ fortad hvp kernel.f90                    # infer inputs; write kernel_hvp.f90
$ fortad all kernel.f90                    # write both JVP and VJP siblings
$ fortad check kernel.f90                 # parser/round-trip check
```

The shortest form, `fortad FILE`, infers the first procedure and its
differentiable dummy arguments and writes `<stem>_jvp.f90` beside the input.
Add a comma-separated positional name list only when you want to override the
inferred inputs. Use `fortad all FILE` when both JVP and VJP siblings are
needed; explicit product names and flags remain available for multi-procedure
files, custom interfaces, and registered rules.

The bare source-first form also accepts `--mode forward|reverse|hessian` and
`--indep NAMES`, so a reverse product with inferred output selection can be
written as `fortad kernel.f90 --mode reverse`. The explicit `jvp`, `vjp`, and
`hvp` forms remain strict compact spellings: their product name already fixes
the mode.

For Tapenade-style scripts, `--head` (also `-head`) combines the procedure
and active arguments: `fortad vjp -head 'kernel(x)' source.f90`. Commas are
accepted too. The procedure name is equivalent to `--proc`; the names inside
the parentheses are equivalent to `--indep`, while omitted names still use
FortAD's inference.

With source-first compact syntax, omitted names are inferred from the selected
procedure's dummy arguments: explicit `intent(out)` dummies are outputs and
the other dummies are treated as differentiable inputs. The first procedure is
the default; `--proc NAME` selects another one. FortAD also chooses the
`<procedure>_<product>` symbol, a checked wrapper module, and a sibling output
file. Add `--verbose` to print those decisions. `--output PATH`, `--module
NAME`, `--name NAME`, `--dep NAME`, and explicit comma-separated names override
the defaults. The full option and custom-rule reference is in [the CLI API
section](docs/design/public-api.md#cli). The flag form and names-first compact
form remain available for scripts and advanced rules:

```text
fortad FILE [NAMES] [OPTIONS]
fortad jvp|vjp|hvp FILE [NAMES] [OPTIONS]
fortad jvp|vjp|hvp NAMES [OPTIONS] FILE
fortad all FILE [NAMES]
fortad check [--proc NAME] [--output PATH] FILE
fortad --indep NAMES [OPTIONS] FILE
fortad [PRODUCT] --head 'NAME(arg1 arg2)' [OPTIONS] FILE
```

`fortad all FILE` is the short path when a workflow needs both forward and
reverse products. It infers the same procedure and active dummies, writes
`<stem>_jvp.f90` and `<stem>_vjp.f90` beside the input, and gives each a
distinct checked wrapper module. Use `jvp` or `vjp` separately when custom
names, modules, or output paths are needed.

`check` parses and re-emits the selected procedure without differentiating it.
A successful check establishes round-trip support for that procedure. It does
not establish whole-file compiler conformance or derivative support.

## Fortran API

```fortran
use fortad, only: fad_result_t, fad_vjp

type(fad_result_t) :: result

result = fad_vjp(source, ["x", "y"], module_name="my_adjoint")
if (.not. result%ok) error stop result%message
```

`fad_jvp`, `fad_vjp`, `fad_hvp`, `fad_taylor`, and `fad_roundtrip` accept
Fortran source and return generated source in `fad_result_t%code`. The
[public API reference](docs/design/public-api.md) lists every exported symbol,
call signature, optional argument, and default.

## Performance evidence

Measurements live in
[`fortad-bench`](https://github.com/lazy-fortran/fortad-bench), together with
the compiler, hardware, flags, raw output, and independent correctness gate.
On its recorded `dot_sin` workload, fortad is about 8% faster than Enzyme for
one forward direction, about ten times faster per direction at sixteen forward
directions, 1.7 to 1.8 times faster for the fused reverse reduction, and 2.1 to
2.2 times faster for the element-write stencil. Enzyme remains about 3.5%
faster on the taped nonlinear recurrence. These results describe those
workloads and toolchains only.

The original VMEC++ gate rejected a universal performance claim. Its result and
the revised measurement policy are recorded in
[the P0.8 decision](docs/design/go-no-go.md). A competing engine remains the
preferred implementation for any workload where its complete measured result
wins.

## Repository map

- [Documentation index](docs/README.md)
- [Runnable Rosenbrock example](example/README.md)
- [Derivative product guide](docs/products.md)
- [Public API](docs/design/public-api.md)
- [Roadmap and open defects](ROADMAP.md)
- [Algorithm provenance](PROVENANCE.md)
- [Legal rules for the external study corpus](LEGAL.md)
- [Historical research dossier](docs/dossier.md)

`fortfront` supplies parsing and semantic analysis. `fortad` lowers the result
to its own IR, differentiates and optimizes that IR, and uses `fortgen` for
source-generation conventions. The output is ordinary Fortran source.

## License

fortad is MIT licensed. Derivative source generated from a user's program is
the derivative of that program. fortad claims no copyright in generated output
and adds no license condition to it.
