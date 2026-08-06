# fortad

`fortad` 0.1.0 transforms a supported subset of Fortran into standard Fortran
procedures for automatic differentiation. The generated source has no runtime
tape library or compiler plug-in dependency. The compiler matrix currently
checks gfortran, ifx, flang-new, nvfortran, and LFortran.

```console
$ fo exec fortad --mode reverse --indep x --module rosenbrock_ad \
    --output build/rosenbrock_ad.f90 example/rosenbrock.f90
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
| GPU directives | one-level fused positive reduction adjoints through OpenMP target and OpenACC |

Forward vector mode places the direction index first. For an array `x(n)`, a
seed block has shape `(n_directions, n)`. Reverse vector mode is not
implemented. Sparse Hessians therefore run one scalar HVP for each color.

Same-file callees can be inlined before differentiation. A call whose body is
unavailable needs a registered derivative rule. The built-in structured rules
cover an opt-in `dgesv` path. The same interface supports caller-supplied rules
for nonlinear roots and fixed points, as well as selected numerical-library
operations.

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

The command reads one Fortran source file and writes generated Fortran to
standard output or `--output`:

```text
fortad --indep NAMES [OPTIONS] FILE

--mode forward|reverse|hessian   derivative mode; forward is the default
--indep a,b                     independent variable names
--dep y                         dependent for reverse mode
--directions n_dir              direction-count argument for vector forward mode
--name procedure_name           generated procedure name
--module module_name            wrap the procedure in a module
--proc source_procedure         target inside a multi-procedure source
--no-primal                     omit outputs used only by the primal result
--roundtrip                     parse and re-emit without differentiation
--rule SPEC                     register scalar partial expressions
--call-rule SPEC                register tangent and adjoint statements
--output PATH                   write to PATH instead of standard output
--version                       print the version
--help                          print command help
```

Use separate option values, as in `--mode reverse`. The
[public API reference](docs/design/public-api.md#cli) gives the two rule formats
and option scope.

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
