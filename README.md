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
| Derived components | bounded concrete scalar, nested, inherited, array, and fixed-source polymorphic component paths |
| Abstract/deferred hierarchy | fixed-dispatch JVP/VJP for statically known bindings and bounded direct polymorphic dispatch |
| Complex JVP/VJP | real-coordinate `conjg`, `real`, `aimag`, `cmplx`, `abs`, multiplication, and division; bounded real-objective VJP through `real(z)`/`dble(z)` |
| Aliasing and sections | bounded rank-one/rank-two contiguous range sections. Named refusals cover `pointer`, `target`, pointer association, vector subscripts, and noncontiguous or computed sections |
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
General allocatable lifetime changes remain refused with a source-line
diagnostic. One scalar nested polymorphic component may use a concrete local
`SOURCE=` acquisition followed by final deallocation in JVP and VJP. The
same fixed `TYPE IS` or `CLASS IS` path also supports a direct scalar
assignment through paired concrete shadows; read-modify-write and ownership-
changing forms remain refused. The boundary and oracle are in [the
polymorphic ownership design note](docs/design/polymorphic-ownership.md).
Literal indexed scalar allocatable components such as `boxes(2)%value` use the
same bounded reallocation replay contract. Concrete rank-one through rank-four
component lifetimes with literal extents also replay `ALLOCATE`, `MOVE_ALLOC`,
and `DEALLOCATE` when active accesses remain scalar; dynamic shapes, whole
component array accesses, and unsafe storage remain refused. Direct same-file
subroutine calls use exact FortFront
actual/formal storage facts and refuse aliases, callbacks, globals, pointers,
allocatables, mismatches, and ambiguous mappings.
The same fixed-path contract now covers scalar `class(*)` component assignment
when one concrete `TYPE IS` or `CLASS IS` arm proves the dynamic type.
Reverse mode also replays one scalar local or dummy `class(*)`, allocatable
owner acquired from a declared concrete `SOURCE=` object when exactly one
matching concrete `SELECT TYPE` arm and one final deallocation are present.
Ambiguous dispatch, polymorphic or factory sources, aliases, transfers, and
global mutable owners remain refusals.
The bounded contiguous rank-one/rank-two section support, storage-identity
proof, and executable refusal oracles are in
[the aliasing and sections design note](docs/design/aliasing.md).
The fixed-dispatch abstract/deferred hierarchy slice and its multi-level
oracle are in [the abstract hierarchy design note](docs/design/abstract-hierarchy.md).
Passive fixed-type polymorphic allocatable dispatch, bounded active nested
`allocate(source=concrete)` ownership, and the remaining semantic boundary
for polymorphic ownership are in
[the polymorphic ownership design note](docs/design/polymorphic-ownership.md).

FortAD's product target is modern Fortran with explicit data ownership,
procedure interfaces, derived values, and abstract or polymorphic components.
The target applications are the maintained production code in the
lazy-fortran and itpplasma groups. Active `COMMON`, mutable module or `SAVE`
state, uncontrolled aliasing, active I/O, and opaque calls without derivative
rules are deliberate refusal boundaries. See the [product scope](docs/design/product-scope.md)
for the full contract and priority order.

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
- active global mutable state is outside the product scope;
- allocatable lifetime and arbitrary storage aliasing remain open semantic
  implementation areas;
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

In a multi-procedure source, an explicit derivative output name can select the
procedure without a separate `--proc` flag. For example,
`fortad vjp source.f90 --output selected_vjp.f90` tries `selected` as the
procedure name, infers its inputs and output, and falls back to the first
procedure if that basename is not present. An explicit `--proc` always wins;
the output filename is only a conservative hint, never a guessed procedure.
This inference applies when the independent-name list is omitted; supplying
names explicitly keeps the existing selected-procedure behavior.

Reverse-mode inference also handles a legacy subroutine with no `INTENT`
annotations when it has one unambiguous output: `fortad vjp legacy.f90`
infers the written dummy as the dependent and excludes it from the
independents. Use `--dep NAME` when the procedure has multiple possible
outputs.

When reverse inference is ambiguous, FortAD refuses to guess and reports the
candidate output names in the diagnostic, followed by the exact `--dep NAME`
override to use. No generated file is written in that case.

For Tapenade-style scripts, `--head` (also `-head`) combines the procedure
and active arguments: `fortad vjp -head 'kernel(x)' source.f90`. Commas are
accepted too. The procedure name is equivalent to `--proc`; the names inside
the parentheses are equivalent to `--indep`, while omitted names still use
FortAD's inference.

The common Tapenade command shapes are also accepted directly. `-O` names
the output directory; `-o` is the Tapenade output stem, so FortAD adds
`_p`, `_d`, or `_b` and `.f90`.

| Tapenade command | FortAD command |
| --- | --- |
| `fortad -p -O out -o kernel source.f` | accepted directly; writes `out/kernel_p.f90` |
| `fortad -d -root kernel -O out -o kernel source.f` | accepted directly; writes `out/kernel_d.f90` |
| `fortad -b -root kernel -O out -o kernel source.f` | accepted directly; writes `out/kernel_b.f90` |
| `fortad -d -root kernel -multi -O out -o kernel source.f` | accepted directly; forward vector directions use `nd` |
| `-head 'kernel(x)'` | `--head 'kernel(x)'` or `-head 'kernel(x)'` |

Tapenade's current multidirectional spelling, `-vector`, is accepted as an
alias for FortAD's older `-multi` spelling. It automatically selects forward
vector mode with the `nd` direction-count dummy, so a normal invocation can
omit both `-root` (when `-o STEM` names the procedure) and `--directions`:
`fortad -d -vector -o kernel source.f`.

For a multi-procedure legacy file, `-root` can be omitted when `-o STEM`
matches the desired procedure name: `fortad -b -O out -o kernel source.f` selects
`kernel` automatically. If the stem does not match a procedure, the first
procedure remains the default.

When no `--indep` list is supplied, the CLI chooses concrete `REAL` component
paths actually read from derived-type dummies, such as `state%inner%q` or
`grid%x`. It never perturbs a whole derived object implicitly; whole-object
activity remains an explicit refusal because it can change dynamic type and
storage identity.

For legacy `-b`/`--reverse` subroutines without `INTENT`, a unique direct write
to a concrete, non-aliased `REAL` component such as `soldat(1)%a` is inferred as
the dependent. The generated VJP receives a separate shaped cotangent seed
for that component and one derived shadow for independent component adjoints;
it does not duplicate a shadow dummy or activate the whole object. Ambiguous,
aliased, polymorphic, allocatable, pointer, global, or multiply-written
component dependents remain named refusals.

For the usual single-procedure file, `fortad source.f90` is shorter still: it
infers the active dummies, procedure name, wrapper module, and output path.
Tapenade-style legacy subroutines without `INTENT` are handled by the direct
`-d`/`-b` aliases when FortAD can identify one dummy written before it is read;
ambiguous procedures still need `--head` or `--dep`.

`-ext FILE` is accepted as a migration aid, but it does not import Tapenade's
external intrinsic-summary format. Register the needed operation with
`--rule` or `--call-rule` instead. Tapenade's multidirectional reverse mode is
not silently emulated: `-vector` and `-multi` currently have forward-mode
semantics only.

The common `-context`, `-fixinterface`, and `-standalonediff` switches are
also accepted. They are no-ops because FortAD always lowers with the complete
source context, emits checked module interfaces, and writes standalone
Fortran source. This lets an existing Tapenade `Options` file remain usable
without a flag-filtering wrapper.

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
