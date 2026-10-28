# fortad

`fortad` is a source-transformation automatic differentiation (AD) tool for Fortran.
It reads Fortran procedures and writes ordinary Fortran source for their
derivatives, with no runtime tape library or compiler plug-in.

Status: version 0.1.0, research code under active development. It handles a
defined subset of Fortran and refuses everything else with a named diagnostic.

## Features

- Forward mode (JVP), reverse mode (VJP) and Hessian-vector products
  (forward-over-reverse), as a command-line tool and as a Fortran library.
- Scalars, arrays, loops, branches, reductions and same-file procedure calls;
  forward vector mode for several directions at once.
- Sparse Jacobians and Hessians by coloring, Taylor-mode higher derivatives for
  straight-line scalar kernels, and Revolve checkpoint schedules for long time
  integrations.
- User-supplied derivative rules for opaque calls, plus an opt-in rule for LAPACK
  `dgesv`.
- Bounded support for derived types, polymorphic dispatch with a statically known
  target, complex values and fixed-form input.
- Command-line options compatible with common Tapenade invocations.

The full capability list and the deliberate refusal boundaries are in
[Supported Fortran and limits](docs/support.md).

## Build

`fpm.toml` uses sibling path dependencies on
[fortfront](https://github.com/lazy-fortran/fortfront) and
[fortgen](https://github.com/lazy-fortran/fortgen). Clone all three repositories
into the same parent directory:

```console
$ mkdir lazy-fortran && cd lazy-fortran
$ git clone https://github.com/lazy-fortran/fortfront.git
$ git clone https://github.com/lazy-fortran/fortgen.git
$ git clone https://github.com/lazy-fortran/fortad.git
$ cd fortad
$ fo check
```

Requirements: a Fortran 2018 compiler, [fpm](https://fpm.fortran-lang.org), and the
[`fo`](https://github.com/lazy-fortran/fo) build driver. `fo check` builds the
library and the command-line tool and runs the test suite; plain `fo` additionally
runs lint and formatting checks.

## Usage

Differentiate the [Rosenbrock example](example/README.md) in reverse mode:

```console
$ fo exec fortad vjp example/rosenbrock.f90 --module rosenbrock_ad \
    --output build/rosenbrock_ad.f90
```

This writes a module containing `rosenbrock_vjp`. The example page shows how to
compile it and check the gradient against the analytic result.

For a file with one procedure, the inputs and outputs are inferred:

```console
$ fortad kernel.f90                 # forward mode, writes kernel_jvp.f90
$ fortad vjp kernel.f90             # reverse mode
$ fortad all kernel.f90             # both
$ fortad check kernel.f90           # parse and re-emit only
```

From Fortran:

```fortran
use fortad, only: fad_result_t, fad_vjp

type(fad_result_t) :: result

result = fad_vjp(source, ["x", "y"], module_name="my_adjoint")
if (.not. result%ok) error stop result%message
```

`fad_jvp`, `fad_vjp`, `fad_hvp`, `fad_taylor` and `fad_roundtrip` take Fortran source
and return the generated source in `result%code`.

## Documentation

- [Documentation index](docs/README.md)
- [Command-line interface](docs/cli.md)
- [Supported Fortran and limits](docs/support.md)
- [Choosing a derivative product](docs/products.md): gradients, Jacobians, HVPs,
  uncertainty propagation, sparse derivatives, checkpointing, Taylor mode
- [Public API reference](docs/design/public-api.md)
- [Performance](docs/performance.md)
- [Roadmap and open defects](ROADMAP.md), [algorithm provenance](PROVENANCE.md)

## Related projects

- [fortfront](https://github.com/lazy-fortran/fortfront): parsing and semantic
  analysis used by fortad.
- [fortgen](https://github.com/lazy-fortran/fortgen): Fortran source-generation
  conventions.
- [fortad-bench](https://github.com/lazy-fortran/fortad-bench): benchmarks and
  comparisons with other AD tools.

## License

MIT. See [LICENSE](LICENSE). fortad claims no copyright in the derivative source it
generates and adds no license condition to it.
