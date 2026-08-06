# Documentation

Start with the [Rosenbrock example](../example/README.md) for a complete CLI
run. Use the [product guide](products.md) to choose a derivative object, then
consult the [public API reference](design/public-api.md) for exact calls and
defaults.

## User guides

| Document | Contents |
| --- | --- |
| [Main README](../README.md) | build, current support, limits, CLI summary, and repository map |
| [Rosenbrock example](../example/README.md) | generate, compile, and check a reverse-mode derivative |
| [Derivative products](products.md) | first-order products, HVPs, UQ, sparse recovery, checkpointing, and Taylor mode |
| [Public API](design/public-api.md) | exported Fortran calls and types, with defaults and CLI syntax |
| [Procedure interfaces](design/procedure-interfaces.md) | optional dummies, `present`, and generated call shape |

The worked gradient, Jacobian, and linear-UQ constructions in the product guide
are executed by
[`test_products_oracle.f90`](../test/test_products_oracle.f90). Other tests
linked below generate derivative source, compile it, run it, and compare it
with an analytic result, finite differences, an adjoint identity, or another
independent oracle.

## Design and validation records

| Record | Scope | Executable evidence |
| --- | --- | --- |
| [IR decision](design/ir.md) | dedicated fortad IR below fortfront's typed AST | [`test_forward_oracle.f90`](../test/test_forward_oracle.f90) and the other generation oracles |
| [Frontend coverage](design/coverage.md) | measured fortfront acceptance of the fortnum corpus | evidence described in the record |
| [P0.8 decision](design/go-no-go.md) | rejected universal performance thesis and revised measurement policy | external `fortad-bench` record |
| [BLAS/LAPACK rules](design/blas-lapack-rules.md) | opt-in `dgesv` tangent and adjoint | [`test_lapack_rule_oracle.f90`](../test/test_lapack_rule_oracle.f90) |
| [Implicit roots](design/implicit-root-rules.md) | caller-supplied products at a converged root | [`test_implicit_root_rule_oracle.f90`](../test/test_implicit_root_rule_oracle.f90) |
| [Fixed points](design/fixed-point-rules.md) | Christianson two-phase rule boundary | [`test_fixed_point_rule_oracle.f90`](../test/test_fixed_point_rule_oracle.f90) |
| [Library rules](design/library-rules.md) | FFT, quadrature, interpolation, and `erf` callbacks | [`test_library_rules_oracle.f90`](../test/test_library_rules_oracle.f90) |
| [GPU emission](design/gpu.md) | restricted OpenMP target and OpenACC loop shape | external `fortad-bench` device record |

## Planning and evidence

- [`ROADMAP.md`](../ROADMAP.md) contains the completed phase checklist, current
  downstream-integration work, and open defects.
- [`PROVENANCE.md`](../PROVENANCE.md) records the publication behind each
  implemented algorithm.
- [`LEGAL.md`](../LEGAL.md) controls how material from the external study corpus
  may be used.
- [`fortad-bench`](https://github.com/lazy-fortran/fortad-bench) owns expensive
  workloads, external tools, raw benchmark output, and compiler-matrix records.

## Research record

The [research dossier](dossier.md) records the hypotheses that preceded the
P0.8 gate. It is historical analysis, not a current capability statement. The
paper notes retain the implementation lessons extracted during Phase 0:

- [Giering and Kaminski 1998](notes/giering-kaminski-1998-recipes.md)
- [Hascoet, Naumann, and Pascual 2005](notes/hascoet-naumann-pascual-2005-tbr.md)
- [Hascoet and Pascual 2013](notes/hascoet-pascual-2013-tapenade.md)
- [Moses and Churavy 2020](notes/moses-churavy-2020-enzyme.md)
