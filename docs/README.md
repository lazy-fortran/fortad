# Documentation

Start with the [Rosenbrock example](../example/README.md) for a complete CLI
run. Use the [product guide](products.md) to choose a derivative object, then
consult the [public API reference](design/public-api.md) for exact calls and
defaults.

## User guides

| Document | Contents |
| --- | --- |
| [Main README](../README.md) | build, current support, limits, CLI summary, and repository map |
| [Product scope](design/product-scope.md) | modern Fortran target, abstraction model, and deliberate refusal boundaries |
| [Rosenbrock example](../example/README.md) | generate, compile, and check a reverse-mode derivative |
| [Derivative products](products.md) | first-order products, HVPs, UQ, sparse recovery, checkpointing, and Taylor mode |
| [Public API](design/public-api.md) | exported Fortran calls and types, with defaults and CLI syntax |
| [Procedure interfaces](design/procedure-interfaces.md) | optional dummies, `present`, elemental procedures, and generated call shape |
| [Source forms](design/source-forms.md) | fixed-form CLI path and open preprocessing boundaries |
| [Derived components](design/derived-components.md) | bounded scalar, nested, inherited, and array component derivatives |
| [Abstract hierarchy](design/abstract-hierarchy.md) | fixed-dispatch overrides across an abstract/deferred hierarchy |
| [Complex values](design/complex-values.md) | real-coordinate JVP contract and bounded real-objective VJP |
| [Allocation lifetime](design/allocation-lifetime.md) | bounded forward ownership slice and reverse replay boundary |
| [Aliasing and sections](design/aliasing.md) | explicit pointer, target, and noncontiguous-section refusal boundary |

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
| [Complex values](design/complex-values.md) | complex intrinsic JVPs, bounded projection VJP, and explicit reverse refusal | [`test_complex_intrinsic_oracle.f90`](../test/test_complex_intrinsic_oracle.f90), [`test_complex_reverse_oracle.f90`](../test/test_complex_reverse_oracle.f90) |
| [Allocation lifetime](design/allocation-lifetime.md) | local/dummy allocatable JVP ownership slice and named reverse refusal | [`test_allocation_lifetime_oracle.f90`](../test/test_allocation_lifetime_oracle.f90) |
| [Polymorphic ownership](design/polymorphic-ownership.md) | passive dispatch plus bounded active `allocate(source=concrete)` ownership for `class(base_t)`/`class(*)`, with semantic refusal outside the fixed path | [`test_polymorphic_ownership_oracle.f90`](../test/test_polymorphic_ownership_oracle.f90) |
| [Abstract hierarchy](design/abstract-hierarchy.md) | fixed concrete overrides plus bounded `select type` dispatch through deferred bindings | [`test_abstract_hierarchy_oracle.f90`](../test/test_abstract_hierarchy_oracle.f90), [`test_runtime_select_type_oracle.f90`](../test/test_runtime_select_type_oracle.f90) |
| [GPU emission](design/gpu.md) | restricted OpenMP target and OpenACC loop shape | external `fortad-bench` device record |
| [Procedure interfaces](design/procedure-interfaces.md) | elemental JVP/VJP preservation and array calls | [`test_elemental_interface_oracle.f90`](../test/test_elemental_interface_oracle.f90) |
| [Source forms](design/source-forms.md) | fixed-form file normalization and legacy procedure purity | [`test_tapenade_fixed_form_oracle.f90`](../test/test_tapenade_fixed_form_oracle.f90) |

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
