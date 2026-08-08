# Active fixed-arm `CLASS IS` dispatch

FortAD differentiates a borrowed `class(base_t)` receiver when a source-level
`SELECT TYPE` has exactly one `CLASS IS` arm for a concrete leaf. The leaf may
inherit its implementation from an abstract intermediate type. The dynamic
type remains passive: FortAD pairs the selected receiver with caller-owned
tangent and cotangent shadows, but does not differentiate a descriptor,
dispatch table, or ownership transition.

The bounded slice accepts one fixed `CLASS IS` path and ordinary active REAL
components. It refuses unresolved or multiple dispatch, `ASSOCIATE` aliases,
pointers and `TARGET` storage, global mutable state, allocatable ownership
changes, and dynamic type perturbations. These are semantic refusals, not
compiler failures. The independent analytic, finite-difference, and adjoint
identity oracle is
[`test_polymorphic_class_is_oracle.f90`](../../test/test_polymorphic_class_is_oracle.f90).
