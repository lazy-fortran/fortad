# Active inherited deferred binding on a fixed concrete path

FortAD differentiates a borrowed `class(base_t)` receiver after a source-level
`SELECT TYPE` narrows it to exactly one concrete leaf whose binding is inherited
from an abstract intermediate type. The intermediate type provides the concrete
implementation of the base deferred binding; the leaf does not override it.

The selector's dynamic type remains passive. FortAD pairs the concrete branch's
receiver with its caller-owned tangent or cotangent shadow and lowers the
inherited binding through the existing type-bound-call path. Active numeric
components of the fixed leaf are ordinary shadows; the dynamic type descriptor,
dispatch table, and object ownership are never differentiated or replayed.

This slice requires one `TYPE IS` arm on a simple borrowed selector and a
same-file inherited implementation with a fixed concrete path. It refuses
multiple concrete arms, `CLASS DEFAULT`-dependent dispatch, `ASSOCIATE` aliases,
pointers or `TARGET` storage, allocatable ownership changes, unresolved runtime
dispatch, and dynamic type perturbations. The independent analytical,
finite-difference, and JVP/VJP adjoint oracle is
[`test_polymorphic_inherited_binding_oracle.f90`](../../test/test_polymorphic_inherited_binding_oracle.f90).
