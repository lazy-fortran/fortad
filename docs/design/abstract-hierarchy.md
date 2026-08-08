# Abstract and deferred bindings

FortAD supports a statically declared concrete type with local or inherited
bindings resolved through its `EXTENDS` parent chain. A concrete child may
also override a deferred binding.
It also supports a bounded runtime dispatch when a `class(base_t)` receiver
calls a deferred binding directly, without a source-visible `SELECT TYPE`:

```fortran
type, abstract :: base_t
contains
    procedure(value_iface), deferred :: value
end type base_t

type, extends(base_t) :: mid_t
contains
    procedure :: value => mid_value
end type mid_t

type, extends(mid_t) :: leaf_t
contains
    procedure :: value => leaf_value
end type leaf_t

pure function evaluate(model, x) result(y)
    class(base_t), intent(in) :: model
    real(8), intent(in) :: x
    y = model%value(x)
end function evaluate
```

The lowerer resolves `leaf_t%value` to the local `leaf_value` override, and
resolves a child with no local binding to the effective implementation found
in its parent chain. It then inlines that body before generating both JVP and
VJP code. A `type(mid_t)` receiver follows the corresponding `mid_value` path.
Inside a `select type` arm, its associate name temporarily receives the arm's
concrete static type, so the same call follows the matching child in both JVP
and VJP generation. For a direct polymorphic call, FortFront's concrete
dispatch-target facts are materialized as structural IR arms before inlining.
The selector is passive. Receiver components are not differentiated in this
case.

The compiled oracle
[`test_abstract_hierarchy_oracle.f90`](../../test/test_abstract_hierarchy_oracle.f90)
checks both levels with hand values, central finite differences, and the
JVP/VJP adjoint identity. The type-bound oracle
[`test_type_bound_oracle.f90`](../../test/test_type_bound_oracle.f90) adds
hand, finite-difference, and adjoint checks for a statically resolved
inherited binding. The abstract oracle checks an unresolved deferred binding.
The direct runtime oracle
[`test_direct_polymorphic_oracle.f90`](../../test/test_direct_polymorphic_oracle.f90)
checks two same-file child implementations through direct function and
subroutine calls, hand derivatives, finite differences, and the JVP/VJP
adjoint identity. The runtime oracle
[`test_runtime_select_type_oracle.f90`](../../test/test_runtime_select_type_oracle.f90)
checks an abstract base with deferred `value`, linear/quadratic/cubic child
bindings, a class-default arm, hand derivatives, finite differences, and the
JVP/VJP adjoint identity.

The direct runtime contract is deliberately narrow: the selector must be a
simple nonallocatable `class(base_t)` dummy; FortFront must report a nonempty,
known set of concrete same-file target types and implementations; every target
must be a non-generic, non-ambiguous, non-deferred function or subroutine with
compatible PASS metadata; and the dynamic type is fixed for one call. Unknown
or empty target sets, generic/ambiguous/deferred targets, unsupported PASS-dummy
compatibility, allocation or ownership changes, and perturbations that change
the selected child remain named refusals. Receiver components remain passive.
