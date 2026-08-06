# Abstract and deferred bindings

FortAD supports a statically declared concrete child with at most one
intermediate inheritance level. The child may override a deferred binding:

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
    type(leaf_t), intent(in) :: model
    real(8), intent(in) :: x
    y = model%value(x)
end function evaluate
```

The lowerer resolves `leaf_t%value` to the local `leaf_value` override, then
inlines that body before generating both JVP and VJP code. A `type(mid_t)`
receiver follows the corresponding `mid_value` path. The receiver is passive.
Receiver components are not differentiated in this case.

The compiled oracle
[`test_abstract_hierarchy_oracle.f90`](../../test/test_abstract_hierarchy_oracle.f90)
checks both levels with hand values, central finite differences, and the
JVP/VJP adjoint identity. It also checks named refusals for a direct
`class(base_t)` dispatch, an inherited-only binding, and an unresolved
deferred binding.

This case resolves `type(mid_t)` and `type(leaf_t)` bindings at generation time.
It does not preserve a runtime type tag for polymorphic type-bound calls. Use
the existing `select type` path for runtime dispatch. Derivative bindings for
every child, polymorphic ownership, and a rule for perturbations that change
the selected child remain P8.4--P8.7 work.
