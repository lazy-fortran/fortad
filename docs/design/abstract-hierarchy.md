# Abstract and deferred bindings

FortAD supports a statically declared concrete child with at most one
intermediate inheritance level. The child may override a deferred binding.
It also supports a bounded runtime dispatch when the caller makes each
concrete child explicit in a `select type` arm:

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
receiver follows the corresponding `mid_value` path. Inside a `select type`
arm, its associate name temporarily receives the arm's concrete static type,
so a deferred call such as `model%value(x)` follows the matching child in
both JVP and VJP generation. The selector is passive. Receiver components are
not differentiated in this case.

The compiled oracle
[`test_abstract_hierarchy_oracle.f90`](../../test/test_abstract_hierarchy_oracle.f90)
checks both levels with hand values, central finite differences, and the
JVP/VJP adjoint identity. It also checks named refusals for a direct
`class(base_t)` dispatch, an inherited-only binding, and an unresolved
deferred binding. The runtime oracle
[`test_runtime_select_type_oracle.f90`](../../test/test_runtime_select_type_oracle.f90)
checks an abstract base with deferred `value`, linear/quadratic/cubic child
bindings, a class-default arm, hand derivatives, finite differences, and the
JVP/VJP adjoint identity.

The runtime contract is deliberately narrow: the selector must be a simple
`class(base_t)` dummy, every differentiated arm must name a concrete child
whose binding implementation is in the same source, and the dynamic type is
fixed for one call. Direct `class(base_t)%value` dispatch without a visible
`select type` remains a named refusal. Active derived-object components,
polymorphic allocation/ownership, callback context, and perturbations that
change the selected child remain future work.
