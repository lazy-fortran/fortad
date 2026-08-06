# Abstract and deferred bindings

FortAD has a bounded positive slice for a statically known concrete child. The
child may override a deferred binding through one intermediate level:

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
Active fields of the receiver are still outside this slice.

The compiled oracle
[`test_abstract_hierarchy_oracle.f90`](../../test/test_abstract_hierarchy_oracle.f90)
checks both levels with hand values, central finite differences, and the
JVP/VJP adjoint identity. It also checks named refusals for a direct
`class(base_t)` dispatch, an inherited-only binding, and an unresolved
deferred binding.

This is fixed dispatch, not a derivative hierarchy. The implementation does
not yet preserve a runtime type tag through a type-bound call, emit derivative
bindings for every child, differentiate polymorphic ownership, or define a
rule when a perturbation changes the selected child. Those remain P8.4--P8.7
work. Use `select type` for the currently supported fixed-trace runtime
dispatch shape.
