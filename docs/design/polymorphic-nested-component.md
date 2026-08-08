# Borrowed nested polymorphic component paths

FortAD supports one bounded nested path in which a concrete holder contains a
caller-owned, already allocated polymorphic component:

```fortran
type(holder_t), intent(in) :: box
class(base_t), allocatable :: payload

select type (item => box%payload)
type is (child_t)
    y = item%scale*x + item%bias
class default
    y = x
end select
```

With independents such as `box%payload%scale`, FortAD emits a paired selector
for the holder shadow (`item_d => box_d%payload` in JVP and
`item_b => box_b%payload` in VJP). The dynamic type is passive: the caller
supplies matching allocated concrete primal, tangent, and cotangent payloads.
The derivative differentiates only the selected concrete components and does
not differentiate a type tag or a descriptor.

The same fixed-source scalar assignment slice accepts an unlimited-polymorphic
`class(*)` component. When `SOURCE=` names one concrete active object and one
`TYPE IS` or `CLASS IS` arm is proven, FortAD allocates the paired concrete
shadow and differentiates a direct assignment such as `item%scale = 4*x`.
Multiple concrete arms, `MOLD=` or factory sources, and other unproven dynamic
type or lifetime changes remain refusals.

The slice is intentionally narrow. The owner must be a concrete, non-aliased
holder and the selector must be a scalar component path with exactly one fixed
concrete `TYPE IS` arm. FortAD refuses unresolved or multi-arm dispatch,
`ASSOCIATE` aliases, pointer or `TARGET` storage, array sections or dynamic
indices/bounds, and any allocation, deallocation, `SOURCE=` acquisition, or
`MOVE_ALLOC` lifetime change on the path. A caller must keep the payload
allocated and type-compatible for every primal and derivative call.

The independent analytic, central finite-difference, adjoint-identity, and
boundary oracle is
`test/test_polymorphic_nested_component_oracle.f90`. The unlimited-polymorphic
assignment case is covered independently by
`test/test_polymorphic_unlimited_assignment_oracle.f90`.
