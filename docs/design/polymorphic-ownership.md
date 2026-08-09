# Polymorphic ownership

The current forward boundary distinguishes passive polymorphic dispatch from
bounded fixed-source ownership and general active polymorphic ownership. A
dummy such as
`class(base_t), allocatable, intent(in)` may keep one fixed dynamic child while
the derivative varies an independent scalar. FortAD copies the allocatable
interface, preserves the `select type` path, and leaves the type tag passive.
The independent
[`test_polymorphic_ownership_oracle.f90`](../../test/test_polymorphic_ownership_oracle.f90)
checks the hand derivative and a central finite difference for that path.

The bounded active forward case is also supported when a local
`class(base_t), allocatable` or `class(*), allocatable` owner is acquired
exactly once with `allocate(owner, source=child)`, where `child` is a concrete,
declared `type(child_t)` object. The existing allocation IR carries the source
expression, and activity lowering emits the paired
`allocate(owner_d, source=child_d)` before the copied `select type` path.

The new reverse slice is one level more specific: a scalar polymorphic
allocatable component such as `box%field%payload` may be acquired once from
that same concrete `child`, selected in exactly one concrete `TYPE IS` arm, and
finally deallocated. FortAD creates the matching concrete shadow component
`box_b%field%payload`, initializes its real payload under `SELECT TYPE`,
accumulates the selected receiver cotangent there, transfers it back to
`child_b`, and destroys both descriptors after the reverse sweep. The
independent oracle covers the hand derivative, central finite differences,
VJP values, and the JVP/VJP adjoint identity; `x_b` remains caller-owned
derivative storage.

The same reverse replay contract now covers a scalar local or dummy
`class(*), allocatable` owner acquired once with `SOURCE=child`, where
`child` is a declared concrete object. The reverse emitter creates a concrete
adjoint shadow with the source type, replays the one matching `SELECT TYPE`
arm, and deallocates the shadow after propagation. The independent ownership
oracle checks the hand derivative, central finite difference, and adjoint
identity for `allocate_star_evaluate`.

Inside that proven single `TYPE IS` or `CLASS IS` arm, a scalar concrete
component may also receive one direct assignment such as
`owner%scale = 3.0d0*x`. JVP and VJP route the assignment through the paired
concrete shadow and clear the overwritten component before replaying the
`SOURCE=` copy, so the old payload is not differentiated through the store.
Read-modify-write forms such as `owner%scale = owner%scale + x` remain a
precise refusal because this bounded slice has no old-value snapshot. The
generated-source, numerical, and refusal coverage is in
[`test_polymorphic_nested_ownership_oracle.f90`](../../test/test_polymorphic_nested_ownership_oracle.f90).

Reverse replay now extends one step to a fixed-shape one-dimensional holder
array. A path such as `holders(2)%field%payload` is supported when the holder
array is a concrete, non-allocatable object, the subscript is one literal
integer, the component is acquired exactly once with `SOURCE=child`, exactly
one concrete `TYPE IS` or `CLASS IS` arm consumes it, and one matching final
`DEALLOCATE` follows. FortAD declares a paired holder-array shadow, allocates
only the selected component in that shadow, opens a matching selected
cotangent, and destroys both the shadow component and primal descriptor after
reverse propagation. The independent
[`test_polymorphic_array_ownership_oracle.f90`](../../test/test_polymorphic_array_ownership_oracle.f90)
checks JVP and VJP hand values, a central finite difference, and the adjoint
identity; the nested ownership oracle also exercises the nested-array VJP.

The reverse emitter also supports a store to the concrete component selected
from one literal element of a one-dimensional allocatable polymorphic owner
array:

```fortran
allocate(owners(2), source=child)
select type (item => owners(2))
type is (child_t)
    item%scale = 3.0d0*x
    y = item%scale*x
end select
```

The owner array and its dynamic type remain passive. FortAD pairs the selected
element with `owners_b(2)`, snapshots its incoming component cotangent before
propagating the store RHS, and then replays the concrete `SOURCE=` component
back to the active source. This prevents a selected element read after the
store from being counted again as part of the store seed. The independent
hand/finite-difference/adjoint oracle is
[`test_polymorphic_owner_array_component_oracle.f90`](../../test/test_polymorphic_owner_array_component_oracle.f90).

This is one selected element, not an array lifetime tape. Computed or dynamic
indices, sections, vector subscripts, rank-two or higher holder paths,
assumed-shape or allocatable holder arrays, pointer/TARGET or other aliases,
factories, polymorphic sources, repeated or path-dependent lifetime changes,
`MOLD=`, polymorphic-component `MOVE_ALLOC`, finalization replay, global mutable state, and ambiguous
or runtime-changing dispatch remain precise reverse refusals. Forward support
for broader array paths is unchanged; this slice does not imply reverse
support for them.

An active component of a polymorphic allocatable outside that fixed-source
shape remains a named forward and reverse refusal. FortFront's `query_storage` facts (`is_polymorphic`,
`is_unlimited_polymorphic`, and allocation classification) are copied into the
FortAD declaration IR. After activity analysis, FortAD refuses only an active
polymorphic allocatable and reports its declared `class(T)` or `class(*)` form
and source line. This is semantic metadata, not a source-text search.

The reason is an IR boundary, not a limitation of `select type` itself. The
fixed concrete source is enough to synchronize this one forward descriptor,
but a factory result, polymorphic source, multiple acquisition, implicit
reallocation, `move_alloc`, or alias needs a paired dynamic-type/ownership
record. The current `FAD_SELECT_TYPE` statement has one selector and the
allocation events still have no general dynamic-type identity or paired
ownership state. A blindly emitted `class(base_t)` tangent could be
unallocated, select a different child, or fail to expose the active component
inside the guard. The refusal therefore protects both derivative correctness
and allocation safety.

The bounded reverse slice deliberately refuses component array sections and
all array-element paths outside the one fixed-shape, literal-indexed holder
case above. It also refuses factories and polymorphic sources (the `class(*)`
case still requires a concrete source), `MOLD=`, repeated or path-dependent
acquisition, aliases, polymorphic-component `move_alloc`, whole-object assignment, finalization
replay, and unresolved or multi-arm dispatch. It also does not
weaken the existing refusals for descriptor changes, reallocation, global
mutable state, pointer/target aliases, or unsupported allocatable components.
Negative cases verify that these boundaries return no derivative output.

The next implementation boundary is a paired dynamic-type/ownership record:
it must carry type identity through polymorphic factories, repeated
`allocate(source=...)`, `select type`, assignment, destruction, and the active
component shadow beyond this one fixed concrete source copy.
