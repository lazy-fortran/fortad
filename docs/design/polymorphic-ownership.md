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

The bounded reverse slice deliberately refuses component array elements and
sections, factories and polymorphic sources, `MOLD=`, repeated or
path-dependent acquisition, aliases, `move_alloc`, whole-object assignment,
finalization replay, and unresolved or multi-arm dispatch. It also does not
weaken the existing refusals for descriptor changes, reallocation, global
mutable state, pointer/target aliases, or unsupported allocatable components.
Negative cases verify that these boundaries return no derivative output.

The next implementation boundary is a paired dynamic-type/ownership record:
it must carry type identity through polymorphic factories, repeated
`allocate(source=...)`, `select type`, assignment, destruction, and the active
component shadow beyond this one fixed concrete source copy.
