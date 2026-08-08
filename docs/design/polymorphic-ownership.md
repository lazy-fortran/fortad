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
`allocate(owner_d, source=child_d)` before the copied `select type` path. The
`allocate(owner_d, source=child_d)` before the copied `select type` path. The
bounded reverse case narrows this to scalar `class(base_t)` ownership:
lowering materializes a concrete source-typed owner shadow, transposes the one
value-copy component into the concrete source shadow, and then performs the
matching cleanup. The independent oracle covers both forward declared
polymorphic forms, plus the reverse hand values, central finite differences, and
JVP/VJP adjoint identity for `class(base_t)`.

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

The bounded reverse slice deliberately refuses factories and polymorphic
sources, repeated or path-dependent acquisition, aliases, `move_alloc`, whole
object assignment, finalization replay, and `class(*)` sources. It also does not
weaken the existing refusals for global mutable state, pointer/target aliases,
implicit reallocation, or allocatable components. Negative cases verify that
these boundaries return no derivative output.

The next implementation boundary is a paired dynamic-type/ownership record:
it must carry type identity through polymorphic factories, repeated
`allocate(source=...)`, `select type`, assignment, destruction, and the active
component shadow beyond this one fixed concrete source copy.
