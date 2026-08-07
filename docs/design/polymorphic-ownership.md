# Polymorphic ownership

The current forward boundary distinguishes passive polymorphic dispatch from
active polymorphic ownership. A dummy such as
`class(base_t), allocatable, intent(in)` may keep one fixed dynamic child while
the derivative varies an independent scalar. FortAD copies the allocatable
interface, preserves the `select type` path, and leaves the type tag passive.
The independent
[`test_polymorphic_ownership_oracle.f90`](../../test/test_polymorphic_ownership_oracle.f90)
checks the hand derivative and a central finite difference for that path.

An active component of a polymorphic allocatable is currently a named forward
refusal. FortFront's `query_storage` facts (`is_polymorphic`,
`is_unlimited_polymorphic`, and allocation classification) are copied into the
FortAD declaration IR. After activity analysis, FortAD refuses only an active
polymorphic allocatable and reports its declared `class(T)` or `class(*)` form
and source line. This is semantic metadata, not a source-text search.

The reason is an IR boundary, not a limitation of `select type` itself. An
active owner needs a tangent allocation descriptor whose dynamic type follows
the primal. The current `FAD_SELECT_TYPE` statement has one selector and the
allocation events have no dynamic-type identity or paired ownership state. A
blindly emitted `class(base_t)` tangent could be unallocated, select a
different child, or fail to expose the active component inside the guard. The
refusal therefore protects both derivative correctness and allocation safety.

This slice does not alter reverse allocation handling. It also does not weaken
the existing refusals for global mutable state, pointer/target aliases,
implicit reallocation, or allocatable components.

The next implementation boundary is a paired dynamic-type/ownership record:
it must carry fixed-path type identity through `allocate(source=...)`,
`select type`, assignment, destruction, and the active component shadow before
polymorphic ownership can be accepted.
