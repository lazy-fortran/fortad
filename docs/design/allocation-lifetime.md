# Allocation lifetime

FortAD now has a bounded forward ownership slice for modern local and dummy
allocatable arrays. The IR records deferred-shape ownership separately from
ordinary arrays and preserves explicit `allocate`, `deallocate`, and
`move_alloc` transitions. JVP generation creates an allocatable tangent and
mirrors those transitions, including `source=`/`mold=` metadata where the
front end exposes it. Dead-code elimination preserves owned storage and its
lifetime statements.

The slice deliberately refuses active module-owned allocatable state. Global
mutable ownership would require a shared lifetime and alias model, so it is a
product boundary rather than an implicit side effect. It also refuses whole
array assignment that may trigger automatic reallocation, allocation status
side channels (`stat=`/`errmsg=`), multiple allocation objects, polymorphic
type-spec allocation, pointer/target aliasing, and allocatable components.

Reverse mode currently reports:

```text
reverse mode: explicit allocation lifetime requires an allocation-state replay tape; use forward mode for this bounded ownership slice
```

That refusal is intentional: the reverse sweep must retain or replay the
descriptor, shape, ownership transfer, and live values across deallocation;
emitting a derivative that reads released storage would be unsound. The next
P7.2 slice is an allocation-state tape and reverse ownership replay.

The executable independent oracle is
[`test_allocation_lifetime_oracle.f90`](../../test/test_allocation_lifetime_oracle.f90).
It compiles and runs a primal using local allocatable arrays, explicit
allocation, `source=`/`mold=`, a `move_alloc` transfer, and deallocation; then
compiles the generated JVP and checks it against a central finite difference.
It also checks the named reverse-mode refusal and the global-state boundary.
