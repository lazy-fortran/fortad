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

Reverse mode now has a bounded retention slice for one explicit lifetime of a
simple local or allocatable dummy array, including one explicit
`MOVE_ALLOC(source, destination)` transfer. The generated VJP allocates a
same-shaped derivative owner, transfers both primal and derivative
descriptors, retains the destination owners through the reverse sweep, and
performs matching explicit `DEALLOCATE` operations only after all reverse
reads. This is a replay boundary represented entirely by the existing IR
lifetime events; it does not pretend to handle arbitrary allocation state.

The supported reverse shape is one straight-line `ALLOCATE` (with passive
shape metadata or `MOLD=`), ordinary active element updates and reads, one
`MOVE_ALLOC` from that owner to a distinct simple local or dummy allocatable,
and one matching final `DEALLOCATE` of the destination. An active `SOURCE=`
value copy is refused until it has its own replay rule. The independent oracle
checks the hand-derived, central finite-difference, and adjoint-identity
gradients for local and dummy owners, and compiles the generated VJP.

Reverse mode still reports a precise refusal for unsupported lifetime forms:

```text
reverse mode: move_alloc requires one straight-line allocation owner and one matching final deallocation
```

Repeated or path-dependent lifetimes, automatic reallocation, allocatable
components, polymorphic ownership, pointer/target aliases, and active
module-owned state remain refused. A `MOVE_ALLOC` outside the one-owner,
straight-line, matching-final-deallocation shape is also refused. Those cases
require storage identity, dynamic ownership, or a per-path allocation-state
tape; emitting a derivative that reads released or aliased storage would be
unsound.

The executable independent oracle is
[`test_allocation_lifetime_oracle.f90`](../../test/test_allocation_lifetime_oracle.f90).
It compiles and runs primals using local and dummy allocatable arrays, explicit
allocation, `source=`/`mold=`, a `move_alloc` transfer, and deallocation; then
compiles the generated JVP and VJPs. It checks hand and central finite
difference values, the adjoint identity, the retained reverse VJP, the named
multi-owner refusal, and the global-state boundary.
