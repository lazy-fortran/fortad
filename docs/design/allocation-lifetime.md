# Allocation lifetime

FortAD now has a bounded forward ownership slice for modern local and dummy
allocatable arrays. The IR records deferred-shape ownership separately from
ordinary arrays and preserves explicit `allocate`, `deallocate`, and
`move_alloc` transitions. JVP generation creates an allocatable tangent and
mirrors those transitions, including `source=`/`mold=` metadata where the
front end exposes it. Dead-code elimination preserves owned storage and its
lifetime statements.

The bounded automatic-reallocation slice accepts one whole assignment to a
concrete scalar, rank-one, or rank-two local or dummy allocatable owner. FortFront's
allocatable storage fact is used when available, with the lowered declaration
fact as the parse-only fallback. The generated primal, JVP, and bounded VJP
keep the assignment as ordinary Fortran assignment, so the compiler performs
the descriptor and payload transition. The slice deliberately refuses active
module-owned allocatable state. Global mutable ownership would require a
shared lifetime and alias model, so it is a product boundary rather than an
implicit side effect. It also refuses allocation status side channels
(`stat=`/`errmsg=`), rank greater than two, polymorphic ownership, pointer or
target aliasing, and array-valued allocatable component reads.

The forward/JVP slice also accepts one whole assignment to a concrete scalar
REAL allocatable component of a local or dummy derived owner. The generated
tangent repeats that ordinary assignment, so the Fortran compiler performs the
component descriptor transition for both owners. Reverse mode deliberately
refuses this case until component lifetime replay can retain and restore the
component descriptor safely:

```text
reverse mode: whole allocatable component automatic reallocation requires component lifetime replay
```

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

Reverse mode accepts one straight-line automatic-reallocation assignment when
the procedure has no other explicit allocation lifetime operation. It reports
precise refusals for unsupported lifetime forms:

```text
reverse mode: move_alloc requires one straight-line allocation owner and one matching final deallocation
```

Repeated or path-dependent automatic reallocation, mixing automatic and
explicit lifetime operations, array-valued component assignment/read,
allocatable component explicit lifetime operations, polymorphic ownership,
pointer/target aliases, and active module-owned state remain refused. A
`MOVE_ALLOC` outside the one-owner, straight-line, matching-final-deallocation
shape is also refused. Those cases require storage identity, dynamic ownership,
or a per-path allocation-state tape; emitting a derivative that reads released
or aliased storage would be unsound.

The executable independent oracle is
[`test_allocation_lifetime_oracle.f90`](../../test/test_allocation_lifetime_oracle.f90).
It compiles and runs primals using local and dummy allocatable arrays, explicit
allocation, `source=`/`mold=`, a `move_alloc` transfer, and deallocation; then
compiles the generated JVP and VJPs. It checks hand and central finite
difference values, the adjoint identity, the retained reverse VJP, the named
multi-owner refusal, and the global-state boundary.

The automatic-reallocation oracle is
[`test_auto_realloc_assignment_oracle.f90`](../../test/test_auto_realloc_assignment_oracle.f90).
It compiles the primal and generated derivatives with gfortran, checks scalar,
rank-one, and rank-two hand derivatives against central differences, checks
scalar and rank-two JVP/VJP adjoint identities, and verifies repeated-lifetime
and rank-three boundary refusals.

The scalar allocatable-component reallocation oracle is
[`test_allocatable_component_reallocation_oracle.f90`](../../test/test_allocatable_component_reallocation_oracle.f90).
It compiles and runs the primal and generated JVP, checks the hand derivative,
and verifies the reverse lifetime-replay refusal.
