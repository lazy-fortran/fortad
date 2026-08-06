# Allocation lifetime

FortAD currently refuses a procedure that contains allocation-state semantics.
The IR has no ownership bit, shape transition, or live/dead object state, so
emitting a tangent or adjoint would risk reading storage after `deallocate` or
losing the lifetime of an active component.

The boundary is explicit and source-local. `fad_jvp`, `fad_vjp`,
`fad_hvp`, `fad_taylor`, and `fad_roundtrip` report the first construct and
line, for example:

```text
unsupported allocation lifetime construct 'allocatable declaration/component' at line 4
active allocation state is not represented yet
```

The scanner recognises allocatable declarations/components and explicit
`allocate`, `deallocate`, and `move_alloc` statements, including
`allocate(source=...)` and `allocate(mold=...)`. Automatic reallocation and
deep assignment are refused because the same source also carries allocatable
state. The oracle exercises both behaviors. Comments and quoted strings are
ignored. This is a refusal contract, not a derivative approximation.

The executable boundary is
[`test_allocation_lifetime_oracle.f90`](../../test/test_allocation_lifetime_oracle.f90).
It compiles and runs a primal using `mold=`, `source=`, deep assignment,
automatic reallocation, and `move_alloc`, then verifies that both JVP and VJP
refuse before lowering. P7.2 remains open until a shadow-allocation model can
replay the same lifetime in reverse without leaks or dead-object reads.
