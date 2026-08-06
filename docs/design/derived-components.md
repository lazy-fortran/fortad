# Derived-component derivatives

FortAD now has a bounded P7.1 slice for concrete derived values.  Name a real
component in the independent list and the generated routine receives a shadow
object with the same declared type:

```fortran
jvp = fad_jvp(source, [character(len=32) :: "state%inner%q", "a"], &
    from="top")
```

For a source expression such as `state%inner%q*a`, the forward routine reads
`state_d%inner%q`; reverse mode writes `state_b%inner%q`.  The shadow preserves
the primal type and layout, so inherited scalar fields, nested fields, and
array components use ordinary Fortran component syntax.  The independent
name may include an array element, for example `state%values(1)`.

The executable contract is [`test_derived_component_oracle.f90`](../../test/test_derived_component_oracle.f90).
It checks generated JVP and VJP code against a hand derivative, central finite
differences, and the adjoint identity.  Its value type includes an inherited
real field, a nested real field, a real array, and an integer tag; no derivative
is generated for the integer component.

This is intentionally not the complete object model.  The active independent
must name a component rather than the whole derived object.  Allocation and
deallocation, pointer aliases, polymorphic ownership, procedure components,
and active character/logical components remain explicit P7.2--P8 boundaries.
Runtime dispatch is covered separately by the
[`select type` oracle](../../test/test_runtime_select_type_oracle.f90).
