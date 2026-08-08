# Derived-component derivatives

FortAD now has a bounded P7.1 slice for concrete derived values.  Name a real
component in the independent list and the generated routine receives a shadow
object with the same declared type:

```fortran
jvp = fad_jvp(source, [character(len=32) :: "state%inner%q", "a"], &
    from="top")
```

For a source expression such as `state%inner%q*a`, the forward routine reads
`state_d%inner%q`. Reverse mode writes `state_b%inner%q`. The same shadow rule
applies when a same-file concrete type-bound method reads a receiver component:
the receiver is passed as an ordinary `type(t)` shadow object, and its numeric
component sensitivities are propagated. The shadow preserves
the primal type and layout, so inherited scalar fields, nested fields, and
array components use ordinary Fortran component syntax.  The independent
name may include an array element, for example `state%values(1)`.

The same bounded path also lowers a same-file concrete type-bound subroutine
with PASS or NOPASS.  A direct one-dimensional concrete allocatable array may
now provide a literal-indexed receiver element; its allocation is caller-owned
and remains passive.  Whole allocatable receivers, dynamic indices, sections,
pointers/targets, polymorphic receivers, and unresolved or generic/deferred
bindings remain named refusal cases.

## Component dependents in reverse mode

A bounded reverse slice also accepts one direct concrete `REAL` component write
as the dependent, including an array component of a concrete derived array:

```fortran
vjp = fad_vjp(source, [character(len=32) :: "soldat(2)%b", "soldat(2)%c"], &
    dependent="soldat(1)%a", from="function")
```

The generated routine has a separate, component-shaped incoming cotangent
dummy (for example `fad_dep_soldat_1__a_b`) and a single `soldat_b` shadow for
the requested independent components. The dependent seed is never represented
as a second `soldat_b` dummy and the whole derived object is never selected as
active. The contract is deliberately narrow: the component must be concrete
and intrinsic `REAL`, non-allocatable, non-pointer, non-`TARGET`,
non-polymorphic, non-global, and written exactly once. The independent
[`test_component_dependent_vjp_oracle.f90`](../../test/test_component_dependent_vjp_oracle.f90)
compiles and runs both explicit API and automatic CLI VJPs, checks the hand
derivative and adjoint identity, and verifies whole-object refusal.

The executable contract is
[`test_derived_component_oracle.f90`](../../test/test_derived_component_oracle.f90).
It checks generated JVP and VJP code against a hand derivative, central finite
differences, and the adjoint identity. Its value type includes an inherited
real field, a nested real field, a real array, and an integer tag. No derivative
is generated for the integer component.

The active independent must name a component rather than the whole derived
object. Allocation and
deallocation, pointer aliases, polymorphic ownership, procedure components,
and active character/logical components remain explicit P7.2--P8 boundaries.
Runtime dispatch is covered separately by the
[`select type` oracle](../../test/test_runtime_select_type_oracle.f90).
