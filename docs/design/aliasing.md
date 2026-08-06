# Aliasing and array sections

FortAD's IR tracks named values and array elements. It does not yet track a
Fortran storage location through `pointer`, `target`, pointer association, or
an overlapping actual argument. Transforming such a procedure would risk
placing a tangent or adjoint on a different object, so the current contract is
to refuse it before derivative code is emitted.

The same boundary covers array sections. A strided section such as
`x(1:size(x):2)` may be noncontiguous, and two sections can overlap without
having the same spelling. FortAD reports that storage identity is not tracked
instead of treating a section as an ordinary element.

```fortran
use fortad, only: fad_jvp, fad_result_t
type(fad_result_t) :: result

result = fad_jvp(source, ["x"])
! result%ok is false for POINTER/TARGET declarations or array sections.
! result%message names the alias or section boundary.
```

The executable boundary is
[`test_alias_boundary_oracle.f90`](../../test/test_alias_boundary_oracle.f90).
It checks both JVP and VJP refusals for `TARGET`, `POINTER`, and a strided
section. Plain element writes remain supported;
[`test_element_target_oracle.f90`](../../test/test_element_target_oracle.f90)
checks those against central differences. Storage-identity analysis for
same-target, different-target, overlapping, and component aliases remains
P7.3 work.
