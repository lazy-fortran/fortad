# Aliasing and array sections

FortAD's IR tracks named values and array elements. It does not yet track a
Fortran storage location through `pointer`, `target`, pointer association, or
an overlapping actual argument. Transforming such a procedure would risk
placing a tangent or adjoint on a different object, so the current contract is
to refuse it before derivative code is emitted.

The same boundary covers array sections. FortAD accepts a bounded rank-one or
rank-two section when FortFront proves a direct declared base, matching rank,
and contiguous storage. Explicit-shape arrays, `CONTIGUOUS` dummies, and
allocatable owners are the supported bases. The section dimensions must be
non-strided ranges. The bounds select storage on the fixed path and are
therefore passive rather than differentiated values.

A strided section such as `x(1:size(x):2)` may be noncontiguous, and two
sections can overlap without having the same spelling. FortAD reports that
storage identity is not tracked instead of treating such a section as an
ordinary element. Vector subscripts such as `x(idx)` where `idx` is an array,
computed or component bases, pointer/target aliases, and rank greater than two
remain explicit refusals. Vector subscripts do not have a range node in the
frontend, so FortAD checks the declared rank of each subscript.

```fortran
use fortad, only: fad_jvp, fad_result_t
type(fad_result_t) :: result

result = fad_jvp(source, ["x"])
! result%ok is false for POINTER/TARGET declarations or unsupported sections.
! A proven contiguous rank-one/rank-two range section is accepted.
! result%message names the alias or section boundary when refused.
```

The executable boundary is
[`test_alias_boundary_oracle.f90`](../../test/test_alias_boundary_oracle.f90).
It checks both JVP and VJP refusals for `TARGET`, `POINTER`, pointer
association, a strided section, and a vector subscript. The positive
rank-one and rank-two section paths are independently checked against hand
derivatives, central differences, and the adjoint identity by
[`test_contiguous_section_oracle.f90`](../../test/test_contiguous_section_oracle.f90)
and
[`test_contiguous_section_rank2_oracle.f90`](../../test/test_contiguous_section_rank2_oracle.f90).
Plain element writes remain supported.
[`test_element_target_oracle.f90`](../../test/test_element_target_oracle.f90)
checks those against central differences. Storage-identity analysis for
same-target, different-target, overlapping, and component aliases remains
P7.3 work.
