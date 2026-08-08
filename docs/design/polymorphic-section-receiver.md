# Active polymorphic section components

FortAD differentiates a borrowed, rank-one `class(base_t)` array section inside
one fixed `SELECT TYPE` arm when the dummy is declared `CONTIGUOUS` and the
section has a literal lower bound. The selected concrete array is not copied;
the JVP carries a paired section of the caller-owned concrete tangent, and the
VJP scatters component cotangents back to the matching caller-owned section.

For a section such as `item => model(2:3)`, the bounded path maps literal
selected elements back to their source elements: `item(1)%scale` is
`model(2)%scale`, and `item(2)%bias` is `model(3)%bias`. This mapping keeps
component activity and reverse seeds aligned without differentiating a Fortran
descriptor or a dynamic type tag.

The contract deliberately refuses pointer or `TARGET` storage, allocatable or
ownership-changing receivers, aliases, vector subscripts, strides, open or
dynamic section bounds, unresolved or multi-arm dispatch, and non-contiguous
borrowed sections. A section with no literal lower bound has no descriptor-free
element mapping and remains outside this slice. The independent analytical,
finite-difference, and adjoint-identity oracle is
`test/test_polymorphic_section_receiver_oracle.f90`.
