# Fixed `SELECT TYPE` component dispatch

FortAD consumes FortFront's `query_select_type_component_dispatch` contract for
one deliberately narrow call shape:

```fortran
select type (typed => object)
type is (container_t)
    call typed%leaf%run(amount, output)
end select
```

The query supplies the source-backed receiver path (`typed%leaf`), terminal
component type, inherited declaring type, concrete implementation, ordered
procedure signature, and effective PASS metadata. FortAD lowers that target as
an ordinary same-file subroutine call, so the existing inliner and both JVP and
VJP passes differentiate the implementation body. A named passed-object dummy
is mapped by the implementation signature; the receiver is not guessed from
the outer `container_t`.

The supported path requires a concrete scalar non-owning component, one
explicit `CALL` as the arm's direct statement, and a unique same-file
subroutine implementation. FortFront remains the authority for the component
path and binding facts.

Generic or unresolved bindings, `ASSOCIATE` aliases, pointer or allocatable
components, array-valued paths, mutable global selector storage, nested calls,
and ownership-changing selectors are explicit refusals. FortAD does not fall
back to the older concrete-receiver resolver for these cases. The independent
hand/finite-difference/adjoint and refusal oracle is
[`test_select_type_component_dispatch_oracle.f90`](../../test/test_select_type_component_dispatch_oracle.f90).
