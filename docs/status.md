# Status and verification

fortad is research software for a defined subset of Fortran. The supported input,
derivative products and explicit refusal boundaries are listed in
[supported Fortran and limits](support.md). Acceptance of a new language construct
does not imply support for every storage, dispatch or control-flow combination.

The native tests generate derivative source, compile it and check the resulting
program against analytical derivatives, finite-difference step sweeps and adjoint
identities. The [documentation index](README.md) links individual features to
their executable oracles. `fo check` runs the suite with the selected compiler;
compiler and external-engine comparisons live in
[fortad-bench](https://github.com/lazy-fortran/fortad-bench).

Arbitrary aliasing, active global mutable state, general polymorphic ownership,
vector reverse mode and general accelerator execution remain outside the verified
scope. Taylor transformation is limited to straight-line scalar kernels. The
current GPU emitter covers the restricted loop shapes documented in
[the GPU guide](design/gpu.md); it is not general device differentiation.

Performance is workload-dependent. Recorded comparisons include cases where
the differentiation tool Enzyme is faster, and an earlier comparison using the VMEC++ equilibrium code rejected a universal
performance claim. See [performance](performance.md) for those qualifications.
Future work is tracked in the [issue tracker](https://github.com/lazy-fortran/fortad/issues).
