# Supported Fortran and limits

This page lists what fortad 0.1.0 can differentiate and where it deliberately
refuses. Unsupported constructs return `fad_result_t%ok = .false.` with the
refused construct named in `message`; the CLI prints the same diagnostic.

## Capabilities

| Capability | Status |
| --- | --- |
| Scalar JVP and VJP | expressions, intrinsic calls, branches, and supported procedure calls |
| Array JVP | whole arrays and subscripts, including loops and batched directions |
| Array VJP | reductions, element writes, nested loops, affine recurrences, and taped nonlinear recurrences |
| Shape-changing linear intrinsics | `SPREAD` JVPs and dimension-wise reverse cotangent reduction |
| Derivative-only output | forward and reverse modes with `--no-primal` |
| Hessian-vector product | scalar-direction forward-over-reverse |
| Sparse Jacobian | static conservative patterns, column coloring, seeding, and recovery |
| Sparse Hessian | repeated HVPs with star coloring and symmetric recovery |
| Higher derivatives | univariate Taylor transformation for straight-line scalar kernels |
| Long integrations | Revolve checkpoint schedules supplied to a caller-owned time loop |
| Opaque procedures | scalar partial rules and statement-based tangent/adjoint rules |
| Elemental procedures | same-file elemental functions retain elemental JVP/VJP array calls |
| Fixed procedure-dummy callbacks | same-file scalar `REAL(8)` function actuals and one fixed procedure-pointer target use FortFront-proven formal/actual interfaces in JVP and VJP; incompatible, unresolved, generic, aliased, and global cases are refused |
| Fixed-form input | CLI `.f`, `.for`, `.ftn`, and `.f77` files with legacy comments and declarations |
| Derived components | bounded concrete scalar, nested, inherited, array, and fixed-source polymorphic component paths |
| Abstract/deferred hierarchy | fixed-dispatch JVP/VJP for statically known bindings and direct polymorphic dispatch with exactly one FortFront-proven concrete target |
| Complex JVP/VJP | real-coordinate `conjg`, `real`, `aimag`, `cmplx`, `abs`, multiplication, and division; bounded real-objective VJP through `real(z)`/`dble(z)` |
| Aliasing and sections | bounded rank-one/rank-two contiguous range sections plus literal unique `INTEGER, PARAMETER` vector subscripts on explicit-shape arrays. Named refusals cover aliases, dynamic/duplicate/out-of-range vectors, vector-plus-range sections, and noncontiguous or computed sections |
| GPU directives | one-level fused positive reduction adjoints through OpenMP target and OpenACC |

## Vector mode

Forward vector mode places the direction index first. For an array `x(n)`, a
seed block has shape `(n_directions, n)`. Reverse vector mode is not
implemented. Sparse Hessians therefore run one scalar HVP for each color.

## Feature notes

Complex forward examples and the real-coordinate contract are in
[the complex-values design note](design/complex-values.md).
The bounded real-objective complex VJP and its hand/finite-difference/adjoint
oracle are covered by [`test_complex_reverse_oracle.f90`](../test/test_complex_reverse_oracle.f90).
The bounded derived-component contract and its JVP/VJP oracle are in
[the derived-components design note](design/derived-components.md).
General allocatable lifetime changes remain refused with a source-line
diagnostic. One scalar nested polymorphic component may use a concrete local
`SOURCE=` acquisition followed by final deallocation in JVP and VJP. The
same fixed `TYPE IS` or `CLASS IS` path also supports a direct scalar
assignment through paired concrete shadows; read-modify-write and ownership-
changing forms remain refused. The boundary and oracle are in [the
polymorphic ownership design note](design/polymorphic-ownership.md).
Literal indexed scalar allocatable components such as `boxes(2)%value` use the
same bounded reallocation replay contract. Concrete rank-one through rank-four
component lifetimes with literal extents also replay `ALLOCATE`, `MOVE_ALLOC`,
and `DEALLOCATE` when active accesses remain scalar; dynamic shapes, whole
component array accesses, and unsafe storage remain refused. Direct same-file
subroutine calls use exact FortFront
actual/formal storage facts and refuse aliases, callbacks, globals, pointers,
allocatables, mismatches, and ambiguous mappings.
The same fixed-path contract now covers scalar `class(*)` component assignment
when one concrete `TYPE IS` or `CLASS IS` arm proves the dynamic type.
Reverse mode also replays one scalar local or dummy `class(*)`, allocatable
owner acquired from a declared concrete `SOURCE=` object when exactly one
matching concrete `SELECT TYPE` arm and one final deallocation are present.
Ambiguous dispatch, polymorphic or factory sources, aliases, transfers, and
global mutable owners remain refusals.
The bounded contiguous rank-one/rank-two section support, storage-identity
proof, and executable refusal oracles are in
[the aliasing and sections design note](design/aliasing.md).
The fixed-dispatch abstract/deferred hierarchy slice and its multi-level
oracle are in [the abstract hierarchy design note](design/abstract-hierarchy.md).
Direct calls through a `class(base_t)` dummy are differentiated only when
FortFront proves one concrete deferred-binding implementation. The compiled
JVP/VJP, finite-difference, adjoint, unresolved-target, and multi-child
refusal oracle is [`test_abstract_deferred_dispatch_oracle.f90`](../test/test_abstract_deferred_dispatch_oracle.f90).
Passive fixed-type polymorphic allocatable dispatch, bounded active nested
`allocate(source=concrete)` ownership, and the remaining semantic boundary
for polymorphic ownership are in
[the polymorphic ownership design note](design/polymorphic-ownership.md).

## Scope

FortAD's product target is modern Fortran with explicit data ownership,
procedure interfaces, derived values, and abstract or polymorphic components.
The target applications are the maintained production code in the
lazy-fortran and itpplasma groups. Active `COMMON`, mutable module or `SAVE`
state, uncontrolled aliasing, active I/O, and opaque calls without derivative
rules are deliberate refusal boundaries. See the [product scope](design/product-scope.md)
for the full contract and priority order.

## Procedure calls and derivative rules

Same-file callees can be inlined before differentiation. A call whose body is
unavailable needs a registered derivative rule. The built-in structured rules
cover an opt-in `dgesv` path. The same interface supports caller-supplied rules
for nonlinear roots and fixed points, as well as selected numerical-library
operations.

Elemental procedures are differentiated as elemental procedures too. The
generated JVP and VJP can therefore be called with scalar or conformable array
actuals. See [`test_elemental_interface_oracle.f90`](../test/test_elemental_interface_oracle.f90)
for the finite-difference and compiled array oracle. Generic resolution by
type, kind, or rank and user-defined operators still need explicit support.

## Source form

The CLI recognizes fixed form from the input filename and emits free-form
derivative source. A minimal legacy example and the remaining source-form
limits are in [the source-forms note](design/source-forms.md).

## Known limits

This release is not a complete Fortran
language implementation. In particular:

- vector reverse mode is absent;
- Taylor transformation refuses arrays, loops, and branches;
- active global mutable state is outside the product scope;
- allocatable lifetime and arbitrary storage aliasing remain open semantic
  implementation areas;
- GPU emission is limited to the reduction shape described above;
- active assumed-size dummy arrays, including lower-bound forms such as
  `(0:*)`, are refused because tangent and adjoint storage has no shape;
- registered statement rules may introduce impure calls into generated code.

The [open defects](../ROADMAP.md#open-defects) section tracks implementation limits
and measured performance losses.

