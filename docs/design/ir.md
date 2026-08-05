# IR decision

## Decision

fortad lowers fortfront's typed AST into a dedicated fortad IR. It does not
extend `transformation_api.f90` for differentiation and it does not mutate the
frontend AST as its primary representation.

## What fortfront already provides

The frontend supplies the pieces that belong to parsing and semantic analysis:

- an arena with integer node indices and subtree cloning
- typed nodes for expressions, declarations, procedures, arrays, loops, and
  branches
- resolved-type and name-binding queries
- structural queries for procedures, declarations, and derived types
- Fortran emission

The P0.4 measurement in [coverage.md](coverage.md) accepted all 161 files in
the measured fortnum source corpus. The frontend is therefore adequate for the
current input boundary.

## Gap analysis

`transformation_api.f90` exposes high-level source transformation entry points
and compilation options. It does not expose a differentiation-oriented rewrite
API. A differentiation pass would still need to provide the following pieces:

1. A representation for three-address expressions, explicit control-flow
   edges, SSA names, and derivative storage locations.
2. Passes for inlining, loop normalization, constant propagation, activity,
   and data-flow reversal.
3. Safe statement insertion and replacement while preserving parent links,
   source locations, and arena ownership.
4. Provenance from generated derivative statements back to primal statements.
5. A representation for named refusals when a construct cannot be
   differentiated correctly.

The AST factory can create individual nodes and the arena can link children.
Those operations do not supply the AD-specific invariants above. Adding them
to the generic frontend would couple a parser and emitter to one transformation
consumer.

## Consequence for fortad

The frontend remains responsible for parsing, semantic information, and final
Fortran emission. fortad owns the intermediate representation and its passes.
The lowering boundary copies the frontend's typed and resolved facts into
arena-indexed fortad records. The derivative pipeline can then rename values,
reorder statements, add tangent or adjoint statements, and refuse unsupported
control flow without mutating the source AST.

This decision was made before the IR work began and is the P0.5 result. It
keeps the upstream API small and makes the AD invariants explicit in the
fortad code.
