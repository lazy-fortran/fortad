# Assumed-rank `SELECT RANK`

FortAD differentiates one deliberately fixed assumed-rank path. FortFront's
`SELECT RANK` query must prove all of the following at the source location:

- the selector is a direct identifier bound to a genuine assumed-rank dummy;
- the selector storage is nonpointer, nonallocatable, nonpolymorphic, and not
  module, `SAVE`, or `COMMON` state;
- the arm is the only explicit `RANK (1)` arm, with no `RANK DEFAULT` or
  `RANK (*)`; and
- `selector_bounds_node_indices` identifies exactly the declaration's one
  source-backed `(..)` bounds node. That node must have no manufactured lower,
  upper, or stride expression.

The last condition is important. An assumed-rank declaration has unknown source
rank until dispatch. FortAD does not infer rank from the spelling of the
declaration or from the arm body. After the query facts prove the fixed arm, the
lowerer changes the IR declaration to the ordinary rank-one assumed-shape
contract. The existing forward and reverse differentiators then emit matching
rank-one tangent and cotangent dummies.

The generated procedures therefore have a fixed-path call contract: callers
must pass a rank-one actual that selects `RANK (1)` in the primal. Other runtime
ranks, default and assumed-size dispatch, multiple arms, aliases or computed
selectors, unsupported kinds, pointers, allocatables, polymorphic selectors,
and mutable global storage remain source-local refusals. FortFront forms that
are not represented in the AST, including selector-associate syntax on the
current pin, are also refused rather than lowered as an empty body. In
particular, the lowerer never creates a descriptor shadow or guesses a dynamic
rank.

`test/test_assumed_rank_select_rank_oracle.f90` is the independent compiled
oracle. It compiles the primal and generated JVP/VJP with GNU Fortran, checks a
hand derivative and a central finite difference, verifies the adjoint identity,
and checks the dispatch, ownership, alias, global-state, and unresolved-fact
refusals. The test's numerical driver is separate from FortAD's IR and does not
rederive the generated source as its oracle.
