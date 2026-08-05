# Giering and Kaminski 1998: Recipes for Adjoint Code Construction

Giering and Kaminski present adjoint generation as a source-level reverse
transformation of the numerical algorithm that computes a model or cost
function. Their target is practical Fortran code used in inverse modeling,
data assimilation, model tuning, and sensitivity analysis. The paper turns the
chain rule into construction rules for statements, loops, procedures, files,
and implicit iterations.

## Core rule

If the numerical algorithm is a composition of steps, the tangent model applies
the Jacobian factors in forward order. The adjoint applies their transposes in
reverse order. For a scalar cost function, this computes the gradient with a
cost that is a small multiple of the original evaluation. The paper gives the
usual advantage over finite differences when the number of controls is large.

For an assignment, the authors form the restricted tangent Jacobian for the
active variables in the statement and transpose it. The adjoint contributions
to right-hand-side variables are accumulated before the adjoint of the
left-hand side is cleared. This ordering is required when the left-hand side
also occurs in the right-hand side. Passive variables receive no adjoint code.

The adjoint of a block reverses statement order. A sequential loop runs its
adjoint kernel with reversed bounds and step. A parallel loop keeps its order
when iterations are independent. A conditional needs either the original
branch decision or enough values to reevaluate the condition. Conditions on
active values are valid only at differentiable points.

## Required values and conflicts

Reverse code needs values from the forward execution. The paper calls these
required variables and recommends a modular bottom-up construction. A required
value that is overwritten creates a conflict. The three offered resolutions are
to store it, expand the variable so values do not overwrite one another, or
recompute it. Storage uses memory or direct-access files. Recomputation saves
storage and costs runtime. The authors recommend a mixed strategy chosen for
the application and machine.

Procedure calls carry active adjoint arguments and required primal arguments.
The adjoint procedure is built before its call because the call signature
depends on the required values discovered inside the procedure. This is a
concrete reason to construct differentiated procedures bottom-up. The paper
also treats active files as state that needs its own adjoint file and warns that
arbitrary aliasing, `GOTO`, `ENTRY`, and return-address tricks defeat the simple
recipes.

The implicit-function section provides a model for fortad. For a solved
equation `x - f(x,p) = 0`, the exact derivative uses the inverse of
`I - df/dx`. If the forward solver is an iteration, the naive adjoint stores
every intermediate iterate. When the forward iteration has converged, the
adjoint can instead solve a fixed-point equation involving the transpose of
`df/dx`. Its convergence is as fast as the forward iteration and it needs the
solution rather than the full trajectory.

## Consequences for fortad

The paper gives fortad three implementation tests. First, every statement rule
must specify which primal values it reads and when its adjoint contribution is
cleared. Second, loop reversal must be driven by dependence analysis, not by a
syntactic reversal that assumes independence. Third, the reverse pipeline needs
an explicit storage, recomputation, or checkpointing policy.

The implicit fixed-point construction is a model for fortad's tape-free affine
recurrence work. It should remain a general transformation for converged
iterations, with an independent adjoint identity test and a convergence-domain
check. The paper's readability and modularity requirements also support keeping
generated code close enough to the source that a user can inspect the reverse
control flow.

## Source

[Published paper, DOI 10.1145/293686.293695](https://doi.org/10.1145/293686.293695)

[Open PDF copy](https://twister.caps.ou.edu/OBAN2019/Giering_recipe4adjoint.pdf)
