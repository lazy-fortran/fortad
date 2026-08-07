# Product scope

FortAD targets modern Fortran programs whose differentiated path has explicit
data ownership, explicit procedure interfaces, and a fixed runtime control
trace. The target workload is the production code in the lazy-fortran and
itpplasma groups. Tapenade compatibility remains useful for migration and
regression testing, but Tapenade's full historical language surface does not
define FortAD's product scope.

## First-class program model

The active path must support the data model used by the applications:

- arrays with explicit shape, section, rank, and storage behavior
- derived values with nested, inherited, and polymorphic components
- allocatable ownership, reallocation, assignment, and destruction
- same-file and module procedure calls with explicit interfaces
- abstract classes, deferred bindings, overrides, and runtime dispatch
- passive runtime choices such as a selected concrete type or fixed event path
- numerical library, MPI, OpenMP, and accelerator operations through registered
  rules with stated derivative contracts

The derivative contract is pathwise. A type switch, event ordering change,
convergence-class change, or other discrete transition ends the local
derivative path and receives a diagnostic.

## Deliberate refusal boundaries

FortAD refuses an active construct when its derivative would depend on state
that the IR cannot represent or replay. The product boundary includes:

- active `COMMON`, mutable module state, `SAVE` state, and other uncontrolled
  global mutation
- arbitrary pointer aliasing, overlapping actual arguments, and sections whose
  storage identity is not known
- active I/O and opaque calls without a registered derivative rule
- callbacks whose selected implementation or context has no derivative
  contract
- perturbations that change a runtime type, event, communication path, or
  convergence class

Passive constants, configuration, and runtime tags may remain outside the
derivative graph. A refusal names the construct, source location, and missing
semantic contract. It is a scope result, not a support result.

## Implementation consequence

The IR must represent storage identity, ownership and lifetime, dynamic type,
binding targets, call-graph edges, and reverse replay storage. Each addition
needs an independent derivative oracle and an application-shaped case. The
implementation order is therefore:

1. repair generated-code correctness and call-graph lowering;
2. add owned arrays, allocatable lifetime, and safe section semantics;
3. complete abstract/deferred hierarchies and polymorphic ownership;
4. add interfaces, callbacks, and numerical or parallel rules required by the
   application manifest;
5. optimize reverse storage, batching, checkpointing, and generated code.

Legacy syntax and Tapenade-only patterns remain valuable negative tests. They
do not displace these priorities unless a maintained application requires
them.
