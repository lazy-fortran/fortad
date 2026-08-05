# Hascoet and Pascual 2013: The Tapenade Automatic Differentiation Tool

Source read: the authors' INRIA Research Report RR-7957, which is the open
version of the 2013 ACM TOMS paper. The report presents Tapenade as a source
transformation tool for Fortran and C that emits tangent or adjoint code. Its
purpose is broader than a collection of local derivative rules. It specifies a
program model, the analyses needed to transform that model, and the structure
of the implementation.

## Program model

Tapenade treats one run of a program as a sequence of elementary instructions.
The mathematical state at a program point is represented by a vector of logical
variables. A source variable that is overwritten represents a new logical value
after the assignment. This distinction is essential for reverse mode because an
adjoint statement may need the value that existed before the overwrite.

The tangent transformation extends each instruction with a derivative
instruction and evaluates the extensions in source order. The adjoint
transformation extends the instructions with their transposed local operators
and evaluates them in reverse order. The two modes therefore share the same
program semantics and differ in traversal direction and the information they
must preserve.

## Analyses and representation

The implementation uses a source internal representation with symbol tables,
procedures, modules, expressions, and control-flow structure. The architecture
separates parsing, representation building, data-flow analysis, differentiation,
rebuilding, and printing. This separation lets the analyses work on the same
representation before either tangent or adjoint code is emitted.

The analysis pipeline is layered. Pointer and input-output information supports
dependency analysis. Differentiable dependency relations feed the activity
analysis. Activity is the intersection of two closures: values varied by the
chosen independent inputs and values useful to the chosen dependent outputs.
Further analyses determine differentiable liveness, values to be recorded, and
values available at the exit of an adjoint block. The call graph and each
procedure's flow graph are part of the analysis domain, so a local rule can be
combined with interprocedural information.

The formal specification matters as much as the architecture. Data-flow
equations describe the sets propagated through instructions, branches, loops,
and calls. Operational rules describe the tangent and adjoint transformations.
This gives a correctness target for the implementation that is more precise
than comparing a few generated numerical results.

## Consequences for fortad

Fortad should preserve source-level logical values until the analyses that need
them have run. A single mutable variable name is insufficient as the semantic
identity of a value. The implementation needs explicit handling for active
values, overwritten values, and values required by reverse statements.

The most useful architectural lesson is to keep the pipeline visible:
front-end representation, dependency and activity analyses, reverse storage or
recomputation analysis, derivative IR, and source emission. A formal rule for
each IR instruction should accompany the implementation. The paper also
supports a bottom-up treatment of procedures and a call-graph-aware treatment
of required values. Those choices fit fortad's procedure-level differentiation
and make unsupported control-flow cases visible at the right layer.

The paper's model is deliberately operational. It differentiates the discrete
algorithm that the source executes. Fortad should use that same boundary when
checking an implementation against hand-derived derivatives. Differentiating a
continuous equation and then discretizing it would answer a different question.

## Source

[INRIA RR-7957, hal-00695839](https://inria.hal.science/hal-00695839/document)

[Published paper, DOI 10.1145/2450153.2450158](https://doi.org/10.1145/2450153.2450158)
