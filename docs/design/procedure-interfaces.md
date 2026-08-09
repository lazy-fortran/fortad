# Procedure interfaces

## Direct same-file call boundaries

FortAD differentiates a bounded modern-Fortran call slice automatically. A
direct call to a procedure defined in the same source is accepted when
FortFront proves an exact formal/actual type, kind, and rank match and a
non-aliased scalar or whole-array storage mapping. Positional and keyword
actuals, omitted optional dummies, scalar values, side-effect-free scalar
arithmetic actuals for scalar `INTENT(IN)` dummies, and assumed-shape whole
arrays use that mapping. The callee is then inlined before differentiation.

The boundary refuses ambiguous or unresolved calls, procedure callbacks,
active global mutable state, pointer or allocatable storage, repeated actual
aliases, incomplete facts, mismatches, and writable formals bound to
expressions or sections. It also refuses side-effecting calls and computed
array actuals at a read-only boundary because the inliner does not replay
those storage mappings. Refusals name the call line and reason. The original
boundary oracle is [`test_procedure_call_boundary_oracle.f90`](../../test/test_procedure_call_boundary_oracle.f90);
the scalar computed-actual JVP/VJP and refusal oracle is
[`test_procedure_call_readonly_actual_oracle.f90`](../../test/test_procedure_call_readonly_actual_oracle.f90).

### Scalar computed actuals

For a scalar `INTENT(IN)` dummy, a caller may pass an intrinsic arithmetic
expression such as `x + shift`. FortAD substitutes that expression as a value,
not as a writable storage name, so both JVP and VJP retain the derivatives of
all active leaves in the caller expression. The bounded path admits constants,
variables, unary arithmetic, and binary arithmetic only. User procedure calls,
array-valued expressions, aliases, ownership-bearing entities, and ambiguous
actual/formal mappings remain refusals.

FortAD keeps an optional dummy optional in generated code.  That makes both
forms of a call valid:

```fortran
call f_jvp(x=x, x_d=1.0d0, y=y, z=z, z_d=z_d) ! y present
call f_jvp(x=x, x_d=1.0d0, z=z, z_d=z_d)       ! y absent
```

The generated routine copies the source `present(y)` branch.  The primal and
derivative agree on the selected path; `y` is passive unless it is listed as
an independent.  Keyword arguments are the portable way to omit an optional
dummy while supplying later outputs.  See
[`test_optional_oracle.f90`](../../test/test_optional_oracle.f90) for a
compiled JVP/VJP check against finite differences.

Active optional forward arguments are supported: the generated tangent dummy
is also `optional`, and the source and tangent actuals may be supplied or
omitted together. Active optional reverse arguments are supported for a legal
`present(y)`-guarded path. The generated VJP keeps the primal `y` optional but
returns the outgoing `y_b` as a required cotangent dummy, so callers may omit
`y` and still receive a zero cotangent. Present and omitted calls must use the
same fixed path; optional arrays, optional components, and ownership-bearing
optionals remain outside this slice. The independent
[`test_active_optional_reverse_oracle.f90`](../../test/test_active_optional_reverse_oracle.f90)
checks compiled JVP/VJP values, central differences, and the adjoint identity.
A bounded procedure-pointer callback is supported when a direct same-scope call
has one preceding unconditional direct assignment to a same-file internal or
external procedure. A second bounded form permits exactly two such assignments
to same-arena scalar `REAL(8)` functions with one `REAL(8), INTENT(IN)`
argument; the second assignment is the active target. FortFront resolves the
target facts, FortAD checks the function kind and argument shape, then lowers
the call as the concrete procedure so the existing inlining and derivative
paths apply. The pointer declaration and passive assignments do not enter
generated AD code. A third or indirect mutation, branch/loop-dependent flow,
`NULL()`/`NULLIFY`, generic or unresolved targets, and module-owned mutable
callback state remain named refusals. See
[`test_callback_call_oracle.f90`](../../test/test_callback_call_oracle.f90) for
the compiled JVP/VJP, finite-difference, adjoint, and refusal oracle.

## Passed-procedure callbacks: bounded fixed-interface P8.6 slice

FortAD accepts one fixed-interface passed-procedure form. A same-file scalar
`REAL(8)` function may be passed directly, or a local procedure pointer may
receive exactly one direct same-scope assignment to that function before it is
passed as the actual for one procedure dummy. The formal procedure interface
and actual target must both be the exact scalar
`real(8) function(real(8), intent(in))` signature. FortFront supplies the
actual/formal mapping, both signatures, compatibility state, and (for a
pointer) the fixed target fact; FortAD consumes those facts rather than
inferring a callback signature. It substitutes the fixed target while
inlining and leaves the procedure dummy, pointer declaration, and pointer
assignment out of generated AD code. The JVP and VJP therefore differentiate
the concrete target using the ordinary call machinery.

This is intentionally a modern-Fortran abstraction boundary, not a runtime
callback implementation. FortAD refuses incompatible or unresolved
formal/actual facts, generic or ambiguous actuals, reassignment, branch or
loop flow, `NULL()` targets, aliases, global mutable state, and target
ownership or pointer-association changes. The refusal names the first violated
boundary. The independent compiled hand, finite-difference, adjoint, and
refusal oracle is
[`test_passed_procedure_callback_oracle.f90`](../../test/test_passed_procedure_callback_oracle.f90).

### Optional callback dummies

The next bounded callback slice accepts the same fixed target when it is
passed by keyword to an optional procedure dummy. It also accepts an omitted
optional callback only when FortFront proves the callee has exactly one direct
`if (present(callback))` guard, with the callback invoked only in that arm and
no `else` or `else if`. The inliner removes the unreachable arm for the
omitted call, so the generated JVP and VJP contain no procedure-pointer
interface or unresolved callback name.

An omitted callback used outside that guard, an `else` fallback, multiple
guards, forwarding to another procedure, or any reassignment remains a
refusal. The independent
[`test_optional_passed_callback_oracle.f90`](../../test/test_optional_passed_callback_oracle.f90)
checks both JVP/VJP paths against central differences and the adjoint identity,
as well as global-state and ownership refusals.

## Elemental procedures

When the selected same-file procedure has an `elemental` prefix, FortAD keeps
that prefix on both generated derivatives.  The resulting elemental JVP and
VJP accept scalar or conformable array actuals, so the array call is still the
compiler's elementwise operation rather than a loop synthesized by FortAD.

```fortran
elemental pure subroutine scale_jvp(x, x_d, z, z_d)
```

[`test_elemental_interface_oracle.f90`](../../test/test_elemental_interface_oracle.f90)
compiles the primal and both derivatives, checks a scalar central difference,
and calls the generated routines on rank-one arrays. Generic resolution by
type, kind, or rank and user-defined operators remain open. These paths must
resolve the selected implementation before derivative generation.
