# Implicit differentiation of nonlinear roots

P3.3 uses the structured call-rule registry to differentiate a converged root
as an implicit equation. The root solver stays opaque; its registrant supplies
the residual products and the converged state.

For a scalar residual `F(x,p)=0`, the implicit-function theorem gives

```text
F_x dx + F_p dp = 0
dx = -F_x^{-1} F_p dp
```

For a scalar objective with root cotangent `x_b`, the reverse rule solves

```text
F_x^T lambda = x_b
p_b -= F_p^T lambda
```

The registry expresses this as ordinary Fortran statements. For a call
`root_solve(p, x)`, a caller can register the two products as:

```fortran
call fad_add_call_rule("root_solve", 2, &
    tangent=[character(len=128) :: &
             "call root_tangent($1, $1d, $2, $2d)"], &
    adjoint=[character(len=128) :: &
             "call root_adjoint($1, $2, $2b, $1b)"])
```

`root_tangent` and `root_adjoint` may use a factorisation or a matrix-free
linear solve. They are evaluated at the converged `x`; no Newton iterate tape
is emitted. The rule is intentionally opt-in because fortad cannot infer the
residual, convergence contract, or the correct linear solve from an opaque
callee.

## Evidence and boundary

`test/test_implicit_root_rule_oracle.f90` registers this rule for the cubic

```text
F(x,p) = x^3 + p1*x - p2
```

and links the generated JVP and VJP to an opaque twelve-step Newton primal.
The driver compares against fresh complete root solves by central finite
differences, checks the VJP adjoint identity, and checks both VJP components.
The test passes on the TU Graz `acluster`.

The existing TU Graz Ryzen 9 scalar-root fixture is the Enzyme comparison:
Enzyme differentiates the twelve Newton steps. At one root product it measures
2.297330 ns for the analytical implicit JVP versus 115.820600 ns for Enzyme
(50.4153x), and 14.117010 ns for the analytical implicit VJP versus
158.803910 ns for Enzyme (11.2491x). This is a cross-record performance
comparison; the fortad oracle itself runs with gfortran on `acluster`, while
the Enzyme fixture uses Flang/LLVM 22.1.8 with Enzyme 22.

The current boundary is a caller-provided residual-product rule for a scalar
root or a root with a fixed-shaped state. Automatic residual extraction,
convergence certification, singular-Jacobian handling, and nested root calls
remain future work.
