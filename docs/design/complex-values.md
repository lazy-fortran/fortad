# Complex values

FortAD's supported complex JVP contract is a real-coordinate directional
derivative. A complex tangent `z_d` represents independent perturbations of
`real(z)` and `aimag(z)`. It is not a holomorphic derivative.

The current forward slice covers complex multiplication and division together
with `conjg`, `real`, `aimag`, `cmplx`, and `abs`. For example,

```fortran
y = z/(1.0d0 + z) + abs(z) + cmplx(real(z), aimag(z), 8) + conjg(z)
```

uses

```text
d abs(z) = real(conjg(z)*z_d)/abs(z)
d conjg(z) = conjg(z_d)
```

and the ordinary product and quotient rules. The independent behavioral
oracle is [`test_complex_intrinsic_oracle.f90`](../../test/test_complex_intrinsic_oracle.f90).
it compiles the generated routine, checks the hand derivative, and checks a
central difference in a complex direction.

Reverse mode currently refuses an active complex cotangent with a named
diagnostic. This is deliberate: a real-only adjoint must not emit compiler-
invalid expressions such as `aimag(1.0d0)`. Complex reverse rules, BLAS, and
non-holomorphic objective conventions remain open in P7.5.
