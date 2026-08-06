# Complex values

FortAD's supported complex JVP contract is a real-coordinate directional
derivative. A complex tangent `z_d` represents independent perturbations of
`real(z)` and `aimag(z)`. It is not a holomorphic derivative.

The current forward slice covers complex multiplication and division together
with `conjg`, `real`, `aimag`, `cmplx`, and `abs`. A bounded reverse slice now
handles a real-valued objective whose active complex inputs enter through a
direct `real(z)` or `dble(z)` projection, followed by ordinary real
arithmetic. Its complex adjoint stores the two real-coordinate gradients:
the real part is the derivative with respect to `real(z)`, and the imaginary
part is the derivative with respect to `aimag(z)`. For example,

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

The projection path is deliberately narrow. Complex arithmetic, `abs`,
`aimag`, `conjg`, complex outputs, and complex BLAS remain explicit refusal
boundaries until their real-coordinate transpose rules are implemented. This
prevents a real-only seed from being mistaken for a complete complex Jacobian.
The positive projection path is checked by
[`test_complex_reverse_oracle.f90`](../../test/test_complex_reverse_oracle.f90),
which compiles the generated VJP and compares a hand derivative, central
finite differences, and the real adjoint identity
`Re(conjg(z_b) dz) = y_b dy`.
