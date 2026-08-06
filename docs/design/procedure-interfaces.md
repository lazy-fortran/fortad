# Procedure interfaces

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

Generic resolution, optional active tangents, and procedure-pointer callbacks
remain explicit roadmap items. An unsupported call is refused rather than
treated as constant.

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
