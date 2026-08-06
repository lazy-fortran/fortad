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

This is the supported subset of Phase 7.4.  Generic resolution, optional
active tangents, and procedure-pointer callbacks remain explicit roadmap
items; an unsupported call is refused rather than treated as constant.
