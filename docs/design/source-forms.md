# Source forms

The CLI selects fixed form from `.f`, `.for`, `.ftn`, or `.f77`. For example:

```fortran
C fixed-form input
      subroutine scale(x,y)
      double precision x,y
      y=x*x
      end
```

```sh
fortad --indep x --proc scale --module scale_derivative \
  --name scale_jvp -o scale_jvp.f90 scale.f
```

FortFront translates fixed-form comment and continuation markers before
FortAD parses the procedure. A legacy procedure without a `PURE` or
`ELEMENTAL` prefix produces an ordinary subroutine, so missing legacy `INTENT`
declarations do not make the generated code invalid. FortAD records explicit
`PURE` and implicit elemental purity, but conservatively removes purity when
the generated routine contains an unresolved call. Legacy I-N loop indices
that are only implicitly typed in the primal are synthesized as local
`integer` declarations in generated JVP/VJP procedures.

[`test_tapenade_fixed_form_oracle.f90`](../../test/test_tapenade_fixed_form_oracle.f90)
runs that CLI path, compiles the generated free-form JVP with gfortran, and
checks a hand derivative and central finite difference.
[`test_implicit_loop_index_oracle.f90`](../../test/test_implicit_loop_index_oracle.f90)
also compiles and runs both products for an exact implicit-index loop.

This is not complete source-form support. The library routines still accept a
source string without a filename and therefore do not infer its form. CPP,
include expansion, mixed fixed/free files, semicolon parity, and broad legacy
syntax remain P7.6 work.
