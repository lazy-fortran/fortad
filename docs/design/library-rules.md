# FFT, quadrature, interpolation, and special-function rules

P3.5 uses the same explicit statement-rule boundary for library operations.
The library owns the mathematical identity and its Fortran interface; fortad
substitutes the registered tangent and adjoint statements and does not invent
an external ABI.

The representative rule table exercised by
`test/test_library_rules_oracle.f90` is:

| Operation | Tangent product | Adjoint product |
|---|---|---|
| real FFT pair `C,S = fft8_r2c(signal)` | the same forward transform of `signal_d` | transpose of the cosine/sine transform |
| four-point quadrature `q = quad4(values)` | weighted sum of `values_d` | `values_b += weights*q_b` |
| four-node Lagrange interpolation | derivative of basis weights with respect to nodes, values, and point | transpose basis accumulation into nodes, values, and point |
| `z = special_erf(alpha)` | `2/sqrt(pi)*exp(-alpha**2)*alpha_d` | the same scalar factor times `z_b` |

The registrations are ordinary call rules. For example, the interpolation
boundary is expressed as:

```fortran
call fad_add_call_rule("interp4", 4, &
    tangent=[character(len=256) :: &
             "call interp4_tangent($1, $1d, $2, $2d, $3, $3d, $4, $4d)"], &
    adjoint=[character(len=256) :: &
             "call interp4_adjoint($1, $2, $3, $4b, $1b, $2b, $3b)"])
```

The generated JVP/VJP contains the registered library calls, not a tape of the
FFT loops, quadrature sum, interpolation basis, or special-function
implementation. Multiple opaque calls are supported in one procedure; their
real actual adjoints are declared and cleared at the call boundary.

## Evidence and boundary

The oracle links the generated composite kernel to independent support
implementations and checks the complete composite output by central finite
differences, the VJP adjoint identity, and component finite differences. It
passes on the TU Graz `acluster`.

The committed TU Graz benchmark corpus supplies the performance context:
length-8 FFT JVP/VJP products are 177.7106/169.9557 ns; four-direction fixed
quadrature JVP is 1247.7167 ns for the explicit analytical product versus
1263.4433 ns for the Enzyme-integrand candidate; one-cotangent VJP is 299.4435
ns versus 317.5576 ns; and the generated `erf` products are 13.52566 ns (JVP)
and 13.58756 ns (VJP). These are existing fortnum records, not same-binary
timings of this generic fortad oracle.

The current boundary is representative real FFT, fixed quadrature, fixed-node
or explicit-basis interpolation, and `erf`. Complex FFT ABI details, adaptive
quadrature, general spline state, and the rest of the special-function catalog
remain caller-supplied extensions rather than silently inferred rules.
