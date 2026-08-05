# Fixed-point adjoints

P3.4 applies the Christianson two-phase construction to an opaque converged
fixed point. For

```text
x = G(x,p)
```

the primal phase computes `x*` until convergence. The tangent callback solves

```text
dx = G_x(x*,p) dx + G_p(x*,p) dp
```

and the adjoint phase iterates the transpose fixed-point equation

```text
lambda = G_x(x*,p)^T lambda + u
p_b = G_p(x*,p)^T lambda
```

where `u` is the cotangent of the converged state. The adjoint iteration is
the second phase: it uses the converged state and the transpose linearized map,
not the full forward trajectory.

For a call `fixed_point_solve(p, x)`, the registry entry has the same explicit
shape as the root and linear-solve rules:

```fortran
call fad_add_call_rule("fixed_point_solve", 2, &
    tangent=[character(len=128) :: &
             "call fixed_point_tangent($1, $1d, $2, $2d)"], &
    adjoint=[character(len=128) :: &
             "call fixed_point_adjoint($1, $2, $2b, $1b)"])
```

The callbacks own the map derivatives, stopping criterion, and any linear
algebra. fortad emits the calls and no per-iteration tape. This is explicit
because an opaque solver does not reveal its map or its convergence domain.

## Evidence and boundary

`test/test_fixed_point_rule_oracle.f90` uses the two-state tanh map from
fortnum, registers both callbacks, and links the generated JVP/VJP to an
opaque converged primal. The driver checks a directional derivative against
fresh complete fixed-point solves, the VJP adjoint identity, and both VJP
components. The focused oracle passes on the TU Graz `acluster`.

The existing TU Graz Ryzen 9 fixed-point records provide the performance
context. Against fresh complete re-solves, the implicit tangent is 97.8032 ns
versus 1224.4016 ns (12.5190x), and the implicit adjoint is 132.3983 ns versus
2495.1440 ns (18.8457x). These are independent analytical/reference records;
there is no compatible Enzyme fixed-point fixture in the available TU Graz
toolchains, and the separate Enzyme Richardson-trace record is not the same
workload.

The current boundary is caller-supplied fixed-shaped map products with a
contractive converged phase. Automatic map extraction, convergence-domain
proofs, noncontractive acceleration, and general shaped-array interfaces
remain future work.
