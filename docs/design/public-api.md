# Public API

Status: frozen at `0.1.0`.

`fortad` is a source transformer. Its public Fortran API accepts Fortran source
and returns generated Fortran in `fad_result_t`; it does not execute a runtime
tape. The API names below are the stable source-transformation boundary. A
breaking change requires a version bump and a roadmap entry.

## Fortran module

The primary product surface is:

```fortran
use fortad, only: fad_result_t, fad_version, &
    fad_jvp, fad_vjp, fad_hvp, fad_taylor, fad_roundtrip, &
    fad_static_pattern, &
    fad_add_rule, fad_add_call_rule, fad_clear_rules, &
    fad_register_blas_lapack_rules
```

The sparse runtime helpers are also public:

```fortran
use fortad, only: sparsity_t, colour_columns, seed_matrix, recover_entries, &
    star_colour_columns, recover_symmetric
```

`revolve_t`, `revolve_action_t`, `revolve_schedule`, and the `REV_*` action
constants form the checkpointing surface. The `tay_*` routines form the
fixed-order Taylor arithmetic surface used by generated Taylor kernels.

All transformation calls return `fad_result_t`. On success, `ok` is true and
`code` contains standard Fortran; on failure, `ok` is false and `message`
names the refused construct. Generated procedures do not carry module-global
state.

## Product names

ADOL-C's driver names remain the useful product taxonomy, but they are not
literal compatibility aliases. The source-transforming mapping is:

| Product name | Frozen fortad surface | Meaning |
|---|---|---|
| `jacobian` | `fad_jvp`, with repeated or vector seeds | Jacobian-vector products; recover a full Jacobian only when the caller needs it |
| `hessian` | `fad_hvp`, with repeated directions or sparse recovery | Hessian-vector products; no materialised Hessian by default |
| `jac_vec` | `fad_jvp` | one forward product |
| `vec_jac` | `fad_vjp` | one reverse product |
| `hess_vec` | `fad_hvp` | one Hessian-vector product |
| sparse Jacobian/Hessian drivers | `fad_static_pattern` plus the sparse helpers | caller-supplied or conservatively inferred pattern, colouring, and recovery |

The caller owns numerical vectors, covariance factors, storage layout, and
optimizer iteration policy. This keeps the generated routine usable from
ordinary Fortran, fortnum's backend-opaque callbacks, and downstream codes
with different data layouts.

## CLI

The standalone `fortad` command freezes the following options:

```text
--mode forward|reverse|hessian
--indep, --dep, --directions, --name, --output
--module, --proc, --no-primal, --roundtrip
--rule, --call-rule, --version, --help
```

The CLI and module expose the same product boundary. Literal ADOL-C runtime
drivers, hidden global tape state, and an optimizer loop are deliberately not
part of the surface.
