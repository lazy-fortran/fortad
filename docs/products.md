# Getting each derivative product out of fortad

Four products, four different derivative objects. Choosing the wrong one is the
most expensive mistake available here — computing a full Jacobian to get a
gradient costs `n` sweeps instead of one — so this page starts from what you
want and works back to the call.

There is no driver layer, and that is deliberate. Each product below is a
generated routine plus a few lines of ordinary Fortran. A wrapper hiding those
lines would have to guess your storage layout, your covariance representation,
and your optimiser's calling convention, and it would be wrong for someone.

| You want | Object | Mode | Cost |
|---|---|---|---|
| A gradient for an optimiser | `∇f` | reverse | one sweep, any number of inputs |
| A Hessian-vector product | `H v` | forward-over-reverse | one sweep per product |
| Sensitivity of many outputs to few inputs | columns of `J` | vector forward | one sweep for all `k` inputs |
| Sensitivity of few outputs to many inputs | rows of `J` | reverse, once per output | one sweep per output |
| Linear uncertainty propagation | `cov(y) = J cov(x) Jᵀ` | vector forward | one sweep for the whole covariance |

## Gradients

```console
$ fortad --mode reverse --indep x --module my_grad kernel.f90
```

One reverse sweep gives the derivative with respect to every independent at
once. This is the cheap-gradient principle and it is why reverse mode exists:
the cost does not grow with the number of inputs.

```fortran
f_b = 1.0_dp
call f_vjp(n, x, f, f_b, x_b)     ! x_b is now grad f
```

Seed `f_b` with something other than one and you get `f_b · ∇f`, which is the
vector-Jacobian product in general.

## Hessian-vector products

```console
$ fortad --mode hessian --indep x --module my_hvp kernel.f90
```

Seed the tangents with the direction `v` and the dependent's adjoint with one;
the tangents of the returned adjoints are `H v`. Cost is one sweep per product
regardless of dimension, which is what makes Newton–Krylov practical: you never
form `H`.

To form `H` anyway, take `n` products with unit directions. Its symmetry is a
free correctness check, and one worth taking — see
`test/test_hessian_oracle.f90`, where symmetry is what catches an error that
finite differences cannot.

## Linear uncertainty propagation

**Vector forward mode already is this product.** No further machinery is
needed, which is why none is provided.

Given inputs with covariance `Σ = L Lᵀ` — `L` from a Cholesky factor, or the
scaled eigenvectors, or just the columns of independent standard deviations —
the first-order propagated covariance is

```
cov(y) = J Σ Jᵀ = (J L)(J L)ᵀ
```

and `J L` is exactly what vector forward mode computes when the tangent seeds
are the columns of `L`:

```console
$ fortad --indep x -d n_dir --module my_jvp_v kernel.f90
```

```fortran
! x_d(:, i) holds column i of L, so y_d(:, i) is column i of J L.
call f_jvp_v(k, n, x, x_d, y, y_d)
cov_y = matmul(y_d, transpose(y_d))
```

One primal sweep carries all `k` columns. The measured cost per direction falls
from about 9.6 ns per element at one direction to 0.9 at sixteen on the
benchmark kernel, so this is far cheaper than `k` separate tangent runs.

**Where this stops being valid.** First-order propagation assumes the map is
close to linear over the input uncertainty. It says nothing about skew, it
cannot see a fold or a threshold, and near a stationary point in an input it
predicts zero sensitivity where the true response is quadratic. If the
nonlinearity matters over the range of `Σ`, this number is not the answer and
no amount of derivative accuracy fixes that. See Saltelli et al., *Global
Sensitivity Analysis: The Primer*, for what to reach for instead.

## Sensitivity analysis

Sensitivity is a Jacobian question, and which mode wins depends only on the
shape:

- **Few inputs, many outputs** — vector forward mode, one sweep for all inputs.
- **Few outputs, many inputs** — reverse mode, one sweep per output.
- **Both large** — neither is cheap, and the answer is usually that you do not
  want the full Jacobian. Ask what it is for: an optimiser wants `Jᵀu`, a
  Krylov solve wants `Jv`, and both are one sweep.

A useful diagnostic that costs one extra sweep: seed a *random* direction and
compare against a directional finite difference. It checks the whole Jacobian
at once, where checking entries one at a time costs `n` differences and misses
cross terms.

## Gauss-Newton

For a least-squares residual `r(x)`, Gauss-Newton needs `J v` and `Jᵀ u`, never
`J` itself:

- `J v` — one vector-forward sweep.
- `Jᵀ u` — one reverse sweep with the residual adjoints seeded to `u`.

Both are matrix-free, so the normal equations are solved by a Krylov method
without ever forming `JᵀJ`. That is the whole reason to have both modes.

## Differentiating a solve, not the solver

The single largest win available here, and the one fortad cannot find on its
own. Given `call linsolve(n, A, b, x)`, differentiating `A x = b` gives

```
A x_d = b_d - A_d x
```

— one more solve with the **same matrix**, so a solver holding a factorisation
reuses it. Differentiating the solver's iterations instead is asymptotically
worse and no loop-level cleverness recovers the difference.

Register the rule and fortad emits it:

```fortran
call fad_add_call_rule("linsolve", 4, &
    tangent=[character(len=80) :: &
             "call linsolve($1, $2, $3d - matmul($2d, $4), $4d)"], &
    adjoint=[character(len=80) :: &
             "call linsolve_transposed($1, $2, $4b, fad_lambda)"])
```

Placeholders: `$k` is the k-th actual argument as written at the call site,
`$kd` its tangent, `$kb` its adjoint. The primal call is emitted **before** the
tangent statements, because a rule generally needs the call's outputs — the
solve's tangent reads `x`. A rule needing a pre-call value must save it itself.

The same shape covers a nonlinear solve (the implicit function theorem at the
converged point), a fixed-point iteration (Christianson's two-phase adjoint),
and a BLAS call (its own transpose). In each case the rule is mathematics the
registrant knows and the tool does not.

**Without a rule, a call is refused by name.** fortad never descends into a
callee and never assumes one is inactive: a silently dropped derivative is
worse than a build failure, because it looks plausible.

## Sparse Jacobians

When most entries are structurally zero, a dense Jacobian costs `n` sweeps to
learn almost nothing. Two columns whose nonzeros never share a row can be
evaluated in the *same* sweep and separated afterwards: in any given row at most
one of them contributes. Grouping columns that way is graph colouring.

```fortran
use fortad, only: sparsity_t, colour_columns, seed_matrix, recover_entries

call colour_columns(pattern, colour, n_colours)
call seed_matrix(pattern, colour, n_colours, seeds)
call f_jvp_v(n_colours, n, x, seeds, y, compressed)   ! one vector sweep
call recover_entries(pattern, colour, compressed, values)
```

`values` comes back in the order of `pattern%rows`, so it pairs directly with
the pattern you supplied. A tridiagonal Jacobian of any size takes three
colours; a diagonal one takes one. A dense one takes `n`, correctly — there is
nothing to compress and the method says so rather than losing entries.

Recovery is **exact**, not approximate: compression is a rearrangement.

**You supply the pattern.** fortad cannot infer it from the source in general,
and inventing one would be worse than asking. Be conservative: an over-full
pattern costs sweeps, an under-full one silently loses derivative entries.

### Sparse Hessians

The same colouring works on a Hessian unchanged, applied to the
Hessian-vector-product seeds instead of the tangent seeds. Compression only
needs "no two columns of a colour share a row", which knows nothing about
symmetry, so a symmetric tridiagonal Hessian takes three colours exactly as a
tridiagonal Jacobian does.

It is not *optimal* for a symmetric matrix. Star colouring exploits symmetry to
use strictly fewer colours, and is the right next step here; it is not
implemented, and the current cost is pinned by a test so that a later star
colouring can be shown to beat it rather than merely claimed to.

## Adjoints of long time integrations

The adjoint of an `n`-step integration needs each step's input state again, in
reverse. Storing all `n` is often impossible; storing none means replaying from
the start for every step, which is `O(n²)`. Binomial checkpointing is the
optimal compromise, and the compromise is very good: with `s` slots and `r`
repetitions it covers `binom(s+r, s)` steps.

Measured from the schedules this generates: **1000 steps in 10 slots costs 3636
forward steps** — about 3.6x the primal work, for 10 stored states instead of
1000. 100 steps in 4 slots costs 379.

```fortran
use fortad, only: revolve_schedule, revolve_t, &
                  REV_ADVANCE, REV_TAKESHOT, REV_RESTORE, REV_TURN

call revolve_schedule(n_steps, n_slots, schedule)
do i = 1, schedule%n_actions
    select case (schedule%actions(i)%kind)
    case (REV_ADVANCE);  ! run the primal from %from to %to
    case (REV_TAKESHOT); ! save the current state into %slot
    case (REV_RESTORE);  ! load %slot back
    case (REV_TURN);     ! adjoint of step %from, whose input state is current
    end select
end do
```

This is a **schedule, not a driver**. fortad does not own your time loop, your
state, or your storage, and a framework that demanded to would be useless in
exactly the codes that need this most.

## Derivatives above second order

Nesting a first-order tool `d` times costs `O(2ᵈ)`. Propagating a truncated
Taylor series costs `O(d²)` per operation and gives every derivative up to `d`
in one sweep.

A Taylor object carries the coefficients of `f(x + t v)` in `t`, so
`a(k) = (1/k!) dᵏ/dtᵏ f(x + t v)`. The rules are recurrences read off the
defining identity of each function — `z = exp(a)` satisfies `z' = z a'`, hence
`k z_k = Σ i a_i z_{k-i}` — so they are exact relations, not differentiated
series approximations.

```fortran
use fortad, only: tay_var, tay_exp, tay_sin_cos, tay_mul, tay_derivative

real(dp) :: x(0:8), e(0:8), s(0:8), c(0:8), z(0:8)

call tay_var(0.6_dp, 1.0_dp, x)     ! value, direction
call tay_exp(x, e)
call tay_sin_cos(x, s, c)
call tay_mul(e, s, z)               ! z = exp(x)*sin(x)
print *, tay_derivative(z, 5)       ! the fifth derivative
```

**What is and is not built.** The arithmetic is here and pinned against
closed-form series — `exp(t)` giving `1/k!`, `1/(1-t)` giving all ones,
`log(1+t)`, `sqrt(1+t)`, the sine and cosine series, and integer powers at a
negative base where an `exp(p log a)` implementation would fail. The
transformation that rewrites a Fortran kernel into calls to these routines is
**not** built. Write the calls yourself, or use forward-over-reverse for second
order, which is generated.

## Which mode, mechanically

```
                    few outputs        many outputs
few inputs          either             vector forward
many inputs         reverse            neither: use matrix-free products
```

If in doubt, generate both and time them. fortad emits ordinary Fortran, so
that costs a compile, not a redesign.
