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

## Which mode, mechanically

```
                    few outputs        many outputs
few inputs          either             vector forward
many inputs         reverse            neither: use matrix-free products
```

If in doubt, generate both and time them. fortad emits ordinary Fortran, so
that costs a compile, not a redesign.
