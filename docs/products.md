# Derivative products

The number of inputs, outputs, and requested directions determines the useful
derivative product. fortad emits product routines and leaves numerical storage
and solver policy with the caller.

| Need | Product | Route | Transformation cost |
| --- | --- | --- | --- |
| scalar-objective gradient | `J^T` with seed 1 | VJP | one reverse sweep |
| directional sensitivity | `Jv` | JVP | one forward sweep |
| several directions | `JV` | vector JVP | one primal sweep for all columns |
| Hessian action | `Hv` | forward-over-reverse | one composed product per direction |
| a few Jacobian rows | `u^T J` | VJP | one reverse sweep per output seed |
| sparse Jacobian or Hessian | compressed products | coloring and recovery | one product per color |

## Gradients

For a scalar objective, seed its cotangent with one. One VJP returns an adjoint
for every independent:

```console
$ fo exec fortad --mode reverse --indep x --module rosenbrock_ad \
    --output build/rosenbrock_ad.f90 example/rosenbrock.f90
```

The generated procedure in the checked-in example has this call:

```fortran
f_b = 1.0d0
call rosenbrock_vjp(x, f, f_b, x_b)
```

Here `x_b` is the gradient. An incoming seed other than one scales the returned
gradient and gives the general vector-Jacobian product. The
[Rosenbrock driver](../example/check_rosenbrock.f90) checks both `f` and `x_b`
against closed-form values.

Use `--no-primal` when the caller needs only the gradient. This lets dead-store
elimination remove work retained solely for the primal result.

## Hessian-vector products

```console
$ fo exec fortad --mode hessian --indep x --module rosenbrock_hvp \
    --output build/rosenbrock_hvp.f90 example/rosenbrock.f90
```

`fad_hvp` first emits a VJP and then differentiates that source in forward
mode. Seed the independent tangents with `v` and the objective cotangent with
one. The tangents of the returned independent adjoints are `Hv`.

The HVP interface accepts one direction. A dense Hessian therefore needs one
call per unit direction. Sparse recovery needs one call per color. There is no
vector-HVP entry point in 0.1.0.

[`test_hessian_oracle.f90`](../test/test_hessian_oracle.f90) checks generated
products against finite differences and checks Hessian symmetry independently.

## Linear uncertainty propagation

Let an input covariance have a factorization

```text
Sigma_x = L transpose(L).
```

First-order propagation gives

```text
Sigma_y = J Sigma_x transpose(J)
        = (J L) transpose(J L).
```

Vector forward mode computes every column of `JL` in one primal sweep. Its
direction index is the first array index. For the two-output model used by the
product oracle, the tested layout is:

```fortran
! L(input, direction); x_d(direction, input)
do j = 1, k
    do i = 1, n
        x_d(j, i) = l(i, j)
    end do
end do

call model_jvp_v(k, n, x, x_d, y1, y1_d, y2, y2_d)
jl(1, :) = y1_d
jl(2, :) = y2_d
cov_y = matmul(jl, transpose(jl))
```

Generate that vector JVP by naming the direction-count dummy:

```console
$ fo exec fortad --mode forward --indep x --directions k \
    --module model_ad --output build/model_ad.f90 model.f90
```

This approximation uses the derivative at one input point. It does not
represent skewness, threshold crossings, folds, or curvature across a wide
input distribution. Near a stationary point, first-order propagation can
return zero even when the response has a quadratic variation. Use a nonlinear
uncertainty method when those effects matter.

[`test_products_oracle.f90`](../test/test_products_oracle.f90) compares this
construction with covariance propagation through an explicit Jacobian built
from scalar tangent products.

## Sensitivity analysis

Use the Jacobian shape to select the mode:

- A few inputs and many outputs favor forward mode. Vector forward mode can
  propagate all input directions in one sweep.
- A few outputs and many inputs favor reverse mode. Run one VJP per output
  seed.
- Large input and output spaces favor matrix-free `Jv` and `u^T J` products or
  sparse compression.

A directional finite difference provides a cheap independent check. Seed a
nontrivial direction `v`, compare the generated `Jv` with
`(f(x+h*v)-f(x-h*v))/(2*h)`, and repeat over decreasing `h` to distinguish
truncation error from a wrong derivative.

## Gauss-Newton

For a least-squares residual `r(x)`, a matrix-free Gauss-Newton step needs two
operations:

1. A JVP computes `w = Jv`.
2. A VJP with residual seed `w` computes `J^T w`.

The Krylov solver consumes this composition as the action of `J^T J`. It does
not need a materialized Jacobian or normal-equations matrix. The downstream
application owns the iteration and preconditioner. It also owns storage and the
convergence policy.

## Structured procedure rules

fortad inlines supported same-file callees before differentiation. A procedure
whose body is unavailable needs one of the public rule interfaces:

- `fad_add_rule` registers one scalar partial expression per argument.
- `fad_add_call_rule` registers tangent and adjoint statement templates for a
  subroutine call.

Statement templates use `$k` for the k-th actual argument, `$kd` for its
tangent, and `$kb` for its adjoint. fortad substitutes the text without parsing
or validating its mathematics. The registrant must supply compilable Fortran
and an independent derivative check.

The structured rule boundary covers operations whose derivative should use a
different algorithm from the primal implementation. Current records include:

- an opt-in [`dgesv` rule](design/blas-lapack-rules.md) that reuses the primal
  LU factorization;
- caller-supplied products for [converged nonlinear roots](design/implicit-root-rules.md);
- a [two-phase fixed-point rule](design/fixed-point-rules.md);
- callbacks for [FFT, quadrature, interpolation, and `erf`](design/library-rules.md).

Calls inside these generated statements can make the generated procedure
impure. The statement templates also determine which external interfaces and
libraries the consumer must provide.

## Sparse derivatives

`sparsity_t` stores a structural pattern by columns. An over-full pattern costs
extra products. A pattern that omits a possible nonzero loses an entry, so use
`fad_static_pattern` for its conservative supported-source result or provide a
conservative application pattern.

For a Jacobian, columns that do not share a nonzero row can use one forward
direction:

```fortran
use fortad, only: sparsity_t, colour_columns, seed_matrix, recover_entries

call colour_columns(pattern, colour, n_colours, stat)
call seed_matrix(pattern, colour, n_colours, seeds)
call model_jvp_v(n_colours, n, x, seeds, y, compressed)
call recover_entries(pattern, colour, compressed, values)
```

`seeds(color, column)` and `compressed(color, row)` both place the direction
first. `values` follows the order of `pattern%rows`. Recovery is an exact
rearrangement when the pattern and coloring are valid.

For a symmetric Hessian, call `star_colour_columns` and
`recover_symmetric`. Run the scalar HVP once for each row of `seeds` and place
each returned `Hv` in the matching row of `compressed`:

```fortran
call star_colour_columns(pattern, colour, n_colours, stat)
call seed_matrix(pattern, colour, n_colours, seeds)
do c = 1, n_colours
    v = seeds(c, :)
    call objective_hvp(..., v, ..., hv)
    compressed(c, :) = hv
end do
call recover_symmetric(pattern, colour, compressed, values, stat)
```

The exact generated HVP argument list depends on the primal procedure. The
ellipsis above marks those primal and seed arguments. Inspect the generated
source before writing the call.

[`test_sparse_oracle.f90`](../test/test_sparse_oracle.f90) checks coloring and
recovery against explicit matrices. It also verifies that star coloring uses
two colors for a ten-column symmetric arrowhead pattern, where ordinary column
coloring needs ten.

## Checkpointing

`revolve_schedule(n_steps, n_slots, schedule)` returns a sequence of actions
for a caller-owned time loop:

```fortran
use fortad, only: revolve_schedule, revolve_t, &
                  REV_ADVANCE, REV_TAKESHOT, REV_RESTORE, REV_TURN

call revolve_schedule(n_steps, n_slots, schedule, stat)
do i = 1, schedule%n_actions
    select case (schedule%actions(i)%kind)
    case (REV_ADVANCE)
        ! Advance from %from to %to.
    case (REV_TAKESHOT)
        ! Save the state in %slot.
    case (REV_RESTORE)
        ! Restore %slot.
    case (REV_TURN)
        ! Apply the adjoint of step %from.
    end select
end do
```

The schedule does not own the state or storage. The caller must implement each
action. [`test_revolve_oracle.f90`](../test/test_revolve_oracle.f90) executes
the schedule against a simulated integration and checks the binomial cost
bound.

## Taylor mode

`fad_taylor` rewrites a straight-line scalar kernel into univariate Taylor
coefficient arithmetic. The generated routine accepts the maximum order at run
time and uses the public `tay_*` helpers. Arrays, loops, and branches are
refused in this mode.

The coefficient convention is

```text
a(k) = (1/k!) d^k/dt^k f(x + t v) at t = 0.
```

`tay_derivative(a, k)` converts `a(k)` back to the k-th directional derivative.
The transformation and helper recurrences cost `O(d^2)` per operation for
order `d`.

[`test_taylor_gen_oracle.f90`](../test/test_taylor_gen_oracle.f90) checks the
generated source. [`test_taylor_oracle.f90`](../test/test_taylor_oracle.f90)
checks the arithmetic against closed-form series.

## Choosing a mode

```text
                    few outputs        many outputs
few inputs          either             vector forward
many inputs         reverse            matrix-free or sparse products
```

Generate both modes and measure the complete downstream workload when either
choice is plausible. Compiler, input shape, memory traffic, and derivative
seeds can change the result.
