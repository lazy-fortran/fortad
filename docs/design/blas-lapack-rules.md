# BLAS/LAPACK structured rules

P3.2 adds an explicit rule table for the external dense linear-solve path.
The first built-in entry is `dgesv`; the registry remains available for other
BLAS/LAPACK routines and project-specific interfaces.

## Registration and interface contract

Call `fad_register_blas_lapack_rules()` after `fad_clear_rules()` and before
generating a derivative. The generated source does not provide BLAS/LAPACK
interfaces or link libraries: the consumer must provide the explicit Fortran
interfaces and link the implementation, for example LAPACK and BLAS.

The registered signature is the standard

```fortran
call dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
```

with real double-precision `a` and `b`. `dgesv` overwrites `a` with its LU
factorisation and `b` with the solution `x`; those post-call values are part of
the rule contract. The current reverse emitter accepts simple variable actuals
outside loops. General expressions, calls inside loops, and other mutating
interfaces remain explicit future work.

## Rule table

For `A X = B`, the forward identity is

\[
  A\,dX = dB - dA\,X.
\]

The registered tangent body therefore performs:

```fortran
call dgemm('N', 'N', n, nrhs, n, -1.0d0, a_d, lda, b, ldb, &
           1.0d0, b_d, ldb)
call dgetrs('N', n, nrhs, a, lda, ipiv, b_d, ldb, info)
```

It reuses the LU factorisation already left in `a` by the primal solve.

For a scalar objective with solution cotangent `X_b`, the reverse identity is

\[
  A^T\,\lambda = X_b,\qquad
  A_b \mathrel{-}= \lambda X^T,\qquad
  B_b = \lambda.
\]

The registered adjoint body performs the transposed solve in-place in `b_b`,
then applies the matrix update:

```fortran
call dgetrs('T', n, nrhs, a, lda, ipiv, b_b, ldb, info)
call dgemm('N', 'T', n, n, nrhs, -1.0d0, b_b, ldb, b, ldb, &
           1.0d0, a_b, lda)
```

The reverse sweep's mutation barrier preserves the pre-call `B_b` contribution
when the call is followed by other uses of the right-hand side.

## Evidence and decision

`test/test_lapack_rule_oracle.f90` generates both modes, links the result to
real LAPACK/BLAS on the TU Graz `acluster`, checks a complete-solve central
finite difference for the JVP, and checks the VJP identity independently.

The performance comparison is recorded in
`fortad-bench/results/p32_blas_lapack_validation.txt`. The existing TU Graz
Ryzen 9 direct-solve fixture measured analytical versus Enzyme at 16 products:
642.542350 ns versus 2449.369550 ns for JVP (3.8120x), and 637.162500 ns
versus 974.323500 ns for VJP (1.5292x). That fixture uses a hand-written dense
solve and Flang/LLVM 22.1.8 with Enzyme 22; it is a cross-record comparison,
not an apples-to-apples timing of this gfortran-generated `dgesv` routine.

Decision: keep the built-in `dgesv` rule as explicit opt-in infrastructure and
do not add automatic hybrid selection. Extend the table only when a routine's
mutating interface and an independent oracle are specified. Reconsider the
performance policy for a representative larger matrix or downstream workload.
