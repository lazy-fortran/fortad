# Rosenbrock reverse-mode example

[`rosenbrock.f90`](rosenbrock.f90) contains an ordinary Fortran subroutine.
The commands below generate its reverse-mode derivative, compile the generated
module with [`check_rosenbrock.f90`](check_rosenbrock.f90), and compare the
result with the analytic value and gradient at `x = (-1.2, 1.0)`.

Run the commands from the repository root after completing the build described
in the [main README](../README.md#build):

```console
$ mkdir -p build/example
$ fo exec fortad vjp example/rosenbrock.f90 \
    --module rosenbrock_ad --output build/example/rosenbrock_ad.f90
$ gfortran -std=f2018 -o build/example/check_rosenbrock \
    build/example/rosenbrock_ad.f90 example/check_rosenbrock.f90
$ build/example/check_rosenbrock
f =  24.2, gradient =  -215.6   -88.0
```

The driver is an independent behavioral check. It compares the generated VJP
with the closed-form Rosenbrock gradient instead of comparing fortad output
with another AD implementation.

Change `--mode reverse` to `--mode forward` for a JVP. Forward mode adds an
`x_d` input seed and an `f_d` output. Use `--mode hessian` for a
forward-over-reverse Hessian-vector product. The generated argument list is the
source of truth for each call, so inspect
`build/example/rosenbrock_ad.f90` before writing a caller.

`fpm.toml` sets `auto-examples = false`. The files in this directory are CLI
inputs and documentation fixtures, so the package build does not compile them
automatically.
