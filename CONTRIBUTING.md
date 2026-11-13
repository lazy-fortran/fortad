# Contributing

Use the [issue tracker](https://github.com/lazy-fortran/fortad/issues) for defects
and proposed extensions. A report should include the input procedure, the command,
the compiler version, and the expected derivative or diagnostic.

Build and run the native test suite with `fo check`. A plain `fo` also runs lint
and formatting checks. Dependencies are pinned in `fpm.toml`; a dependency change
must be checked with the transformation and generated-code oracles.

Add regression coverage in `test/` for a change in accepted input or emitted
code. Check generated derivatives against an analytical result, a finite-difference
step sweep, or another independent oracle. Agreement with another automatic
differentiation tool is supporting evidence.

Keep expensive benchmarks and external toolchains in
[fortad-bench](https://github.com/lazy-fortran/fortad-bench). Record workload,
compiler, flags, engine versions and correctness checks for performance claims.
Read [LEGAL.md](LEGAL.md) and update [PROVENANCE.md](PROVENANCE.md) before adapting
third-party material.
