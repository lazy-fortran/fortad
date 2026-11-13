# Changelog

## Unreleased

- Pin the parser and source-generation dependencies so a single checkout builds
  without sibling repositories.
- Guard declaration lookups before indexing and allow recursive lowering of
  nested call arguments in runtime-checked builds.
- Add citation metadata and contributor guidance; keep capability and verification
  limits in the user documentation.

## 0.1.0

Initial source-transformation interface, including forward and reverse products,
Hessian-vector products, sparse recovery, and bounded higher-order propagation.
See [supported Fortran and limits](docs/support.md) for the implemented subset.
