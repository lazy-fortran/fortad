# fortfront coverage of fortnum

## Measurement

The corpus measurement ran on 2026-08-05 on an AMD EPYC 7282
machine. The corpus was the 161 Fortran source files under `fortnum/src` at
fortnum commit `5bfdde8b8400486d33de73f22e5b4e31d7d86687`. The frontend probe
came from fortfront commit `896fe8d5fbb0d4dc4ee9113510de5c8acef92021` and was
run sequentially on one pinned core.

The probe called `compile_frontend_from_file` with semantic analysis enabled.
A file counted as accepted only when both `parse_ok` and `semantic_ok` were
true.

| result | files |
| --- | ---: |
| parsed and semantically accepted | 161 |
| rejected | 0 |
| total | 161 |

No unsupported construct was observed in this corpus. This result bounds the
claim to the constructs present in fortnum. It does not establish complete
Fortran language coverage.

## Query usability

The representative `dot_sin` kernel parses and analyzes through the same
frontend API. The query surface needed by lowering is exercised by fortfront's
focused API tests:

- `test_compiler_resolved_type_query.f90` checks resolved kinds for literals,
  declarations, identifiers, binary expressions, intrinsic calls, function
  references, arrays, and unresolved names.
- `test_compiler_scope_resolution.f90` checks direct, host, and use-associated
  bindings. It also covers block and associate scopes, procedure names, and
  generic interfaces.
- `test_compiler_program_unit_queries.f90` checks derived-type components and
  type-bound procedure bindings, along with program-unit and declaration
  queries.

These APIs expose the type and binding information required by fortad's
lowering boundary. The coverage gate therefore selects fortfront as the
frontend for the measured fortnum corpus. LFortran ASR remains a fallback for
sources outside that measured boundary.

## Reproduction

At the fortfront commit named above, run `fo exec frontend_probe FILE` from the
fortfront checkout for each `.f90` file under the named fortnum tree. Count a
file as accepted under this condition:

```text
accepted = parse_ok and semantic_ok
```
