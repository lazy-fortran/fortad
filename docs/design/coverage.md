# fortfront coverage of fortnum

## Measurement

The corpus measurement ran on 2026-08-05 on `acluster`, an AMD EPYC 7282
machine. The corpus was the 161 Fortran source files under `fortnum/src` at
fortnum commit `a466951cd4702b2ba6ee36f62897e2530500992f`. The frontend probe
came from fortfront commit `0a0fcd23ecb32b4b51162d840dd060e00ae7544c` and was
run sequentially on one pinned core.

The probe called `compile_frontend_from_file` with semantic analysis enabled.
A file counted as accepted only when both `parse_ok` and `semantic_ok` were
true. The scan took 13.21 seconds and used 22,604 kB peak RSS.

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
- `test_compiler_scope_resolution.f90` checks direct, host, use-associated,
  block, associate, procedure, and generic-interface bindings.
- `test_compiler_program_unit_queries.f90` checks derived-type components and
  type-bound procedure bindings, along with program-unit and declaration
  queries.

These APIs expose the type and binding information required by fortad's
lowering boundary. The coverage gate therefore selects fortfront as the
frontend for the measured fortnum corpus. LFortran ASR remains a fallback for
sources outside that measured boundary.

## Reproduction

The scan used the existing fortfront `frontend_probe` executable and the
following per-file condition:

```text
accepted = parse_ok and semantic_ok
```

The raw per-file scan was retained on the cluster as
`/home/ert/fortad-p0-4/coverage.psv` during the measurement.
