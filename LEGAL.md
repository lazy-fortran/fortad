# Legal and distribution policy for fortad

fortad studies a large body of third-party automatic-differentiation software
and literature. This document states what fortad does and does not distribute,
and the rules contributors follow so that the result stays distributable under
its own MIT licence.

**This is not legal advice.** It records the project's engineering policy and
the reasoning behind it.

## 1. fortad's own licence

fortad is MIT licensed. See [LICENSE](LICENSE). Everything under `src/`,
`test/`, `app/`, `scripts/` and `docs/` is original work written for this
project unless a file header says otherwise.

## 2. The one rule that matters

**fortad never redistributes anyone else's code or anyone else's literature.**

Concretely:

- `upstream/` and `literature/` are gitignored in full. They are populated
  locally by `scripts/fetch_upstreams.py` and `scripts/fetch_literature.py` and
  they never enter a commit, a release tarball, or a container image.
- No upstream source file is copied, vendored, or transcribed into fortad.
- No publisher PDF, no figure, and no formatted excerpt from a paper is
  committed. Bibliographic metadata — author, title, venue, year, DOI — is
  factual and is committed, in [`docs/bibliography.bib`](docs/bibliography.bib).
- `docs/generated/` is gitignored because generated inventories quote upstream
  licence text.

If a commit would add a file under `upstream/`, `literature/`, or
`docs/generated/`, it is wrong. The `.gitignore` enforces this, and a CI check
should too.

## 3. Study is not adaptation

Reading a publicly available source file to understand an algorithm is
permitted for every project listed in `docs/upstreams.toml`, subject to §5.
Copying it is not. The two are separated by an explicit boundary:

| Activity | Allowed | Record required |
|---|---|---|
| Read upstream source to understand an algorithm | yes | note in `PROVENANCE.md` |
| Implement an algorithm from its **publication** | yes | `PROVENANCE.md` row citing the paper |
| Reproduce upstream's *interface* (function names, argument order) | case by case | `PROVENANCE.md` row; interfaces are weakly protected but not always unprotected |
| Copy, translate, or closely paraphrase upstream **code** | **only** from a permissive licence, and only with a `PROVENANCE.md` row carrying the upstream revision, file list, licence notice, and deviations | mandatory before merge |
| Copy code from a GPL or LGPL project | **never** | — |
| Copy code from a project with no discoverable licence | **never** | — |

Every entry in `docs/upstreams.toml` currently declares `adapt = "none"`. That
is the default and it changes only through a reviewed pull request that adds the
`PROVENANCE.md` row first.

## 4. Clean-room boundaries for copyleft projects

Several of the most instructive systems are copyleft: Clad (LGPL), CoDiPack
(GPL-3), CasADi (LGPL), ColPack (LGPL), pyadjoint (LGPL), SU2 (LGPL). They are
listed because their *published algorithms* are the state of the art, not
because their code is available to us.

For these projects:

- Read the papers and documentation first, and prefer them as the source.
- Reading the source to resolve an ambiguity in a paper is acceptable; writing
  fortad code with that source open is not.
- fortad has **no build-time or run-time dependency** on any of them. There is
  no linking question to answer because there is no linking.
- Where a copyleft project is used as a benchmark baseline, it is invoked as a
  separate process or a separate build, and its numbers — not its code — enter
  fortad.

## 5. Registration-gated and unlicensed distributions

ADIFOR and TAF/TAC++ are distributed under restrictive or commercial terms.
fortad uses their **published papers and manuals only**. Their source is not
fetched, not read, and not present in `docs/upstreams.toml` as a clone target.

A checkout with no discoverable licence file is treated as all-rights-reserved.
`scripts/fetch_upstreams.py --licenses` flags these explicitly. Such a project
may be cited as metadata and may be run as a black-box baseline; its source is
not a study target.

## 6. Benchmark inputs and corpora

Benchmark *problems* carry their own terms. ADBench is MIT and its workloads may
be ported with attribution. MITgcm is MIT. SU2 is LGPL: its cases are run, never
ported. Where fortad needs a workload it cannot license, it writes its own from
the mathematical statement and records that in `PROVENANCE.md`.

Any workload ported from an upstream corpus gets a `PROVENANCE.md` row naming
the upstream revision, file, and licence, and carries the upstream copyright
notice in the ported file's header.

## 7. Generated code

Fortran source that fortad emits from a user's program is a derivative of the
*user's* program, not of fortad. fortad claims no copyright in its output and
imposes no licence on it. Emitted files carry a header saying so.

## 8. Contributor checklist

Before opening a pull request:

1. `git status --porcelain upstream literature docs/generated` is empty.
2. No new file contains a copyright notice belonging to someone else without a
   corresponding `PROVENANCE.md` row.
3. Any new algorithm has a `PROVENANCE.md` row citing its publication.
4. Any new upstream added to `docs/upstreams.toml` has a verified licence and
   `adapt = "none"` unless §3 has been satisfied.
