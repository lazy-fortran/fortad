#!/usr/bin/env python3
"""Fetch third-party AD projects into the gitignored upstream/ tree.

Read-only study copies. Nothing fetched here may be committed to fortad or
redistributed in any form. See LEGAL.md.

Usage:
    scripts/fetch_upstreams.py                 # fetch everything
    scripts/fetch_upstreams.py enzyme clad     # fetch selected entries
    scripts/fetch_upstreams.py --category julia
    scripts/fetch_upstreams.py --list
    scripts/fetch_upstreams.py --licenses      # re-scan checkouts, write inventory
"""

from __future__ import annotations

import argparse
import subprocess
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MANIFEST = ROOT / "docs" / "upstreams.toml"
DEST = ROOT / "upstream"
GENERATED = ROOT / "docs" / "generated"

# Entries whose `url` is not a git remote.
NON_GIT = {"web"}

LICENSE_NAMES = (
    "LICENSE", "LICENSE.txt", "LICENSE.md", "LICENCE", "LICENCE.txt",
    "COPYING", "COPYING.txt", "COPYING.LESSER", "LICENSE.TXT",
    "LICENSE-MIT", "LICENSE-APACHE", "NOTICE",
)


def load() -> list[dict]:
    with MANIFEST.open("rb") as fh:
        return tomllib.load(fh)["upstream"]


def clone(entry: dict, depth: int, jobs_note: str = "") -> bool:
    name, url, ref = entry["name"], entry["url"], entry["ref"]
    target = DEST / name

    if ref in NON_GIT:
        target.mkdir(parents=True, exist_ok=True)
        (target / "FETCH_NOTE.txt").write_text(
            f"{name}: not a git remote.\n"
            f"Documentation-only entry. Consult {url} in a browser.\n"
            "Do not copy source from registration-gated distributions.\n"
        )
        print(f"  note   {name}: documentation-only, see {url}")
        return True

    if target.exists():
        print(f"  update {name}")
        rc = subprocess.run(
            ["git", "-C", str(target), "fetch", "--depth", str(depth), "origin", ref],
            capture_output=True, text=True,
        )
        if rc.returncode != 0:
            print(f"  WARN   {name}: fetch failed: {rc.stderr.strip().splitlines()[-1:]}")
            return False
        subprocess.run(["git", "-C", str(target), "checkout", "-q", "FETCH_HEAD"], check=False)
        return True

    print(f"  clone  {name}  <- {url} @ {ref}")
    rc = subprocess.run(
        ["git", "clone", "--depth", str(depth), "--branch", ref,
         "--single-branch", url, str(target)],
        capture_output=True, text=True,
    )
    if rc.returncode != 0:
        # Some refs are commit hashes, which --branch rejects.
        rc2 = subprocess.run(["git", "clone", "--filter=blob:none", url, str(target)],
                             capture_output=True, text=True)
        if rc2.returncode != 0:
            tail = (rc.stderr or rc2.stderr).strip().splitlines()[-1:]
            print(f"  FAIL   {name}: {tail}")
            return False
        subprocess.run(["git", "-C", str(target), "checkout", "-q", ref], check=False)
    return True


def record_revision(entry: dict) -> str:
    target = DEST / entry["name"]
    rc = subprocess.run(["git", "-C", str(target), "rev-parse", "HEAD"],
                        capture_output=True, text=True)
    return rc.stdout.strip() if rc.returncode == 0 else "n/a"


def scan_licenses(entries: list[dict]) -> None:
    """Re-read licence files from the checkouts and write a local inventory.

    The inventory lands under docs/generated/, which is gitignored: it quotes
    upstream licence text, and fortad does not redistribute upstream text.
    """
    GENERATED.mkdir(parents=True, exist_ok=True)
    out = GENERATED / "license-inventory.md"
    lines = [
        "# Local licence inventory (generated, not committed)",
        "",
        "Regenerate with `scripts/fetch_upstreams.py --licenses`.",
        "Declared values come from `docs/upstreams.toml`; found values are read",
        "from the local checkout. A mismatch must be resolved before any code in",
        "fortad is written against that project.",
        "",
        "| project | pinned ref | local revision | declared | licence files found |",
        "|---|---|---|---|---|",
    ]
    mismatches = 0
    for e in entries:
        target = DEST / e["name"]
        if not target.exists():
            lines.append(f"| {e['name']} | {e['ref']} | NOT FETCHED | {e['license']} | - |")
            continue
        found = sorted(
            p.name for n in LICENSE_NAMES
            for p in target.glob(n)
        )
        if not found:
            found_s = "**NONE FOUND**"
            mismatches += 1
        else:
            found_s = ", ".join(found)
        lines.append(
            f"| {e['name']} | {e['ref']} | {record_revision(e)[:12]} | "
            f"{e['license']} | {found_s} |"
        )
    lines += ["", f"Entries with no licence file: {mismatches}.", ""]
    out.write_text("\n".join(lines))
    print(f"\nwrote {out.relative_to(ROOT)}")
    if mismatches:
        print(f"WARNING: {mismatches} checkout(s) have no discoverable licence file. "
              "Treat those as all-rights-reserved: metadata only, no study of "
              "the source, no adaptation.")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("names", nargs="*", help="entry names to fetch (default: all)")
    ap.add_argument("--category", help="fetch one category only")
    ap.add_argument("--depth", type=int, default=1, help="clone depth (default 1)")
    ap.add_argument("--list", action="store_true", help="list entries and exit")
    ap.add_argument("--licenses", action="store_true",
                    help="skip fetching, only rescan licences of existing checkouts")
    args = ap.parse_args()

    entries = load()
    if args.category:
        entries = [e for e in entries if e["category"] == args.category]
    if args.names:
        wanted = set(args.names)
        entries = [e for e in entries if e["name"] in wanted]
        missing = wanted - {e["name"] for e in entries}
        if missing:
            print(f"unknown entries: {sorted(missing)}", file=sys.stderr)
            return 2

    if args.list:
        width = max(len(e["name"]) for e in entries)
        for e in entries:
            print(f"{e['name']:<{width}}  {e['category']:<11}  {e['license']}")
        return 0

    if args.licenses:
        scan_licenses(entries)
        return 0

    DEST.mkdir(parents=True, exist_ok=True)
    print(f"fetching {len(entries)} upstream(s) into {DEST}\n")
    failed = [e["name"] for e in entries if not clone(e, args.depth)]
    scan_licenses(entries)
    if failed:
        print(f"\nfailed: {', '.join(failed)}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
