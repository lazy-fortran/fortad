#!/usr/bin/env python3
"""Resolve fortad's bibliography and fetch only openly licensed full text.

Policy (see LEGAL.md):
  * Bibliographic metadata is a fact and is committed to this repository.
  * Publisher PDFs are not. This script downloads into the gitignored
    `literature/` tree and never into the repository proper.
  * It downloads only from arXiv and from DOIs that Unpaywall reports as having
    an open-access location with a licence. Everything else is listed as
    "retrieve yourself" with its identifier, so the reader can use their own
    institutional access or Zotero.

Usage:
    scripts/fetch_literature.py --resolve      # Crossref/arXiv lookup only
    scripts/fetch_literature.py --fetch        # resolve, then download OA copies
    scripts/fetch_literature.py --report       # print what is and is not local
"""

from __future__ import annotations

import argparse
import json
import re
import sys
import time
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BIB = ROOT / "docs" / "bibliography.bib"
DEST = ROOT / "literature"
RESOLVED = DEST / "resolved.json"

# Unpaywall requires a contact address. Override with FORTAD_CONTACT_EMAIL.
import os
EMAIL = os.environ.get("FORTAD_CONTACT_EMAIL", "albert@tugraz.at")
UA = "fortad-literature/1 (https://github.com/lazy-fortran/fortad; mailto:%s)" % EMAIL

ENTRY_RE = re.compile(r"@(\w+)\{([^,]+),(.*?)\n\}", re.S)
FIELD_RE = re.compile(r"(\w+)\s*=\s*\{(.*?)\}\s*,?\s*\n", re.S)


def strip_tex(s: str) -> str:
    s = re.sub(r"[{}\\]", "", s)
    s = re.sub(r"\s+", " ", s)
    return s.strip()


def parse_bib() -> list[dict]:
    text = BIB.read_text()
    out = []
    for kind, key, body in ENTRY_RE.findall(text):
        fields = {k.lower(): strip_tex(v) for k, v in FIELD_RE.findall(body + "\n")}
        fields["_key"] = key.strip()
        fields["_type"] = kind
        out.append(fields)
    return out


def get_json(url: str, timeout: int = 20):
    req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return json.loads(r.read().decode("utf-8", "replace"))
    except Exception as exc:                                   # network is optional
        return {"_error": str(exc)}


def crossref(entry: dict) -> dict | None:
    q = urllib.parse.urlencode({
        "query.bibliographic": f"{entry.get('title','')} {entry.get('author','')}",
        "rows": 1, "mailto": EMAIL,
    })
    data = get_json(f"https://api.crossref.org/works?{q}")
    items = (data.get("message") or {}).get("items") or []
    if not items:
        return None
    it = items[0]
    got = strip_tex((it.get("title") or [""])[0]).lower()
    want = entry.get("title", "").lower()
    # Reject weak matches rather than record a wrong DOI.
    overlap = len(set(got.split()) & set(want.split()))
    if overlap < max(3, len(want.split()) // 3):
        return None
    return {
        "doi": it.get("DOI"),
        "matched_title": strip_tex((it.get("title") or [""])[0]),
        "container": (it.get("container-title") or [""])[0],
        "year": (it.get("issued", {}).get("date-parts") or [[None]])[0][0],
    }


def unpaywall(doi: str) -> dict | None:
    data = get_json(f"https://api.unpaywall.org/v2/{doi}?email={EMAIL}")
    loc = data.get("best_oa_location")
    if not loc:
        return None
    return {"url": loc.get("url_for_pdf") or loc.get("url"),
            "license": loc.get("license"),
            "version": loc.get("version"),
            "host": loc.get("host_type")}


def arxiv(entry: dict) -> dict | None:
    q = urllib.parse.urlencode({
        "search_query": f'ti:"{entry.get("title","")[:120]}"',
        "max_results": 1,
    })
    req = urllib.request.Request(f"http://export.arxiv.org/api/query?{q}",
                                 headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(req, timeout=20) as r:
            xml = r.read().decode("utf-8", "replace")
    except Exception:
        return None
    m = re.search(r"<id>http://arxiv\.org/abs/([^<]+)</id>", xml[xml.find("<entry>"):]) \
        if "<entry>" in xml else None
    if not m:
        return None
    return {"arxiv_id": m.group(1), "pdf": f"https://arxiv.org/pdf/{m.group(1)}"}


def download(url: str, dest: Path) -> bool:
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(req, timeout=60) as r:
            data = r.read()
    except Exception as exc:
        print(f"    download failed: {exc}")
        return False
    if not data.startswith(b"%PDF"):
        print("    not a PDF, skipped")
        return False
    dest.write_bytes(data)
    return True


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--resolve", action="store_true")
    ap.add_argument("--fetch", action="store_true")
    ap.add_argument("--report", action="store_true")
    args = ap.parse_args()
    if not (args.resolve or args.fetch or args.report):
        ap.print_help()
        return 0

    DEST.mkdir(parents=True, exist_ok=True)
    entries = parse_bib()

    if args.report:
        state = json.loads(RESOLVED.read_text()) if RESOLVED.exists() else {}
        have = sorted(p.stem for p in DEST.glob("*.pdf"))
        print(f"{len(entries)} bibliography entries, {len(have)} local PDFs\n")
        for e in entries:
            k = e["_key"]
            mark = "PDF" if k in have else "   "
            doi = (state.get(k) or {}).get("doi", "")
            print(f"  [{mark}] {k:<28} {doi}")
        print("\nEntries without a local PDF are not missing data. Retrieve them"
              "\nthrough your own institutional access or Zotero if you need the"
              "\nfull text; fortad neither ships nor requires them.")
        return 0

    state = json.loads(RESOLVED.read_text()) if RESOLVED.exists() else {}
    for e in entries:
        key = e["_key"]
        rec = state.setdefault(key, {"title": e.get("title", "")})
        print(f"{key}")
        if "doi" not in rec:
            cr = crossref(e)
            if cr:
                rec.update(cr)
                print(f"    crossref doi={cr['doi']}")
            else:
                print("    crossref: no confident match (DOI left unset on purpose)")
            time.sleep(0.4)
        if "arxiv_id" not in rec:
            ax = arxiv(e)
            if ax:
                rec.update(ax)
                print(f"    arxiv {ax['arxiv_id']}")
            time.sleep(0.4)
        if rec.get("doi") and "oa" not in rec:
            oa = unpaywall(rec["doi"])
            rec["oa"] = oa
            if oa:
                print(f"    open access: {oa.get('license')} via {oa.get('host')}")
            time.sleep(0.3)

        if args.fetch:
            target = DEST / f"{key}.pdf"
            if target.exists():
                continue
            url = None
            if rec.get("pdf"):
                url = rec["pdf"]                      # arXiv: author-posted
            elif rec.get("oa") and rec["oa"].get("license"):
                url = rec["oa"]["url"]                # licensed OA copy only
            if url:
                print(f"    fetching {url}")
                if download(url, target):
                    print(f"    -> literature/{key}.pdf")
            else:
                print("    no openly licensed copy; retrieve via your own access")

    RESOLVED.write_text(json.dumps(state, indent=2, sort_keys=True))
    print(f"\nwrote {RESOLVED.relative_to(ROOT)} (gitignored)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
