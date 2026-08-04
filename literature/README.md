# literature/ — local paper copies, never committed

Everything in this directory except this file is gitignored.

```bash
scripts/fetch_literature.py --resolve   # Crossref/arXiv/Unpaywall lookup
scripts/fetch_literature.py --fetch     # download openly licensed copies only
scripts/fetch_literature.py --report    # what is local, what is not
```

The script downloads only arXiv preprints and DOIs that Unpaywall reports as
having a licensed open-access location. Paywalled items are listed with their
identifier and nothing else; retrieve those through your own institutional
access or Zotero.

Bibliographic metadata lives in [`../docs/bibliography.bib`](../docs/bibliography.bib)
and is committed, because a citation is a fact. Full text is not committed,
because it is someone else's work. See [../LEGAL.md](../LEGAL.md).
