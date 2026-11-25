#!/usr/bin/env python3
"""Add a .bib entry for every public study in the validation corpus.

Reads revision/dataset_citations.csv (dataset, bibkey, doi). For each bibkey
that paper/revision/references.bib does not yet hold, fetches the Crossref
record of the DOI and appends an @article entry. Titles are double-braced so
that proper nouns keep their case. Run from the project root:
    python3 -I analysis/revision/rv_14_dataset_bib.py [--dry-run]
"""
import csv, html, json, re, sys, time, urllib.request, urllib.parse

BIB = "paper/revision/references.bib"
MAP = "revision/dataset_citations.csv"

def tex(s):
    s = html.unescape(re.sub(r"<[^>]+>", "", s or ""))
    s = s.replace("\u2010", "-").replace("\u2011", "-")
    s = s.replace("’", "'").replace("‘", "`").replace("“", "``").replace("”", "''")
    s = s.replace("–", "--").replace("—", "---").replace(" ", " ")
    s = re.sub(r"([&%#_])", r"\\\1", s)
    return re.sub(r"\s+", " ", s).strip()

def fetch(doi):
    url = "https://api.crossref.org/works/" + urllib.parse.quote(doi, safe="/")
    req = urllib.request.Request(url, headers={"User-Agent": "bib-builder/1.0"})
    with urllib.request.urlopen(req, timeout=60) as r:
        return json.load(r)["message"]

def entry(key, doi, m):
    names = [f"{a.get('family', a.get('name', ''))}, {a.get('given', '')}".rstrip(", ") for a in m.get("author", [])]
    if len(names) > 10:   # consortium papers: three names and "et al."
        names = names[:3] + ["others"]
    authors = " and ".join(names)
    year = None
    for f in ("published-print", "issued", "published-online", "published"):
        parts = (m.get(f) or {}).get("date-parts") or [[None]]
        if parts[0][0]:
            year = parts[0][0]; break
    pages = (m.get("page") or m.get("article-number") or "").replace("-", "--")
    if pages and pages in doi:   # online first: Crossref repeats the DOI suffix as the page
        pages = ""
    fields = [("author", authors), ("title", "{" + tex((m.get("title") or [""])[0]) + "}"),
              ("journal", tex((m.get("container-title") or [""])[0])), ("volume", m.get("volume")),
              ("number", m.get("issue")), ("pages", pages),
              ("year", str(year) if year else None), ("doi", doi)]
    body = ",\n".join(f"  {k} = {{{tex(v) if k in ('author',) else v}}}" for k, v in fields if v)
    return f"@article{{{key},\n{body}\n}}\n"

def main():
    dry = "--dry-run" in sys.argv
    bib = open(BIB, encoding="utf-8").read()
    have = set(re.findall(r"@\w+\{([^,\s]+),", bib))
    todo = {}
    for row in csv.DictReader(open(MAP, encoding="utf-8")):
        if row["bibkey"] and row["bibkey"] not in have and row["doi"]:
            todo[row["bibkey"]] = row["doi"]
    out = []
    for key, doi in sorted(todo.items()):
        try:
            out.append(entry(key, doi, fetch(doi)))
        except Exception as e:  # report and go on: a missing entry shows up as a BibTeX warning
            print(f"FAILED {key} {doi}: {e}", file=sys.stderr)
        time.sleep(1)
    text = "".join("\n" + e for e in out)
    if dry:
        print(text)
    elif out:
        with open(BIB, "a", encoding="utf-8") as fh:
            if "% Public studies in the validation corpus" not in bib:
                fh.write("\n% Public studies in the validation corpus (analysis/revision/rv_14_dataset_bib.py)\n")
            fh.write(text)
    print(f"{len(out)} entries {'printed' if dry else 'added'}; {len(todo) - len(out)} failed", file=sys.stderr)

if __name__ == "__main__":
    main()
