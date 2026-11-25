# Compare rebuilt harmonized files with the originals. Prints hashes, shapes, column names and diff COUNTS only.
import csv, hashlib, json, os, sys, glob, collections
csv.field_size_limit(10**9)
REPO = "/home/buddleman/Documents/github/research/projects/lucdh_opentext-surveys"
RB = "/tmp/claude-1000/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/bfe7aca0-b2db-41ad-bf8b-6a8c26510331/scratchpad/rebuild"
def sha(p): return hashlib.sha256(open(p, "rb").read()).hexdigest()
def rd(p):
    with open(p, newline="", encoding="utf-8") as f:
        r = list(csv.reader(f))
    return r[0], r[1:]
pairs = [(p, os.path.join(RB, "own", os.path.basename(p))) for p in sorted(glob.glob(REPO + "/data/harmonized/*.csv"))]
pairs += [(p, os.path.join(RB, "external", p.split("/")[-2], os.path.basename(p))) for p in sorted(glob.glob(REPO + "/data/external/*/harmonized_*.csv"))]
pairs += [(REPO + "/data/harmonized/OWN_INDEX.md", RB + "/own/OWN_INDEX.md")]
res = []
for gt, rb in pairs:
    card = os.path.basename(gt).replace("harmonized_", "").replace(".csv", "")
    r = {"card": card, "gt": gt[len(REPO)+1:], "gt_sha": sha(gt)}
    if not os.path.exists(rb): r["status"] = "MISSING"; res.append(r); continue
    r["rb_sha"] = sha(rb)
    if r["gt_sha"] == r["rb_sha"]: r["status"] = "IDENTICAL"; res.append(r); continue
    if gt.endswith(".md"):
        a = open(gt, encoding="utf-8").read().splitlines(); b = open(rb, encoding="utf-8").read().splitlines()
        r.update(status="DIFFERS", lines=(len(a), len(b)), lines_differ=sum(x != y for x, y in zip(a, b)) + abs(len(a) - len(b)))
        res.append(r); continue
    ha, ra = rd(gt); hb, rbr = rd(rb)
    r.update(status="DIFFERS", dims_gt=(len(ra), len(ha)), dims_rb=(len(rbr), len(hb)),
             cols_same=ha == hb, cols_only_gt=[c for c in ha if c not in hb], cols_only_rb=[c for c in hb if c not in ha])
    # align on id when ids are unique in both, else by position
    ia, ib = ha.index("id") if "id" in ha else None, hb.index("id") if "id" in hb else None
    ida = [x[ia] for x in ra]; idb = [x[ib] for x in rbr]
    if len(set(ida)) == len(ida) and len(set(idb)) == len(idb):
        mb = {k: n for n, k in enumerate(idb)}
        r["align"] = "id"; r["ids_only_gt"] = sum(k not in mb for k in ida); r["ids_only_rb"] = len(set(idb) - set(ida))
        r["row_order_same"] = ida == idb
        matched = [(x, rbr[mb[x[ia]]]) for x in ra if x[ia] in mb]
    else:
        r["align"] = "position"; matched = list(zip(ra, rbr))
    diffs = collections.OrderedDict()
    for c in ha:
        if c in hb:
            ja, jb = ha.index(c), hb.index(c)
            diffs[c] = sum(1 for x, y in matched if x[ja] != y[jb])
    r["cell_diffs"] = {k: v for k, v in diffs.items() if v}
    # bytes-only difference (quoting / line endings) when cells all agree
    r["cells_equal"] = (len(ra) == len(rbr) and ha == hb and not r["cell_diffs"] and r.get("ids_only_gt", 0) == 0)
    res.append(r)
json.dump(res, open(os.path.join(RB, "compare.json"), "w"), indent=1)
for r in res:
    print(r["card"], r["status"], {k: v for k, v in r.items() if k not in ("card", "status", "gt", "gt_sha", "rb_sha")})
