import json, glob, os, pickle, collections
P = os.path.expanduser("~/.claude/projects/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys")
SID = "ac14b90b-b4fc-47f6-b280-5ec7accc121e"
files = [f"{P}/{SID}.jsonl"] + sorted(glob.glob(f"{P}/{SID}/subagents/*.jsonl"))
occ = collections.defaultdict(list)
for f in files:
    src = os.path.basename(f)
    with open(f, encoding="utf-8") as fh:
        for n, line in enumerate(fh):
            try: e = json.loads(line)
            except Exception: continue
            c = (e.get("message") or {}).get("content")
            if not isinstance(c, list): continue
            for b in c:
                if isinstance(b, dict) and b.get("type") == "tool_use":
                    occ[b["id"]].append((src, n, e.get("timestamp"), e.get("isSidechain"), e.get("agentId")))
pickle.dump(dict(occ), open("occ.pkl", "wb"))
multi = {k: v for k, v in occ.items() if len(v) > 1}
print("calls in >1 file:", len(multi))
print(collections.Counter(tuple(sorted(set(x[0][6:15] for x in v))) for v in multi.values()))
print(collections.Counter(tuple((x[0][6:15], x[2] is None, x[4]) for x in v)[:3] for v in list(multi.values())[:5]))
