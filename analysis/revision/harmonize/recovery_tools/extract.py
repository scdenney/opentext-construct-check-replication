# Index all tool calls in the October session transcripts, in position order.
import json, glob, os, pickle
P = os.path.expanduser("~/.claude/projects/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys")
SID = "ac14b90b-b4fc-47f6-b280-5ec7accc121e"
files = [f"{P}/{SID}.jsonl"] + sorted(glob.glob(f"{P}/{SID}/subagents/*.jsonl"))
calls = []   # (file, line, idx, id, name, input, ts, sidechain)
results = {} # id -> (is_error, text)
seen = {}
for f in files:
    src = os.path.basename(f)
    with open(f, encoding="utf-8") as fh:
        for n, line in enumerate(fh):
            try: e = json.loads(line)
            except Exception: continue
            msg = e.get("message") or {}
            c = msg.get("content")
            if not isinstance(c, list): continue
            for k, b in enumerate(c):
                if not isinstance(b, dict): continue
                if b.get("type") == "tool_result":
                    t = b.get("content")
                    if isinstance(t, list):
                        t = "\n".join(x.get("text", "") for x in t if isinstance(x, dict))
                    results.setdefault(b.get("tool_use_id"), (bool(b.get("is_error")), t or "", src, n))
                elif b.get("type") == "tool_use":
                    tid = b.get("id")
                    if tid in seen: continue
                    seen[tid] = src
                    calls.append((src, n, k, tid, b.get("name"), b.get("input") or {}, e.get("timestamp"), e.get("uuid")))
pickle.dump((calls, results), open(os.path.join(os.path.dirname(__file__), "calls.pkl"), "wb"))
print(len(calls), "unique calls;", len(results), "results")
import collections
print(collections.Counter((c[0], c[4]) for c in calls).most_common(80))
