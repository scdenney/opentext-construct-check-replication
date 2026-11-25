import pickle
calls, results = pickle.load(open("calls.pkl","rb"))
occ = pickle.load(open("occ.pkl","rb"))
FORKS = {"agent-a159ac9719aee5a59.jsonl","agent-abb3ff4abafc1417b.jsonl","agent-ac675e364395e6544.jsonl","agent-af15fcc752ca7a957.jsonl"}
def owner(tid):
    v = occ[tid]; files = [x[0] for x in v]
    nonfork = [f for f in files if f not in FORKS]
    if nonfork: return nonfork[0]
    # call only in forks: if only in one fork, that fork owns it
    return sorted(set(files))[0] if len(set(files)) == 1 else "MULTI:" + ",".join(sorted(set(files)))
def ts(tid):
    t = [x[2] for x in occ[tid] if x[2]]
    return t[0] if t else ""
def line(tid, f):
    l = [x[1] for x in occ[tid] if x[0] == f and x[2]]
    return l[0] if l else min(x[1] for x in occ[tid] if x[0]==f)
CALLS = []
for (src, n, k, tid, name, inp, t, u) in calls:
    o = owner(tid)
    CALLS.append(dict(id=tid, name=name, inp=inp, owner=o, ts=ts(tid), line=line(tid, o) if not o.startswith("MULTI") else -1,
                      err=results.get(tid, (None,))[0], res=results.get(tid, (None, ""))[1]))
CALLS.sort(key=lambda c: (c["ts"], c["owner"], c["line"]))
