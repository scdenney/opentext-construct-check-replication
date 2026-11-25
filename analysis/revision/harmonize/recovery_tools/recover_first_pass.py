import json, glob, os, sys, re, collections
P = os.path.expanduser("~/.claude/projects/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys")
SID = "ac14b90b-b4fc-47f6-b280-5ec7accc121e"
PREFIX = f"/tmp/claude-1000/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/{SID}/scratchpad/"
files = [f"{P}/{SID}.jsonl"] + sorted(glob.glob(f"{P}/{SID}/subagents/*.jsonl"))
ops, errors, bash = [], set(), []
for f in files:
    with open(f, encoding="utf-8") as fh:
        for n, line in enumerate(fh):
            try: e = json.loads(line)
            except Exception: continue
            msg = e.get("message") or {}
            content = msg.get("content")
            if not isinstance(content, list): continue
            ts = e.get("timestamp", "")
            for b in content:
                if not isinstance(b, dict): continue
                if b.get("type") == "tool_result" and b.get("is_error"): errors.add(b.get("tool_use_id"))
                if b.get("type") != "tool_use": continue
                inp = b.get("input") or {}
                name = b.get("name")
                fp = inp.get("file_path", "")
                if name in ("Write", "Edit") and fp.startswith(PREFIX):
                    ops.append((ts, n, os.path.basename(f), name, b.get("id"), fp, inp))
                elif name == "Bash" and PREFIX.rstrip("/") in inp.get("command", "") or (name == "Bash" and "scratchpad" in inp.get("command", "")):
                    cmd = inp.get("command", "")
                    if re.search(r"(cat|tee)\s*>+?\s*\S*scratchpad|>\s*\S*scratchpad\S*\.(R|py|sh)|<<", cmd) and "scratchpad" in cmd:
                        bash.append((ts, os.path.basename(f), b.get("id"), cmd))
ops.sort(key=lambda x: (x[0], x[1]))
state, log = {}, collections.defaultdict(list)
for ts, n, src, name, tid, fp, inp in ops:
    if tid in errors: log[fp].append(f"{ts} {name} FAILED (skipped)"); continue
    if name == "Write":
        state[fp] = inp.get("content", ""); log[fp].append(f"{ts} Write {len(state[fp])} chars [{src}]")
    else:
        cur = state.get(fp)
        old, new = inp.get("old_string", ""), inp.get("new_string", "")
        if cur is None: log[fp].append(f"{ts} Edit with no prior Write: UNRECOVERABLE BASE"); continue
        if old not in cur: log[fp].append(f"{ts} Edit old_string not found: SKIPPED"); continue
        state[fp] = cur.replace(old, new) if inp.get("replace_all") else cur.replace(old, new, 1)
        log[fp].append(f"{ts} Edit ok")
out = sys.argv[1]
for fp, txt in state.items():
    rel = fp[len(PREFIX):]
    dst = os.path.join(out, rel)
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    with open(dst, "w", encoding="utf-8") as fh: fh.write(txt)
with open(os.path.join(out, "_replay_log.txt"), "w") as fh:
    for fp in sorted(log):
        fh.write(fp[len(PREFIX):] + "\n" + "".join("   " + l + "\n" for l in log[fp]))
with open(os.path.join(out, "_bash_writes.jsonl"), "w") as fh:
    for ts, src, tid, cmd in bash: fh.write(json.dumps({"ts": ts, "src": src, "error": tid in errors, "cmd": cmd}) + "\n")
print(len(state), "files recovered;", len(bash), "bash commands that may write scripts")
for fp in sorted(state):
    probs = [l for l in log[fp] if "SKIPPED" in l or "UNRECOVERABLE" in l or "FAILED" in l]
    print(f"{len(state[fp]):>7} {fp[len(PREFIX):]}  ops={len(log[fp])} problems={len(probs)}")
