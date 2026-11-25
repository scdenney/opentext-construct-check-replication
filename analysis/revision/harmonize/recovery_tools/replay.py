# Replay curated operations on recovered script files.
import sys, os, re, subprocess, tempfile, json
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lib import CALLS
OLD = "/tmp/claude-1000/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/ac14b90b-b4fc-47f6-b280-5ec7accc121e/scratchpad"
def cmd(i): return CALLS[i]["inp"].get("command", "")
def heredocs(c):
    """Return list of (redirect_target_text, op('>' or '>>'), body) for cat heredocs in a command string."""
    out = []
    for m in re.finditer(r"cat\s*(>>?)\s*(\S+)\s*<<\s*'?(\w+)'?[^\n]*\n", c):
        op, tgt, tag = m.group(1), m.group(2), m.group(3)
        rest = c[m.end():]
        e = re.search(r"^" + re.escape(tag) + r"$", rest, re.M)
        out.append((tgt, op, rest[:e.start()]))
    return out
class State:
    def __init__(self): self.files = {}; self.log = {}
    def note(self, f, s): self.log.setdefault(f, []).append(s)
    def write(self, f, i):
        x = CALLS[i]; assert x["name"] == "Write" and not x["err"], i
        assert x["inp"]["file_path"].endswith(f), (f, x["inp"]["file_path"])
        self.files[f] = x["inp"]["content"]; self.note(f, f"[{i}] {x['owner'][6:15]} Write ({len(self.files[f])} chars)")
    def edit(self, f, i):
        x = CALLS[i]; assert x["name"] == "Edit", i
        assert x["inp"]["file_path"].endswith(f), (f, x["inp"]["file_path"])
        if x["err"]: self.note(f, f"[{i}] Edit FAILED in transcript, skipped"); return
        o, n = x["inp"]["old_string"], x["inp"]["new_string"]; s = self.files[f]
        c = s.count(o)
        if c == 0: raise SystemExit(f"Edit [{i}] old_string not found in {f}")
        if c > 1 and not x["inp"].get("replace_all"): raise SystemExit(f"Edit [{i}] ambiguous in {f}")
        self.files[f] = s.replace(o, n) if x["inp"].get("replace_all") else s.replace(o, n, 1)
        self.note(f, f"[{i}] {x['owner'][6:15]} Edit ok")
    def heredoc(self, f, i, k=None, allow_err=False):
        x = CALLS[i]; assert allow_err or not x["err"], f"[{i}] errored"
        hs = [h for h in heredocs(cmd(i)) if h[0].rstrip("\"'").endswith(os.path.basename(f))]
        if k is not None: hs = [hs[k]]
        assert len(hs) == 1, (i, f, [h[0] for h in heredocs(cmd(i))])
        tgt, op, body = hs[0]
        self.files[f] = (self.files.get(f, "") + body) if op == ">>" else body
        self.note(f, f"[{i}] {x['owner'][6:15]} heredoc cat {op} ({len(body)} chars)")
    def shell(self, f, i, snippet, desc):
        """Run a shell/python snippet (taken verbatim from call i) against a temp copy of f; OLD path rewritten to temp."""
        x = CALLS[i]; assert snippet in cmd(i), f"snippet not in [{i}]"
        d = tempfile.mkdtemp(dir=os.path.dirname(os.path.abspath(__file__)))
        rel = f
        p = os.path.join(d, rel); os.makedirs(os.path.dirname(p), exist_ok=True)
        open(p, "w", encoding="utf-8").write(self.files[f])
        sn = snippet.replace(OLD + "/", d + "/")
        r = subprocess.run(["bash", "-c", sn], cwd=os.path.dirname(p), capture_output=True, text=True)
        if r.returncode != 0: raise SystemExit(f"[{i}] shell replay failed: {r.stderr[:500]}")
        new = open(p, encoding="utf-8").read()
        changed = new != self.files[f]
        self.files[f] = new
        self.note(f, f"[{i}] {x['owner'][6:15]} {desc} ({'changed' if changed else 'NO CHANGE'})")
        return changed
    def dump(self, outdir):
        for f, s in self.files.items():
            p = os.path.join(outdir, f); os.makedirs(os.path.dirname(p), exist_ok=True)
            open(p, "w", encoding="utf-8").write(s)

def py_heredocs(c):
    out = []
    for m in re.finditer(r"python3?[^\n<]*<<\s*'?(\w+)'?[^\n]*\n", c):
        tag = m.group(1); rest = c[m.end():]
        e = re.search(r"^" + re.escape(tag) + r"$", rest, re.M)
        out.append(m.group(0) + rest[:e.end()])
    return out
def pyrun(self, f, i, k=0, desc="python in-place rewrite"):
    blocks = py_heredocs(cmd(i)); assert blocks, i
    return self.shell(f, i, blocks[k], desc)
State.pyrun = pyrun
