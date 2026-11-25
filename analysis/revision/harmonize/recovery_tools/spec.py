import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from replay import *
from ops import sed_cmds
def sed(self, f, i):
    """Run every `sed -i` in call i that targets basename(f), verbatim (no shell), on a temp copy."""
    base = os.path.basename(f); hits = 0
    for toks in sed_cmds(cmd(i)):
        if os.path.basename(toks[-1].rstrip("\"'")) != base: continue
        d = tempfile.mkdtemp(dir=os.path.dirname(os.path.abspath(__file__)))
        p = os.path.join(d, base); open(p, "w", encoding="utf-8").write(self.files[f])
        r = subprocess.run(toks[:-1] + [p], capture_output=True, text=True)
        assert r.returncode == 0, (i, r.stderr)
        new = open(p, encoding="utf-8").read(); ch = new != self.files[f]; self.files[f] = new; hits += 1
        self.note(f, f"[{i}] {CALLS[i]['owner'][6:15]} sed -i {' '.join(toks[1:-1])[:90]!r} ({'changed' if ch else 'NO CHANGE'})")
    assert hits, f"no sed for {f} in [{i}]"
State.sed = sed
S = State()
# ---------- own surveys (agent a8dd92c41 + forks) ----------
O = "own_harmonize/"
S.write(O+"common.R", 658); S.sed(O+"common.R", 661); S.edit(O+"common.R", 1099); S.edit(O+"common.R", 1103); S.sed(O+"common.R", 1191)
S.write(O+"rok2024_roc2024.R", 952); S.sed(O+"rok2024_roc2024.R", 964); S.sed(O+"rok2024_roc2024.R", 1143); S.pyrun(O+"rok2024_roc2024.R", 1199)
S.write(O+"nwo26.R", 922); S.pyrun(O+"nwo26.R", 971); S.sed(O+"nwo26.R", 1143); S.edit(O+"nwo26.R", 1178)
S.write(O+"prc_roc26.R", 930); S.pyrun(O+"prc_roc26.R", 1031); S.edit(O+"prc_roc26.R", 1039); S.sed(O+"prc_roc26.R", 1045); S.pyrun(O+"prc_roc26.R", 1053); S.sed(O+"prc_roc26.R", 1143)
S.write(O+"ifes2023.R", 999); S.pyrun(O+"ifes2023.R", 1007); S.pyrun(O+"ifes2023.R", 1040); S.pyrun(O+"ifes2023.R", 1156)
S.write(O+"aks21.R", 1060); S.sed(O+"aks21.R", 1071); S.sed(O+"aks21.R", 1164); S.sed(O+"aks21.R", 1176)
S.write(O+"build_index.R", 1058); S.pyrun(O+"build_index.R", 1213); S.pyrun(O+"build_index.R", 1222)
# ---------- external wave 1 (agent aa95643b7) ----------
H = "harmonize_ext/"
S.heredoc(H+"inspect.R", 1193); S.sed(H+"inspect.R", 1218); S.heredoc(H+"inspect.R", 1320)
S.heredoc(H+"run.sh", 1218)
S.write(H+"build.R", 1372); S.pyrun(H+"build.R", 1375); S.pyrun(H+"build.R", 1387); S.pyrun(H+"build.R", 1403)
# ---------- external wave 2 ----------
S.heredoc("wave2_scripts/harmonize/harmonize.R", 1366); S.pyrun("wave2_scripts/harmonize/harmonize.R", 1370)
S.heredoc("wave2_scripts/econ/harmonize.py", 1257); S.sed("wave2_scripts/econ/harmonize.py", 1265); S.sed("wave2_scripts/econ/harmonize.py", 1293)
S.heredoc("wave2_scripts/osf/harmonize.py", 1296, allow_err=True); S.sed("wave2_scripts/osf/harmonize.py", 1300)
out = sys.argv[1]
S.dump(out)
with open(os.path.join(out, "_replay_log.txt"), "w") as fh:
    for f in sorted(S.log): fh.write(f + "\n" + "".join("   " + l + "\n" for l in S.log[f]))
print(open(os.path.join(out, "_replay_log.txt")).read())
