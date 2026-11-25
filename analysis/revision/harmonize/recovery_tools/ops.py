import sys, re, shlex, os
sys.path.insert(0, "/tmp/claude-1000/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/bfe7aca0-b2db-41ad-bf8b-6a8c26510331/scratchpad/rec2")
from lib import CALLS
def sed_cmds(c):
    out = []
    for m in re.finditer(r"\bsed\s+-i\b", c):
        lx = shlex.shlex(c[m.start():], posix=True, punctuation_chars=";&|\n")
        lx.whitespace = " \t"; lx.whitespace_split = True
        toks = []
        for t in lx:
            if t and set(t) <= set(";&|\n"): break
            toks.append(t)
        out.append(toks)
    return out
def py_targets(c):
    return re.findall(r"""p\s*=\s*['"]([^'"]+)['"]""", c) + re.findall(r"""open\((['"][^'"]+['"])\s*,\s*['"]w""", c)
def heredoc_targets(c):
    return [(m.group(1), m.group(2)) for m in re.finditer(r"cat\s*(>>?)\s*(\S+)\s*<<", c)]
if __name__ == "__main__":
    for i, x in enumerate(CALLS):
        o = x["owner"][6:15]
        if x["name"] in ("Write", "Edit"):
            fp = x["inp"]["file_path"]
            if "scratchpad" in fp: print(i, o, x["name"], "err" if x["err"] else "", fp.split("scratchpad/")[1])
        elif x["name"] == "Bash":
            c = x["inp"]["command"]
            if "scratchpad" not in c and "$S" not in c and "$D" not in c and "cd $" not in c and "own_harmonize" not in c: pass
            h = heredoc_targets(c); s = [t[-1] for t in sed_cmds(c)]; p = py_targets(c) if re.search(r"open\([^)]*['\"]w|write_text", c) else []
            h = [t for t in h if re.search(r"\.(R|py|sh)\W*$", t[1])]
            if h or s or p:
                print(i, o, "Bash", "err" if x["err"] else "", "heredoc:", h, "sed:", s, "py:", p)
