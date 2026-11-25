"""Split a runner results file into one file per prompt hash (fast pre-step for R import).

    python3 -I analysis/revision/rv_split_results.py data/results/<run>/results.jsonl
writes data/results/<run>/by_prompt/<prompt_hash>.jsonl (rewritten each time).
"""
import json
import sys
from collections import defaultdict
from pathlib import Path

src = Path(sys.argv[1])
out = src.parent / "by_prompt"
out.mkdir(exist_ok=True)
groups = defaultdict(list)
with src.open(encoding="utf-8") as f:
    for line in f:
        if line.strip():
            groups[json.loads(line)["prompt_hash"]].append(line if line.endswith("\n") else line + "\n")
for h, lines in groups.items():
    (out / f"{h}.jsonl").write_text("".join(lines), encoding="utf-8")
print(f"{sum(len(v) for v in groups.values()):,} results in {len(groups):,} prompt files -> {out}")
