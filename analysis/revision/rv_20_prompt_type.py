#!/usr/bin/env python3
"""Detection by open-text prompt type (post hoc; see revision/deviations.md).

Prompts are hand-coded from their wording in the study cards
(analysis/output/revision/prompt_type_handcoded.csv): own_answer asks for
reasons for an answer, choice, rating or decision the respondent gave; other is
everything else; unclear (one study) is excluded. This replaces a keyword rule
(22 Nov 2026) that misclassified at least ten studies. prompt_type.csv keeps the
keyword classes next to the hand codes.

Detection: p < .05 (randomization inference) with the theorized sign, as in
rv_09_summary.R. Topic-model rates are over contrasts with a matched topic.
Run from the repository root: python3 analysis/revision/rv_20_prompt_type.py
"""
import csv
import glob
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
REG = os.path.join(ROOT, "revision", "registry")
EFF = os.path.join(ROOT, "analysis", "output", "revision", "effects_a1.csv")
OUT = os.path.join(ROOT, "analysis", "output", "revision")

REASON = re.compile(r"\bwhy\b|explain|explanation|\breasons?\b|reasoning|justif|what mattered|led you|"
                    r"how you came to|tell us why|considerations|what makes you think|come up with|came up with", re.I)


def card_prompts():
    """Study id -> prompt text, from the latest card per id (registry file names carry a hash)."""
    prompts = {}
    for f in sorted(glob.glob(os.path.join(REG, "*.yaml")), key=os.path.getmtime):
        txt = open(f, encoding="utf-8").read()
        sid = re.search(r"^id:\s*(\S+)", txt, re.M)
        pr = re.search(r"^  prompt:\s*(.+)$", txt, re.M)
        if sid:
            prompts[sid.group(1)] = pr.group(1).strip().strip('"') if pr else ""
    return prompts


def main():
    prompts = card_prompts()
    keyword = {k: ("reason" if REASON.search(v) else "other") for k, v in prompts.items()}
    with open(os.path.join(OUT, "prompt_type_handcoded.csv"), encoding="utf-8") as f:
        kind = {r["dataset"]: r["code"] for r in csv.DictReader(f)}
    rows = [r for r in csv.DictReader(open(EFF, encoding="utf-8"))
            if r["role"] == "target" and r["primary"] == "TRUE"]
    log = []

    def rate(model, arm, subset=None):
        sel = [r for r in rows if r["model"] == model and r["arm"] == arm
               and (subset is None or kind.get(r["dataset"]) == subset)]
        det = [float(r["p_ri"]) < 0.05 and (float(r["estimate"]) * float(r["expected_sign"]) > 0) for r in sel]
        return len(sel), (sum(det) / len(det) if det else float("nan")), len({r["dataset"] for r in sel})

    n, d, s = rate("qwen36_35b", "A1_presence")
    log.append(f"check: qwen36_35b A1_presence primary targets n={n} ({s} studies), detected {d:.3f} "
               f"(paper: 205 contrasts, 0.67)")
    missing = sorted({r["dataset"] for r in rows} - set(kind))
    log.append(f"studies without a card prompt: {len(missing)} {missing[:5]}")
    for sub in ("own_answer", "other"):
        ids = sorted(k for k, v in kind.items() if v == sub)
        log.append(f"\n{sub}: {len(ids)} studies")
        for model, arm in (("qwen36_35b", "A1_presence"), ("qwen36_35b", "A1_presence_soft"),
                           ("qwen36_35b", "C_rating"), ("gemma4_31b", "A1_presence"),
                           ("gpt6_luna", "A1_presence"), ("gemma4_e4b", "A1_presence"), ("stm", "D_topic")):
            n, d, s = rate(model, arm, sub)
            log.append(f"  {model:11s} {arm:17s} n={n:3d} contrasts ({s:3d} studies)  detected {d:.2f}")
    with open(os.path.join(OUT, "prompt_type.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["dataset", "prompt_type", "keyword_rule", "prompt"])
        for k in sorted(kind):
            w.writerow([k, kind[k], keyword[k], prompts[k]])
    open(os.path.join(OUT, "prompt_type.log"), "w", encoding="utf-8").write("\n".join(log) + "\n")
    print("\n".join(log))


if __name__ == "__main__":
    sys.exit(main())
