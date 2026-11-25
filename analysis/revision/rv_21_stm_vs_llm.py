#!/usr/bin/env python3
"""Structural topic models against zero-shot language-model classification:
what each gets right, what each costs.

Arms, all on the same primary target contrasts and held-out estimation samples:
  llm35   Qwen3.6-35B-A3B, zero-shot yes/no (A1_presence), the reference model
  llm4    Gemma 4 E4B, zero-shot yes/no, the compact model
  stm     STM fit on the discovery sample without a covariate (the original arm)
  stmcov  STM fit on the discovery sample with the randomized arms as a
          prevalence covariate (STM_VARIANT=cov in rv_13_stm_arm.R); the
          estimation sample is projected without the covariate in both STM arms

Detection is the paper's rule: randomization p < .05 with the theorized sign.
A target with no matched topic counts as undetected in the "all contrasts" rate.

Inputs (analysis/output/revision/): effects_a1.csv, effects_stmcov.csv,
prompt_type.csv, tables/human_codes_effects[_stmcov].csv,
tables/human_codes_agreement[_stmcov].csv, stm_fit_cov_timing.csv,
stm_searchK_timing_all.csv, compute_sacct_22nov.txt; request counts from
data/jobs/submitted/<run>/requests.jsonl.
Outputs: analysis/output/revision/stm_vs_llm.md and stm_vs_llm_contrasts.csv.
Run from the repository root: python3 analysis/revision/rv_21_stm_vs_llm.py
"""
import csv
import math
import os
import re
import statistics as st

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
OUT = os.path.join(ROOT, "analysis", "output", "revision")
ARMS = {"llm35": ("qwen36_35b", "A1_presence"), "llm4": ("gemma4_e4b", "A1_presence"),
        "stm": ("stm", "D_topic"), "stmcov": ("stmcov", "D_topic")}
LABEL = {"llm35": "LLM yes/no, 35B", "llm4": "LLM yes/no, 4B", "stm": "STM, no covariate",
         "stmcov": "STM, treatment covariate"}


def read(path):
    with open(path, encoding="utf-8") as f:
        return list(csv.DictReader(f))


def num(x):
    try:
        return float(x)
    except (TypeError, ValueError):
        return float("nan")


def detected(r):
    return num(r["p_ri"]) < 0.05 and num(r["estimate"]) * num(r["expected_sign"]) > 0


def pct(k, n):
    return f"{100 * k / n:.0f}% ({k}/{n})" if n else "--"


def main():
    eff = read(os.path.join(OUT, "effects_a1.csv"))
    cov_f = os.path.join(OUT, "effects_stmcov.csv")
    eff += read(cov_f) if os.path.exists(cov_f) else []
    prim = [r for r in eff if r["role"] == "target" and r["primary"] == "TRUE"]
    key = lambda r: (r["dataset"], r["construct"], r["contrast"])
    by = {a: {key(r): r for r in prim if (r["model"], r["arm"]) == ma} for a, ma in ARMS.items()}
    universe = sorted(by["llm35"])                      # the 205 primary contrasts of the reference model
    ptype = {r["dataset"]: r["prompt_type"] for r in read(os.path.join(OUT, "prompt_type.csv"))}
    small = {k: min(num(by["llm35"][k]["n1"]), num(by["llm35"][k]["n0"])) < 200 for k in universe}
    L = [f"# STM versus zero-shot LLM classification ({len(universe)} primary contrasts)", ""]

    # 1. Detection
    L += ["## Detection of the theorized shift", "",
          "| Arm | Covered | Detected (covered) | Detected (all) | Theorized sign (covered) |", "|---|---|---|---|---|"]
    for a in ARMS:
        cov = [k for k in universe if k in by[a]]
        det = [k for k in cov if detected(by[a][k])]
        sign = [k for k in cov if num(by[a][k]["estimate"]) * num(by[a][k]["expected_sign"]) > 0]
        L.append(f"| {LABEL[a]} | {len(cov)} | {pct(len(det), len(cov))} | {pct(len(det), len(universe))} | "
                 f"{pct(len(sign), len(cov))} |")

    # 2. Where each arm works: arm size and prompt type (all contrasts; unmatched = undetected)
    L += ["", "## Detection by design feature (all contrasts; no matched topic counts as undetected)", "",
          "| Arm | Smaller arm < 200 | Smaller arm >= 200 | Own-answer prompts | Other prompts |", "|---|---|---|---|---|"]
    for a in ARMS:
        cells = []
        for sel in (lambda k: small[k], lambda k: not small[k],
                    lambda k: ptype.get(k[0]) == "own_answer", lambda k: ptype.get(k[0]) == "other"):
            ks = [k for k in universe if sel(k)]
            cells.append(pct(sum(1 for k in ks if k in by[a] and detected(by[a][k])), len(ks)))
        L.append(f"| {LABEL[a]} | " + " | ".join(cells) + " |")

    # 3. Agreement between arms, contrast by contrast
    L += ["", "## Agreement with the reference LLM, contrast by contrast", "",
          "| Arm | Same detection call | LLM detects, arm misses | Arm detects, LLM misses | r (standardized effects) |",
          "|---|---|---|---|---|"]
    rows_out = []
    for k in universe:
        rows_out.append({"dataset": k[0], "construct": k[1], "contrast": k[2], "small_arm": small[k],
                         "prompt_type": ptype.get(k[0], ""),
                         **{f"det_{a}": (detected(by[a][k]) if k in by[a] else "") for a in ARMS},
                         **{f"est_sd_{a}": (by[a][k].get("est_sd", "") if k in by[a] else "") for a in ARMS}})
    for a in ("llm4", "stm", "stmcov"):
        ks = [k for k in universe if k in by[a]]
        d0 = [detected(by["llm35"][k]) for k in ks]
        d1 = [detected(by[a][k]) for k in ks]
        x = [num(by["llm35"][k]["est_sd"]) for k in ks]
        y = [num(by[a][k]["est_sd"]) * (1 if a.startswith("llm") else 1) for k in ks]
        ok = [(u, v) for u, v in zip(x, y) if not (math.isnan(u) or math.isnan(v))]
        r = st.correlation([u for u, _ in ok], [v for _, v in ok]) if len(ok) > 2 else float("nan")
        L.append(f"| {LABEL[a]} | {pct(sum(p == q for p, q in zip(d0, d1)), len(ks))} | "
                 f"{sum(p and not q for p, q in zip(d0, d1))} | {sum(q and not p for p, q in zip(d0, d1))} | {r:.2f} |")

    # 4. Human codes
    L += ["", "## Against the original authors' human codes", ""]
    for arm, sfx in (("stm", ""), ("stmcov", "_stmcov")):
        fa = os.path.join(OUT, "tables", f"human_codes_agreement{sfx}.csv")
        fe = os.path.join(OUT, "tables", f"human_codes_effects{sfx}.csv")
        if not (os.path.exists(fa) and os.path.exists(fe)):
            L.append(f"- {LABEL[arm]}: human-code comparison not run")
            continue
        ag, ef = read(fa), read(fe)
        auc_t = [num(r.get("auc_stm")) for r in ag if not math.isnan(num(r.get("auc_stm")))]
        auc_m = [num(r.get("auc")) for r in ag if not math.isnan(num(r.get("auc")))]
        rec = lambda c: sum(1 for r in ef if r.get(c) == "TRUE")
        L.append(f"- {LABEL[arm]}: median AUC {st.median(auc_t):.2f} over {len(auc_t)} matched targets "
                 f"(LLM {st.median(auc_m):.2f}); detects {rec('rec_stm')} of {len(ef)} human-coded contrasts "
                 f"(human codes {rec('rec_human')}, LLM {rec('rec_model')})")

    # 5. Compute
    L += ["", "## Compute", ""]
    tf = os.path.join(OUT, "stm_fit_cov_timing.csv")
    if os.path.exists(tf):
        t = read(tf)
        sec = [num(r["fit_seconds_cov"]) for r in t]
        L.append(f"- STM fit with the covariate, one CPU core, K fixed: median {st.median(sec):.1f} s per study, "
                 f"max {max(sec):.1f} s, {sum(sec) / 60:.1f} CPU-minutes for {len(sec)} studies. "
                 f"Vocabulary identical to the original fit in {sum(r['vocab_identical'] == 'TRUE' for r in t)} of {len(t)}.")
    kf = os.path.join(OUT, "stm_searchK_timing_all.csv")
    if os.path.exists(kf):
        k = read(kf)
        ks = [num(r["searchK_s"]) for r in k]
        L.append(f"- STM choice of K (searchK over K = 4..12), {len(k)} studies, one core: "
                 f"median {st.median(ks):.0f} s, max {max(ks):.0f} s per study.")
    sf = os.path.join(OUT, "compute_sacct_22nov.txt")
    if os.path.exists(sf):
        agg = {}
        for line in open(sf, encoding="utf-8"):
            parts = line.rstrip("\n").split("|")
            if len(parts) < 5 or not parts[2].startswith("COMPLETED"):
                continue
            run = parts[1][3:] if parts[1].startswith("oc-") else parts[1]
            sub = os.path.join(ROOT, "data", "jobs", "submitted")
            cands = [d for d in os.listdir(sub) if d.startswith(run)]
            if len(cands) != 1 or not os.path.exists(os.path.join(sub, cands[0], "requests.jsonl")):
                continue
            n = sum(1 for _ in open(os.path.join(sub, cands[0], "requests.jsonl"), encoding="utf-8"))
            m = re.sub(r"^.*?_(qwen|gemma)", r"\1", cands[0])
            if cands[0].startswith("stm_label"):
                m = "STM topic labeling, " + m
            g = re.search(r"gres/gpu:([a-z0-9_.]+)=", parts[4])
            a = agg.setdefault(m, [0.0, 0, 0, g.group(1) if g else "?"])
            a[0] += float(parts[3]); a[1] += n; a[2] += 1
        for m, (secs, n, jobs, gpu) in sorted(agg.items()):
            L.append(f"- {m}: {jobs} jobs, {secs / 3600:.1f} GPU-hours ({gpu}) for {n:,} requests "
                     f"({1000 * secs / max(n, 1):.1f} ms per request, model loading included)")
    per = []
    for f in sorted(os.listdir(os.path.join(ROOT, "data", "results", "a1"))):
        mf = os.path.join(ROOT, "data", "results", "a1", f, "qwen36_35b", "measures.csv")
        if os.path.exists(mf):
            with open(mf, encoding="utf-8") as fh:
                rd = csv.reader(fh); h = next(rd); per.append(sum(1 for _ in rd) * sum(c.startswith("yes__") for c in h))
    if per:
        L.append(f"- Binary yes/no labels on the estimation samples: {sum(per):,} ({st.median(per):,.0f} per study, median). "
                 f"At the measured rates: 35B model {sum(per) * 25.7 / 3.6e6:.1f} A40-hours for the corpus, "
                 f"{st.median(per) * 25.7 / 6e4:.1f} GPU-minutes per median study; 4B model {sum(per) * 8.7 / 3.6e6:.1f} hours, "
                 f"{st.median(per) * 8.7 / 6e4:.1f} minutes.")
    with open(os.path.join(OUT, "stm_vs_llm_contrasts.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows_out[0]))
        w.writeheader()
        w.writerows(rows_out)
    open(os.path.join(OUT, "stm_vs_llm.md"), "w", encoding="utf-8").write("\n".join(L) + "\n")
    print("\n".join(L))


if __name__ == "__main__":
    main()
