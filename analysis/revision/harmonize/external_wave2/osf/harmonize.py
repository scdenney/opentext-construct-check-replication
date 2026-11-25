"""Write harmonized CSVs (no text printed). Usage: python3 -I harmonize.py <external_dir> <grat_clean_ids>"""
import sys, csv, collections
csv.field_size_limit(10**9)
E, ids_path = sys.argv[1], sys.argv[2]
import os; O = os.environ.get('HARMONIZE_OUT', E)  # path changed on recovery (2026-11-17): output root (default: as originally)
NA = ("", "NA")
def write(path, rows, cols):
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=cols); w.writeheader(); w.writerows(rows)
    print(path, "rows", len(rows), "cols", cols,
          "treat", dict(collections.Counter(r["treatment"] for r in rows)),
          "excl", dict(collections.Counter(r["exclude"] for r in rows)))
# 1. ML3 power recall
src = list(csv.DictReader(open(f"{E}/ebersole2016_powerrecall/raw/ML3_PPool_and_MTurk_Data.csv", newline="", encoding="mac_roman")))
out = []; seen = collections.Counter()
for i, r in enumerate(src):
    cond = r["PowerCond"]
    if cond not in ("HighPower", "LowPower"): continue
    t = r["highpower"] if cond == "HighPower" else r["lowpower"]
    if t.strip() in NA: continue
    sid = r["session_id"].strip()
    rid = sid if sid not in NA else f"ebersole2016_powerrecall_{i+1}"
    seen[rid] += 1
    out.append({"id": rid, "dataset": "ebersole2016_powerrecall",
                "country": "CAN" if r["Site"] in ("UniversityOfToronto", "CarletonUniversity") else "USA",
                "lang": "en", "text": t, "treatment": 1 if cond == "HighPower" else 0,
                "exclude": "TRUE" if r["sarcasm"].strip() in NA else "FALSE"})
dups = {k for k, v in seen.items() if v > 1}
for j, o in enumerate(out):
    if o["id"] in dups: o["id"] = f"{o['id']}_{j+1}"
print("ML3 duplicate session ids fixed:", len(dups))
write(f"{O}/ebersole2016_powerrecall/harmonized_ebersole2016_powerrecall.csv", out,  # path changed on recovery (2026-11-17)
      ["id", "dataset", "country", "lang", "text", "treatment", "exclude"])
# 2. Landrum & Huxster thought lists
src = list(csv.DictReader(open(f"{E}/landrum2025_mechexplain/raw/Coded_Thought_Lists_2.csv", newline="", encoding="utf-8-sig")))
hc = {"FINAL_CODE": "human_final_code", "JKH GLOBAL": "human_coder1_global", "ARL GLOBAL": "human_coder2_global",
      "NOTICE": "human_notice", "MECH": "human_mech", "S_PLATFORM": "human_s_platform", "P_PLATFORM": "human_p_platform"}
out = []
for r in src:
    th = [r[f"T{k}"].strip() for k in range(1, 11) if r[f"T{k}"].strip() not in NA]
    if not th: continue
    o = {"id": r["ResponseId"], "dataset": "landrum2025_mechexplain", "country": "USA", "lang": "en",
         "text": " | ".join(th), "treatment": 1 if r["QUALITY"] == "LOW" else 0, "exclude": "FALSE"}
    for k, v in hc.items(): o[v] = r[k]
    out.append(o)
write(f"{O}/landrum2025_mechexplain/harmonized_landrum2025_mechexplain.csv", out,  # path changed on recovery (2026-11-17)
      ["id", "dataset", "country", "lang", "text", "treatment", "exclude"] + list(hc.values()))
# 3. Gratitude megastudy: list (treated) vs events (control)
keep = set(l.strip() for l in open(ids_path, encoding="utf-8") if l.strip())
src = csv.DictReader(open(f"{E}/coles2026_gratitude/raw/GlobalGratitude_Raw.csv", newline="", encoding="utf-8"))
out = []; ids = collections.Counter()
for r in src:
    if r["condition"] == "list": t, z = r["grat_list"], 1
    elif r["condition"] == "events": t, z = r["control_events"], 0
    else: continue
    if t.strip() in NA: continue
    lab = r["lab"].strip()
    out.append({"id": r["ResponseId"], "dataset": "coles2026_gratitude", "country": lab.split("_")[0] if lab else "",
                "lang": r["UserLanguage"].lower(), "text": t, "treatment": z,
                "exclude": "FALSE" if r["ResponseId"] in keep else "TRUE"})
    ids[r["ResponseId"]] += 1
print("gratitude dup ids:", sum(1 for v in ids.values() if v > 1))
write(f"{O}/coles2026_gratitude/harmonized_coles2026_gratitude.csv", out,  # path changed on recovery (2026-11-17)
      ["id", "dataset", "country", "lang", "text", "treatment", "exclude"])
print("gratitude lang:", collections.Counter(o["lang"] for o in out).most_common())
print("gratitude countries:", len(set(o["country"] for o in out)))
