# Copy replayed scripts into the repo and apply path-only changes (each must match exactly once).
import os, shutil, sys
SRC = "out"
DST = "/home/buddleman/Documents/github/research/projects/lucdh_opentext-surveys/analysis/revision/harmonize"
OLD = "/tmp/claude-1000/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/ac14b90b-b4fc-47f6-b280-5ec7accc121e/scratchpad"
REPO = "/home/buddleman/Documents/github/research/projects/lucdh_opentext-surveys"
M = "# path changed on recovery (2026-11-17)"
H = "analysis/revision/harmonize"
files = {
 "own_harmonize/common.R": "own/common.R", "own_harmonize/rok2024_roc2024.R": "own/rok2024_roc2024.R",
 "own_harmonize/nwo26.R": "own/nwo26.R", "own_harmonize/prc_roc26.R": "own/prc_roc26.R",
 "own_harmonize/ifes2023.R": "own/ifes2023.R", "own_harmonize/aks21.R": "own/aks21.R",
 "own_harmonize/build_index.R": "own/build_index.R",
 "harmonize_ext/build.R": "external_wave1/build.R", "harmonize_ext/inspect.R": "external_wave1/inspect.R",
 "harmonize_ext/run.sh": "external_wave1/run.sh",
 "wave2_scripts/harmonize/harmonize.R": "external_wave2/dataverse/harmonize.R",
 "wave2_scripts/econ/harmonize.py": "external_wave2/econ/harmonize.py",
 "wave2_scripts/osf/harmonize.py": "external_wave2/osf/harmonize.py",
}
src_common = f'source("{OLD}/own_harmonize/common.R")'
dst_common = f'source("{H}/own/common.R")  {M}'
edits = {
 "own/common.R": [
   (f'OH_REPO <- "{REPO}"', f'OH_REPO <- "."  {M}: run from the repository root'),
   ('OH_OUT <- file.path(OH_REPO, "data/harmonized")', f'OH_OUT <- Sys.getenv("HARMONIZE_OUT", file.path(OH_REPO, "data/harmonized"))  {M}'),
   (f'OH_MANIFEST <- "{OLD}/own_harmonize/manifest"', f'OH_MANIFEST <- Sys.getenv("HARMONIZE_MANIFEST", "analysis/output/revision/harmonize_manifest")  {M}')],
 "own/rok2024_roc2024.R": [(src_common, dst_common)], "own/nwo26.R": [(src_common, dst_common)],
 "own/prc_roc26.R": [(src_common, dst_common)], "own/ifes2023.R": [(src_common, dst_common)],
 "own/aks21.R": [(src_common, dst_common)], "own/build_index.R": [(src_common, dst_common)],
 "external_wave1/build.R": [
   (f'source("{OLD}/harmonize_ext/inspect.R")', f'source("{H}/external_wave1/inspect.R")  {M}'),
   (f'EXT <- "{REPO}/data/external"', f'EXT <- "data/external"  {M}: inputs, relative to the repository root\nOUT <- Sys.getenv("HARMONIZE_OUT", EXT)  {M}: output root (default: write next to raw/, as originally)'),
   (f'SUMMARY <- "{OLD}/harmonize_ext/summary.rds"', f'SUMMARY <- Sys.getenv("HARMONIZE_SUMMARY", file.path(tempdir(), "harmonize_ext_summary.rds"))  {M}'),
   ('  path <- file.path(EXT, dataset, paste0("harmonized_", card, ".csv"))', f'  path <- file.path(OUT, dataset, paste0("harmonized_", card, ".csv"))  {M}')],
 "external_wave1/run.sh": [
   (f'ERR={OLD}/harmonize_ext/last_stderr.txt', f'ERR=${{HARMONIZE_STDERR:-${{TMPDIR:-/tmp}}/harmonize_ext_last_stderr.txt}}  {M}'),
   (f'cd {REPO}/data/external', f'cd "$(dirname "$0")/../../../.."  {M}: repository root (was data/external; build.R paths are relative to the root)')],
 "external_wave2/dataverse/harmonize.R": [
   (f'E <- "{REPO}/data/external"', f'E <- "data/external"  {M}: inputs, relative to the repository root\nOUT <- Sys.getenv("HARMONIZE_OUT", E)  {M}: output root (default: as originally)'),
   ('  out <- file.path(E, ds, paste0("harmonized_", card, ".csv"))', f'  out <- file.path(OUT, ds, paste0("harmonized_", card, ".csv"))  {M}')],
 "external_wave2/econ/harmonize.py": [
   ("P, S = sys.argv[1], sys.argv[2]\n", f"P, S = sys.argv[1], sys.argv[2]\nimport os; O = os.environ.get('HARMONIZE_OUT', P)  {M}: output root (default: as originally)\n"),
   ("out(d[['id','dataset','country','lang','text','treatment','exclude','human_bias_motive_class']], P + '/chopra2024_newsdemand/harmonized_chopra2024_newsdemand.csv')",
    f"out(d[['id','dataset','country','lang','text','treatment','exclude','human_bias_motive_class']], O + '/chopra2024_newsdemand/harmonized_chopra2024_newsdemand.csv')  {M}"),
   ("out(x[['id','dataset','country','lang','text','treatment','exclude']+['human_'+c for c in codes]], P + '/graeber2024_storiesmemory/harmonized_graeber2024_storiesmemory.csv')",
    f"out(x[['id','dataset','country','lang','text','treatment','exclude']+['human_'+c for c in codes]], O + '/graeber2024_storiesmemory/harmonized_graeber2024_storiesmemory.csv')  {M}"),
   ("out(a[['id','dataset','country','lang','text','treatment','exclude']], P + '/andre2025_macronarratives/harmonized_andre2025_macronarratives.csv')",
    f"out(a[['id','dataset','country','lang','text','treatment','exclude']], O + '/andre2025_macronarratives/harmonized_andre2025_macronarratives.csv')  {M}")],
 "external_wave2/osf/harmonize.py": [
   ("E, ids_path = sys.argv[1], sys.argv[2]\n", f"E, ids_path = sys.argv[1], sys.argv[2]\nimport os; O = os.environ.get('HARMONIZE_OUT', E)  {M}: output root (default: as originally)\n"),
   ('write(f"{E}/ebersole2016_powerrecall/harmonized_ebersole2016_powerrecall.csv", out,', f'write(f"{{O}}/ebersole2016_powerrecall/harmonized_ebersole2016_powerrecall.csv", out,  {M}'),
   ('write(f"{E}/landrum2025_mechexplain/harmonized_landrum2025_mechexplain.csv", out,', f'write(f"{{O}}/landrum2025_mechexplain/harmonized_landrum2025_mechexplain.csv", out,  {M}'),
   ('write(f"{E}/coles2026_gratitude/harmonized_coles2026_gratitude.csv", out,', f'write(f"{{O}}/coles2026_gratitude/harmonized_coles2026_gratitude.csv", out,  {M}')],
}
for s, d in files.items():
    p = os.path.join(DST, d); os.makedirs(os.path.dirname(p), exist_ok=True)
    txt = open(os.path.join(SRC, s), encoding="utf-8").read()
    for old, new in edits.get(d, []):
        n = txt.count(old); assert n == 1, (d, n, old[:80]); txt = txt.replace(old, new)
    assert OLD not in txt.replace("scratchpad `own_harmonize/`", ""), d
    open(p, "w", encoding="utf-8").write(txt)
    if d.endswith(".sh"): os.chmod(p, 0o755)
    print("installed", d, len(edits.get(d, [])), "path edits")
