#!/bin/bash
# Run every recovered builder with outputs redirected to the scratchpad; snapshot ground truth before and after.
set -u
REPO=/home/buddleman/Documents/github/research/projects/lucdh_opentext-surveys
RB=/tmp/claude-1000/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/bfe7aca0-b2db-41ad-bf8b-6a8c26510331/scratchpad/rebuild
H=analysis/revision/harmonize
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
cd "$REPO" || exit 1
snap() { (sha256sum data/harmonized/* data/external/*/harmonized_*.csv; find revision -path revision/wave3 -prune -o -type f -newer "$RB/.start" -print 2>/dev/null | sed 's/^/REVISION_FILE_NEWER_THAN_START /') > "$1"; }
rm -rf "$RB/own" "$RB/external" "$RB/manifest" "$RB/logs"; mkdir -p "$RB/own" "$RB/external" "$RB/manifest" "$RB/logs" "$RB/tmp"
touch "$RB/.start"; snap "$RB/snap_before.txt"
for d in data/external/*/; do ls "$d"harmonized_*.csv >/dev/null 2>&1 && mkdir -p "$RB/external/$(basename "$d")"; done
run() { local name=$1; shift; local t0=$(date +%s); "$@" > "$RB/logs/$name.out" 2> "$RB/logs/$name.err"; echo "$name exit=$? secs=$(( $(date +%s) - t0 ))" | tee -a "$RB/logs/_status.txt"; }
: > "$RB/logs/_status.txt"
# own surveys
export HARMONIZE_OUT="$RB/own" HARMONIZE_MANIFEST="$RB/manifest"
for s in sktw_naturalization_2024 rok2024_roc2024 nwo26 prc_roc26 ifes2023 ifes2023_manifest_notes aks21 build_index; do run "own_$s" Rscript "$H/own/$s.R"; done
# external wave 1
export HARMONIZE_OUT="$RB/external" HARMONIZE_SUMMARY="$RB/tmp/harmonize_ext_summary.rds"
run ext_wave1_build Rscript "$H/external_wave1/build.R"
# external wave 2: Dataverse R harmonizer, one card per call (as run on 2026-10-28)
for c in john2022_citizenvoice sambanis2025_externalthreat davies2013_audiencecosts marx2025_genderlanguage bursztyn2023_dissent_exp2 bursztyn2023_dissent_exp4 bursztyn2023_dissent_aux5 lupu2024_lawsofwar tripp2026_refugees marsh2025_lifestyleconjoint; do
  run "ext_wave2_dv_$c" Rscript "$H/external_wave2/dataverse/harmonize.R" "$c"; done
# economics (S = deleted staging dir; the cleaned Chopra files are not available)
run ext_wave2_econ python3 -I "$H/external_wave2/econ/harmonize.py" "$REPO/data/external" "$RB/tmp/stage_missing"
# OSF: rebuild the gratitude clean-ID list first, then the harmonizer
run ext_wave2_osf_ids Rscript "$H/external_wave2/osf/grat_clean_ids.R" data/external/coles2026_gratitude/raw/GlobalGratitude_Final_Cleaned.Rds "$RB/tmp/grat_clean_ids.txt"
run ext_wave2_osf python3 -I "$H/external_wave2/osf/harmonize.py" "$REPO/data/external" "$RB/tmp/grat_clean_ids.txt"
snap "$RB/snap_after.txt"
if cmp -s "$RB/snap_before.txt" "$RB/snap_after.txt"; then echo "GROUND TRUTH UNCHANGED"; else echo "!!! GROUND TRUTH SNAPSHOT CHANGED"; diff "$RB/snap_before.txt" "$RB/snap_after.txt" | head; fi | tee -a "$RB/logs/_status.txt"
