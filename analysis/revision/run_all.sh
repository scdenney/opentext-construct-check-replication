#!/bin/bash
# run_all.sh -- master script for the revision analysis (added 24 Nov 2026).
# Run from the project root:  bash analysis/revision/run_all.sh [reporting|audit]
#
# Stage 1, measurement (not run here; needs the GPU cluster or a paid API, and the response data):
#   rv_01_splits.R            draw and lock the splits; corpus table
#   rv_00_pilot.R             discovery-sample pilot of the prompts
#   rv_02_bundle_a1.R         freeze prompts and models in a lock; export the estimation requests
#   rv_04_induce.R            induced codebooks (request, codebooks, import)
#   rv_03_import_a1.R         import model results; measures per response
#   rv_13_stm_arm.R           topic models on the discovery samples; rv_26_stm_refits.R for the 20 refits
#   rv_07_nk_heldout.R        entrepreneurship benchmark
# Stage 2, estimation (reads stored labels; CPU; about an hour):
#   rv_06_effects.R, rv_06b_effects_a2.R, rv_05_published_on_E.R, rv_17_human_codes.R, rv_19_closed_outcomes.R,
#   rv_13e_stm_robustness.R, rv_13f_stm_convergence.R, rv_26_task_level.R
# Stage 3, reporting (this script, default): tables, figures and the numbers quoted in the text, from the
#   stored estimates. Order matters: rv_09 writes the per-sample rates that rv_16 reads, and rv_31 and rv_34
#   write tables that rv_10, rv_11 and rv_16 read.
# Stage "audit": the permutation audit and its checks (CPU, hours): rv_08_run_all.sh, rv_29, rv_30.
set -euo pipefail
export LANG=en_US.UTF-8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
stage="${1:-reporting}"
run() { echo "== $1"; Rscript "analysis/revision/$1" > "analysis/output/revision/run_all_${1%.R}.log" 2>&1; }
if [ "$stage" = "audit" ]; then
  bash analysis/revision/rv_08_run_all.sh
  run rv_29_audit_order.R
  run rv_30_audit_arms.R
  exit 0
fi
for s in rv_09_summary.R rv_31_flow.R rv_32_registered_targets.R rv_33_benchmark_checks.R rv_34_displayed_content.R \
         rv_16_robustness.R rv_15_text_numbers.R rv_24_note_checks.R rv_27_stm_refits_summary.R rv_23_stm_tables.R \
         rv_28_prompt_as_treatment.R rv_11_si_tables.R rv_18_study_catalog.R rv_10_note_figures.R rv_25_human_figure.R \
         rv_12_si_figures.R session_info.R; do
  run "$s"
done
echo "done; logs in analysis/output/revision/run_all_*.log"
