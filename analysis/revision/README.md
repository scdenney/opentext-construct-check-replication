# Revision analysis: the split-first protocol on every dataset

Run from the project root with `LANG=en_US.UTF-8` (in a C locale R turns CJK text into `<U+XXXX>` escapes; the package refuses such text). All computation goes through the `opencheck` package (`~/Documents/github/resources/opencheck`), which `rv_config.R` loads.

| Step | Script | What it does | Reads E? |
|---|---|---|---|
| 0 | `rv_config.R` | Paths, model roster (`MODELS`, `MODEL_PATHS`), ALICE submit and fetch helpers, prompt builder `a1_prompts()` | – |
| 0 | `rv_treatments.R` | Each card's randomized contrast(s) as code. Conjoint contrasts are on the pair shown | – |
| 1 | `rv_01_splits.R` | Draws and locks the holdout (seed 42, ×11) and two-fold (seed 4242) splits for every registered card; writes the corpus table | no |
| 2 | `rv_00_pilot.R`, `rv_00b_pilot_check.R` | Discovery-fold pilot of the theory-only prompts: valid-answer rate, probability on valid tokens, cross-model κ | no (D only) |
| 3 | `rv_02_bundle_a1.R` | Freezes the A1/B/C lock (card + exact prompt hashes + roster), then exports the E requests: yes/no presence (A1), 0–9 rating (C), pairwise (B) | after lock |
| 4 | `rv_04_induce.R request` → `codebooks` → `import` | A2: an LLM-induced codebook from each split's D, frozen per split, classifying that split's E | after lock |
| 5 | `rv_03_import_a1.R <batch>` | Imports runner/Jev results; BT fit plus held-out accuracy and ICI; measures per response | after lock |
| 6 | `rv_06_effects.R`, `rv_06b_effects_a2.R` | Neyman SE + randomization p for every target and placebo under every contrast; A2 per split, median-aggregated | after lock |
| 7 | `rv_07_nk_heldout.R prepare` → `revise r` → `classify r` → `evaluate` | Codebook iteration vs model size, scored only on held-out human-coded items (NK entrepreneurship) | test half only |
| 8 | `rv_08_doubledip.R`, `rv_08_run_all.sh` | False-positive rate under permuted labels: old full-sample STM procedure vs split | after lock |
| 9 | `rv_09_summary.R` | Cross-study tables, figures, and `summary_numbers.md` | – |

Batch inference:
- **ALICE:** `alice_submit()` writes a SLURM job from the package template (offline vLLM, first-token logprobs, thinking off). Results come back with `alice_fetch()`. `rv_split_results.py` splits a results file by prompt before import.
- **Jev:** `inst/python/oc_jev_client.py` (TypeSafe endpoint) writes results in the same format.

Provenance:
- `revision/registry/`: registered study cards (append-only, amendments logged).
- `revision/splits/`, `revision/locks/`: manifests and lockfiles, write-once, hash-checked.
- `revision/access_log.jsonl`: every discovery and estimation read.
- `revision/deviations.md`: every departure from a draft card.
- `revision/WORKLOG.md`: the overnight run.
