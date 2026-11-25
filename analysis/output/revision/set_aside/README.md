# Set aside: Jev 1.13

On 18 Nov 2026 Steven decided to take Jev out of the paper. It had been run in
October 2026 on 49 of the first 53 studies (yes/no, rating, pairwise) and was
not run on the 86 studies added in November, so its coverage was uneven; GPT-6
Luna and Gemma 4 31B give the cross-model check on the full corpus.

Nothing was deleted. These files hold the Jev results as they stood in the
139-study analysis of 18 Nov 2026:

- `effects_a1_jev.csv`: every Jev estimate (rows of `effects_a1.csv`).
- `agreement_models_jev.csv`, `agreement_arms_jev.csv`: agreement with other
  models and between arms.
- `bt_diagnostics_jev.csv`: pairwise diagnostics (median held-out accuracy
  0.94, against 0.79 for the reference model on the same fits).

Summary on the 48 studies with a primary target: recovered (significant in the
theorized direction) 0.73 yes/no, 0.75 rating, 0.79 pairwise; the reference
model on the same contrasts 0.70, 0.73, 0.81.

Response-level measures are in `data/results/set_aside/jev/<study>/` (not in
git), and the raw runs in `data/results/a1_b{1,2,3,4}_jev/`. To bring Jev back:
move the study folders to `data/results/a1/<study>/jev/`, restore the `jev`
entry in `MODELS` (`rv_config.R`), and rerun from `rv_06_effects.R`.
