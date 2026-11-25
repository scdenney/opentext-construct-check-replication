# Figure and table crosswalk

Every figure and table of the article and the Supplementary Information, in paper order. Built from the
manuscript sources: the file each one includes and the script that names that file.

## Article

| No. | Type | Caption | Script | File | In this package |
|---|---|---|---|---|---|
| 1 | Table | The validation corpus of 139 studies (open-text items) | typed in the manuscript source |  |  |
| 1 | Figure | False-positive rates under permuted treatment: full-sample versus split-sample procedures | `analysis/revision/rv_10_note_figures.R` | `paper/revision/figures/note_doubledip.pdf` | yes |
| 2 | Table | Detection of theorized treatment effects across primary contrasts (percent) | typed in the manuscript source |  |  |
| 2 | Figure | Test statistics and detection rates of primary target contrasts by measure | `analysis/revision/rv_10_note_figures.R` | `paper/revision/figures/note_recovery.pdf` | yes |
| 3 | Figure | Effects on the original authors' codes and on each measure | `analysis/revision/rv_25_human_figure.R` | `paper/revision/figures/note_human.pdf` | yes |
| 3 | Table | Recommended practices and the evidence for each | typed in the manuscript source |  |  |
| 4 | Figure | Split-sample validation applied to immigrant naturalization conjoints in South Korea and Taiwan | `analysis/revision/rv_10_note_figures.R` | `paper/revision/figures/note_example.pdf` | yes |

## Supplementary Information

| No. | Type | Caption | Script | File | In this package |
|---|---|---|---|---|---|
| S1 | Table | Search and inclusion by wave and source | typed in the manuscript source |  |  |
| S2 | Table | Distribution of the 139 studies by experimental design, open-text question type, respondent location, and fiel | `analysis/revision/rv_18_study_catalog.R` | `paper/revision/si_tables/catalog_summary_rows.tex` | yes |
| S3 | Table | Evaluated language models, weight precision, and corpus coverage | typed in the manuscript source |  |  |
| S4 | Table | Rows dropped by exclusion flags before the split, by study and arm of the primary contrast | `analysis/revision/rv_31_flow.R` | `paper/revision/si_tables/exclusions_rows.tex` | yes |
| S5 | Table | Detection of theorized treatment effects under alternative specifications, primary target contrasts | `analysis/revision/rv_11_si_tables.R`, `analysis/revision/rv_16_robustness.R` | `paper/revision/si_tables/robustness_rows.tex` | yes |
| S6 | Table | Detection by type of open-text item, primary target contrasts | `analysis/revision/rv_23_stm_tables.R` | `paper/revision/si_tables/prompt_type_rows.tex` | yes |
| S7 | Table | Detection by whether the target restates content the treatment displayed, primary target contrasts | `analysis/revision/rv_34_displayed_content.R` | `paper/revision/si_tables/displayed_rows.tex` | yes |
| S8 | Table | Primary target contrasts by model and measure: share detected, share statistically significant, and share with | `analysis/revision/rv_11_si_tables.R`, `analysis/revision/rv_18_study_catalog.R` | `paper/revision/si_tables/summary_rows.tex` | yes |
| S9 | Table | Comparison of alternative models against the reference model (Qwen3.6-35B-A3B) on primary target contrasts | `analysis/revision/rv_11_si_tables.R` | `paper/revision/si_tables/model_agreement_rows.tex` | yes |
| S1 | Figure | Standardized treatment effect estimates across models versus the reference model, binary yes/no measure | `analysis/revision/rv_12_si_figures.R` | `paper/revision/figures/si_model_agreement.pdf` | yes |
| S10 | Table | Placebo constructs across all contrasts: share significantly increasing and decreasing (p<.05) | `analysis/revision/rv_11_si_tables.R` | `paper/revision/si_tables/placebo_rows.tex` | yes |
| S11 | Table | Agreement of the reference language model and structural topic model with original authors' human codes, estim | `analysis/revision/rv_17_human_codes.R` | `paper/revision/si_tables/human_agreement_rows.tex` | yes |
| S12 | Table | Primary treatment contrasts estimated using original human codes, reference model binary labels, and matched t | `analysis/revision/rv_17_human_codes.R` | `paper/revision/si_tables/human_effects_rows.tex` | yes |
| S13 | Table | Open-text construct detection cross-classified by statistical significance of the study's closed-ended outcome | `analysis/revision/rv_19_closed_outcomes.R` | `paper/revision/si_tables/closed_outcome_rows.tex` | yes |
| S14 | Table | Structural topic model diagnostics: share of all 205 primary target contrasts detected by number of topics K a | `analysis/revision/rv_11_si_tables.R` | `paper/revision/si_tables/stm_robustness_rows.tex` | yes |
| S15 | Table | Topic models with and without the treatment covariate, and the zero-shot prompt, primary target contrasts | `analysis/revision/rv_23_stm_tables.R` | `paper/revision/si_tables/stm_arms_rows.tex` | yes |
| S16 | Table | Topic-model results over 20 refits that change only the order of documents and words | `analysis/revision/rv_27_stm_refits_summary.R` | `paper/revision/si_tables/stm_refits_rows.tex` | yes |
| S17 | Table | False-positive rates under permuted treatment assignment (p<.05, 200 permutations per study across 138 studies | `analysis/revision/rv_11_si_tables.R` | `paper/revision/si_tables/doubledip_summary_rows.tex` | yes |
| S17 | Table | False-positive rates under permuted treatment assignment (p<.05, 200 permutations per study across 138 studies | `analysis/revision/rv_23_stm_tables.R` | `paper/revision/si_tables/doubledip_bonferroni_rows.tex` | yes |
| S18 | Table | Sources of the full-sample false-positive rate: fit, topic tested and test (percent of permutations with p<.05 | `analysis/revision/rv_30_audit_arms.R` | `paper/revision/si_tables/audit_arms_rows.tex` | yes |
| S19 | Table | Performance of pairwise Bradley--Terry scaling by target construct prevalence, reference model | `analysis/revision/rv_11_si_tables.R` | `paper/revision/si_tables/pairwise_prevalence_rows.tex` | yes |
| S20 | Table | Agreement with held-out human codes across model scales and codebook versions, test halves (10 random splits) | `analysis/revision/rv_11_si_tables.R` | `paper/revision/si_tables/nk_rows.tex` | yes |
| S21 | Table | Analyses by status | typed in the manuscript source |  |  |
| S22 | Table | Catalog of the 139 survey experiments in the validation corpus | `analysis/revision/rv_18_study_catalog.R` | `paper/revision/si_tables/studies_rows.tex` | yes |
| S23 | Table | Primary target contrast estimates by study, reference model (Qwen3.6-35B-A3B) | `analysis/revision/rv_11_si_tables.R` | `paper/revision/si_tables/primary_qwen36_35b_rows.tex` | yes |
| S24 | Table | Disclosure elements for the nine surveys I fielded | typed in the manuscript source |  |  |
| S25 | Table | Ethics review, consent, and funding | typed in the manuscript source |  |  |
