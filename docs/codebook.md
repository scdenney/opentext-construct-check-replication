# Codebook

Every data file in the package is of one of the kinds below. `docs/codebook_variables.csv` lists every variable of every answer file, with its type, its number of rows and missing values, and its values or range. The constructs and their definitions are in the study cards (`revision/registry/<study>.<hash>.yaml`).

Missing values are empty cells or the string `NA`.

## Answer files

`data/harmonized/<study>.csv` (the author's surveys) and `data/external/<deposit>/harmonized_<study>.csv` (public datasets). One row per respondent who reached the open-text question, or per task in the four studies randomized at the task level. UTF-8, comma-separated.

| Variable | Description |
|---|---|
| `id` | Row identifier. The source's respondent or response identifier where it has one, otherwise the row order of the source file (noted in the study card). Joins the answers to the labels and the split manifests. |
| `dataset` | Study identifier, as in `revision/registry/` and SI Table S22. |
| `country` | ISO 3166-1 alpha-2 code of the respondent's country. |
| `lang` | Language of the answer (BCP 47 code). |
| `text` | The open-text answer, verbatim and trimmed. |
| `exclude` | `TRUE` for rows the original authors drop, rows without a randomized assignment, and pilots. These rows stay in the file and are dropped at analysis, together with empty answers. |
| `treatment` and other design columns | The randomized condition and the design variables the study card names. The contrasts built from them are defined in `analysis/revision/rv_treatments.R`. |
| `chosen_*`, `other_*`, `profile_*` | Conjoint designs: attribute levels of the profile chosen, of the profile not chosen, or of the single profile shown, in the task before the open-text question. |
| `human_*` | Human codes from the original study's archive, where it has them. |

## Labels

`data/results/a1/<study>/<model>/measures.csv`, one row per estimation-sample answer, keyed by `id`. The model folders are `qwen36_35b` (reference model), `qwen36_27b`, `gemma4_31b`, `gemma4_e4b`, `gpt6_luna`, `stm` (topic model without the treatment covariate) and `stmcov` (with it). Column names join a measure and a construct name with two underscores.

| Column | Description |
|---|---|
| `yes__<construct>` | 1 if the model answered yes to the question whether the answer expresses the construct, 0 if no. |
| `pyes__<construct>` | Probability of yes from the first token's log-probabilities, normalized over yes and no. Not available for `gpt6_luna`. |
| `pvalid__<construct>` | Probability mass on tokens that begin yes or no. |
| `rate__<construct>` | Rating from 0 (not at all) to 9 (the central point of the answer). |
| `bt__<construct>`, `btse__<construct>` | Bradley-Terry score from pairwise comparisons and its standard error (first target construct, reference model). |
| `stm__<construct>` | Share of the matched topic in the answer. Answers with no in-vocabulary word take the discovery-sample mean. |
| `stmne__<construct>` | The same share, missing for answers with no in-vocabulary word. |

`pairs_y.csv` in the same folder holds the pairwise comparisons: the two row identifiers, whether the pair is held out, and the probability that the first answer expresses the construct more strongly.

## Split manifests

`revision/splits/<study>.holdout.json` and `<study>.twofold.json`. `meta` records the scheme, the share of the discovery sample (0.3), the seed, the strata, the number of rows and a hash of the data. `assignments` gives, for every `id`, the sample it falls in under each of the 11 splits. Split 1 is the one the article reports.

## Topic models

`analysis/stm_fits/<study>.stm.rds` and `<study>.stmcov.rds` are R lists with the fitted model (`mod`), its vocabulary, the number of topics and the grid searched. `<study>.topic_choice.csv` records the topic matched to each target construct. The fits hold word lists and topic shares, not answers.

## Induced codebooks

`revision/codebooks_induced/<study>/k01.yaml` and so on: the multi-category codebook a language model drafted from the discovery sample of each split. They quote discovery-sample answers as examples, so they are present only for studies whose answers are deposited.

## Benchmark with human codes

`data/nk_entrepreneurship/whycheck.csv`: answers to the policy conjoint of the 2021 South Korean survey, with the codes of two trained coders.

| Variable | Description |
|---|---|
| `respid` | Respondent identifier. |
| `kor` | The answer in Korean. |
| `eng.original`, `eng.edited`, `eng.new` | English translations. |
| `codePW`, `codeCG` | Code assigned by each of the two coders. |
| `combine` | Whether the two coders agree. |
| `valuenew` | The reconciled code. |

`revision/codebooks_nk/` holds the codebooks compared in that benchmark.

## Stored estimates

`analysis/output/revision/effects_a1.csv` has one row per study, model, measure, construct and contrast: the estimate, its standard error and interval, the randomization-inference p-value (`p_ri`), the arm sizes (`n1`, `n0`), the control mean and the estimate in standard-deviation units. The other files in that folder are described in `analysis/revision/README.md`.
