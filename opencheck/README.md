# opencheck

Split-sample construct checks for open-text responses in survey experiments. Development version, 0.0.0.9000.

The package tests whether a randomized treatment changes the considerations respondents write in open-text answers. It does so without letting the estimation data shape the coding scheme. The protocol:

1. **Study card.** A YAML file stating the treatment, the target construct, placebo constructs and the estimand. It is written and registered before any text is read (`oc_study_register()`).
2. **Split.** `oc_split()` and `oc_split_write()` assign units once to a discovery fold D and an estimation fold E, stratified, seeded, and independent of row order.
3. **Discovery on D only.** `oc_discovery_data()` is the only accessor. `oc_assert_discovery()` checks hand-picked examples.
4. **Freeze.** `oc_freeze()` hashes the codebook, prompts, models and decoding settings into a lockfile. Lockfiles are written once.
5. **Estimation on E.** `oc_estimation_data()` opens E only with a valid lock: the files are unchanged and the lock was built on this split.

Implemented so far:
- Steps 1–5: study cards, splits, the fold lock, freezing, access logging.
- Codebook YAML (Halterman–Keith fields). Few-shot examples must state their source.
- Single-label and yes/no presence prompts.
- Backends: an OpenAI-compatible backend with first-token logprobs (vLLM, Ollama, OpenRouter, OpenAI), an offline backend for batch results, and a mock backend for tests.
- Resumable classification. Estimation-fold runs refuse prompts whose sources are not frozen.
- Agreement: Cohen's and Fleiss' kappa, majority vote.
- Effects: difference in means with a Neyman SE, randomization inference, and median aggregation across splits.
- Pairwise Bradley–Terry: pair sampler, ridge BT fit, held-out accuracy and ICI. The fit matches `fit_bt_fast` from the jev archive exactly.
- Topic-model exploration of the discovery fold (STM): fit, topic matching, projection onto E.
- ALICE jobs: request bundles, an offline vLLM runner (`inst/python/oc_vllm_runner.py`), SLURM scripts, and result import.

## Hardware tiers

`oc_profile("local" | "workstation" | "hpc")` gives the recommended models and backend. The protocol is identical at every tier.
- **Local:** a laptop with Ollama, Gemma 4 E4B.
- **Workstation:** one 24 GB GPU, Gemma 4 12B or Qwen3.6-27B.
- **HPC:** A40/A100 via SLURM, Qwen3.6-35B-A3B or Gemma 4 31B, plus API models.

In the validation corpus, Gemma 4 E4B matched the larger models against held-out human codes. That result comes from the full-precision model on a cluster GPU; a quantized Ollama copy has not been validated, so check it against a full-precision run on a sample first.

Sorting is locale-independent (`method = "radix"`), and all files are read and written as UTF-8 bytes. Splits, hashes and CJK text are therefore the same in every locale.

Planned: a BERTopic discovery wrapper, LLM codebook induction, an Anthropic backend and the Jev typed-decision backend, DSL/PPI correction, the permutation placebo through the whole pipeline, and reports.

```r
s <- oc_split(dat, id = "ResponseId", strata = c("treat", "lang"), seed = 42)
oc_split_write(s, "split.json")
d <- oc_discovery_data(dat, s)                       # build the codebook on d
oc_freeze("lock.json", s, components = c(codebook = "codebook.yaml",
                                         prompt = "prompt.txt"))
e <- oc_estimation_data(dat, s, "lock.json")         # classify and estimate on e
```

## Exploring the discovery fold with a topic model

Before freezing, a structural topic model can show what respondents write about, whether the target construct comes up at all, and whether a placebo is missing. Two rules hold: the model is fit on discovery-fold answers only (`oc_topics_fit()` refuses any other id), and treatment is never a covariate, because fitting treatment-dependent topic prevalence on the answers later tested inflates false positives. The topic model is for exploration; the frozen yes/no question is the measure. Matched topics can serve as a comparison measure on E.

```r
fit <- oc_topics_fit(d, s)                  # K chosen over 4-12; Korean: tokenizer = oc_tokenizer_kiwi()
fit                                         # words and example answers per topic
reqs <- oc_topics_match_requests(fit, "study.yaml")   # which topic matches each construct?
# ... run the requests, then
match <- oc_topics_match_import("topic_match_results.jsonl")
oc_topics_save(fit, "topics.rds")           # frozen with the rest, in place of the lock above
oc_freeze("lock.json", s, components = c(codebook = "codebook.yaml", topics = "topics.rds"))
stm <- oc_topics_measure(fit, e, match)     # stm__<construct> columns for oc_effect()
```

## Guided workflow

The package ships a skill for Claude Code and Codex that takes a researcher through the steps in order. It reads the questionnaire and the pre-analysis plan or paper, drafts the study card and asks the researcher to confirm each entry, draws the split, fits a topic model to the discovery fold, tries the yes/no questions there, writes the lock, and then estimates and reports every target and placebo on the estimation fold. Its reference script, `reference/pipeline.R`, runs all seven steps on a synthetic example with a mock model, so it can be tried without data or a model.

```sh
Rscript -e 'opencheck::oc_skill_install("claude")'   # Claude Code: ~/.claude/skills/opencheck
Rscript -e 'opencheck::oc_skill_install("codex")'    # Codex: ~/.codex/skills/opencheck
```

Tests: `testthat::test_local()`. The licence (MIT) is a placeholder until it is decided.
