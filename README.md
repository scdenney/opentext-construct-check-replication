# What Were They Thinking? Open-Text Answers as a Construct Check in Survey Experiments

Replication materials for the working paper by Steven Denney (Leiden University), November 2026.

This repository is temporary. The materials will be deposited in a Dataverse upon publication.

## What is here

| Path | Contents |
|---|---|
| `analysis/revision/` | Analysis code. Its `README.md` lists the steps, and `run_all.sh` is the master script. |
| `opencheck/` | The `opencheck` R package at the commit that produced the results (`4c29455`, version 0.0.0.9000). |
| `revision/registry/` | Registered pre-specification records (study cards) and the registry log `registry.jsonl`. |
| `revision/locks/` | Lockfiles with the hashes of each split, record, prompt set, and model setting. |
| `revision/access_log.jsonl` | Every read of a discovery or estimation sample. |
| `revision/deviations.md` | Every departure from a record or from the planned analysis. |
| `revision/study_metadata.csv`, `revision/dataset_citations.csv` | Study metadata and the source of each dataset. |
| `revision/coding/` | The displayed-content coding of the primary contrasts and the rule it follows. |
| `analysis/output/revision/` | Stored estimates and tables at the study and contrast level, and `session_info.txt`. |
| `paper/revision/figures/`, `paper/revision/si_tables/` | The figures and table rows of the article and the Supplementary Information. |
| `docs/crosswalk.md` | Each figure and table, the script that writes it, and its file. |

## What is not here

No survey response is in this repository, and no file with one row per respondent. That leaves out the raw and harmonized data, the split manifests, the model labels, the saved topic-model fits, and the codebooks induced from responses.

The public datasets are not redistributed. `revision/dataset_citations.csv` gives the source article or deposit of each. The author's own surveys are not public.

## Running the code

Paths are relative to the repository root. The analysis ran on R 4.6.1, and `analysis/output/revision/session_info.txt` records the package versions.

`analysis/revision/rv_config.R` loads `opencheck` from `~/Documents/github/resources/opencheck`. Point that line to the `opencheck/` folder here.

`bash analysis/revision/run_all.sh` runs the reporting stage, and `bash analysis/revision/run_all.sh audit` runs the permutation audit. The measurement and estimation stages, listed at the top of that script, need the response data and a GPU cluster or an API.

The scripts have not yet been run from a clean copy of this repository.

## License

No license has been set yet.

The file checklist follows Yusaku Horiuchi's [replication-package-guide](https://github.com/yhoriuchi/replication-package-guide).
