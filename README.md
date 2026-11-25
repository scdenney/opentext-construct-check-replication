# What Were They Thinking? Open-Text Answers as a Construct Check in Survey Experiments

Replication materials for the working paper by Steven Denney (Leiden University, s.c.denney@hum.leidenuniv.nl). The paper is on SSRN: <https://papers.ssrn.com/abstract=6912419>.

The same materials will be deposited in a Dataverse, with a DOI, upon publication. Checked from a clean copy on 2026-11-25 (see "What was checked").

## What this package reproduces

Every table and figure of the article and its Supplementary Information, and the numbers quoted in the text. The article reanalyzes 303,080 open-text answers to 139 items from survey experiments. For each item the package holds the pre-specification record, the split into discovery and estimation samples, the lockfile, the language-model labels and topic-model fits, and the estimates. It holds the answers themselves for 88 items. Seven items from the author's surveys will be added once the papers that report those surveys are published. The other 44 come from public deposits whose terms do not allow reposting; the package names each source and includes the script that rebuilds the analysis file from it.

## How to run

From the package root, with R 4.6 and the packages in `renv.lock`:

```
Rscript master.R               # tables and figures from the stored estimates
Rscript master.R estimation    # re-estimate the contrasts of the deposited studies and compare with the stored ones
Rscript master.R audit         # the permutation audit (CPU, hours)
```

`master.R` writes its logs and `session_info_master.log` to `analysis/output/revision/`. The stages are described in `analysis/revision/README.md`, and `analysis/revision/run_all.sh` lists every script in order.

What each stage needs:

| Stage | Needs | Runs here |
|---|---|---|
| Measurement: labels from language models, topic-model fits | The answers, and a GPU cluster or a paid API | No. Its outputs are in `data/results/a1/` and `analysis/stm_fits/` |
| Estimation: treatment contrasts on the estimation samples | The answers, labels, splits and lockfiles | Yes, for the studies whose answers are present |
| Reporting: tables, figures, numbers in the text | The stored estimates; seven scripts also read answers that are not all here | Yes. See "What was checked" |

## Software requirements

- R 4.6.1. Package versions are in `renv.lock`, which was written from the session recorded in `analysis/output/revision/session_info.txt`, not by `renv::snapshot()`. The scripts also need `pkgload`, which loads the bundled `opencheck/` package (version 0.0.0.9000, commit `4c29455`).
- `bash`. Python 3 for `rv_14`, `rv_20`, `rv_21` and the Python harmonization scripts.
- XeLaTeX is not needed: the package writes figures (PDF) and table rows (`.tex`), not the article.
- The analysis ran on macOS 27 (Apple silicon) and was rerun on Linux (x86-64) for this package.
- Runtime on a 24-core Linux workstation: reporting 8 minutes (one core); estimation of the 88 studies 5 minutes (12 cores). The audit took hours of CPU time when the article was written and was not rerun here.

## Data sources

`docs/data_sources.csv` has one row per study: where its answers are, the source deposit and files, the license and the date it was checked, the script that builds the analysis file, and the reason for the decision.

| Source | Items | Answers | Where | Terms |
|---|---:|---:|---|---|
| The author's surveys, available now | 14 | 26,554 | `data/harmonized/` | CC BY 4.0 |
| The author's surveys, to be added on publication of their own papers | 7 | 29,261 | Not yet included | |
| Public datasets, CC0 or CC BY 4.0 | 59 | 141,041 | `data/external/<deposit>/harmonized_<study>.csv` | Those of each source; see `docs/ATTRIBUTION.md` |
| Public datasets, CC BY-NC 4.0 | 15 | 18,100 | `data/external/<deposit>/harmonized_<study>.csv` | CC BY-NC 4.0; see `docs/ATTRIBUTION.md` |
| Public datasets with no license that allows reposting | 44 | 100,638 | Not included; cited in `docs/data_sources.csv` | Those of each source |

Counts are non-empty answers before exclusion flags. `docs/data_sources.csv` says in the column `in_this_repository` whether a study's answers are here.

I have the right to redistribute every data file in this package: the author's surveys are mine, and each public file carries a license that permits redistribution.

To add one of the 44 linked datasets, download the files named in the `source_files` column of `docs/data_sources.csv` from the source in `source_id` into `data/external/<deposit>/raw/`, and run the script in `rebuild_script`. It writes `data/external/<deposit>/harmonized_<study>.csv`, the file the analysis reads. `analysis/revision/harmonize/README.md` and `REPRODUCTION.md` describe the builders. The labels, split and lockfile of the study are already here.

The harmonized files of the author's surveys were built from vendor exports by `analysis/revision/harmonize/own/`. Those exports are not deposited, and each of these scripts names their folder on the author's machine at its top.

## Folder structure

```
master.R                      entry point
README.md, LICENSE.md, renv.lock
analysis/revision/            analysis code (rv_*.R, run_all.sh) and harmonization builders
analysis/output/revision/     stored estimates, tables and logs
analysis/stm_fits/            saved topic models
data/harmonized/              answers, the author's surveys
data/external/                answers, public datasets (harmonized copies)
data/results/a1/              labels: <study>/<model>/measures.csv
data/nk_entrepreneurship/     benchmark with two coders' codes
opencheck/                    the R package that implements the protocol
revision/registry/            pre-specification records (study cards)
revision/splits/              split manifests
revision/locks/               lockfiles
revision/codebooks_induced/   codebooks induced from discovery samples
revision/codebooks_nk/        codebooks of the benchmark
revision/access_log.jsonl     every read of a discovery or estimation sample
revision/deviations.md        every departure from a record or from the planned analysis
paper/revision/               figures and table rows of the article and the SI
docs/                         codebook, variable list, sources, attribution, crosswalk
```

## Documentation

- `docs/codebook.md` describes every kind of data file, and `docs/codebook_variables.csv` lists every variable of every answer file.
- `docs/crosswalk.md` maps each figure and table to the script that writes it and to its file.
- `docs/ATTRIBUTION.md` gives the license, source and citation of each public file.

## What was checked

On 25 November 2026 this tree was copied to an empty folder on a Linux workstation and run there.

- `Rscript master.R` finished without error in 8 minutes. With the answers of 88 of the 139 studies present it runs nine of the 16 reporting scripts. The 21 files of SI table rows they write are identical to the stored ones, and six CSV tables differ from the stored ones only in the last digits (largest difference 1e-15, from rounding on a different machine). The figures were redrawn and not compared pixel by pixel.
- `Rscript master.R estimation` recomputed all 8,012 estimates for the 88 studies whose answers are here, in 5 minutes on 12 cores. Every one matches the stored value (largest difference 1e-14 in an estimate, 1e-15 in a p-value).

Not checked:

- The seven reporting scripts that read answers not in this repository: `rv_31`, `rv_33`, `rv_15`, `rv_27`, `rv_28` and `rv_18` read all 139 studies, and `rv_32` reads the two items of the 2026 four-country survey. `master.R` skips them, and the tables they write (respondent flow, benchmark checks, numbers quoted in the text, topic-model refits, prompt as treatment, study catalog, registered targets) are included as stored.
- Rebuilding the 44 linked datasets from fresh downloads.
- The permutation audit.

Known limits:

- Topic-model fits do not reproduce exactly across machines, so the saved fits in `analysis/stm_fits/` are the ones to use (the SI's section on structural topic model diagnostics reports refits).
- The lockfiles, some logs and the builders for the author's surveys contain paths on the author's machines. No analysis script that `master.R` runs depends on them.
- Language-model labels were not regenerated for this package.

## Citation

Denney, Steven. "What Were They Thinking? Open-Text Answers as a Construct Check in Survey Experiments." Cite the article and this dataset once they have DOIs.

## License

See `LICENSE.md`: code under MIT, the author's data and documentation under CC BY 4.0, and each public dataset under the license of its source (CC0, CC BY 4.0 or CC BY-NC 4.0).

## Attribution

The package follows the conventions of Yusaku Horiuchi's [replication-package-guide](https://github.com/yhoriuchi/replication-package-guide) and the FAIR principles (Wilkinson et al. 2016, doi:10.1038/sdata.2016.18).
