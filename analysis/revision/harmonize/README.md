# Harmonization builders (recovered)

These scripts turn the raw deposits and survey exports into the harmonized analysis files, one CSV per study card: `data/harmonized/*.csv` for the author's own surveys and `data/external/<dataset>/harmonized_<card>.csv` for external datasets. The scripts were written on 28 October 2026 (UTC) in a Claude Code session whose temporary scratchpad has since been deleted. On 17 November 2026 they were rebuilt from that session's transcripts. Each one was then run with its output redirected to a temporary folder and compared with the existing files. Results are in [`REPRODUCTION.md`](REPRODUCTION.md): 53 of the 55 harmonized CSVs are rebuilt byte for byte, and two differ only in `exclude` because an input changed after 28 October.

## Which script builds which file

| Script | Builds | Inputs |
|---|---|---|
| `own/sktw_naturalization_2024.R` | `data/harmonized/sktw_naturalization_2024.csv` | `replication/data/citizenship_conjoint/corpus.csv` |
| `own/rok2024_roc2024.R` | `rok2024_q40_election.csv`, `roc2024_q74_national.csv` | `~/Documents/github/research/replication/korean-election-conjoint/`, `.../projects/_archive/ROK-Experiments-2024/`, `.../projects/roc-natid-cbc/data/` |
| `own/nwo26.R` | `nwo26_exp2.csv`, `nwo26_exp3_comprehension.csv` | `.../projects/nwo26-immigration-backlash/tools/softlaunch-audit/data/snapshots/2026-08-31T1349Z/` and `tools/paper-figures/runs/2026-08-31T1349Z/long/sample_ids_<CC>.txt` |
| `own/prc_roc26.R` | `prc2026_q22_national.csv`, `prc2026_q60_ngo.csv`, `roc2026_q12_national.csv` | `.../projects/political-regimes-nationalism/` (prc-cbcs, roc-cbc26, replication) |
| `own/ifes2023.R` | the seven `ifes2023_*.csv` | `.../projects/_archive/IFES2023/` |
| `own/aks21.R` | the six `aks21_*.csv` | `.../projects/_archive/Pre-2024/AKS21/` and `NK-business-support_conjoint/Data/` |
| `own/ifes2023_manifest_notes.R` | nothing in `data/`; adds the IFES "not verified" notes to the manifests | manifests written by `ifes2023.R` |
| `own/build_index.R` | `data/harmonized/OWN_INDEX.md` | the manifests, the CSVs, `revision/study_cards/drafts/` (read only), and the `opencheck` package at `~/Documents/github/resources/opencheck` |
| `own/common.R` | helper sourced by every `own/` builder (`write_harmonized()`, output and manifest paths) | none |
| `external_wave1/build.R` | 18 files: `egami2022_prison_exp{2,3}`, `ofosu2024_malawi`, `bisgaard2019_study{1,3,4}`, `gidron2025_polarization`, `glazier2021_selfcoding`, `gadarian2014_anxiety`, `hartnett2022_whytrump`, `busby2019_study{1,2}`, `janssen2026_{england,us}`, `dur2026_{china,us}`, `ziegler2022_{kane,church}` | `data/external/<dataset>/raw/` |
| `external_wave1/inspect.R` | helper sourced by `build.R` (`fix_enc()`, `nonempty()`, `safe_table()`) | none |
| `external_wave1/run.sh` | the original wrapper: runs an R script with UTF-8 locale, stderr to a file | none |
| `external_wave2/dataverse/harmonize.R <card>` | one card per call: `john2022_citizenvoice`, `sambanis2025_externalthreat`, `davies2013_audiencecosts`, `marx2025_genderlanguage`, `bursztyn2023_dissent_{exp2,exp4,aux5}`, `lupu2024_lawsofwar`, `tripp2026_refugees`, `marsh2025_lifestyleconjoint` | `data/external/<dataset>/raw/` |
| `external_wave2/econ/harmonize.py` | `graeber2024_storiesmemory`, `andre2025_macronarratives`; `chopra2024_newsdemand` only if the cleaned files are supplied (see caveat 1) | `data/external/<dataset>/raw/`, plus a staging folder (2nd argument) |
| `external_wave2/econ/harmonize.recovered.py` | the same script without the repair (paths changed only) | as above; fails without the staged files |
| `external_wave2/osf/grat_clean_ids.R` | the ID list of the authors' cleaned gratitude sample (input to `osf/harmonize.py`) | `data/external/coles2026_gratitude/raw/GlobalGratitude_Final_Cleaned.Rds` |
| `external_wave2/osf/harmonize.py` | `ebersole2016_powerrecall`, `landrum2025_mechexplain`, `coles2026_gratitude` | `data/external/<dataset>/raw/` and the ID list |

The `own/` inputs live outside this repository, at absolute paths under `~/Documents/github/research/` that the scripts name. A replication package needs those exports copied in, or the paths changed. That has not been done here.

## How to run

Run everything from the repository root, in a UTF-8 locale (the October session lost text to `<U+XXXX>` escapes when R ran in the C locale). By default each script writes where the original did, which **overwrites the harmonized files**. To write elsewhere, set `HARMONIZE_OUT`. For the external builders, the `<dataset>/` subfolders must already exist under that folder.

```bash
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
H=analysis/revision/harmonize
export HARMONIZE_OUT=/some/tmp/own HARMONIZE_MANIFEST=/some/tmp/manifest    # omit to write to data/harmonized
for s in sktw_naturalization_2024 rok2024_roc2024 nwo26 prc_roc26 ifes2023 ifes2023_manifest_notes aks21 build_index; do
  Rscript $H/own/$s.R 2>> stderr.txt
done

export HARMONIZE_OUT=/some/tmp/external                                      # omit to write next to raw/
Rscript $H/external_wave1/build.R 2>> stderr.txt                             # or: $H/external_wave1/run.sh $H/external_wave1/build.R
for c in john2022_citizenvoice sambanis2025_externalthreat davies2013_audiencecosts marx2025_genderlanguage \
         bursztyn2023_dissent_exp2 bursztyn2023_dissent_exp4 bursztyn2023_dissent_aux5 lupu2024_lawsofwar \
         tripp2026_refugees marsh2025_lifestyleconjoint; do
  Rscript $H/external_wave2/dataverse/harmonize.R $c 2>> stderr.txt
done
python3 -I $H/external_wave2/econ/harmonize.py "$PWD/data/external" /path/to/stage
Rscript $H/external_wave2/osf/grat_clean_ids.R data/external/coles2026_gratitude/raw/GlobalGratitude_Final_Cleaned.Rds /some/tmp/grat_clean_ids.txt
python3 -I $H/external_wave2/osf/harmonize.py "$PWD/data/external" /some/tmp/grat_clean_ids.txt
```

Other variables: `HARMONIZE_MANIFEST` (own manifests; default `analysis/output/revision/harmonize_manifest`), `HARMONIZE_SUMMARY` (wave-1 summary RDS; default in `tempdir()`), `HARMONIZE_STDERR` (wave-1 `run.sh` stderr file). The builders print counts, column names and arm tables only, never text values. Send stderr to a file anyway, because R encoding warnings can echo strings.

The check on 17 November used R 4.6.1 (data.table 1.18.4, haven 2.5.5, readxl 1.4.5, readr 2.2.0, dplyr 1.2.1, jsonlite 2.0.0, pkgload 1.5.3), Python 3.14.8 and pandas 3.0.1 (from the user site-packages, which `econ/harmonize.py` adds to `sys.path` itself).

## How the scripts were recovered

Sources: the main transcript `~/.claude/projects/-home-buddleman-Documents-github-research-projects-lucdh-opentext-surveys/ac14b90b-b4fc-47f6-b280-5ec7accc121e.jsonl` and the subagent transcripts in `.../ac14b90b-b4fc-47f6-b280-5ec7accc121e/subagents/agent-<id>.jsonl`. Every `tool_use` block was indexed. Calls that appear more than once (forked agents copy their parent's history, and each file re-logs some calls without a timestamp) were deduplicated by tool-use id and attributed to the agent that made them, and the calls were ordered by their logged time. Each operation that touched a script was then replayed in that order: `Write` content, `Edit` string replacements, `cat >` / `cat >>` heredoc bodies, `sed -i` commands (run verbatim on a copy), and the Python snippets that rewrote a script in place (run verbatim on a copy). Failed calls were checked against their `tool_result`. Three scripts existed only as inline `Rscript -e` code and were extracted verbatim into files. The first-pass replay (`recover.py`) failed for `common.R`, `nwo26.R` and `prc_roc26.R` because it ignored the `sed -i` and Python rewrites in between and ordered untimestamped duplicates first. With those replayed, every Edit applies cleanly.

Operations replayed per script (UTC times on 2026-10-28, agent id):

- `own/common.R` (scratchpad `own_harmonize/common.R`, agent a8dd92c41b140b1c9): Write 21:12:59; `sed -i` 21:13:06; Edit 21:24:22; Edit 21:24:28; `sed -i` 21:27:15.
- `own/rok2024_roc2024.R` (a8dd92c41b140b1c9): Write 21:20:11; `sed -i` 21:20:30; `sed -i` 21:25:38; Python rewrite 21:27:41.
- `own/nwo26.R`: Write 21:19:21 and Python rewrite 21:20:44 (fork abb3ff4abafc1417b); `sed -i` 21:25:38 and Edit 21:26:48 (a8dd92c41b140b1c9).
- `own/prc_roc26.R`: Write 21:19:34, Python rewrite 21:22:32, Edit 21:22:44, `sed -i` 21:22:53, Python rewrite 21:23:09 (fork af15fcc752ca7a957); `sed -i` 21:25:38 (a8dd92c41b140b1c9).
- `own/ifes2023.R` (fork ac675e364395e6544): Write 21:21:37; Python rewrites 21:21:51, 21:22:44, 21:25:52.
- `own/ifes2023_manifest_notes.R` (fork ac675e364395e6544): inline `Rscript -e`, 21:25:45.
- `own/aks21.R` (fork a159ac9719aee5a59): Write 21:23:22; `sed -i` 21:23:40, 21:26:12, 21:26:44.
- `own/build_index.R` (a8dd92c41b140b1c9): Write 21:23:20; Python rewrites 21:28:05, 21:28:27.
- `own/sktw_naturalization_2024.R` (main thread): inline `Rscript -e`, 21:20:11. That call first moved aside an earlier copy of the file that had been written in a C locale. The code here is the corrected rebuild.
- `external_wave1/inspect.R` (agent aa95643b7b4dc3d41): `cat >` 21:27:24; `sed -i` 21:28:17; `cat >>` 21:33:54. `run.sh`: `cat >` 21:28:17.
- `external_wave1/build.R` (aa95643b7b4dc3d41): Write 21:38:46; Python rewrites 21:38:57, 21:39:48, 21:41:16. The last run (21:41:16) matches the 22:41 local file times.
- `external_wave2/dataverse/harmonize.R` (scratchpad `wave2_scripts/harmonize/harmonize.R`, agent a4d76d3791c417768): `cat >` 21:37:38; Python rewrite 21:38:28.
- `external_wave2/econ/harmonize.py` (scratchpad `wave2_scripts/econ/harmonize.py`, agent a0a7d9f7f0257a5b5, shell variable `$D`): `cat >` 21:30:16; `sed -i` 21:30:30, 21:32:02.
- `external_wave2/osf/harmonize.py` (scratchpad `wave2_scripts/osf/harmonize.py`, agent a5416643704dd1ce6): `cat >` 21:32:12 (the call is marked as an error because the run that followed hit a `UnicodeDecodeError`, but the file was written; the next call edits it); `sed -i` 21:32:28 (input encoding set to `mac_roman`).
- `external_wave2/osf/grat_clean_ids.R` (a5416643704dd1ce6): inline `Rscript -e`, 21:31:52.

## Changes made on recovery

- **Path changes only**, each marked `# path changed on recovery (2026-11-17)`: `source()` lines now point into this folder. Absolute repository paths are now relative to the repository root. Output roots can be overridden by `HARMONIZE_OUT` (and the manifest, summary and stderr locations by the variables above), with defaults that keep the original destinations. Scratchpad-only locations (own manifests, wave-1 `summary.rds`, `run.sh` stderr file) moved to the defaults listed above. `run.sh` now changes to the repository root instead of `data/external`. In the Python scripts an output root `O` was added and the three output paths use it.
- **Inline code extracted to files**: `own/sktw_naturalization_2024.R`, `own/ifes2023_manifest_notes.R`, `external_wave2/osf/grat_clean_ids.R`. Each has a header naming its source call. The code is verbatim apart from the marked path lines.
- **One repair**: `external_wave2/econ/harmonize.py` wraps the Chopra block in a check for the cleaned files and skips it when they are missing, so that the Graeber and Andre blocks after it can run. The unrepaired script (path changes only) is kept as `harmonize.recovered.py`.

## Caveats

1. **`chopra2024_newsdemand` needed a second download.** Its `exclude` flag needs the authors' cleaned files `study1_cleaned.dta` and `study2_cleaned.dta` from the Zenodo package (10.5281/zenodo.21244470). In October they were unzipped into the scratchpad and never copied to `raw/`. On 17 November the package was downloaded again (`raw/MS20260707-replication.zip`), the two files were extracted to `raw/cleaned/`, and the builder now falls back to that folder when the staging path is absent. The rebuilt file is identical to the analyzed one.
2. **`nwo26_exp2` and `nwo26_exp3_comprehension` differ from the 28 October files in `exclude` only** (59 and 24 German respondents now included). The exclusion lists `sample_ids_<CC>.txt` in the nwo26 project were regenerated on 2026-11-05, after the files were built. Every other cell is identical. The originals are therefore the files that were built from the 28 October inputs. Re-running the builder today would change which German respondents are flagged in the files the splits were drawn from. Decide which sample is correct before rebuilding.
3. **`OWN_INDEX.md`** rebuilds identical except the two nwo26 "N excluded" counts (caveat 2), but only when `ifes2023_manifest_notes.R` runs between `ifes2023.R` and `build_index.R`. Without it the seven IFES "not verified" notes are missing. `build_index.R` also needs the `opencheck` package at `~/Documents/github/resources/opencheck` and the draft cards.
4. **Run order inside the October session was not one clean pass.** Scripts were edited and re-run many times. What is recovered is each script's final state. Matching output hashes for 52 files show that this final state produces the files on disk. In the replay every `Edit` found its target string, every `replace()` target in the Python rewrites was present, and every `sed -i` changed the file. A `sed` command with several `s///` expressions was only checked as a whole, and the output hashes cover that.
5. **Inputs outside the repository.** All `own/` builders read exports in other project folders under `~/Documents/github/research/`. They all existed on 17 November. Apart from the nwo26 exclusion lists, they were unchanged as far as the identical outputs show.
6. **Encoding choices recorded in the code**: `osf/harmonize.py` reads the Many Labs 3 CSV as `mac_roman`; wave-1 `build.R` decodes invalid UTF-8 as CP1252 with a Latin-1 fallback, and strict Latin-1 for the Ziegler church text; wave-2 `harmonize.R` undoes double encoding (Lupu & Wallace, Tripp & Kam). See `data/external/HARMONIZED_INDEX.md` and `INDEX_wave2.md`.

## Scripts from the session that are not builders (not included)

No builder sources or reads any of these. They inspected, checked or downloaded data, and they can be recovered from the same transcripts if wanted.

- Own surveys: `ifes2023_explore.R`, `ifes2023_explore2.R`, `ifes2023_levels.R`, `ifes2023_verify.R` (its results are the text of `ifes2023_manifest_notes.R`), `ifes2023_textcheck.R`, `verify_aks21.R`, `nwo26_explore.R`, `nwo26_explore2.R`, `nwo26_explore3.R`, `nwo26_explore4.R`, `explore_prc.R`, `explore_prc2.R`, `explore_prc3.R`, `explore_roc.R`.
- External wave 1 checks (agent aa95643b7b4dc3d41): `bis_check.R`, `bis_check2.R`, `bs_check.R`, `bytes_check.R`, `zg_check.R`, `zg_check2.R`, `zg_bytes.R`, `bh_check.R`, `bh_check2.R`, `bh_check3.R`, `gg_check.R`, `gg_check2.R`, `gid_check.R`, `jan_check.R`, `jan_check2.R`, `dur_check.R`, `dur_check2.R`, `orig_check.R`, `hart_check.R`, `fix_check.R`, `verify.R`, `index_rows.R`.
- Acquisition (agent a439447b32c4d8e73): `ext_scripts/dv_meta.R`, `ext_scripts/inspect.R`, `ext_scripts/html2txt.py`, `dl.sh`, `filetable.py`, `add_context.py`.
- Wave-2 search and download (agents a4d76d3791c417768, a0a7d9f7f0257a5b5, a5416643704dd1ce6): `dvsearch.py`, `dvbatch.py`, `dvinfo.py`, `dvfile.py`, `dvquick.py`, `dvcite.py`, `dvget.py`, `wave2_scripts/inspect.R`, `tess.py`, `osfls.py`, `zen.py`, `manifest/write_manifests.py`; econ `search.py`, `files.py`, `ddi.py`, `inspect.py`, `inspect_sav.R`; OSF `osf_tree.py`, `csv_profile.py`, `ml3_meta.py`, `ml3_xtab.py`, `osf_search.py`, `docx_text.py`, `sav_meta.py`, `code_counts.py`, `grat_levels.py`.

## Recovery tooling

`recovery_tools/` holds the scripts that replayed the October transcripts (`extract.py`, `replay.py`, `ops.py`, `lib.py`, `spec.py`, `occ.py`, `install.py`, `show.py`, `checkpy.py`), the rebuild driver (`run_rebuild.sh`), the comparison (`compare.py`), and the first-pass replay (`recover_first_pass.py`). They read the Claude Code session transcripts under `~/.claude/projects/`, which are not part of this repository, so they document how the recovery was done rather than offer a rerunnable step.
