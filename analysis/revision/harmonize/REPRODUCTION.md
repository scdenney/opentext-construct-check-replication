# Reproduction check

Run on 2026-11-17 from the repository root with `LANG=en_US.UTF-8`, every builder writing to a scratchpad folder through `HARMONIZE_OUT` (and `HARMONIZE_MANIFEST` / `HARMONIZE_SUMMARY`). Each rebuilt file was compared with the existing one by SHA-256 and, where the hashes differ, by dimensions, column names and the number of differing cells per column, matching rows on `id`. No cell values were printed. All 22 builder runs exited 0 with empty stderr. The SHA-256 of every file in `data/harmonized/` and every `data/external/*/harmonized_*.csv` was the same before and after the run. The wave-1 builder was also run through `external_wave1/run.sh`, and its 18 outputs were byte-identical as well.

Summary: 55 harmonized CSVs. 53 IDENTICAL, 2 DIFFERS (`exclude` only, input changed after the build). Every CSV has a recovered builder.

| card id | builder | status |
|---|---|---|
| `aks21_c1_hire` | `own/aks21.R` | IDENTICAL (SHA-256 52e0a2992c1757dd…) |
| `aks21_c1_neighbor` | `own/aks21.R` | IDENTICAL (SHA-256 9c17bdce84d96528…) |
| `aks21_c1_vote` | `own/aks21.R` | IDENTICAL (SHA-256 3e6d6115828703dc…) |
| `aks21_c2_nkbusiness` | `own/aks21.R` | IDENTICAL (SHA-256 67ef81b0fa904e90…) |
| `aks21_c3_unification` | `own/aks21.R` | IDENTICAL (SHA-256 92d67ce15fe626c8…) |
| `aks21_c4_nsa` | `own/aks21.R` | IDENTICAL (SHA-256 fe76f47f5cd7a709…) |
| `ifes2023_de_jobtraining` | `own/ifes2023.R` | IDENTICAL (SHA-256 ad4fc803f9d80464…) |
| `ifes2023_de_natid` | `own/ifes2023.R` | IDENTICAL (SHA-256 0e1e42a729549f0f…) |
| `ifes2023_de_politician` | `own/ifes2023.R` | IDENTICAL (SHA-256 ced92761140f8928…) |
| `ifes2023_nk_natid` | `own/ifes2023.R` | IDENTICAL (SHA-256 e88c21a26770d299…) |
| `ifes2023_nk_politician` | `own/ifes2023.R` | IDENTICAL (SHA-256 9400de9c3fe4f4e7…) |
| `ifes2023_sk_jobtraining` | `own/ifes2023.R` | IDENTICAL (SHA-256 a21ce63d056163cd…) |
| `ifes2023_sk_politician` | `own/ifes2023.R` | IDENTICAL (SHA-256 0d293e812c56f0e6…) |
| `nwo26_exp2` | `own/nwo26.R` | DIFFERS: SHA-256 be818e49f241… vs 3af661b5379d…; same dimensions 11,626 × 18, same column names and order, same ids in the same order; `exclude` differs in 59 cells, all other columns 0. All are German (DE) respondents flagged TRUE in the original and FALSE now: the exclusion list `sample_ids_DE.txt` in the nwo26 project was regenerated on 2026-11-05, after the 28 October build. |
| `nwo26_exp3_comprehension` | `own/nwo26.R` | DIFFERS: SHA-256 c2c7ab7a2224… vs b151b485d46f…; same dimensions 4,024 × 9, same column names and order, same ids in the same order; `exclude` differs in 24 cells, all other columns 0. All are German (DE) respondents flagged TRUE in the original and FALSE now: the exclusion list `sample_ids_DE.txt` in the nwo26 project was regenerated on 2026-11-05, after the 28 October build. |
| `prc2026_q22_national` | `own/prc_roc26.R` | IDENTICAL (SHA-256 fa296933d406d42e…) |
| `prc2026_q60_ngo` | `own/prc_roc26.R` | IDENTICAL (SHA-256 a2dc46658da79fbd…) |
| `roc2024_q74_national` | `own/rok2024_roc2024.R` | IDENTICAL (SHA-256 c0ae577a56e3a79f…) |
| `roc2026_q12_national` | `own/prc_roc26.R` | IDENTICAL (SHA-256 75b9938b540111e9…) |
| `rok2024_q40_election` | `own/rok2024_roc2024.R` | IDENTICAL (SHA-256 ef771942f1ba9ce8…) |
| `sktw_naturalization_2024` | `own/sktw_naturalization_2024.R` | IDENTICAL (SHA-256 892ee3b82ef31e1f…) |
| `OWN_INDEX.md` | `own/build_index.R` | DIFFERS: 61 vs 61 lines, 2 lines differ; the only differing cells are the two nwo26 'N excluded' counts (1,630 → 1,571; 294 → 270), from the nwo26 input change below |
| `andre2025_macronarratives` | `external_wave2/econ/harmonize.py` | IDENTICAL (SHA-256 8299a4f8019d1a10…) |
| `bisgaard2019_study1` | `external_wave1/build.R` | IDENTICAL (SHA-256 7c8ad7af79bd29d2…) |
| `bisgaard2019_study3` | `external_wave1/build.R` | IDENTICAL (SHA-256 a79a5c22ebb29f93…) |
| `bisgaard2019_study4` | `external_wave1/build.R` | IDENTICAL (SHA-256 3d5022c3d183665f…) |
| `bursztyn2023_dissent_aux5` | `external_wave2/dataverse/harmonize.R bursztyn2023_dissent_aux5` | IDENTICAL (SHA-256 9c7d3e462fe4fc4d…) |
| `bursztyn2023_dissent_exp2` | `external_wave2/dataverse/harmonize.R bursztyn2023_dissent_exp2` | IDENTICAL (SHA-256 67dfdd5f8eb734cc…) |
| `bursztyn2023_dissent_exp4` | `external_wave2/dataverse/harmonize.R bursztyn2023_dissent_exp4` | IDENTICAL (SHA-256 b4a0eb732c49394a…) |
| `busby2019_study1` | `external_wave1/build.R` | IDENTICAL (SHA-256 268203c35006ffe5…) |
| `busby2019_study2` | `external_wave1/build.R` | IDENTICAL (SHA-256 69e77b1b00f9e503…) |
| `chopra2024_newsdemand` | `external_wave2/econ/harmonize.py` | IDENTICAL (second pass, 2026-11-17): the Zenodo package (10.5281/zenodo.21244470, `MS20260707-replication.zip`, md5 `8f66058e3d7388e5268d5be5cb3ce41c`) was downloaded again; `study{1,2}_cleaned.dta` are now kept in `raw/cleaned/` and the builder falls back to that folder. SHA-256 `46d32e3910871ab0...` matches the analyzed file. |
| `coles2026_gratitude` | `external_wave2/osf/harmonize.py (+ grat_clean_ids.R)` | IDENTICAL (SHA-256 6f1082ce07c30328…) |
| `davies2013_audiencecosts` | `external_wave2/dataverse/harmonize.R davies2013_audiencecosts` | IDENTICAL (SHA-256 2e941e06a725658e…) |
| `dur2026_china` | `external_wave1/build.R` | IDENTICAL (SHA-256 21eff34030e4a016…) |
| `dur2026_us` | `external_wave1/build.R` | IDENTICAL (SHA-256 cf60f911c490a8b1…) |
| `ebersole2016_powerrecall` | `external_wave2/osf/harmonize.py` | IDENTICAL (SHA-256 b91d5470c6b72a15…) |
| `egami2022_prison_exp2` | `external_wave1/build.R` | IDENTICAL (SHA-256 b26bd144790d0f01…) |
| `egami2022_prison_exp3` | `external_wave1/build.R` | IDENTICAL (SHA-256 09f61cedc6e949d1…) |
| `gadarian2014_anxiety` | `external_wave1/build.R` | IDENTICAL (SHA-256 433785c22afc28fd…) |
| `gidron2025_polarization` | `external_wave1/build.R` | IDENTICAL (SHA-256 77d77e180ab118a7…) |
| `glazier2021_selfcoding` | `external_wave1/build.R` | IDENTICAL (SHA-256 dd0678d117d4fc1f…) |
| `graeber2024_storiesmemory` | `external_wave2/econ/harmonize.py` | IDENTICAL (SHA-256 05dbe7b7fd366cb6…) |
| `hartnett2022_whytrump` | `external_wave1/build.R` | IDENTICAL (SHA-256 bd8d2fe6da120a9d…) |
| `janssen2026_england` | `external_wave1/build.R` | IDENTICAL (SHA-256 6971ebab563ddca4…) |
| `janssen2026_us` | `external_wave1/build.R` | IDENTICAL (SHA-256 a76cad0ffe7552c2…) |
| `john2022_citizenvoice` | `external_wave2/dataverse/harmonize.R john2022_citizenvoice` | IDENTICAL (SHA-256 75f76384ba5fb2f4…) |
| `landrum2025_mechexplain` | `external_wave2/osf/harmonize.py` | IDENTICAL (SHA-256 764bc7fa7296664c…) |
| `lupu2024_lawsofwar` | `external_wave2/dataverse/harmonize.R lupu2024_lawsofwar` | IDENTICAL (SHA-256 1f6ced83ec34e2cc…) |
| `marsh2025_lifestyleconjoint` | `external_wave2/dataverse/harmonize.R marsh2025_lifestyleconjoint` | IDENTICAL (SHA-256 f2e500abe2283ffd…) |
| `marx2025_genderlanguage` | `external_wave2/dataverse/harmonize.R marx2025_genderlanguage` | IDENTICAL (SHA-256 83b6089ad8e6adb9…) |
| `ofosu2024_malawi` | `external_wave1/build.R` | IDENTICAL (SHA-256 d63f41210d2e187d…) |
| `sambanis2025_externalthreat` | `external_wave2/dataverse/harmonize.R sambanis2025_externalthreat` | IDENTICAL (SHA-256 de5b40ace8647bc8…) |
| `tripp2026_refugees` | `external_wave2/dataverse/harmonize.R tripp2026_refugees` | IDENTICAL (SHA-256 7308294acc2795af…) |
| `ziegler2022_church` | `external_wave1/build.R` | IDENTICAL (SHA-256 6af6fb929df19022…) |
| `ziegler2022_kane` | `external_wave1/build.R` | IDENTICAL (SHA-256 3e9a6bf64b935c54…) |

Run order used: `own/` scripts `sktw_naturalization_2024`, `rok2024_roc2024`, `nwo26`, `prc_roc26`, `ifes2023`, `ifes2023_manifest_notes`, `aks21`, `build_index`; then `external_wave1/build.R`; the ten Dataverse cards; `econ/harmonize.py` (second argument pointing to an empty folder); `osf/grat_clean_ids.R`; `osf/harmonize.py`. See README.md for commands.
