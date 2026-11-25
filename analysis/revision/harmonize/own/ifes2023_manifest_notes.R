# Recovered 2026-11-17 from an inline `Rscript -e` in agent ac675e364395e6544 (IFES fork), Bash tool call at
# 2026-10-28T21:25:45Z. It writes the 'unverified' notes (results of ifes2023_verify.R and manual checks) into the
# seven IFES manifest JSONs, which ifes2023.R then carries forward and build_index.R prints in OWN_INDEX.md.
# Affects the index only, not the CSVs. Run after ifes2023.R and before build_index.R. Code verbatim except the marked line.
# The original read file.path("manifest", ...) relative to the scratchpad own_harmonize/ folder.
u <- list(
 ifes2023_de_politician = "Choice in task 6 agrees 100% with Germans/Data/german.pol-conjoint.csv (export 17.46) on 2,661 overlapping ids; 15 harmonized ids not in that file (it requires all 8 choices).",
 ifes2023_sk_politician = "Choice agrees 100% with skdf.pol-conjoint.csv (export 16.10) on 2,119 ids; 18 harmonized ids not there, 2 of its ids absent from export 16.45.",
 ifes2023_nk_politician = "Task-5 linkage not cross-checked: project NK wide files key on row number, not ResponseId. Eligibility rule reproduces CPCS n (309 vs 311 reported).",
 ifes2023_de_jobtraining = "Task-5 chosen attributes match replication/data/germany_job_training.csv for all 1,882 validated ids. Designed solidarity prime not fielded (no arm).",
 ifes2023_sk_jobtraining = "Task-4 chosen attributes match replication/data/korea_job_training.csv for 1,762 of 1,763 ids (1 replication id absent from export 16.45). Designed solidarity prime not fielded (no arm).",
 ifes2023_de_natid = "Religion arm derived from attribute-name columns plus final-report footnote; no randomizer field to verify assignment. Task-5 linkage not cross-checked (german.natid-conjoint.csv keys on row number).",
 ifes2023_nk_natid = "Task-4 linkage not cross-checked (nkdf.natid-conjoint.csv keys on row number).")
MANIFEST <- Sys.getenv("HARMONIZE_MANIFEST", "analysis/output/revision/harmonize_manifest")  # path changed on recovery (2026-11-17)
for (id in names(u)) { p <- file.path(MANIFEST, paste0(id, ".json")); m <- jsonlite::read_json(p); m$unverified <- u[[id]]; jsonlite::write_json(m, p, auto_unbox = TRUE, pretty = TRUE, null = "null") }  # path changed on recovery (2026-11-17): was file.path("manifest", ...)
cat("updated", length(u), "\n")
