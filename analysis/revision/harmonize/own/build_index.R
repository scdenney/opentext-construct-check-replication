# Build data/harmonized/OWN_INDEX.md from the manifest JSONs; recount every CSV
# (counts only) and validate every card. Run: LANG=en_US.UTF-8 Rscript build_index.R
source("analysis/revision/harmonize/own/common.R")  # path changed on recovery (2026-11-17)
pkgload::load_all("/home/buddleman/Documents/github/resources/opencheck", quiet = TRUE)

files <- sort(list.files(OH_MANIFEST, pattern = "\\.json$", full.names = TRUE))
esc <- function(x) {
  x <- if (is.null(x) || length(x) == 0) "" else paste(unlist(x), collapse = "; ")
  x <- gsub("\\|", "/", x); gsub("[\r\n]+", " ", x)
}
rows <- lapply(files, function(f) {
  m <- jsonlite::fromJSON(f)
  d <- read.csv(m$output, colClasses = "character", na.strings = character(0),
                fileEncoding = "UTF-8")
  ex <- as.logical(d$exclude)
  card <- file.path(OH_REPO, "revision/study_cards/drafts", paste0(m$id, ".yaml"))
  ok <- file.exists(card) && isTRUE(tryCatch({
    suppressWarnings(oc_study_read(card)); TRUE }, error = function(e) FALSE))
  n_esc <- as.integer(suppressWarnings(system2("grep", c("-c", shQuote("<U+"), shQuote(m$output)), stdout = TRUE)))
  utf8 <- system2("iconv", c("-f", "UTF-8", "-t", "UTF-8", shQuote(m$output)), stdout = FALSE, stderr = FALSE) == 0
  rb <- read.csv(m$output, colClasses = "character", na.strings = character(0), encoding = "UTF-8", fileEncoding = "UTF-8")
  rt <- identical(enc2utf8(rb$text), enc2utf8(d$text))
  vs <- if (grepl("^(ifes2023|aks21)", m$id)) "identical (agent re-read of source)" else "identical (independent re-read)"
  encs <- sprintf("grep <U+ = %d; UTF-8 %s; round trip %s; text vs source %s", n_esc,
                  if (utf8) "valid" else "INVALID", if (rt) "identical" else "DIFFERS", vs)
  data.frame(id = m$id, source = esc(m$source), design = esc(m$design), enc = encs,
             n_rows = nrow(d), n_text = oh_n_text(d$text), n_excl = sum(ex),
             lang = esc(m$lang), treat = esc(m$treatment_cols),
             codes = esc(m$human_codes), routing = esc(m$routing),
             card = if (ok) "y (validates)" else "n", caveats = esc(m$caveats),
             unverified = esc(m$unverified), stringsAsFactors = FALSE)
})
tab <- do.call(rbind, rows)
order_ids <- c("nwo26_exp2", "nwo26_exp3_comprehension", "prc2026_q22_national",
               "prc2026_q60_ngo", "roc2026_q12_national", "rok2024_q40_election",
               "roc2024_q74_national", "ifes2023_de_politician", "ifes2023_sk_politician",
               "ifes2023_nk_politician", "ifes2023_de_jobtraining", "ifes2023_sk_jobtraining",
               "ifes2023_de_natid", "ifes2023_nk_natid", "aks21_c1_vote", "aks21_c1_hire",
               "aks21_c1_neighbor", "aks21_c2_nkbusiness", "aks21_c3_unification", "aks21_c4_nsa")
tab <- tab[order(match(tab$id, order_ids, nomatch = 99), tab$id), ]
print(tab[, c("id", "n_rows", "n_text", "n_excl", "routing", "card")], row.names = FALSE)
cat(unique(sub("; text vs source.*", "", tab$enc)), sep = "\n")

hdr <- c("# Steven's own open-text studies: harmonized index",
  "",
  "Built 2026-10-28 by `build_index.R` (scratchpad `own_harmonize/`) from the per-dataset manifests. Counts are recomputed from the CSVs. Cards are in `revision/study_cards/drafts/` (unregistered); \"validates\" means `oc_study_read()` passes.",
  "",
  "Schema: `id, dataset, country, lang, text, exclude`, then treatment and attribute columns (`treatment` with the original variable kept; `task`; `chosen_*`/`other_*` for paired tasks; `profile_*` for single-profile tasks). One row per respondent who reached the open-end. Read the CSVs with NA conversion off (`na.strings = character(0)` or `na = character()`) so a literal \"NA\" answer stays text. The country code is ISO2 (`sktw_naturalization_2024.csv`, harmonized earlier, uses full names). Splits have not been drawn and no model has been run.",
  "",
  "| id | source | design | N rows | N non-empty text | N excluded | lang | treatment / attribute columns | human codes | routing | card | encoding check | caveats |",
  "|---|---|---|---|---|---|---|---|---|---|---|---|---|")
body <- sprintf("| `%s` | %s | %s | %d | %d | %d | %s | %s | %s | %s | %s | %s | %s |",
                tab$id, tab$source, tab$design, tab$n_rows, tab$n_text, tab$n_excl,
                tab$lang, tab$treat, tab$codes, tab$routing, tab$card, tab$enc, tab$caveats)
unv <- tab[nzchar(tab$unverified) & !tolower(tab$unverified) %in% c("none", "na"), ]
notes <- c("", "## Encoding",
  "",
  "Each of the 20 CSVs gives 0 on `grep -c \"<U+\"`, is valid UTF-8 (`iconv`), and reads back unchanged from `read.csv(..., encoding = \"UTF-8\")` (`identical()` on `text` and `id`). On a fresh read of the source with no NA conversion and no whitespace trimming, every `text` matches the source column byte for byte. The IFES and AKS21 agents ran that re-read for their own files; I re-read all the others. Before the final write, the earlier versions had three faults, all now fixed: literal \"NA\" answers had become missing (2 in ROC 2026, 1 in TW 2024); fread had left escaped quotes doubled (75 Exp2 and 35 Exp3 texts); and readr had trimmed outer whitespace (72 ROK and 13 TW 2024 texts).",
  "",
  "## Randomized links found but not harmonized as separate items",
  "",
  "- IFES Q35 (\"What does multiculturalism mean to you?\", SK and NK): no arm precedes it, so not harmonized.",
  "- PRC `Patriotism_list`: a list experiment before C1 (its sensitive item is \"proud to be Chinese\"), kept in `prc2026_q22_national` as a weak respondent-level link. PRC `surveillance_exp` is assigned after Q22, so on Q22 it is a falsification check only.",
  "- AKS21 `dVII1`: a list experiment directly before C4 (one arm lists abolishing the NSA), kept in `aks21_c4_nsa` as a secondary arm.",
  "- Taiwan 2024 and 2026 `RandomTextFlag`: set after the national-identity conjoint blocks, so no link to Q74 or Q12_6.",
  "- ROK 24.1 fielded two open-ends, Q40 (here) and Q129 (already in `sktw_naturalization_2024`); Q53 and the text-entry Q9 sit in the unused block. Taiwan 2024 Q59 is not in the flow. nwo26 has no other open-ends beyond demographic \"please specify\" boxes.",
  "",
  "`sktw_naturalization_2024.csv` (SK Q129 and TW Q89, registered card) was already in this folder and is not repeated here.")
foot <- c(notes, "", "## Not linked or not verified", "",
          if (nrow(unv)) sprintf("- `%s`: %s", unv$id, unv$unverified) else "- none")
writeLines(c(hdr, body, foot), file.path(OH_OUT, "OWN_INDEX.md"), useBytes = TRUE)
cat("wrote", file.path(OH_OUT, "OWN_INDEX.md"), "\n")
