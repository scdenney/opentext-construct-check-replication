# ==============================================================================
# rv_01_splits.R -- draw and lock splits for every registered dataset
# ==============================================================================
# For each registered study card with a harmonized data file: draw the holdout
# split (seed 42; split 1 primary, 2-11 sensitivity) and the two-fold split
# (seed 4242), stratified by treatment (and country/language when present).
# Manifests are written once; rerunning only verifies them. Writes the corpus
# table (N per dataset and fold).
# ==============================================================================

source("analysis/revision/rv_config.R")

cards <- registered_cards()
rows <- list()
for (id in names(cards)) {
  card <- oc_study_read(cards[[id]])
  d <- tryCatch(read_harmonized(id), error = function(e) NULL)
  if (is.null(d)) {
    message("No harmonized data for ", id, "; skipped.")
    next
  }
  d <- d[nchar(trimws(d$text)) > 0, ]
  strata <- intersect(c("treatment", "country", "lang", EXTRA_STRATA[[id]]), names(d))
  strata <- strata[vapply(strata, function(s) length(unique(d[[s]])) > 1, logical(1))]
  f1 <- file.path(SPLITS, paste0(id, ".holdout.json"))
  f2 <- file.path(SPLITS, paste0(id, ".twofold.json"))
  if (!file.exists(f1)) {
    oc_split_write(oc_split(d, "id", strata = strata, p_discovery = P_DISCOVERY,
                            seed = SPLIT_SEED, n_splits = N_SPLITS), f1)
  }
  if (!file.exists(f2)) {
    oc_split_write(oc_split(d, "id", strata = strata, seed = TWOFOLD_SEED,
                            scheme = "twofold"), f2)
  }
  s <- oc_split_read(f1)
  oc_split_read(f2)
  s1 <- s[s$split == 1, ]
  rows[[id]] <- data.frame(
    dataset = id, title = card$title, languages = paste(card$languages, collapse = "/"),
    unit = card$unit, n_text = nrow(d), n_D = sum(s1$fold == "D"), n_E = sum(s1$fold == "E"),
    strata = paste(strata, collapse = "+"),
    written_blind = isTRUE(card$provenance$written_before_reading_text),
    stringsAsFactors = FALSE)
}
tab <- do.call(rbind, rows)
write.csv(tab, file.path(OUT, "corpus_table.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(tab[, c("dataset", "languages", "n_text", "n_D", "n_E", "strata")], row.names = FALSE)
