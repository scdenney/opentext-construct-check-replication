# ==============================================================================
# rv_00b_pilot_check.R -- prompt-following on the discovery-fold pilot
# ==============================================================================
# For each pilot run: share of valid answers, mean probability mass on valid
# tokens, label distribution per prompt, and agreement between models on the
# presence labels. Usage: Rscript analysis/revision/rv_00b_pilot_check.R <dataset>
# ==============================================================================

source("analysis/revision/rv_config.R")
args <- commandArgs(trailingOnly = TRUE)
id <- args[1]
card_path <- registered_cards()[[id]]
card <- oc_study_read(card_path)
pr <- a1_prompts(card, card_path)
pr <- pr[vapply(pr, function(p) p$kind != "pairwise", logical(1))]
s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
d <- read_harmonized(id)
dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = "pilot check")
runs <- list.files(file.path(JOBS, "submitted"), pattern = "^pilot_")
# PILOT_RUNS=run1,run2 restricts the check to those runs (one fetch per run,
# not one per run per dataset: the login node is not to be polled in a loop).
if (nzchar(Sys.getenv("PILOT_RUNS"))) runs <- strsplit(Sys.getenv("PILOT_RUNS"), ",")[[1]]
rows <- list()
labels <- list()
for (run in runs) {
  mk <- sub("^pilot_[a-z]+_", "", run)
  res <- file.path(RESULTS, run, "results.jsonl")
  if (!file.exists(res)) res <- alice_fetch(run)
  if (!file.exists(res)) next
  be <- oc_backend_offline(MODELS[[mk]])
  for (nm in names(pr)) {
    cache <- file.path(CACHE, "pilot", id, mk, paste0(nm, ".jsonl"))
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    oc_import_results(res, pr[[nm]], be, cache)
    ids <- if (!file.exists(cache)) character(0) else unique(vapply(opencheck:::read_jsonl(cache), function(e) as.character(e$id), ""))
    if (!length(ids)) next
    r <- oc_classify(dd[dd$id %in% ids, ], pr[[nm]], be, cache, progress = FALSE)
    rows[[paste(mk, nm)]] <- data.frame(
      model = mk, prompt = nm, n = nrow(r), valid = mean(r$valid),
      p_valid = mean(r$p_valid, na.rm = TRUE),
      share_yes = if (pr[[nm]]$kind == "presence") mean(r$label == "yes", na.rm = TRUE) else NA,
      mean_rating = if (pr[[nm]]$kind == "rating") mean(oc_rating_score(r), na.rm = TRUE) else NA)
    if (pr[[nm]]$kind == "presence") labels[[nm]][[mk]] <- stats::setNames(r$label, r$id)
  }
}
tab <- do.call(rbind, rows)
print(tab, row.names = FALSE, digits = 3)
for (nm in names(labels)) {
  L <- labels[[nm]]
  if (length(L) < 2) next
  ids <- Reduce(intersect, lapply(L, names))
  m <- sapply(L, function(x) x[ids])
  cat(nm, ": Fleiss kappa across", ncol(m), "models =", round(oc_fleiss(m)[["kappa"]], 3), "\n")
}
write.csv(tab, file.path(OUT, paste0("pilot_check_", id, ".csv")), row.names = FALSE)
