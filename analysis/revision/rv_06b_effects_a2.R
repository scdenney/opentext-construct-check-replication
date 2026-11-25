# ==============================================================================
# rv_06b_effects_a2.R -- A2 (induced codebook) contrasts, per split and pooled
# ==============================================================================
# For each dataset, model and split k: the share of E_k responses coded as the
# card's (first) target construct, contrasted under each treatment contrast.
# Split 1 is the primary estimate; splits 1-11 give the distribution R2 asked
# for, summarised by the median estimate with the median-adjusted SE.
# Writes analysis/output/revision/effects_a2_splits.csv and effects_a2.csv.
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")

rows <- list()
for (id in list.dirs(file.path(RESULTS, "a2"), recursive = FALSE, full.names = FALSE)) {
  card <- oc_study_read(registered_cards()[[id]])
  target <- card$constructs$target[[1]]$name
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  for (mk in list.dirs(file.path(RESULTS, "a2", id), recursive = FALSE, full.names = FALSE)) {
    for (f in list.files(file.path(RESULTS, "a2", id, mk), pattern = "^k[0-9]+\\.csv$", full.names = TRUE)) {
      k <- as.integer(sub("^k([0-9]+)\\.csv$", "\\1", basename(f)))
      if (k > 5) next   # splits 1-5 were classified (see WORKLOG, 03:45)
      lock <- oc_lock_read(file.path(LOCKS, sprintf("%s.a2.k%02d.json", id, k)))
      ek <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = paste("A2 effects split", k))
      r <- read.csv(f, stringsAsFactors = FALSE)
      y <- as.integer(r$label == target)[match(ek$id, r$id)]
      if (mean(!is.na(y)) < 0.95) next
      strata <- effect_strata(id, ek)
      cs <- TREATMENTS[[id]]
      cs <- if (is.null(cs)) list(ctr("treatment", as.integer(ek$treatment), target)) else cs(ek)
      for (cn in cs) {
        keep <- !is.na(cn$treat) & !is.na(y)
        if (sum(cn$treat[keep] == 1) < 20 || sum(cn$treat[keep] == 0) < 20) next
        ef <- oc_effect(y[keep], cn$treat[keep], strata = if (is.null(strata)) NULL else strata[keep],
                        n_perm = 1000L, seed = 42L)
        rows[[length(rows) + 1]] <- data.frame(dataset = id, model = mk, k = k, construct = target,
          contrast = cn$name, primary = target %in% cn$primary, ef,
          share_target = mean(y[keep]), stringsAsFactors = FALSE)
      }
    }
  }
}
sp <- do.call(rbind, rows)
write.csv(sp, file.path(OUT, "effects_a2_splits.csv"), row.names = FALSE)
agg <- do.call(rbind, lapply(split(sp, list(sp$dataset, sp$model, sp$contrast), drop = TRUE), function(x) {
  a <- oc_aggregate_splits(x[, c("estimate", "se")])
  k1 <- x[x$k == 1, ]
  data.frame(dataset = x$dataset[1], model = x$model[1], contrast = x$contrast[1],
             primary = x$primary[1], construct = x$construct[1],
             k1_estimate = if (nrow(k1)) k1$estimate else NA, k1_se = if (nrow(k1)) k1$se else NA,
             k1_p_ri = if (nrow(k1)) k1$p_ri else NA, a, share_sig = mean(x$p_ri < 0.05))
}))
write.csv(agg, file.path(OUT, "effects_a2.csv"), row.names = FALSE)
print(agg[agg$primary, ], row.names = FALSE, digits = 3)
