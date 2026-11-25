# ==============================================================================
# rv_06c_agreement.R -- reliability across models and across arms (E fold)
# ==============================================================================
# Per dataset and construct:
#   - Cohen's kappa between every pair of models on the yes/no presence label;
#   - Spearman correlations between arms within a model: P(yes) (A1), the
#     expected 0-9 rating (C) and the Bradley-Terry score (B, target only).
# Agreement here is reliability, not validity (shared pretraining; R3).
# Writes analysis/output/revision/agreement_models.csv and agreement_arms.csv.
# ==============================================================================

source("analysis/revision/rv_config.R")
mrows <- list(); arows <- list()
for (id in list.dirs(file.path(RESULTS, "a1"), recursive = FALSE, full.names = FALSE)) {
  models <- list.dirs(file.path(RESULTS, "a1", id), recursive = FALSE, full.names = FALSE)
  ms <- list()
  for (mk in models) {
    f <- file.path(RESULTS, "a1", id, mk, "measures.csv")
    if (file.exists(f)) ms[[mk]] <- read.csv(f, stringsAsFactors = FALSE)
  }
  cons <- unique(sub("^yes__", "", unlist(lapply(ms, function(m) grep("^yes__", names(m), value = TRUE)))))
  for (cn in cons) {
    col <- paste0("yes__", cn)
    have <- names(ms)[vapply(ms, function(m) col %in% names(m), logical(1))]
    if (length(have) >= 2) for (i in seq_along(have)) for (j in seq_along(have)) if (i < j) {
      a <- ms[[have[i]]]; b <- ms[[have[j]]]
      m <- merge(a[, c("id", col)], b[, c("id", col)], by = "id")
      kp <- oc_kappa(m[[2]], m[[3]])
      mrows[[length(mrows) + 1]] <- data.frame(dataset = id, construct = cn, model_a = have[i],
        model_b = have[j], kappa = kp[["kappa"]], agreement = kp[["agreement"]], n = kp[["n"]],
        share_yes_a = mean(m[[2]], na.rm = TRUE), share_yes_b = mean(m[[3]], na.rm = TRUE))
    }
    for (mk in names(ms)) {
      m <- ms[[mk]]
      pairs <- list(c("pyes", "rate"), c("pyes", "bt"), c("rate", "bt"))
      for (pp in pairs) {
        x <- paste0(pp[1], "__", cn); y <- paste0(pp[2], "__", cn)
        if (!all(c(x, y) %in% names(m))) next
        ok <- stats::complete.cases(m[[x]], m[[y]])
        if (sum(ok) < 30) next
        arows[[length(arows) + 1]] <- data.frame(dataset = id, construct = cn, model = mk,
          arm_a = pp[1], arm_b = pp[2], spearman = stats::cor(m[[x]][ok], m[[y]][ok], method = "spearman"),
          n = sum(ok))
      }
    }
  }
}
am <- do.call(rbind, mrows); aa <- do.call(rbind, arows)
if (!is.null(am)) write.csv(am, file.path(OUT, "agreement_models.csv"), row.names = FALSE)
if (!is.null(aa)) write.csv(aa, file.path(OUT, "agreement_arms.csv"), row.names = FALSE)
if (!is.null(am)) print(aggregate(kappa ~ model_a + model_b, data = am, FUN = median), digits = 3)
if (!is.null(aa)) print(aggregate(spearman ~ arm_a + arm_b + model, data = aa, FUN = median), digits = 3)
