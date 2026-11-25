# ==============================================================================
# rv_03b_bt_diagnostics.R -- Bradley-Terry diagnostics from saved pair outcomes
# ==============================================================================
# Refits BT on the training pairs in data/results/a1/<dataset>/<model>/pairs_y.csv
# and scores the held-out pairs (accuracy, ICI) and the position effect (mean
# |fwd - rev| on pairs asked both ways). Rebuilds bt_diagnostics.csv without
# re-importing. Fits match those used for the measures (same pairs, same rule).
# ==============================================================================

source("analysis/revision/rv_config.R")
# One fit per dataset and model, in parallel: exact fits of 1,000 to 3,000 items take minutes each.
files <- Sys.glob(file.path(RESULTS, "a1", "*", "*", "pairs_y.csv"))
rows <- parallel::mclapply(files, function(f) {
  mk <- basename(dirname(f)); id <- basename(dirname(dirname(f)))
  y <- read.csv(f, stringsAsFactors = FALSE)
  if (mean(!is.na(y$y)) < 0.95) return(NULL)
  # package default for large_n (3000), as in rv_03_import_a1.R, so that the fit scored here is the
  # fit behind the stored measure; an earlier version forced large_n = 1000 and scored a different fit
  fit <- suppressWarnings(oc_bt(y[!y$holdout & !is.na(y$y), ]))
  message(id, " / ", mk, ": ", nrow(fit), " items, ", fit$method[1])
  v <- oc_bt_validate(fit, y[y$holdout & !is.na(y$y), ])
  data.frame(dataset = id, model = mk, n_items = nrow(fit),
    n_pairs = sum(!is.na(y$y)), lambda = fit$lambda[1], bt_method = fit$method[1],
    heldout_acc = v$accuracy, ici = v$ici, order_gap = mean(abs(y$order_gap), na.rm = TRUE))
}, mc.cores = min(16L, parallel::detectCores()), mc.preschedule = FALSE)
bad <- vapply(rows, inherits, NA, "try-error")
if (any(bad)) stop("Failed fits: ", paste(files[bad], collapse = ", "))
dg <- do.call(rbind, rows)
write.csv(dg, file.path(OUT, "bt_diagnostics.csv"), row.names = FALSE)
print(aggregate(cbind(heldout_acc, ici, order_gap) ~ model, data = dg, FUN = median), digits = 3)
cat(nrow(dg), "dataset x model fits\n")
