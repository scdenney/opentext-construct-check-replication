# ==============================================================================
# rv_03_import_a1.R -- import theory-only arm results (A1, B, C) per model
# ==============================================================================
# For each dataset in the A1 index and each model run that has results:
#   presence/rating: score into caches, then one row per E response
#   pairwise:        BT scores from E pairs, held-out accuracy and ICI
# Writes data/results/a1/<dataset>/<model>/measures.csv (one row per response,
# one column per measure) and analysis/output/revision/bt_diagnostics.csv.
# Usage: Rscript analysis/revision/rv_03_import_a1.R <run_prefix> [dataset ...]
#   run_prefix e.g. "a1_b1": runs named a1_b1_<model> hold batch b1's results.
# ==============================================================================

source("analysis/revision/rv_config.R")
args <- commandArgs(trailingOnly = TRUE)
prefix <- args[1]
idx <- read.csv(file.path(JOBS, "a1", "index.csv"), stringsAsFactors = FALSE)
ids <- if (length(args) > 1) args[-1] else unique(idx$dataset)
runs <- list.files(file.path(JOBS, "submitted"), pattern = paste0("^", prefix, "_"))
diag <- list()
for (run in runs) {
  mk <- sub(paste0("^", prefix, "_"), "", run)
  if (!mk %in% names(MODELS)) next
  # ONLY_MODEL restricts the import to one model (e.g. while another run is still writing)
  if (nzchar(Sys.getenv("ONLY_MODEL")) && mk != Sys.getenv("ONLY_MODEL")) next
  res <- alice_fetch(run)
  if (!file.exists(res)) { message("No results yet for ", run); next }
  be <- oc_backend_offline(MODELS[[mk]])
  system2("python3", c("-I", "analysis/revision/rv_split_results.py", res))
  by_prompt <- file.path(dirname(res), "by_prompt")
  run_hashes <- sub("\\.jsonl$", "", list.files(by_prompt, pattern = "\\.jsonl$"))
  res_for <- function(h) file.path(by_prompt, paste0(h, ".jsonl"))
  for (id in ids) {
    if (!any(idx$prompt_hash[idx$dataset == id] %in% run_hashes)) next
    card_path <- registered_cards()[[id]]
    card <- oc_study_read(card_path)
    pr <- a1_prompts(card, card_path)
    lock <- oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json")))
    s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
    d <- read_harmonized(id)
    d <- d[nchar(trimws(d$text)) > 0, ]
    e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = paste("import", run))
    meas <- data.frame(id = e$id, stringsAsFactors = FALSE)
    out_dir <- file.path(RESULTS, "a1", id, mk)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    for (nm in names(pr)) {
      p <- pr[[nm]]
      stopifnot(p$hash %in% idx$prompt_hash[idx$dataset == id & idx$prompt == nm])
      if (p$kind == "pairwise") {
        pairs <- read.csv(file.path(JOBS, "a1", id, "pairs.csv"), stringsAsFactors = FALSE)
        if (!p$hash %in% run_hashes) next
        y <- oc_import_pairs(res_for(p$hash), pairs, p,
                             id_prefix = if (id %in% PAIR_PREFIX) id else NULL)
        if (all(is.na(y$y))) next
        fit <- suppressWarnings(oc_bt(y[!y$holdout & !is.na(y$y), ]))
        v <- oc_bt_validate(fit, y[y$holdout & !is.na(y$y), ])
        meas[[paste0("bt__", p$construct)]] <- fit$estimate[match(meas$id, fit$id)]
        meas[[paste0("btse__", p$construct)]] <- fit$se[match(meas$id, fit$id)]
        diag[[paste(id, mk)]] <- data.frame(
          dataset = id, model = mk, construct = p$construct, n_pairs = sum(!is.na(y$y)),
          lambda = fit$lambda[1], bt_method = fit$method[1], heldout_acc = v$accuracy, ici = v$ici,
          order_gap = mean(abs(y$order_gap), na.rm = TRUE))
        write.csv(y, file.path(out_dir, "pairs_y.csv"), row.names = FALSE)
      } else {
        cache <- file.path(CACHE, "a1", id, mk, paste0(nm, ".jsonl"))
        dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
        if (!p$hash %in% run_hashes) next
        oc_import_results(res_for(p$hash), p, be, cache)
        done <- if (!file.exists(cache)) character(0) else unique(vapply(opencheck:::read_jsonl(cache), function(e) as.character(e$id), ""))
        # Identical prompts can occur across datasets (e.g. two studies sharing a
        # card's constructs), so the cache may hold other datasets' ids.
        done <- intersect(done, e$id)
        if (!length(done)) next
        r <- oc_classify(e[e$id %in% done, ], p, be, cache, lock = lock, progress = FALSE)
        if (p$kind == "presence") {
          meas[[paste0("yes__", p$construct)]] <- as.integer(r$label == "yes")[match(meas$id, r$id)]
          meas[[paste0("pyes__", p$construct)]] <- r$p_yes[match(meas$id, r$id)]
          meas[[paste0("pvalid__", p$construct)]] <- r$p_valid[match(meas$id, r$id)]
        } else {
          meas[[paste0("rate__", p$construct)]] <- oc_rating_score(r)[match(meas$id, r$id)]
        }
      }
    }
    mf <- file.path(out_dir, "measures.csv")
    if (file.exists(mf)) {
      old <- read.csv(mf, stringsAsFactors = FALSE)
      keep_old <- setdiff(names(old), names(meas))
      if (length(keep_old)) meas <- merge(meas, old[, c("id", keep_old)], by = "id", all.x = TRUE)
    }
    if (ncol(meas) > 1) write.csv(meas, mf, row.names = FALSE)
    message(id, " / ", mk, ": ", ncol(meas) - 1, " measures for ", nrow(meas), " responses")
  }
}
if (length(diag)) {
  dg <- do.call(rbind, diag)
  f <- file.path(OUT, "bt_diagnostics.csv")
  if (file.exists(f)) {
    old <- read.csv(f, stringsAsFactors = FALSE)
    dg <- dplyr::bind_rows(old[!paste(old$dataset, old$model) %in% paste(dg$dataset, dg$model), ], dg)
  }
  write.csv(dg, f, row.names = FALSE)
  print(dg, row.names = FALSE, digits = 3)
}
