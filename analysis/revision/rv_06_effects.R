# ==============================================================================
# rv_06_effects.R -- treatment contrasts on the estimation fold, every arm
# ==============================================================================
# Reads data/results/a1/<dataset>/<model>/measures.csv and the E data, applies
# each contrast in rv_treatments.R, and estimates every measured construct
# (targets and placebos) with a Neyman SE and a randomization-inference p
# (permuting within country when the data span countries). Every measure is
# also reported in SD units. Adds a cross-model majority vote for presence.
# Writes analysis/output/revision/effects_a1.csv.
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")

measure_info <- function(col) {
  arm <- sub("__.*$", "", col)
  list(construct = sub("^[a-z]+__", "", col),
       arm = switch(arm, yes = "A1_presence", pyes = "A1_presence_soft",
                    rate = "C_rating", bt = "B_pairwise", stm = "D_topic", stmne = "D_topic_nonempty",
                    stmorig = "D_topic_original", NA_character_))
}

contrasts_for <- function(id, e, targets) {
  f <- TREATMENTS[[id]]
  if (!is.null(f)) return(f(e))
  if ("treatment" %in% names(e) && all(e$treatment %in% c(0, 1, NA, "0", "1", ""))) {
    tr <- suppressWarnings(as.integer(e$treatment))
    return(list(ctr("treatment", tr, targets, "default binary treatment; every target primary")))
  }
  stop("No contrast specification for ", id)
}

expected_sign <- function(card, construct) {
  for (t in card$constructs$target) if (identical(t$name, construct)) {
    dir <- tolower(paste(t$expected_direction %||% "", collapse = " "))
    if (grepl("^decrease|^negative|^lower", dir)) return(-1L)
    if (grepl("^increase|^positive|^higher", dir)) return(1L)
    return(NA_integer_)
  }
  NA_integer_
}

args <- commandArgs(trailingOnly = TRUE)
ids <- if (length(args)) args else list.dirs(file.path(RESULTS, "a1"), recursive = FALSE, full.names = FALSE)
out_file <- file.path(OUT, if (length(args)) paste0("effects_a1_", paste(args, collapse = "_"), ".csv") else "effects_a1.csv")
if (nzchar(Sys.getenv("OUT_NAME"))) out_file <- file.path(OUT, Sys.getenv("OUT_NAME"))
# One study at a time, in parallel: studies are independent, and every
# randomization test is seeded, so the estimates do not depend on the order.
one_study <- function(id) {
  rows <- list()
  card_path <- registered_cards()[[id]]
  if (is.null(card_path)) return(NULL)
  card <- oc_study_read(card_path)
  targets <- vapply(card$constructs$target, `[[`, "", "name")
  lock <- oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json")))
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "effects")
  strata_all <- effect_strata(id, e)
  meas_all <- list()
  for (mk in list.dirs(file.path(RESULTS, "a1", id), recursive = FALSE, full.names = FALSE)) {
    f <- file.path(RESULTS, "a1", id, mk, "measures.csv")
    if (file.exists(f)) meas_all[[mk]] <- read.csv(f, stringsAsFactors = FALSE)
  }
  # ONLY_MODELS=a,b (added 22 Nov 2026): estimate only these model folders, e.g. the
  # STM covariate variant (stmcov), without recomputing every other arm.
  only <- Sys.getenv("ONLY_MODELS")
  if (nzchar(only)) meas_all <- meas_all[intersect(names(meas_all), strsplit(only, ",")[[1]])]
  yes_cols <- unique(unlist(lapply(meas_all, function(m) grep("^yes__", names(m), value = TRUE))))
  if (length(meas_all) >= 2 && length(yes_cols)) {
    maj <- data.frame(id = e$id)
    for (yc in yes_cols) {
      v <- matrix(sapply(meas_all, function(m) {
        if (yc %in% names(m)) m[[yc]][match(e$id, m$id)] else rep(NA, nrow(e))
      }), nrow = nrow(e))
      k <- rowSums(v, na.rm = TRUE)
      n <- rowSums(!is.na(v))
      maj[[yc]] <- ifelse(n == 0, NA, as.integer(k > n / 2))
    }
    meas_all[["majority"]] <- maj
  }
  for (cn in contrasts_for(id, e, targets)) {
    tr <- cn$treat
    for (mk in names(meas_all)) {
      m <- meas_all[[mk]]
      for (col in grep("^(yes|pyes|rate|bt|stm|stmne|stmorig)__", names(m), value = TRUE)) {
        info <- measure_info(col)
        y <- m[[col]][match(e$id, m$id)]
        coverage <- mean(!is.na(y))
        # incomplete runs; the STM sensitivity
        # measure is missing by design for answers with no in-vocabulary word
        if (coverage < 0.95 && info$arm != "D_topic_nonempty") next
        keep <- !is.na(tr) & !is.na(y)
        if (sum(tr[keep] == 1) < 20 || sum(tr[keep] == 0) < 20) next
        ef <- oc_effect(y[keep], tr[keep],
                        strata = if (is.null(strata_all)) NULL else strata_all[keep],
                        n_perm = 2000L, seed = 42L)
        sdy <- stats::sd(y[keep])
        rows[[length(rows) + 1]] <- data.frame(
          dataset = id, model = mk, arm = info$arm, construct = info$construct,
          role = if (info$construct %in% targets) "target" else "placebo",
          contrast = cn$name, primary = info$construct %in% cn$primary,
          # Moderator cards (Bisgaard): the theorized sign flips in the opposition subgroup.
          expected_sign = expected_sign(card, info$construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L),
          ef, mean_control = mean(y[keep][tr[keep] == 0]), sd = sdy,
          est_sd = ef$estimate / sdy, se_sd = ef$se / sdy, coverage = coverage,
          stringsAsFactors = FALSE)
      }
    }
  }
  message(id, ": done")
  if (length(rows)) do.call(rbind, rows) else NULL
}
res <- parallel::mclapply(ids, function(id) tryCatch(one_study(id), error = function(e) structure(conditionMessage(e), class = "try-error")),
                          mc.cores = min(12L, parallel::detectCores()), mc.preschedule = FALSE)
bad <- vapply(res, inherits, NA, "try-error")
if (any(bad)) stop("Failed: ", paste(sprintf("%s (%s)", ids[bad], unlist(res[bad])), collapse = "; "))
eff <- do.call(rbind, res)
write.csv(eff, out_file, row.names = FALSE)
cat(nrow(eff), "estimates written to", out_file, "\n")
