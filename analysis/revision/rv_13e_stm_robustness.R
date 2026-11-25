# ==============================================================================
# rv_13e_stm_robustness.R -- is the topic-model result an artifact of the
# labeler or of the number of topics?
# ==============================================================================
# The topic-model arm (rv_13) recovers the theorized construct far less often
# than the language-model arms. Two choices could be responsible: the model that
# matches a topic to the target, and the number of topics K. This script removes
# both. For every study and every K in the grid it fits the topic model on the
# discovery fold (the stored model is reused for the K that was used), projects
# the estimation fold onto it, and asks, for each primary target contrast:
#   best   the topic whose proportion correlates most with the reference model's
#          yes/no label for the target ON THE ESTIMATION FOLD. No labeler could
#          do better; the choice uses the outcome, so this is an upper bound,
#          not a valid procedure.
#   any    whether ANY topic moves significantly in the theorized direction
#          (normal approximation), with and without a Bonferroni correction
#          over the K topics. This ignores the construct altogether.
# Nothing here feeds back into a measure. Writes
# analysis/output/revision/tables/stm_robustness.csv and prints the summary.
# Run: OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript analysis/revision/rv_13e_stm_robustness.R
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
src13 <- readLines("analysis/revision/rv_13_stm_arm.R")
eval(parse(text = src13[grep("^SW_KO <-", src13):(grep("^choose_K <-", src13) - 1)]))   # tokenizer
src06 <- readLines("analysis/revision/rv_06_effects.R")
eval(parse(text = src06[grep("^contrasts_for <-", src06):(grep("^args <-", src06) - 1)])) # contrasts, signs
STM_DIR <- file.path(JOBS, "stm")
K_GRID <- c(4L, 6L, 8L, 10L, 12L)
REF <- "qwen36_35b"
cards <- registered_cards()

one <- function(id) {
  lock <- file.path(LOCKS, paste0(id, ".stm.k1.json"))
  ynf <- file.path(RESULTS, "a1", id, REF, "measures.csv")
  if (!file.exists(lock) || !file.exists(ynf)) return(NULL)
  card <- oc_study_read(cards[[id]])
  targets <- vapply(card$constructs$target, `[[`, "", "name")
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = "stm robustness: fit on D")
  e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "stm robustness: measure E")
  fit0 <- readRDS(file.path(STM_DIR, paste0(id, ".stm.rds")))
  lang_d <- if ("lang" %in% names(dd)) dd$lang else rep("en", nrow(dd))
  lang_e <- if ("lang" %in% names(e)) e$lang else rep("en", nrow(e))
  md <- dfm_trim(dfm(tokens_for(dd$text, lang_d)), min_docfreq = 3)
  std <- convert(md[rowSums(md) > 0, ], to = "stm")
  me_all <- dfm(tokens_for(e$text, lang_e))
  yn <- read.csv(ynf)
  strata_all <- effect_strata(id, e)
  cns <- contrasts_for(id, e, targets)
  out <- list()
  for (K in K_GRID) {
    used <- K == fit0$K
    mod <- if (used) fit0$mod else stm(std$documents, std$vocab, K = K, init.type = "Spectral",
                                       max.em.its = 75, verbose = FALSE, seed = 42)
    vocab <- if (used) fit0$vocab else std$vocab
    m <- dfm_match(me_all, vocab)
    ok <- rowSums(m) > 0
    theta <- matrix(colMeans(mod$theta), nrow(e), K, byrow = TRUE)   # answers with no known word: D mean
    if (any(ok)) theta[ok, ] <- fitNewDocuments(model = mod, documents = stm_documents(m[ok, ]),
                                                newData = NULL, origData = NULL, prevalence = NULL, verbose = FALSE)$theta
    for (cn in cns) for (construct in cn$primary) {
      col <- paste0("yes__", construct)
      if (!col %in% names(yn)) next
      y <- yn[[col]][match(e$id, yn$id)]
      r <- suppressWarnings(apply(theta[ok, , drop = FALSE], 2, function(t) cor(t, y[ok], use = "complete.obs")))
      if (all(is.na(r))) next
      sgn <- expected_sign(card, construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L)
      tr <- cn$treat
      keep <- !is.na(tr)
      if (sum(tr[keep] == 1) < 20 || sum(tr[keep] == 0) < 20) next
      st <- if (is.null(strata_all)) NULL else strata_all[keep]
      z <- vapply(seq_len(K), function(k) {
        ef <- oc_effect(theta[keep, k], tr[keep], strata = st, n_perm = 0L)
        ef$estimate / ef$se
      }, 0) * sgn
      best <- which.max(r)
      eb <- oc_effect(theta[keep, best], tr[keep], strata = st, n_perm = 2000L, seed = 42L)
      out[[length(out) + 1]] <- data.frame(
        dataset = id, construct = construct, contrast = cn$name, K = K, K_used = fit0$K, expected_sign = sgn,
        best = best, r_best = r[best], est_sd_best = eb$estimate / stats::sd(theta[keep, best]), p_best = eb$p_ri,
        rec_best = eb$p_ri < 0.05 & sign(eb$estimate) == sgn,
        any_topic = any(z > stats::qnorm(0.975), na.rm = TRUE),
        any_topic_bonf = any(z > stats::qnorm(1 - 0.025 / K), na.rm = TRUE), stringsAsFactors = FALSE)
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}

rows <- mclapply(names(cards), function(id) tryCatch(one(id), error = function(err) structure(conditionMessage(err), class = "try-error")),
                 mc.cores = min(12L, detectCores()), mc.preschedule = FALSE)
bad <- vapply(rows, inherits, NA, "try-error")
if (any(bad)) message("Failed: ", paste(sprintf("%s (%s)", names(cards)[bad], unlist(rows[bad])), collapse = "; "))
res <- do.call(rbind, rows[!bad])
write.csv(res, file.path(OUT, "tables", "stm_robustness.csv"), row.names = FALSE)

show <- function(d, lab) cat(sprintf(
  "%-10s contrasts %d (%d studies); best topic in hindsight: recovered %.3f, median r %.2f; any topic: %.3f; any topic, Bonferroni: %.3f\n",
  lab, nrow(d), length(unique(d$dataset)), mean(d$rec_best), median(d$r_best), mean(d$any_topic), mean(d$any_topic_bonf)))
show(res[res$K == res$K_used, ], "K used")
for (K in K_GRID) show(res[res$K == K, ], paste("K =", K))
# the most favorable K for each contrast, chosen after the fact
bestK <- do.call(rbind, lapply(split(res, paste(res$dataset, res$construct, res$contrast)), function(d) d[which.max(d$r_best), ]))
show(bestK, "best K")
cat(sprintf("recovered at one K or more with the best topic in hindsight: %.3f of %d contrasts\n",
            mean(tapply(res$rec_best, paste(res$dataset, res$construct, res$contrast), any)), nrow(bestK)))
