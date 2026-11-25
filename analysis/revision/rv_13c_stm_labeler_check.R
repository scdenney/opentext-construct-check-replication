# ==============================================================================
# rv_13c_stm_labeler_check.R -- does the topic-labeling model matter?
# ==============================================================================
# The STM arm (rv_13) matched topics to targets with Gemma 4 12B, because the
# planned labeler (Qwen3.6-35B) was held up in the queue (revision/deviations.md).
# The Qwen run finished later. This script repeats the measure step with Qwen's
# choices and compares:
#   - which topic each labeler chose for each target (D-fold topic models are
#     the same files; only the choice differs)
#   - the primary-contrast estimate on E under each labeler
# The primary STM arm is unchanged. The Qwen measures get their own lock
# (<id>.stmq.k1.json) and live in data/results/stm_check/, which no other
# script reads.
# Writes analysis/output/revision/tables/stm_labeler_check.csv.
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
src13 <- readLines("analysis/revision/rv_13_stm_arm.R")
eval(parse(text = src13[grep("^SW_KO <-", src13):(grep("^choose_K <-", src13) - 1)]))   # tokenizer
src06 <- readLines("analysis/revision/rv_06_effects.R")
eval(parse(text = src06[grep("^contrasts_for <-", src06):(grep("^args <-", src06) - 1)])) # contrasts, signs
STM_DIR <- file.path(JOBS, "stm")
CHECK <- file.path(RESULTS, "stm_check")
K_GRID <- c(4L, 6L, 8L, 10L, 12L)

read_choice <- function(labeler) {
  res <- opencheck:::read_jsonl(file.path(RESULTS, paste0("stm_label2_", labeler), "results.jsonl"))
  do.call(rbind, lapply(res, function(r) {
    p <- exp(vapply(r$top, function(x) x$logprob, 0)); names(p) <- vapply(r$top, function(x) x$token, "")
    p <- p[names(p) %in% c("0", LETTERS)]
    pick <- names(p)[which.max(p)]
    data.frame(key = r$id, topic = if (pick == "0") 0L else match(pick, LETTERS), p = max(p) / sum(p))
  }))
}
g <- read_choice("gemma4_12b")
q <- read_choice("qwen36_35b")
q$unit <- sub("::.*$", "", q$key); q$dataset <- sub("@.*$", "", q$unit); q$construct <- sub("^.*::", "", q$key)
cmp <- merge(g, q, by = "key", suffixes = c("_gemma", "_qwen"))
write.csv(q, file.path(STM_DIR, "topic_choice_qwen36_35b.csv"), row.names = FALSE)

cards <- registered_cards()
rows <- mclapply(unique(q$dataset), function(id) {
  ch <- q[q$dataset == id, ]
  chf <- file.path(STM_DIR, paste0(id, ".topic_choice_qwen36_35b.csv"))
  write.csv(ch, chf, row.names = FALSE)
  units <- unique(ch$unit)
  rds <- setNames(file.path(STM_DIR, paste0(units, ".stm.rds")), paste0("stm_", units))
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  lock <- file.path(LOCKS, paste0(id, ".stmq.k1.json"))
  if (!file.exists(lock)) oc_freeze(lock, s, components = c(rds, topic_choice = chf),
                                    models = c(labeler = MODELS[["qwen36_35b"]]),
                                    decoding = list(K_grid = K_GRID, init = "Spectral", seed = 42))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "stm labeler check (qwen): measure E")
  meas <- data.frame(id = e$id)
  for (u in units) {
    fit <- readRDS(file.path(STM_DIR, paste0(u, ".stm.rds")))
    orig <- grepl("@orig_", u)
    rws <- if (orig) which(e$country == c(KR = "South Korea", TW = "Taiwan")[sub("^.*@orig_", "", u)]) else seq_len(nrow(e))
    lang <- if ("lang" %in% names(e)) e$lang[rws] else rep("en", length(rws))
    m <- dfm_match(dfm(tokens_for(e$text[rws], lang, fit$sw_ko)), fit$vocab)
    ok <- rowSums(m) > 0
    theta <- matrix(NA_real_, length(rws), fit$K)
    if (any(ok)) {
      theta[ok, ] <- fitNewDocuments(model = fit$mod, documents = stm_documents(m[ok, ]), newData = NULL,
                                     origData = NULL, prevalence = NULL, verbose = FALSE)$theta
    }
    for (i in which(ch$unit == u)) {
      if (ch$topic[i] == 0) next
      v <- theta[, ch$topic[i]]
      v[is.na(v)] <- fit$theta_mean[ch$topic[i]]
      col <- paste0(if (orig) "stmorig__" else "stm__", ch$construct[i])
      if (is.null(meas[[col]])) meas[[col]] <- NA_real_
      meas[[col]][rws] <- v
    }
  }
  dir.create(file.path(CHECK, id), recursive = TRUE, showWarnings = FALSE)
  write.csv(meas, file.path(CHECK, id, "measures.csv"), row.names = FALSE)
  card <- oc_study_read(cards[[id]])
  targets <- vapply(card$constructs$target, `[[`, "", "name")
  strata_all <- effect_strata(id, e)
  out <- list()
  for (cn in contrasts_for(id, e, targets)) for (col in grep("^(stm|stmorig)__", names(meas), value = TRUE)) {
    construct <- sub("^[a-z]+__", "", col)
    if (!construct %in% cn$primary) next
    y <- meas[[col]]; tr <- cn$treat
    if (mean(!is.na(y)) < 0.95) next
    keep <- !is.na(tr) & !is.na(y)
    if (sum(tr[keep] == 1) < 20 || sum(tr[keep] == 0) < 20) next
    ef <- oc_effect(y[keep], tr[keep], strata = if (is.null(strata_all)) NULL else strata_all[keep],
                    n_perm = 2000L, seed = 42L)
    out[[length(out) + 1]] <- data.frame(
      dataset = id, arm = if (grepl("^stmorig", col)) "D_topic_original" else "D_topic",
      construct = construct, contrast = cn$name,
      expected_sign = expected_sign(card, construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L),
      est_sd_qwen = ef$estimate / stats::sd(y[keep]), p_qwen = ef$p_ri, stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else NULL
}, mc.cores = 8)
bad <- vapply(rows, inherits, NA, "try-error")
if (any(bad)) message("Failed: ", paste(unique(q$dataset)[bad], collapse = ", "), "\n", paste(unlist(rows[bad]), collapse = "\n"))
qe <- do.call(rbind, rows[!bad])

# Gemma-labeler estimates: the primary STM arm, as already estimated.
a1 <- read.csv(file.path(OUT, "effects_a1.csv"), stringsAsFactors = FALSE)
ge <- a1[a1$model == "stm" & a1$arm %in% c("D_topic", "D_topic_original") & a1$primary & a1$role == "target",
         c("dataset", "arm", "construct", "contrast", "est_sd", "p_ri", "expected_sign")]
names(ge)[5:7] <- c("est_sd_gemma", "p_gemma", "expected_sign_gemma")
tp <- cmp[!grepl("@", cmp$key), c("key", "topic_gemma", "topic_qwen", "p_gemma", "p_qwen")]
names(tp)[4:5] <- c("pchoice_gemma", "pchoice_qwen")
tp$dataset <- sub("::.*$", "", tp$key); tp$construct <- sub("^.*::", "", tp$key)
res <- merge(merge(ge, qe, all = TRUE), tp[, -1], by = c("dataset", "construct"), all.x = TRUE)
# the expected sign of a target, whichever labeler produced an estimate for it
res$expected_sign <- ifelse(is.na(res$expected_sign), res$expected_sign_gemma, res$expected_sign)
res$expected_sign_gemma <- NULL
res$same_topic <- res$topic_gemma == res$topic_qwen
write.csv(res, file.path(OUT, "tables", "stm_labeler_check.csv"), row.names = FALSE)

main <- cmp[!grepl("@", cmp$key), ]
cat(sprintf("targets labeled: %d; same topic %d (%.0f%%); Gemma chose none %d, Qwen chose none %d\n",
            nrow(main), sum(main$topic_gemma == main$topic_qwen), 100 * mean(main$topic_gemma == main$topic_qwen),
            sum(main$topic_gemma == 0), sum(main$topic_qwen == 0)))
rate <- function(est, p, sgn) sprintf("n = %d, p<.05 = %.2f, expected sign = %.2f", sum(!is.na(est)),
                                      mean(p < 0.05, na.rm = TRUE), mean(sign(est) == sgn, na.rm = TRUE))
dt <- res[res$arm == "D_topic", ]
cat("primary contrasts, Gemma labeler:", rate(dt$est_sd_gemma, dt$p_gemma, dt$expected_sign), "\n")
cat("primary contrasts, Qwen labeler: ", rate(dt$est_sd_qwen, dt$p_qwen, dt$expected_sign), "\n")
both <- dt[!is.na(dt$est_sd_gemma) & !is.na(dt$est_sd_qwen), ]
cat(sprintf("both estimated: %d; r between estimates %.2f; same significance call %.0f%%\n", nrow(both),
            cor(both$est_sd_gemma, both$est_sd_qwen), 100 * mean((both$p_gemma < .05) == (both$p_qwen < .05))))
dg <- file.path(OUT, "tables", "stm_topic_diagnostic.csv")
if (file.exists(dg)) {
  dd <- merge(read.csv(dg), tp[, c("dataset", "construct", "topic_qwen")])
  cat(sprintf("picked the best-correlated topic (hindsight): Gemma %.0f%%, Qwen %.0f%% of targets with a choice\n",
              100 * mean(dd$best_is_chosen[dd$chosen > 0], na.rm = TRUE),
              100 * mean((dd$topic_qwen == dd$best)[dd$topic_qwen > 0], na.rm = TRUE)))
}
print(res[res$dataset == "sktw_naturalization_2024", ], row.names = FALSE, digits = 3)
