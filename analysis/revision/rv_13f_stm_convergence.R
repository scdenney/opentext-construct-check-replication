# ==============================================================================
# rv_13f_stm_convergence.R -- does stopping the topic models at 75 EM
# iterations matter?
# ==============================================================================
# The topic models of the frozen arm (rv_13) were fit with max.em.its = 75, and
# most stopped at that cap before meeting the convergence criterion. This check
# refits each study's model on the discovery fold with the same data, the same
# number of topics, the same spectral initialization and seed, and up to 500
# iterations. Because the fit is deterministic, the longer run continues the
# same EM path, so topic k of the refit is topic k of the frozen model and the
# labeler's choice carries over. The estimation fold is projected onto the refit
# and each primary contrast is estimated on
#   chosen  the topic the labeler chose (as in the frozen arm)
#   best    the topic most correlated with the reference model's yes/no label on
#           the estimation fold (hindsight; an upper bound, as in rv_13e)
# Descriptive: the frozen arm and its lock are not changed, and nothing here
# feeds back into a measure. Writes tables/stm_convergence.csv.
# Run: LANG=en_US.UTF-8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript analysis/revision/rv_13f_stm_convergence.R
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
src13 <- readLines("analysis/revision/rv_13_stm_arm.R")
eval(parse(text = src13[grep("^SW_KO <-", src13)[1]:(grep("^choose_K <-", src13) - 1)]))   # tokenizer
src06 <- readLines("analysis/revision/rv_06_effects.R")
eval(parse(text = src06[grep("^contrasts_for <-", src06):(grep("^args <-", src06) - 1)])) # contrasts, signs
STM_DIR <- file.path(JOBS, "stm")
REF <- "qwen36_35b"
MAX_ITS <- 500L
cards <- registered_cards()

one <- function(id) {
  lock <- file.path(LOCKS, paste0(id, ".stm.k1.json"))
  chf <- file.path(STM_DIR, paste0(id, ".topic_choice.csv"))
  ynf <- file.path(RESULTS, "a1", id, REF, "measures.csv")
  if (!file.exists(lock) || !file.exists(chf) || !file.exists(ynf)) return(NULL)
  card <- oc_study_read(cards[[id]])
  targets <- vapply(card$constructs$target, `[[`, "", "name")
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = "stm convergence check: refit on D")
  e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "stm convergence check: measure E")
  fit0 <- readRDS(file.path(STM_DIR, paste0(id, ".stm.rds")))
  lang_d <- if ("lang" %in% names(dd)) dd$lang else rep("en", nrow(dd))
  lang_e <- if ("lang" %in% names(e)) e$lang else rep("en", nrow(e))
  md <- dfm_trim(dfm(tokens_for(dd$text, lang_d)), min_docfreq = 3)
  std <- convert(md[rowSums(md) > 0, ], to = "stm")
  stopifnot(identical(std$vocab, fit0$vocab))
  K <- fit0$K
  mod <- stm(std$documents, std$vocab, K = K, init.type = "Spectral", max.em.its = MAX_ITS, verbose = FALSE, seed = 42)
  project <- function(model) {
    m <- dfm_match(dfm(tokens_for(e$text, lang_e)), std$vocab)
    ok <- rowSums(m) > 0
    th <- matrix(colMeans(model$theta), nrow(e), K, byrow = TRUE)   # answers with no known word: D mean
    if (any(ok)) th[ok, ] <- fitNewDocuments(model = model, documents = stm_documents(m[ok, ]), newData = NULL,
                                             origData = NULL, prevalence = NULL, verbose = FALSE)$theta
    list(theta = th, ok = ok)
  }
  p0 <- project(fit0$mod); p1 <- project(mod)
  ch <- read.csv(chf, stringsAsFactors = FALSE)
  ch <- ch[ch$unit == id, ]
  yn <- read.csv(ynf)
  st_all <- effect_strata(id, e)
  out <- list()
  for (cn in contrasts_for(id, e, targets)) for (construct in cn$primary) {
    sgn <- expected_sign(card, construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L)
    keep <- !is.na(cn$treat)
    if (sum(cn$treat[keep] == 1) < 20 || sum(cn$treat[keep] == 0) < 20) next
    st <- if (is.null(st_all)) NULL else st_all[keep]
    rec <- function(v) {
      ef <- oc_effect(v[keep], cn$treat[keep], strata = st, n_perm = 2000L, seed = 42L)
      ef$p_ri < 0.05 & sign(ef$estimate) == sgn
    }
    k <- ch$topic[match(construct, ch$construct)]
    chosen <- !is.na(k) && k > 0
    y <- yn[[paste0("yes__", construct)]]
    y <- if (is.null(y)) rep(NA_real_, nrow(e)) else y[match(e$id, yn$id)]
    best <- function(p) {
      r <- suppressWarnings(apply(p$theta[p$ok, , drop = FALSE], 2, function(t) cor(t, y[p$ok], use = "complete.obs")))
      if (all(is.na(r))) NA_integer_ else which.max(r)
    }
    b0 <- best(p0); b1 <- best(p1)
    out[[length(out) + 1]] <- data.frame(
      dataset = id, construct = construct, contrast = cn$name, K = K,
      converged_75 = isTRUE(fit0$mod$convergence$converged), its_75 = fit0$mod$convergence$its,
      converged_500 = isTRUE(mod$convergence$converged), its_500 = mod$convergence$its,
      has_topic = chosen,
      rec_chosen_75 = if (chosen) rec(p0$theta[, k]) else FALSE,
      rec_chosen_500 = if (chosen) rec(p1$theta[, k]) else FALSE,
      r_theta_chosen = if (chosen) suppressWarnings(cor(p0$theta[, k], p1$theta[, k])) else NA_real_,
      rec_best_75 = if (!is.na(b0)) rec(p0$theta[, b0]) else FALSE,
      rec_best_500 = if (!is.na(b1)) rec(p1$theta[, b1]) else FALSE,
      stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else NULL
}

res <- mclapply(names(cards), function(id) tryCatch(one(id), error = function(err) structure(conditionMessage(err), class = "try-error")),
                mc.cores = min(12L, detectCores()), mc.preschedule = FALSE)
bad <- vapply(res, inherits, NA, "try-error")
if (any(bad)) message("Failed: ", paste(sprintf("%s (%s)", names(cards)[bad], unlist(res[bad])), collapse = "; "))
x <- do.call(rbind, res[!bad])
write.csv(x, file.path(OUT, "tables", "stm_convergence.csv"), row.names = FALSE)

s <- x[!duplicated(x$dataset), ]
cat(sprintf("studies: %d; converged within 75 iterations: %d; within %d: %d; median iterations of the refit: %.0f\n",
            nrow(s), sum(s$converged_75), MAX_ITS, sum(s$converged_500), stats::median(s$its_500)))
m <- x[x$has_topic, ]
cat(sprintf("matched topic, %d contrasts in %d studies: recovered at 75 iterations %.3f, run to convergence %.3f; same call %.3f; median r between the two topic shares %.2f\n",
            nrow(m), length(unique(m$dataset)), mean(m$rec_chosen_75), mean(m$rec_chosen_500),
            mean(m$rec_chosen_75 == m$rec_chosen_500), stats::median(m$r_theta_chosen, na.rm = TRUE)))
cat(sprintf("all %d primary contrasts (no topic counts as not recovered): matched topic %.3f and %.3f; best topic in hindsight %.3f and %.3f\n",
            nrow(x), mean(x$rec_chosen_75), mean(x$rec_chosen_500), mean(x$rec_best_75), mean(x$rec_best_500)))
nc <- x[!x$converged_75 & x$has_topic, ]
cat(sprintf("studies that had not converged at 75 iterations, %d contrasts: matched topic %.3f at 75, %.3f run to convergence\n",
            nrow(nc), mean(nc$rec_chosen_75), mean(nc$rec_chosen_500)))
