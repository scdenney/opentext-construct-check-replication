# ==============================================================================
# rv_26_stm_refits.R -- how much do the topic-model results move across refits?
# ==============================================================================
# Both topic-model arms (rv_13: no covariate; prevalence = ~arm) refit on the
# discovery fold R times in one environment. Between refits only the order of
# documents and vocabulary changes: spectral initialization is deterministic, so
# the seed would not. Refit 0 is the reference order (file order of documents,
# vocabulary in byte order); refits 1..R-1 are random orders, the same for both
# arms. K, tokenization, labeler, topic-choice rule and estimator are the frozen
# ones. Plan and reporting rule: revision/deviations.md (23 Nov 2026).
# Steps:
#   fit      fit on D, write data/jobs/stm_refit/<id>.rds and label_requests.jsonl
#   submit   send the labeling requests to ALICE (stm_refit_label_gemma4_12b)
#   measure  freeze fits and choices, open E, project, estimate every contrast
#   example  the SK/TW example of rv_08 (full sample and split) in R orders
# Run: LANG=en_US.UTF-8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 \
#      KIWI_PYTHON=python3.14 Rscript analysis/revision/rv_26_stm_refits.R <step> [dataset ...]
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
args <- commandArgs(trailingOnly = TRUE)
step <- args[1]
R <- 20L
ORDER_SEED <- 20261123L
CORES <- 11L
RUN <- "stm_refit_label_gemma4_12b"
FROZEN <- file.path(JOBS, "stm")
DIR <- file.path(JOBS, "stm_refit"); dir.create(DIR, showWarnings = FALSE)
RES <- file.path(RESULTS, "stm_refit"); dir.create(RES, showWarnings = FALSE)
REF <- "qwen36_35b"
ARMS <- c("none", "cov")
cards <- registered_cards()
ids <- if (length(args) > 1) args[-1] else names(cards)
src13 <- readLines("analysis/revision/rv_13_stm_arm.R")
at <- function(p, s = src13) grep(p, s)[1]
eval(parse(text = src13[at("^arm_factor <-"):(at("^dir.create\\(STM_DIR") - 1)]))
eval(parse(text = src13[at("^SW_KO <-"):(at("^choose_K <-") - 1)]))             # tokenizer
eval(parse(text = src13[at("^label_requests <-"):(at('^if \\(step == "fit"\\)') - 1)]))
src06 <- readLines("analysis/revision/rv_06_effects.R")
eval(parse(text = src06[at("^contrasts_for <-", src06):(at("^args <-", src06) - 1)]))

orders <- function(n_doc, n_voc, r) {
  if (r == 0L) return(list(doc = seq_len(n_doc), voc = seq_len(n_voc)))
  set.seed(ORDER_SEED + r)
  list(doc = sample(n_doc), voc = sample(n_voc))
}
tag <- function(arm, r) sprintf("%s_r%02d", arm, r)
par_ids <- function(f) {
  res <- mclapply(ids, function(id) tryCatch(f(id), error = function(e) structure(paste(id, conditionMessage(e)), class = "try-error")),
                  mc.cores = CORES, mc.preschedule = FALSE)
  bad <- vapply(res, function(x) is.null(x) || inherits(x, "try-error"), NA)
  if (any(bad)) message("Failed: ", paste(ids[bad], vapply(res[bad], function(x) paste(x, collapse = " "), ""), sep = ": ", collapse = "\n"))
  res[!bad]
}

topic_text <- function(mod, texts) {
  lab <- labelTopics(mod, n = 10)
  K <- mod$settings$dim$K
  vapply(seq_len(K), function(k) {
    ex <- findThoughts(mod, texts = texts, topics = k, n = 2)$docs[[1]]
    ex <- vapply(ex, function(x) if (nchar(x) > 160) paste0(substr(x, 1, 160), "...") else x, "")
    sprintf("Topic %s\n  Most probable words: %s\n  Most distinctive words: %s\n  Example answers: %s",
            LETTERS[k], paste(lab$prob[k, ], collapse = ", "), paste(lab$frex[k, ], collapse = ", "),
            paste(sprintf("\"%s\"", gsub("[\r\n]+", " ", ex)), collapse = " | "))
  }, "")
}

fit_one <- function(id) {
  f <- file.path(DIR, paste0(id, ".rds"))
  rq <- file.path(DIR, paste0(id, ".requests.jsonl"))
  if (file.exists(f) && file.exists(rq)) return(id)                # resumable
  card <- oc_study_read(cards[[id]])
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id); d <- d[nchar(trimws(d$text)) > 0, ]
  dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = "stm refits: fit on D")
  lang <- if ("lang" %in% names(dd)) dd$lang else rep("en", nrow(dd))
  m <- dfm_trim(dfm(tokens_for(dd$text, lang)), min_docfreq = 3)
  ok <- rowSums(m) > 0
  m <- m[ok, ]
  m <- m[, sort(colnames(m), method = "radix")]
  texts <- dd$text[ok]; arm <- arm_factor(id, dd)[ok]
  fit0 <- readRDS(file.path(FROZEN, paste0(id, ".stm.rds")))
  K <- fit0$K
  fits <- list(); reqs <- list()
  for (r in 0:(R - 1L)) {
    o <- orders(nrow(m), ncol(m), r)
    mr <- m[o$doc, o$voc]
    docs <- stm_documents(mr)
    for (a in ARMS) {
      mod <- if (a == "none") {
        stm(docs, colnames(mr), K = K, init.type = "Spectral", max.em.its = 75, verbose = FALSE, seed = 42)
      } else {
        stm(docs, colnames(mr), K = K, prevalence = ~arm, data = data.frame(arm = droplevels(arm[o$doc])),
            init.type = "Spectral", max.em.its = 75, verbose = FALSE, seed = 42)
      }
      topics <- topic_text(mod, texts[o$doc])
      reqs <- c(reqs, label_requests(paste0(id, "@", tag(a, r)), card, topics))
      fits[[tag(a, r)]] <- list(mod = mod, theta_mean = colMeans(mod$theta))
    }
  }
  saveRDS(list(fits = fits, K = K, vocab_frozen = fit0$vocab, n_docs = nrow(m), n_vocab = ncol(m),
               vocab_same = setequal(colnames(m), fit0$vocab)), f)
  opencheck:::write_utf8(vapply(reqs, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), ""), rq)
  message(id, ": done (K = ", K, ", docs ", nrow(m), ", vocab ", ncol(m), ")")
  id
}

if (step == "fit") {
  done <- par_ids(fit_one)
  cat(length(done), "of", length(ids), "studies fit\n")
  if (length(args) == 1) {
    all <- unlist(lapply(Sys.glob(file.path(DIR, "*.requests.jsonl")), opencheck:::read_lines_utf8))
    opencheck:::write_utf8(all, file.path(DIR, "label_requests.jsonl"))
    cat(length(all), "labeling requests\n")
  }
}

if (step == "submit") {
  # hardware of the stm_labelcov run: one L4, FP8
  alice_submit(file.path(DIR, "label_requests.jsonl"), RUN, "gemma4_12b", time = "03:00:00", max_len = 8192L,
               hw = list(partition = "gpu-short", account = "humanities", gres = "gpu:l4:1",
                         quant = "fp8", venv = NEW_VENV, eager = TRUE))
}

read_choice <- function() {
  res <- opencheck:::read_jsonl(file.path(RESULTS, RUN, "results.jsonl"))
  ch <- do.call(rbind, lapply(res, function(r) {
    p <- exp(vapply(r$top, function(x) x$logprob, 0)); names(p) <- vapply(r$top, function(x) x$token, "")
    p <- p[names(p) %in% c("0", LETTERS)]
    pick <- names(p)[which.max(p)]
    data.frame(key = r$id, topic = if (pick == "0") 0L else match(pick, LETTERS), p = max(p) / sum(p))
  }))
  ch$construct <- sub("^.*::", "", ch$key)
  ch$unit <- sub("::.*$", "", ch$key)
  ch$dataset <- sub("@.*$", "", ch$unit)
  ch$fit <- sub("^.*@", "", ch$unit)
  ch
}

if (step == "measure") {
  choice <- read_choice()
  measure_one <- function(id) {
    ch <- choice[choice$dataset == id, ]
    chf <- file.path(DIR, paste0(id, ".topic_choice.csv"))
    write.csv(ch, chf, row.names = FALSE)
    card <- oc_study_read(cards[[id]])
    targets <- vapply(card$constructs$target, `[[`, "", "name")
    stopifnot(nrow(ch) == 2L * R * length(targets))
    s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
    lock <- file.path(LOCKS, paste0(id, ".stmrefit.k1.json"))
    rds <- file.path(DIR, paste0(id, ".rds"))
    if (!file.exists(lock)) oc_freeze(lock, s, components = c(fits = rds, topic_choice = chf),
                                      models = c(labeler = MODELS[["gemma4_12b"]]),
                                      decoding = list(refits = R, order_seed = ORDER_SEED, init = "Spectral", seed = 42,
                                                      prevalence = "none; ~arm (discovery fit only)"))
    d <- read_harmonized(id); d <- d[nchar(trimws(d$text)) > 0, ]
    e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "stm refits: measure E")
    lang <- if ("lang" %in% names(e)) e$lang else rep("en", nrow(e))
    me <- dfm(tokens_for(e$text, lang))
    x <- readRDS(rds)
    ynf <- file.path(RESULTS, "a1", id, REF, "measures.csv")
    yn <- if (file.exists(ynf)) read.csv(ynf) else NULL
    st_all <- effect_strata(id, e)
    cns <- contrasts_for(id, e, targets)
    rows <- list(); meas <- list()
    for (nm in names(x$fits)) {
      fit <- x$fits[[nm]]
      m <- dfm_match(me, fit$mod$vocab)
      ok <- rowSums(m) > 0
      th <- matrix(fit$theta_mean, nrow(e), x$K, byrow = TRUE)       # answers with no known word: D mean
      if (any(ok)) th[ok, ] <- fitNewDocuments(model = fit$mod, documents = stm_documents(m[ok, ]), newData = NULL,
                                               origData = NULL, prevalence = NULL, verbose = FALSE)$theta
      ms <- data.frame(id = e$id)
      best <- list()
      for (t in targets) {
        k <- ch$topic[ch$fit == nm & ch$construct == t]
        if (k > 0) ms[[paste0("stm__", t)]] <- th[, k]
        y <- if (is.null(yn)) NULL else yn[[paste0("yes__", t)]][match(e$id, yn$id)]
        if (!is.null(y)) {
          rr <- suppressWarnings(apply(th[ok, , drop = FALSE], 2, function(v) cor(v, y[ok], use = "complete.obs")))
          if (!all(is.na(rr))) best[[t]] <- which.max(rr)
        }
      }
      meas[[nm]] <- ms
      for (cn in cns) for (t in cn$primary) {
        sgn <- expected_sign(card, t) * (if (grepl("_opposition$", cn$name)) -1L else 1L)
        keep <- !is.na(cn$treat)
        if (sum(cn$treat[keep] == 1) < 20 || sum(cn$treat[keep] == 0) < 20) next
        one <- function(v) oc_effect(v[keep], cn$treat[keep], strata = if (is.null(st_all)) NULL else st_all[keep],
                                     n_perm = 2000L, seed = 42L)
        col <- ms[[paste0("stm__", t)]]
        ef <- if (is.null(col)) NULL else one(col)
        eb <- if (is.null(best[[t]])) NULL else one(th[, best[[t]]])
        det <- function(z) !is.null(z) && isTRUE(z$p_ri < 0.05 && z$estimate * sgn > 0)
        rows[[length(rows) + 1]] <- data.frame(
          dataset = id, construct = t, contrast = cn$name, arm = sub("_r.*$", "", nm), refit = as.integer(sub("^.*_r", "", nm)),
          matched = !is.null(ef), estimate = if (is.null(ef)) NA else ef$estimate,
          est_sd = if (is.null(ef)) NA else ef$estimate / sd(col[keep]), p_ri = if (is.null(ef)) NA else ef$p_ri,
          expected_sign = sgn, detected = det(ef), detected_best = det(eb),
          converged = isTRUE(fit$mod$convergence$converged), stringsAsFactors = FALSE)
      }
    }
    saveRDS(meas, file.path(RES, paste0(id, ".measures.rds")))
    message(id, ": measured")
    do.call(rbind, rows)
  }
  out <- do.call(rbind, par_ids(measure_one))
  write.csv(out, file.path(OUT, if (length(args) == 1) "stm_refits_effects.csv" else "stm_refits_effects_part.csv"), row.names = FALSE)
  cat(nrow(out), "estimates written\n")
}

if (step == "example") {
  src08 <- readLines("analysis/revision/rv_08_doubledip.R")
  args <- c("sktw_naturalization_2024", "1", "8", "1")
  id <- args[1]; K <- 8L
  eval(parse(text = src08[at("^d <- read_harmonized", src08):(at("^fit_stm <-", src08) - 1)]))
  eval(parse(text = src08[at("^pick_topic <-", src08):(at("^# SPLIT_ONLY", src08) - 1)]))
  fit_stm <- function(m, treat) {                                   # rv_08's fit, without convert()'s locale sort
    meta <- data.frame(treat = treat)
    list(mod = stm(stm_documents(m), colnames(m), K = K, prevalence = ~treat, data = meta, init.type = "Spectral",
                   max.em.its = 75, verbose = FALSE, seed = 42), meta = meta)
  }
  full <- full[, sort(colnames(full), method = "radix")]
  dD <- dD[, sort(colnames(dD), method = "radix")]
  ex <- do.call(rbind, mclapply(0:(R - 1L), function(r) {
    o <- orders(nrow(full), ncol(full), r)
    f <- fit_stm(full[o$doc, o$voc], d$treatment[o$doc]); set.seed(1); pf <- pick_topic(f)
    o <- orders(nrow(dD), ncol(dD), r)
    g <- fit_stm(dD[o$doc, o$voc], d$treatment[iD][o$doc]); set.seed(1); pg <- pick_topic(g)
    th <- fitNewDocuments(g$mod, documents = stm_documents(dfm_match(dE, g$mod$vocab)), prevalencePrior = "None", verbose = FALSE)$theta
    et <- e_test(th, pg$k, d$treatment[iE])
    data.frame(refit = r, t_full = unname(pf$t[pf$k]), t_split_D = unname(pg$t[pg$k]), t_split_E = unname(et["t"]),
               words_full = paste(labelTopics(f$mod, n = 5)$frex[pf$k, ], collapse = " "),
               words_split = paste(labelTopics(g$mod, n = 5)$frex[pg$k, ], collapse = " "))
  }, mc.cores = CORES))
  write.csv(ex, file.path(OUT, "stm_refits_example.csv"), row.names = FALSE)
  print(ex[, 1:4], digits = 3)
}
