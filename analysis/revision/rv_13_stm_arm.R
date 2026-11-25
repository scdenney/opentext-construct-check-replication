# ==============================================================================
# rv_13_stm_arm.R -- D: structural topic model as a measurement arm
# ==============================================================================
# The topic-model workflow done under the split, set up as a careful analyst
# would on the discovery fold alone:
#   fit      tokenize (Korean: kiwipiepy content words, as in
#            analysis/R/02_preprocess.R; Chinese/Japanese: quanteda word
#            segmentation, words of 2+ characters; other languages: lower-cased
#            words without stopwords), choose K on D with searchK over
#            K_GRID (largest sum of standardized semantic coherence and
#            exclusivity), fit STM (spectral, no treatment covariate), and write
#            one labeling request per target: each topic's top words and two
#            representative D answers, plus the card's definition of the target.
#            Variant "orig" (SK/TW only) repeats the original paper's setup:
#            one model per country, K = 6 (Korea) and 8 (Taiwan), the paper's
#            Korean stopword list.
#   (ALICE)  Qwen3.6-35B names the topic that matches the target (a letter), or
#            0 = none, as a researcher labeling topics would.
#   measure  freeze the models and the chosen topics in a lock, open the
#            estimation fold, project it onto the D models (fitNewDocuments),
#            and write to data/results/a1/<dataset>/stm/measures.csv:
#              stm__<target>     topic proportion; answers with no word in the
#                                D vocabulary get the D mean of that topic
#              stmne__<target>   the same, missing for those answers
#              stmorig__<target> the "orig" variant (SK/TW)
# Usage: Rscript analysis/revision/rv_13_stm_arm.R fit|measure [dataset ...]
#
# STM_VARIANT=cov (added 22 Nov 2026, robustness check added in revision after
# the main results were known; see revision/deviations.md): the discovery fit
# adds the randomized arms as a prevalence covariate (prevalence = ~arm), at
# the K the original fit chose, so the topics can follow the treatment as STM
# intends. Labels, lock and measures get their own names (.stmcov, label
# requests *_cov, results/a1/<id>/stmcov). The estimation fold is still
# projected with no covariate (prevalence = NULL, average prior), so treatment
# never enters the measure that is tested.
# ==============================================================================

source("analysis/revision/rv_config.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
args <- commandArgs(trailingOnly = TRUE)
step <- args[1]
cards <- registered_cards()
ids <- if (length(args) > 1) args[-1] else names(cards)
K_GRID <- c(4L, 6L, 8L, 10L, 12L)
STM_DIR <- file.path(JOBS, "stm")
VARIANT <- Sys.getenv("STM_VARIANT", "")
if (!VARIANT %in% c("", "cov")) stop("STM_VARIANT must be empty or cov")
SFX <- if (VARIANT == "cov") ".stmcov" else ".stm"
if (VARIANT == "cov") source("analysis/revision/rv_treatments.R")

# The randomized arms of a study as one factor: the joint value of all its
# contrasts on the discovery fold (NA = not in that contrast). Same contrast
# definitions as rv_06_effects.R.
arm_factor <- function(id, d) {
  f <- TREATMENTS[[id]]
  cns <- if (!is.null(f)) f(d) else list(list(treat = suppressWarnings(as.integer(d$treatment))))
  factor(do.call(paste, c(lapply(cns, function(cn) ifelse(is.na(cn$treat), "x", cn$treat)), sep = "|")))
}
dir.create(STM_DIR, recursive = TRUE, showWarnings = FALSE)
SW_KO <- read_lines_utf8(file.path(ROOT, "data/source/stopwords_ko.txt"))
SW_KO <- trimws(SW_KO[nchar(trimws(SW_KO)) > 0])

kiwi_tokens <- function(text) {
  tmp <- tempfile(fileext = ".json"); out <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(as.character(text)), tmp, useBytes = TRUE)
  # KIWI_PYTHON: an interpreter with kiwipiepy 0.22.2 (python3.14 after the Homebrew 3.15 upgrade)
  system2(Sys.getenv("KIWI_PYTHON", "python3"), c("-P", file.path(ROOT, "analysis/revision/rv_kiwi_tokens.py"), tmp, out))
  jsonlite::fromJSON(out, simplifyVector = FALSE) |> lapply(unlist) |> lapply(function(x) x %||% character(0))
}

sw_for <- function(l) {
  base <- sub("-.*$", "", l)
  if (base %in% stopwords::stopwords_getlanguages("snowball")) stopwords::stopwords(base, source = "snowball")
  else character(0)
}

tokens_for <- function(text, lang, sw_ko = character(0)) {
  toks <- vector("list", length(text))
  ko <- lang == "ko"
  zh <- lang %in% c("zh", "zh-Hans", "zh-Hant", "ja")
  if (any(ko)) toks[ko] <- lapply(kiwi_tokens(text[ko]), function(w) w[!w %in% sw_ko])
  if (any(zh)) {
    tz <- tokens(text[zh], remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
    toks[zh] <- lapply(as.list(tz), function(w) w[nchar(w) > 1])
  }
  for (l in setdiff(unique(lang[!ko & !zh]), NA)) {
    i <- which(lang == l)
    tw <- tokens(char_tolower(text[i]), remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
    toks[i] <- as.list(tokens_remove(tw, unique(c(sw_for(l), stopwords::stopwords("en")))))
  }
  as.tokens(lapply(toks, function(x) if (is.null(x)) character(0) else x))
}

choose_K <- function(st) {
  # heldout.seed fixes the held-out word draw. It was not set for the first 53
  # studies (their fits are frozen as run; see revision/deviations.md and
  # rv_13d_stm_k_stability.R); studies fit from 17 Nov 2026 on use seed 42.
  sk <- searchK(st$documents, st$vocab, K = K_GRID, init.type = "Spectral", heldout.seed = 42,
                max.em.its = 75, verbose = FALSE, cores = 1, seed = 42)
  r <- data.frame(K = unlist(sk$results$K), coh = unlist(sk$results$semcoh),
                  exc = unlist(sk$results$exclus), heldout = unlist(sk$results$heldout))
  r$score <- as.numeric(scale(r$coh)) + as.numeric(scale(r$exc))
  list(K = r$K[which.max(r$score)], grid = r)
}

# Fit one topic model on discovery-fold answers; returns the model and the
# topic descriptions shown to the labeling model.
fit_unit <- function(key, texts, lang, K = NULL, sw_ko = character(0), arm = NULL) {
  m <- dfm_trim(dfm(tokens_for(texts, lang, sw_ko)), min_docfreq = 3)
  ok <- rowSums(m) > 0
  st <- convert(m[ok, ], to = "stm")
  ck <- if (is.null(K)) choose_K(st) else list(K = K, grid = NULL)
  t0 <- proc.time()[["elapsed"]]
  mod <- if (is.null(arm)) {
    stm(st$documents, st$vocab, K = ck$K, init.type = "Spectral", max.em.its = 75, verbose = FALSE, seed = 42)
  } else {
    stm(st$documents, st$vocab, K = ck$K, prevalence = ~arm, data = data.frame(arm = droplevels(arm[ok])),
        init.type = "Spectral", max.em.its = 75, verbose = FALSE, seed = 42)
  }
  saveRDS(list(mod = mod, vocab = st$vocab, K = ck$K, grid = ck$grid, sw_ko = sw_ko,
               theta_mean = colMeans(mod$theta), variant = VARIANT,
               fit_seconds = proc.time()[["elapsed"]] - t0, n_docs = sum(ok), n_vocab = length(st$vocab),
               arm_levels = if (is.null(arm)) NULL else table(arm[ok])),
          file.path(STM_DIR, paste0(key, SFX, ".rds")))
  lab <- labelTopics(mod, n = 10)
  tx <- texts[ok]
  vapply(seq_len(ck$K), function(k) {
    ex <- findThoughts(mod, texts = tx, topics = k, n = 2)$docs[[1]]
    ex <- vapply(ex, function(x) if (nchar(x) > 160) paste0(substr(x, 1, 160), "...") else x, "")
    sprintf("Topic %s\n  Most probable words: %s\n  Most distinctive words: %s\n  Example answers: %s",
            LETTERS[k], paste(lab$prob[k, ], collapse = ", "), paste(lab$frex[k, ], collapse = ", "),
            paste(sprintf("\"%s\"", gsub("[\r\n]+", " ", ex)), collapse = " | "))
  }, "")
}

label_requests <- function(key, card, topics) {
  K <- length(topics)
  lapply(card$constructs$target, function(t) {
    system <- paste(c(
      trimws(card_context(card)),
      "A topic model was fit to a sample of these answers. Each topic is shown with its most probable words, its most distinctive words, and two answers that load heavily on it.",
      paste0("Which topic best captures this consideration?\n", t$name, ": ", trimws(t$definition)),
      sprintf("Answer with the topic letter (A-%s), or 0 if no topic captures it. Write only the letter or 0.", LETTERS[K])
    ), collapse = "\n\n")
    user <- paste0(paste(topics, collapse = "\n\n"), "\n\nTopic:")
    list(id = paste(key, t$name, sep = "::"),
         prompt_hash = substr(digest::digest(paste(system, user), "sha256"), 1, 16),
         system = system, user = user, codes = c("0", LETTERS[seq_len(K)]))
  })
}

if (step == "fit") {
  units <- lapply(ids, function(id) list(key = id, id = id, group = NULL))
  if ("sktw_naturalization_2024" %in% ids && VARIANT == "") units <- c(units, list(
    list(key = "sktw_naturalization_2024@orig_KR", id = "sktw_naturalization_2024", group = "South Korea", K = 6L),
    list(key = "sktw_naturalization_2024@orig_TW", id = "sktw_naturalization_2024", group = "Taiwan", K = 8L)))
  reqs <- mclapply(units, function(u) {
    card <- oc_study_read(cards[[u$id]])
    s <- oc_split_read(file.path(SPLITS, paste0(u$id, ".holdout.json")))
    d <- read_harmonized(u$id)
    d <- d[nchar(trimws(d$text)) > 0, ]
    dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = paste("stm arm: fit on D", u$key))
    if (!is.null(u$group)) dd <- dd[dd$country == u$group, ]
    lang <- if ("lang" %in% names(dd)) dd$lang else rep("en", nrow(dd))
    if (VARIANT == "cov") {
      K0 <- readRDS(file.path(STM_DIR, paste0(u$key, ".stm.rds")))$K   # the original fit's K
      topics <- fit_unit(u$key, dd$text, lang, K = K0, arm = arm_factor(u$id, dd))
    } else {
      topics <- fit_unit(u$key, dd$text, lang, K = u$K, sw_ko = if (is.null(u$group)) character(0) else SW_KO)
    }
    label_requests(u$key, card, topics)
  }, mc.cores = 8)
  bad <- vapply(reqs, inherits, NA, "try-error")
  if (any(bad)) message("Failed: ", paste(vapply(units[bad], `[[`, "", "key"), collapse = ", "), "\n",
                        paste(unique(unlist(reqs[bad])), collapse = "\n"))
  reqs <- unlist(reqs[!bad], recursive = FALSE)
  # LABEL_BATCH names the request file of a later batch (default: the original file)
  f <- file.path(STM_DIR, paste0("label_requests", if (VARIANT == "cov") "_cov" else "", Sys.getenv("LABEL_BATCH"), ".jsonl"))
  opencheck:::write_utf8(vapply(reqs, function(r) jsonlite::toJSON(r, auto_unbox = TRUE), ""), f)
  cat(length(reqs), "labeling requests written to", f, "\n")
}

if (step == "measure") {
  # Topic labeler: Gemma 4 12B (primary); LABELER=qwen36_35b for the check
  labeler <- Sys.getenv("LABELER", "gemma4_12b")
  # stm_label2_* holds the first 53 studies; later batches are stm_label3_*, ...
  res <- unlist(lapply(Sys.glob(file.path(RESULTS, paste0(if (VARIANT == "cov") "stm_labelcov_" else "stm_label[2-9]_", labeler), "results.jsonl")),
                       opencheck:::read_jsonl), recursive = FALSE)
  choice <- do.call(rbind, lapply(res, function(r) {
    p <- exp(vapply(r$top, function(x) x$logprob, 0)); names(p) <- vapply(r$top, function(x) x$token, "")
    p <- p[names(p) %in% c("0", LETTERS)]
    pick <- names(p)[which.max(p)]
    data.frame(key = r$id, topic = if (pick == "0") 0L else match(pick, LETTERS), p = max(p) / sum(p))
  }))
  choice$unit <- sub("::.*$", "", choice$key)
  choice$dataset <- sub("@.*$", "", choice$unit)
  choice$construct <- sub("^.*::", "", choice$key)
  write.csv(choice, file.path(STM_DIR, paste0("topic_choice", if (VARIANT == "cov") "_cov" else "", ".csv")), row.names = FALSE)
  for (id in intersect(ids, unique(choice$dataset))) {
    ch <- choice[choice$dataset == id, ]
    chf <- file.path(STM_DIR, paste0(id, if (VARIANT == "cov") ".stmcov" else "", ".topic_choice.csv"))
    write.csv(ch, chf, row.names = FALSE)
    units <- unique(ch$unit)
    rds <- setNames(file.path(STM_DIR, paste0(units, SFX, ".rds")), paste0("stm_", units))
    s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
    lock <- file.path(LOCKS, paste0(id, SFX, ".k1.json"))
    if (!file.exists(lock)) oc_freeze(lock, s, components = c(rds, topic_choice = chf),
                                      models = c(labeler = MODELS[[labeler]]),
                                      decoding = list(K_grid = K_GRID, init = "Spectral", seed = 42,
                                                      prevalence = if (VARIANT == "cov") "~arm (discovery fit only)" else "none"))
    d <- read_harmonized(id)
    d <- d[nchar(trimws(d$text)) > 0, ]
    e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "stm arm: measure E")
    meas <- data.frame(id = e$id)
    for (u in units) {
      fit <- readRDS(file.path(STM_DIR, paste0(u, SFX, ".rds")))
      orig <- grepl("@orig_", u)
      rows <- if (orig) which(e$country == c(KR = "South Korea", TW = "Taiwan")[sub("^.*@orig_", "", u)]) else seq_len(nrow(e))
      lang <- if ("lang" %in% names(e)) e$lang[rows] else rep("en", length(rows))
      m <- dfm_match(dfm(tokens_for(e$text[rows], lang, fit$sw_ko)), fit$vocab)
      ok <- rowSums(m) > 0
      theta <- matrix(NA_real_, length(rows), fit$K)
      if (any(ok)) {
        theta[ok, ] <- fitNewDocuments(model = fit$mod, documents = stm_documents(m[ok, ]), newData = NULL,
                                       origData = NULL, prevalence = NULL, verbose = FALSE)$theta
      }
      for (i in which(ch$unit == u)) {
        if (ch$topic[i] == 0) next   # no topic matched the target
        v <- theta[, ch$topic[i]]
        if (orig) {
          col <- paste0("stmorig__", ch$construct[i])
          if (is.null(meas[[col]])) meas[[col]] <- NA_real_
          v[is.na(v)] <- fit$theta_mean[ch$topic[i]]
          meas[[col]][rows] <- v
        } else {
          meas[[paste0("stmne__", ch$construct[i])]] <- v
          v[is.na(v)] <- fit$theta_mean[ch$topic[i]]
          meas[[paste0("stm__", ch$construct[i])]] <- v
        }
      }
      message(u, ": K = ", fit$K, "; ", sum(ch$unit == u & ch$topic > 0), " of ", sum(ch$unit == u),
              " targets matched a topic; ", round(100 * mean(!ok)), "% of E answers had no in-vocabulary word")
    }
    out <- file.path(RESULTS, "a1", id, if (VARIANT == "cov") "stmcov" else "stm")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    if (ncol(meas) > 1) write.csv(meas, file.path(out, "measures.csv"), row.names = FALSE)
  }
}
