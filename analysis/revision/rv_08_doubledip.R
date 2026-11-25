# ==============================================================================
# rv_08_doubledip.R -- how much does double-dipping inflate false positives?
# ==============================================================================
# Under permuted treatment labels (true effect zero), compare:
#   old:   STM with prevalence ~ treatment on ALL responses; pick the topic with
#          the largest treatment effect; test it on the same responses.
#   split: STM with prevalence ~ treatment on the discovery fold D; pick the
#          topic there; project the estimation fold E onto the D model
#          (fitNewDocuments, no covariate) and test the picked topic on E.
# The false-positive rate is the share of permutations with p < 0.05.
# Tokenization (simulation only): character bigrams for Korean/Chinese/Japanese,
# lower-cased words without stopwords otherwise; terms in >= 5 documents.
# Usage: Rscript analysis/revision/rv_08_doubledip.R <dataset> [B=100] [K=8] [cores=20]
# ==============================================================================

source("analysis/revision/rv_config.R")
suppressMessages({library(stm); library(quanteda); library(parallel)})

args <- commandArgs(trailingOnly = TRUE)
id <- args[1]
B <- if (length(args) > 1) as.integer(args[2]) else 100L
K <- if (length(args) > 2) as.integer(args[3]) else 8L
cores <- if (length(args) > 3) as.integer(args[4]) else 20L

source("analysis/revision/rv_treatments.R")
d <- read_harmonized(id)
if (!is.null(TREATMENTS[[id]])) {
  d$treatment <- TREATMENTS[[id]](d)[[1]]$treat   # first (primary) contrast
} else {
  d$treatment <- suppressWarnings(as.integer(d$treatment))
}
d <- d[!is.na(d$treatment) & d$treatment %in% c(0, 1), ]
s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
s1 <- s[s$split == 1, ]
d$fold <- s1$fold[match(d$id, s1$id)]
d <- d[!is.na(d$fold), ]
strata <- effect_strata(id, d) %||% rep("all", nrow(d))   # country, plus any EXTRA_STRATA column
lang <- if ("lang" %in% names(d)) d$lang else rep("en", nrow(d))

make_dfm <- function(text, lang) {
  cjk <- lang %in% c("ko", "zh", "zh-Hans", "zh-Hant", "ja")
  toks <- vector("list", length(text))
  if (any(cjk)) {
    tc <- tokens(text[cjk], what = "character", remove_punct = TRUE,
                 remove_numbers = TRUE, remove_separators = TRUE)
    toks[cjk] <- as.list(tokens_ngrams(tc, n = 2, concatenator = ""))
  }
  if (any(!cjk)) {
    tw <- tokens(char_tolower(text[!cjk]), remove_punct = TRUE, remove_numbers = TRUE)
    sw <- unique(c(stopwords("en"), stopwords("de"), stopwords("da")))
    toks[!cjk] <- as.list(tokens_remove(tw, sw))
  }
  dfm(as.tokens(toks))
}

full <- make_dfm(d$text, lang)
docnames(full) <- d$id
full <- dfm_trim(full, min_docfreq = 5)
keep <- rowSums(full) > 0
full <- full[keep, ]
d <- d[keep, ]
strata <- strata[keep]
iD <- which(d$fold == "D")
iE <- which(d$fold == "E")
dD <- dfm_trim(full[iD, ], min_docfreq = 3)
okD <- rowSums(dD) > 0
iD <- iD[okD]
dD <- dD[okD, ]
dE <- dfm_match(full[iE, ], featnames(dD))
okE <- rowSums(dE) > 0
iE <- iE[okE]
dE <- dE[okE, ]
cat(id, ": N =", nrow(d), " D =", length(iD), " E =", length(iE), " V =", nfeat(full), "\n")

fit_stm <- function(m, treat) {
  st <- convert(m, to = "stm")
  meta <- data.frame(treat = treat)
  mod <- stm(st$documents, st$vocab, K = K, prevalence = ~treat, data = meta,
             init.type = "Spectral", max.em.its = 75, verbose = FALSE, seed = 42)
  list(mod = mod, st = st, meta = meta)
}

pick_topic <- function(f) {
  ee <- estimateEffect(1:K ~ treat, f$mod, metadata = f$meta, uncertainty = "Global")
  tt <- summary(ee)$tables
  t <- vapply(tt, function(x) x["treat", "t value"], 0)
  p <- vapply(tt, function(x) x["treat", "Pr(>|t|)"], 0)
  list(k = which.max(abs(t)), p = p[which.max(abs(t))], t = t, p_all = p)
}

e_test <- function(th, k, tr) {
  y <- th[, k]
  est <- mean(y[tr == 1]) - mean(y[tr == 0])
  se <- sqrt(var(y[tr == 1]) / sum(tr == 1) + var(y[tr == 0]) / sum(tr == 0))
  c(p = 2 * pnorm(-abs(est / se)), t = est / se)
}

# SPLIT_ONLY=1 reruns the split procedure alone and carries the full-sample
# columns over from the existing <id>_perm.csv and <id>_summary.csv (same seed,
# same permutations). Used on 19 Nov 2026 after the projection of E was
# corrected (stm_documents() in rv_config.R); the full-sample procedure does
# not project anything and was not affected.
split_only <- Sys.getenv("SPLIT_ONLY") == "1"

one_run <- function(treat, carried = NULL) {
  if (is.null(carried)) {
    o <- pick_topic(fit_stm(full, treat))
    carried <- c(p_old = o$p, t_old = unname(o$t[o$k]), p_old_fixed = unname(o$p_all[1]))
  }
  fd <- fit_stm(dD, treat[iD])
  pk <- pick_topic(fd)
  new_docs <- stm_documents(dfm_match(dE, fd$st$vocab))   # words numbered as in the D model
  th <- fitNewDocuments(fd$mod, documents = new_docs, prevalencePrior = "None",
                        verbose = FALSE)$theta
  tr <- treat[iE]
  sel <- e_test(th, pk$k, tr)
  fix <- e_test(th, 1L, tr)
  # p_old_fixed / p_split_fixed: topic 1, no selection (separates the selection
  # effect from the effect of fitting prevalence ~ treatment on the same data).
  c(p_old = carried[["p_old"]], p_split = unname(sel["p"]), t_old = carried[["t_old"]],
    t_split_D = unname(pk$t[pk$k]), t_split_E = unname(sel["t"]),
    p_old_fixed = carried[["p_old_fixed"]], p_split_fixed = unname(fix["p"]))
}

out_dir <- file.path(OUT, "doubledip")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
prev <- prev_sum <- NULL
if (split_only) {
  prev <- read.csv(file.path(out_dir, paste0(id, "_perm.csv")))
  prev_sum <- read.csv(file.path(out_dir, paste0(id, "_summary.csv")))
  stopifnot(nrow(prev) == B, prev_sum$K == K)
}
real <- one_run(d$treatment, if (split_only) c(p_old = prev_sum$real_p_old, t_old = prev_sum$real_t_old, p_old_fixed = NA_real_))
perm <- with_seed <- NULL
perms <- opencheck:::with_seed(20261029, lapply(seq_len(B), function(b) {
  opencheck:::permute_within(d$treatment, strata)
}))
res <- mclapply(seq_len(B), function(b) tryCatch(
  one_run(perms[[b]], if (split_only) c(p_old = prev$p_old[b], t_old = prev$t_old[b], p_old_fixed = prev$p_old_fixed[b])),
  error = function(e) rep(NA_real_, 7)), mc.cores = cores)
res <- as.data.frame(do.call(rbind, res))
write.csv(res, file.path(out_dir, paste0(id, "_perm.csv")), row.names = FALSE)
summ <- data.frame(dataset = id, B = sum(!is.na(res$p_old)), K = K,
                   fpr_old = mean(res$p_old < 0.05, na.rm = TRUE),
                   fpr_split = mean(res$p_split < 0.05, na.rm = TRUE),
                   fpr_old_fixed = mean(res$p_old_fixed < 0.05, na.rm = TRUE),
                   fpr_split_fixed = mean(res$p_split_fixed < 0.05, na.rm = TRUE),
                   real_p_old = real[["p_old"]], real_p_split = real[["p_split"]],
                   real_t_old = real[["t_old"]], real_t_split_E = real[["t_split_E"]])
write.csv(summ, file.path(out_dir, paste0(id, "_summary.csv")), row.names = FALSE)
print(summ)
