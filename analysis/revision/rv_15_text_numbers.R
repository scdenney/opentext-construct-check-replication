# ==============================================================================
# rv_15_text_numbers.R -- numbers quoted in the note and SI that no other
# script prints
# ==============================================================================
# Reads the outputs of rv_06 ... rv_13d and writes
# analysis/output/revision/text_numbers.md. Nothing here feeds back into a
# measure; the script only summarizes estimates that already exist.
# "Search 1": the 53 cards registered on 28 Oct 2026. "Search 2": the 86 cards
# registered on 17-18 Nov 2026.
# ==============================================================================

source("analysis/revision/rv_config.R")
suppressMessages(library(dplyr))
out <- character(0)
say <- function(...) out <<- c(out, sprintf(...))
rd <- function(f) { p <- file.path(OUT, f); if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL }
REF <- "qwen36_35b"

reg <- lapply(opencheck:::read_lines_utf8(file.path(REGISTRY, "registry.jsonl")), jsonlite::fromJSON)
first_time <- tapply(vapply(reg, `[[`, "", "time"), vapply(reg, `[[`, "", "id"), min)
search1 <- names(first_time)[as.Date(substr(first_time, 1, 10)) < as.Date("2026-11-01")]
is_own <- function(x) grepl("^(sktw|aks21|ifes2023|nwo26|prc|roc|rok)", x)
grp <- function(x) ifelse(is_own(x), "own", "public")

# --- 1. Corpus -----------------------------------------------------------------
ct <- rd("corpus_table.csv")
ct <- ct[ct$dataset %in% names(first_time), ]
ct$inst <- instrument_of(ct$dataset)
say("## Corpus")
cnt <- function(d, lab) say("- %s: %d cards, %d samples, %s answers, %s in E; smallest card %d answers", lab, nrow(d),
                            n_distinct(d$inst), format(sum(d$n_text), big.mark = ","), format(sum(d$n_E), big.mark = ","), min(d$n_text))
cnt(ct, "all"); cnt(ct[is_own(ct$dataset), ], "own"); cnt(ct[!is_own(ct$dataset), ], "public")
cnt(ct[ct$dataset %in% search1, ], "search 1"); cnt(ct[!ct$dataset %in% search1, ], "search 2")
cnt(ct[!is_own(ct$dataset) & ct$dataset %in% search1, ], "public, search 1")
lang_tab <- function(d) { l <- sort(table(unlist(strsplit(d$languages, "/"))), decreasing = TRUE); paste(sprintf("%s %d", names(l), l), collapse = ", ") }
say("- languages (cards), own: %s", lang_tab(ct[is_own(ct$dataset), ]))
say("- languages (cards), public: %s", lang_tab(ct[!is_own(ct$dataset), ]))
multi <- sort(table(ct$inst)[table(ct$inst) > 1], decreasing = TRUE)
say("- samples with several cards: %s", paste(sprintf("%s (%d)", names(multi), multi), collapse = "; "))
say("- one-card-per-sample corpora: %s", format(prod(multi), big.mark = ","))
say("- cards written blind: %d of %d public", sum(ct$written_blind[!is_own(ct$dataset)]), sum(!is_own(ct$dataset)))
# answers by language: counts only, no text is shown
lang_n <- unlist(lapply(ct$dataset, function(id) {
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  l <- if ("lang" %in% names(d)) as.character(d$lang) else rep(ct$languages[ct$dataset == id], nrow(d))
  table(l)
}))
lang_n <- sort(tapply(lang_n, names(lang_n), sum), decreasing = TRUE)
say("- answers by language: %s", paste(sprintf("%s %s", names(lang_n), format(lang_n, big.mark = ",", trim = TRUE)), collapse = ", "))
say("- languages with at least 1,000 answers: %d; with any: %d", sum(lang_n >= 1000), length(lang_n))

# --- 2. Recovery, reference model ------------------------------------------------
a1 <- rd("effects_a1.csv")
a1$sig <- a1$p_ri < 0.05
a1$sign_ok <- ifelse(is.na(a1$expected_sign) | a1$expected_sign == 0, NA, sign(a1$estimate) == a1$expected_sign)
a1$rec <- a1$sig & !is.na(a1$sign_ok) & a1$sign_ok   # recovered: significant in the theorized direction
prim <- a1[a1$primary & a1$role == "target", ]
rate <- function(d, lab) {
  if (!nrow(d)) return(invisible())
  w <- 1 / ave(rep(1, nrow(d)), instrument_of(d$dataset), FUN = sum)
  say("- %s: %d contrasts, %d cards, %d samples; recovered %.3f; per sample %.3f; any p<.05 %.3f; expected sign %.3f; significantly reversed %d",
      lab, nrow(d), n_distinct(d$dataset), n_distinct(instrument_of(d$dataset)), mean(d$rec), weighted.mean(d$rec, w),
      mean(d$sig), mean(d$sign_ok, na.rm = TRUE), sum(d$sig & !d$sign_ok, na.rm = TRUE))
}
say("\n## Recovery of the target, primary contrasts")
arms <- c(A1_presence = REF, C_rating = REF, B_pairwise = REF, D_topic = "stm", D_topic_nonempty = "stm")
for (a in names(arms)) {
  d <- prim[prim$model == arms[[a]] & prim$arm == a, ]
  rate(d, paste(a, arms[[a]], "all"))
  rate(d[is_own(d$dataset), ], paste(a, "own")); rate(d[!is_own(d$dataset), ], paste(a, "public"))
  rate(d[d$dataset %in% search1, ], paste(a, "search 1")); rate(d[!d$dataset %in% search1, ], paste(a, "search 2"))
}
no_primary <- setdiff(ct$dataset, prim$dataset[prim$model == REF & prim$arm == "A1_presence"])
say("- cards without a primary target contrast for the reference model: %s", paste(no_primary, collapse = ", "))

# --- 3. Induced codebooks ----------------------------------------------------------
a2 <- rd("effects_a2.csv")
if (!is.null(a2)) {
  p2 <- a2[a2$primary & a2$model == REF, ]
  p2$sig <- abs(p2$estimate / p2$se) > 1.96
  # theorized sign of the coded target under each contrast, from the card-based estimates
  es <- unique(prim[prim$model == REF & prim$arm == "A1_presence", c("dataset", "contrast", "construct", "expected_sign")])
  p2$expected_sign <- es$expected_sign[match(paste(p2$dataset, p2$contrast, p2$construct), paste(es$dataset, es$contrast, es$construct))]
  say("- A2 primary contrasts without a matching card-based target (sign taken as positive): %d", sum(is.na(p2$expected_sign)))
  p2$expected_sign[is.na(p2$expected_sign)] <- 1
  p2$rec <- p2$sig & sign(p2$estimate) == p2$expected_sign
  say("\n## Induced codebooks (A2), primary contrasts")
  a2rate <- function(d, lab) say("- %s: %d contrasts, %d cards; recovered %.3f; any p<.05 %.3f; theorized sign %d; positive %d", lab, nrow(d), n_distinct(d$dataset),
                                 mean(d$rec), mean(d$sig), sum(sign(d$estimate) == d$expected_sign), sum(d$estimate > 0))
  a2rate(p2, "all"); a2rate(p2[is_own(p2$dataset), ], "own"); a2rate(p2[!is_own(p2$dataset), ], "public")
  a2rate(p2[p2$dataset %in% search1, ], "search 1"); a2rate(p2[!p2$dataset %in% search1, ], "search 2")
  five <- p2[p2$n_splits == 5, ]
  say("- five-split studies: %d contrasts; median range across splits %.1f points", nrow(five), 100 * median(five$max - five$min))
  y <- prim[prim$model == REF & prim$arm == "A1_presence", c("dataset", "construct", "contrast", "estimate")]
  # an induced codebook maps one code to the card's first target, so match on dataset and contrast
  m <- merge(p2[, c("dataset", "contrast", "estimate")], y[!duplicated(y[, c("dataset", "contrast")]), ],
             by = c("dataset", "contrast"), suffixes = c("_a2", "_a1"))
  say("- r between A2 and yes/no estimates: all %.2f (n = %d); own %.2f (n = %d)", cor(m$estimate_a2, m$estimate_a1), nrow(m),
      cor(m$estimate_a2[is_own(m$dataset)], m$estimate_a1[is_own(m$dataset)]), sum(is_own(m$dataset)))
  st <- do.call(rbind, lapply(Sys.glob(file.path(OUT, "a2_induction_status*.csv")), read.csv, stringsAsFactors = FALSE))
  st <- st[st$k == 1 & !duplicated(st[, c("dataset", "k")], fromLast = TRUE), ]
  say("- induction, first split: %d of %d codebooks parsed; failed: %s", sum(st$ok), nrow(st), paste(st$dataset[!st$ok], collapse = ", "))
}

# --- 4. Placebos ---------------------------------------------------------------------
say("\n## Placebos, all contrasts")
pl <- a1[a1$role == "placebo" & a1$arm %in% c("A1_presence", "C_rating"), ]
for (k in split(pl, list(pl$model, pl$arm), drop = TRUE))
  say("- %s %s: n = %d (%d cards); rise %.3f; fall %.3f", k$model[1], k$arm[1], nrow(k), n_distinct(k$dataset),
      mean(k$sig & k$estimate > 0), mean(k$sig & k$estimate < 0))

# --- 5. Other models against the reference ---------------------------------------------
say("\n## Other models against the reference model, primary target contrasts")
am <- rd("agreement_models.csv")
for (mk in setdiff(unique(prim$model), c(REF, "stm", "majority", "qwen36_27b", "gemma4_12b"))) for (a in c("A1_presence", "C_rating")) {
  key <- c("dataset", "construct", "contrast")
  m <- merge(prim[prim$model == mk & prim$arm == a, c(key, "est_sd", "sig", "sign_ok", "rec")],
             prim[prim$model == REF & prim$arm == a, c(key, "est_sd", "sig", "sign_ok", "rec")], by = key, suffixes = c("", "_ref"))
  if (!nrow(m)) next
  say("- %s %s: %d contrasts, %d cards (%d from search 1); recovered %.3f vs reference %.3f; any p<.05 %.3f vs %.3f; expected sign %.3f vs %.3f; r %.3f; same recovery call %.3f; same significance call %.3f",
      mk, a, nrow(m), n_distinct(m$dataset), sum(unique(m$dataset) %in% search1), mean(m$rec), mean(m$rec_ref), mean(m$sig), mean(m$sig_ref),
      mean(m$sign_ok, na.rm = TRUE), mean(m$sign_ok_ref, na.rm = TRUE), cor(m$est_sd, m$est_sd_ref, use = "complete.obs"), mean(m$rec == m$rec_ref), mean(m$sig == m$sig_ref))
}
if (!is.null(am)) {
  am$pair <- paste(pmin(am$model_a, am$model_b), pmax(am$model_a, am$model_b), sep = " ~ ")
  for (k in split(am, am$pair)) say("- kappa %s: median %.3f over %d constructs in %d cards", k$pair[1], median(k$kappa, na.rm = TRUE), nrow(k), n_distinct(k$dataset))
}
aa <- rd("agreement_arms.csv")
if (!is.null(aa)) for (k in split(aa, list(aa$model, aa$arm_a, aa$arm_b), drop = TRUE))
  say("- within %s, %s ~ %s: median Spearman %.3f (%d constructs)", k$model[1], k$arm_a[1], k$arm_b[1], median(k$spearman, na.rm = TRUE), nrow(k))

# --- 6. Double-dipping ---------------------------------------------------------------------
dd <- rd("tables/doubledip_fpr.csv")
if (!is.null(dd)) {
  say("\n## Double-dipping")
  ddr <- function(d, lab) say("- %s: %d studies; full %.3f (%.2f-%.2f); split %.3f (%.2f-%.2f); fixed topic: full %.3f, split %.3f; full fixed > .35 in %d; per sample (%d): full %.3f, split %.3f",
    lab, nrow(d), mean(d$fpr_old), min(d$fpr_old), max(d$fpr_old), mean(d$fpr_split), min(d$fpr_split), max(d$fpr_split),
    mean(d$fpr_old_fixed), mean(d$fpr_split_fixed), sum(d$fpr_old_fixed > 0.35), n_distinct(instrument_of(d$dataset)),
    mean(tapply(d$fpr_old, instrument_of(d$dataset), mean)), mean(tapply(d$fpr_split, instrument_of(d$dataset), mean)))
  ddr(dd, "all"); ddr(dd[dd$dataset %in% search1, ], "search 1"); ddr(dd[!dd$dataset %in% search1, ], "search 2")
  say("- not in the simulation table: %s", paste(setdiff(ct$dataset, dd$dataset), collapse = ", "))
}

# --- 7. Topic-model diagnostics ----------------------------------------------------------------
dg <- rd("tables/stm_topic_diagnostic.csv")
if (!is.null(dg)) {
  say("\n## Topic-model arm")
  ch <- dg[dg$chosen > 0 & !is.na(dg$r_chosen), ]
  say("- targets with a topic and a reference label: %d; labeler chose the best-correlated topic for %d (%.0f%%); median r chosen %.3f; median r best %.3f (over %d targets)",
      nrow(ch), sum(ch$best_is_chosen), 100 * mean(ch$best_is_chosen), median(ch$r_chosen), median(dg$r_best, na.rm = TRUE), sum(!is.na(dg$r_best)))
  say("- targets where the labeler chose no topic: %d of %d", sum(dg$chosen == 0), nrow(dg))
}
ks <- rd("tables/stm_k_stability.csv")
if (!is.null(ks)) {
  say("- K stability: %d studies; K used wins in a median of %.0f%% of draws; modal K differs from K used in %d; same K in every draw in %d",
      nrow(ks), 100 * median(ks$share_K_used), sum(ks$K_modal != ks$K_used), sum(ks$n_distinct_K == 1))
}

# --- 8. Pairwise diagnostics -----------------------------------------------------------------------
bt <- rd("tables/bt_diagnostics.csv")
if (!is.null(bt)) {
  say("\n## Pairwise diagnostics")
  for (k in split(bt, bt$model)) say("- %s: %d fits; held-out accuracy median %.3f (%.2f-%.2f); ICI median %.3f; order gap median %.3f",
    k$model[1], nrow(k), median(k$heldout_acc, na.rm = TRUE), min(k$heldout_acc, na.rm = TRUE), max(k$heldout_acc, na.rm = TRUE),
    median(k$ici, na.rm = TRUE), median(abs(k$order_gap), na.rm = TRUE))
}

# --- 9. Per-sample rates against per-contrast rates, reference model -------------------------------------
ps <- rd("tables/a1_rates_per_sample.csv")
if (!is.null(ps)) {
  say("\n## Per sample against per contrast")
  k <- ps[(ps$model == REF & ps$arm %in% c("A1_presence", "C_rating", "B_pairwise")) | (ps$model == "stm" & ps$arm == "D_topic"), ]
  for (i in seq_len(nrow(k))) say("- %s %s (recovered): per contrast %.3f; sample-weighted %.3f (%+.1f points); one card per sample median %.3f (%+.1f points), range %.3f-%.3f over %s corpora",
    k$model[i], k$arm[i], k$unweighted[i], k$weighted[i], 100 * (k$weighted[i] - k$unweighted[i]), k$one_med[i],
    100 * (k$one_med[i] - k$unweighted[i]), k$one_min[i], k$one_max[i], format(k$n_corpora[i], big.mark = ","))
}

# --- 10. Pairwise scaling and how common the construct is -----------------------------------------------
# A pair is informative about a construct only if one of the two answers shows
# it. With the share of answers labelled yes (reference model, estimation fold)
# as the prevalence, (1 - share)^2 of random pairs compare two answers that both
# lack the construct.
say("\n## Pairwise scaling and prevalence of the construct")
key <- c("dataset", "construct", "contrast")
qy <- prim[prim$model == REF & prim$arm == "A1_presence", ]
qb <- prim[prim$model == REF & prim$arm == "B_pairwise", ]
pw <- merge(qb[, c(key, "rec", "est_sd")], qy[, c(key, "rec", "est_sd", "mean_control", "estimate", "n1", "n0")], by = key, suffixes = c("_pairwise", "_yesno"))
say("- the %d contrasts of the pairwise arm: recovered by pairwise %.3f, by yes/no %.3f; same call %.3f; pairwise only %d, yes/no only %d; r of estimates (SD units) %.2f",
    nrow(pw), mean(pw$rec_pairwise), mean(pw$rec_yesno), mean(pw$rec_pairwise == pw$rec_yesno), sum(pw$rec_pairwise & !pw$rec_yesno),
    sum(!pw$rec_pairwise & pw$rec_yesno), cor(pw$est_sd_pairwise, pw$est_sd_yesno))
pw$share <- (pw$mean_control * pw$n0 + (pw$mean_control + pw$estimate) * pw$n1) / (pw$n0 + pw$n1)
sh <- tapply(pw$share, pw$dataset, mean)
say("- first targets, %d studies: share of answers labelled yes: median %.2f (IQR %.2f-%.2f); under 0.10 in %d studies",
    length(sh), median(sh), quantile(sh, .25), quantile(sh, .75), sum(sh < .10))
say("- random pairs in which neither answer shows the construct: median %.2f (IQR %.2f-%.2f); exactly one does: median %.2f",
    median((1 - sh)^2), quantile((1 - sh)^2, .25), quantile((1 - sh)^2, .75), median(2 * sh * (1 - sh)))
btd <- rd("bt_diagnostics.csv"); btd <- btd[btd$model == REF, ]; btd$share <- sh[btd$dataset]
band <- function(x) cut(x, c(-Inf, .10, .25, Inf), labels = c("under 0.10", "0.10 to 0.25", "over 0.25"), right = FALSE)
for (b in levels(band(0))) {
  k <- pw[band(pw$share) == b, ]
  say("- prevalence %s: %d contrasts; recovered: yes/no %.2f, pairwise %.2f; held-out pairwise accuracy (median over %d fits) %.2f",
      b, nrow(k), mean(k$rec_yesno), mean(k$rec_pairwise), sum(band(btd$share) == b, na.rm = TRUE), median(btd$heldout_acc[band(btd$share) == b], na.rm = TRUE))
}
say("- correlation between held-out pairwise accuracy and prevalence: %.2f", cor(btd$heldout_acc, btd$share, use = "complete.obs"))

# --- 11. Topic model against the original authors' hand codes (rv_17_human_codes.R) --------------------
# stm: answers with no word of the discovery vocabulary get the topic's
# discovery mean; stmne: those answers are left out (sensitivity). Effects
# "with a matched topic" are on the rows where hand code, label and topic share
# are all present; over all contrasts, a target without a topic counts as not
# recovered by the topic model and keeps the two-measure comparison.
ha <- rd("tables/human_codes_agreement.csv"); he <- rd("tables/human_codes_effects.csv")
if (!is.null(ha) && "n_stm" %in% names(ha)) {
  say("\n## Topic model against the hand codes")
  for (v in c("stm", "stmne")) {
    sfx <- if (v == "stm") "_t" else "_ne"
    a <- ha[ha[[paste0("n_", v)]] > 0, ]
    say("- %s: %d of %d targets (%d studies) with a matched topic; median AUC against the hand code: probability of yes %.2f, topic share %.2f; median r with the hand code: yes/no label %.2f, probability of yes %.2f, topic share %.2f",
        v, nrow(a), nrow(ha), n_distinct(a$dataset), median(a[[paste0("auc_pyes_", v)]], na.rm = TRUE), median(a[[paste0("auc_", v)]], na.rm = TRUE),
        median(a[[paste0("r_yes_", v)]], na.rm = TRUE), median(a[[paste0("r_pyes_", v)]], na.rm = TRUE), median(a[[paste0("r_", v)]], na.rm = TRUE))
    hr <- function(d, lab) say("- %s, %s: %d contrasts (%d studies); recovered: hand code %d, model label %d, topic share %d; topic and hand code both %d",
      v, lab, nrow(d), n_distinct(d$dataset), sum(d[[paste0("rec_human", sfx)]]), sum(d[[paste0("rec_model", sfx)]]), sum(d[[paste0("rec_", v)]]),
      sum(d[[paste0("rec_", v)]] & d[[paste0("rec_human", sfx)]]))
    hr(he[he[[paste0("has_", v)]], ], "contrasts with a matched topic"); hr(he, "all primary contrasts")
  }
}

writeLines(c("# Numbers quoted in the note and SI (generated by rv_15_text_numbers.R)", "", out), file.path(OUT, "text_numbers.md"))
cat(out, sep = "\n")
