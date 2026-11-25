# rv_33_benchmark_checks.R -- three checks asked for by the pre-submission review.
# Post hoc, written 24 Nov 2026 after the pre-submission review; not in the plan.
#   1. compact model (gemma4_e4b) against the authors' hand codes, as rv_17 does
#      for the reference model: same answers, targets, recode rules and estimator
#   2. does the classifier over-respond? model-coded effect regressed on the
#      hand-coded effect (rv_17 contrasts), cluster bootstrap by dataset
#   3. do placebo falls track target rises? reference model, A1_presence
# Estimation-fold answers are read as in rv_17: read_harmonized(), then the
# package's logged reader oc_estimation_data() (one access_log entry per study);
# the frozen labels come from data/results/a1/<id>/<model>/measures.csv.
# No open-text value is printed or written; the text column is dropped on read.
# Writes tables/human_codes_e4b.csv, human_codes_e4b_effects.csv,
# overresponse.csv and placebo_crowding.csv.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_33_benchmark_checks.R
source("analysis/revision/rv_config.R"); source("analysis/revision/rv_treatments.R")
src06 <- readLines("analysis/revision/rv_06_effects.R")
eval(parse(text = src06[grep("^contrasts_for <-", src06):(grep("^args <-", src06) - 1)])) # contrasts, signs
src17 <- readLines("analysis/revision/rv_17_human_codes.R")
eval(parse(text = src17[grep("^num <-", src17):(grep("^cards <-", src17) - 1)]))          # recodes, MAP, kappa2, auc
REF <- "qwen36_35b"; E4B <- "gemma4_e4b"; TAB <- file.path(OUT, "tables")
det <- function(est, p, sgn) !is.na(p) & p < 0.05 & !is.na(sgn) & sign(est) == sgn

# --- Check 1: compact model against the hand codes ------------------------------
cards <- registered_cards(); agree <- list(); eff <- list()
for (id in unique(vapply(MAP, `[[`, "", "id"))) {
  card <- oc_study_read(cards[[id]])
  targets <- vapply(card$constructs$target, `[[`, "", "name")
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id); d <- d[nchar(trimws(d$text)) > 0, ]
  e <- oc_estimation_data(d, s, oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json"))), log = ACCESS_LOG,
                          purpose = "human-code comparison, compact model (rv_33)")
  rm(d); e$text <- NULL
  lab <- lapply(c(ref = REF, e4b = E4B), function(mod) read.csv(file.path(RESULTS, "a1", id, mod, "measures.csv")))
  strata_all <- effect_strata(id, e); cns <- contrasts_for(id, e, targets)
  for (m in Filter(function(x) x$id == id, MAP)) {
    get <- function(mod, pre) { v <- lab[[mod]][[paste0(pre, "__", m$construct)]]; if (is.null(v)) NULL else v[match(e$id, lab[[mod]]$id)] }
    h <- m$h(e); yr <- get("ref", "yes"); y4 <- get("e4b", "yes"); p4 <- get("e4b", "pyes")
    ok_r <- !is.na(h) & !is.na(yr); ok <- ok_r & !is.na(y4)
    agree[[length(agree) + 1]] <- data.frame(
      dataset = id, construct = m$construct, coder = m$coder, match = m$match, n = sum(ok), n_rv17 = sum(ok_r),
      share_human = mean(h[ok]), share_ref = mean(yr[ok]), share_e4b = mean(y4[ok]),
      kappa_ref = kappa2(h[ok], yr[ok]), kappa_e4b = kappa2(h[ok], y4[ok]), accuracy_e4b = mean(h[ok] == y4[ok]),
      auc_e4b = if (is.null(p4)) NA_real_ else auc(p4[ok], h[ok]), has_pyes_e4b = !is.null(p4), stringsAsFactors = FALSE)
    for (cn in cns) {
      if (!m$construct %in% cn$primary) next
      keep <- ok & !is.na(cn$treat)
      if (sum(cn$treat[keep] == 1) < 20 || sum(cn$treat[keep] == 0) < 20) next
      st <- if (is.null(strata_all)) NULL else strata_all[keep]
      fx <- lapply(list(human = h, model = yr, e4b = y4), function(v) oc_effect(v[keep], cn$treat[keep], strata = st, n_perm = 2000L, seed = 42L))
      eff[[length(eff) + 1]] <- data.frame(
        dataset = id, construct = m$construct, contrast = cn$name, coder = m$coder, match = m$match,
        expected_sign = expected_sign(card, m$construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L),
        n1 = fx$human$n1, n0 = fx$human$n0, n_keep_rv17 = sum(ok_r & !is.na(cn$treat)),
        est_human = fx$human$estimate, p_human = fx$human$p_ri, est_model = fx$model$estimate, p_model = fx$model$p_ri,
        est_e4b = fx$e4b$estimate, p_e4b = fx$e4b$p_ri, stringsAsFactors = FALSE)
    }
  }
}
agree <- do.call(rbind, agree); eff <- do.call(rbind, eff)
for (v in c("human", "model", "e4b")) eff[[paste0("rec_", v)]] <- det(eff[[paste0("est_", v)]], eff[[paste0("p_", v)]], eff$expected_sign)
agree$kappa_diff <- agree$kappa_ref - agree$kappa_e4b
write.csv(agree, file.path(TAB, "human_codes_e4b.csv"), row.names = FALSE)
write.csv(eff, file.path(TAB, "human_codes_e4b_effects.csv"), row.names = FALSE)

# Same answers as rv_17? (n per target and contrast, and the stored rv_17 estimates)
old <- read.csv(file.path(TAB, "human_codes_effects.csv"))
mo <- merge(old[, c("dataset", "construct", "contrast", "est_human", "est_model")], eff, by = c("dataset", "construct", "contrast"), suffixes = c("_rv17", ""))
cat(sprintf("Check 1: %d targets, %d primary contrasts; answers lost to a missing 4B label: %d (targets), %d (contrast rows); max |estimate - rv_17| human %.2g, reference %.2g\n",
            nrow(agree), nrow(eff), sum(agree$n_rv17 - agree$n), sum(eff$n_keep_rv17 - eff$n1 - eff$n0),
            max(abs(mo$est_human - mo$est_human_rv17)), max(abs(mo$est_model - mo$est_model_rv17))))
same <- agree$match == "same construct" & agree$coder == "researchers"
cat(sprintf("  median kappa, all %d targets: reference %.2f, 4B %.2f; same construct (%d): reference %.2f, 4B %.2f\n",
            nrow(agree), median(agree$kappa_ref, na.rm = TRUE), median(agree$kappa_e4b, na.rm = TRUE),
            sum(same), median(agree$kappa_ref[same], na.rm = TRUE), median(agree$kappa_e4b[same], na.rm = TRUE)))
cat(sprintf("  paired kappa difference (reference - 4B): median %.2f; favors reference %d, favors 4B %d, tied %d\n",
            median(agree$kappa_diff, na.rm = TRUE), sum(agree$kappa_diff > 0, na.rm = TRUE),
            sum(agree$kappa_diff < 0, na.rm = TRUE), sum(agree$kappa_diff == 0, na.rm = TRUE)))
cat(sprintf("  4B probability of yes stored for %d of %d targets; median AUC %.2f\n",
            sum(agree$has_pyes_e4b), nrow(agree), median(agree$auc_e4b, na.rm = TRUE)))
tab2 <- function(a, b, la, lb) cat(sprintf("  %s vs %s: both %d, %s only %d, %s only %d, neither %d\n",
                                           la, lb, sum(a & b), la, sum(a & !b), lb, sum(!a & b), sum(!a & !b)))
cat(sprintf("  detected of %d: human %d, reference %d, 4B %d\n", nrow(eff), sum(eff$rec_human), sum(eff$rec_model), sum(eff$rec_e4b)))
tab2(eff$rec_e4b, eff$rec_human, "4B", "human"); tab2(eff$rec_e4b, eff$rec_model, "4B", "reference")

# --- Check 2: over-response -----------------------------------------------------
NZ <- 0.02  # "human effect not near zero": |human-coded effect| >= 2 percentage points
slopes <- function(x, y, s) c(ols = unname(coef(lm(y ~ x))[2]), origin = sum(x * y) / sum(x^2),
                              ols_oriented = unname(coef(lm(I(s * y) ~ I(s * x)))[2]))
over <- function(x, y, ds, s, model) {
  set.seed(42); u <- unique(ds); rows <- split(seq_along(ds), ds)
  bs <- t(replicate(5000, { i <- unlist(rows[sample(u, replace = TRUE)], use.names = FALSE); slopes(x[i], y[i], s[i]) }))
  b <- slopes(x, y, s); nz <- abs(x) >= NZ; r <- abs(y[nz]) / abs(x[nz])
  data.frame(model = model, n = length(x), n_datasets = length(u),
             term = c("ols", "origin", "ols_oriented"), slope = b,
             ci_low = apply(bs, 2, quantile, 0.025, na.rm = TRUE), ci_high = apply(bs, 2, quantile, 0.975, na.rm = TRUE),
             intercept = c(coef(lm(y ~ x))[1], 0, coef(lm(I(s * y) ~ I(s * x)))[1]),
             n_nonzero = sum(nz), ratio_median = median(r), ratio_mean = mean(r),
             n_model_exceeds = sum(abs(y) > abs(x)), row.names = NULL)
}
ov <- rbind(over(old$est_human, old$est_model, old$dataset, old$expected_sign, REF),
            over(eff$est_human, eff$est_e4b, eff$dataset, eff$expected_sign, E4B))
write.csv(ov, file.path(TAB, "overresponse.csv"), row.names = FALSE)
cat("\nCheck 2: model-coded effect on hand-coded effect (bootstrap by dataset, 5,000 draws, seed 42; ratio where |human| >= 0.02)\n")
print(ov, digits = 3, row.names = FALSE)

# --- Check 3: placebo falls against target rises --------------------------------
a1 <- read.csv(file.path(OUT, "effects_a1.csv"))
a1 <- a1[a1$model == REF & a1$arm == "A1_presence", ]
pl <- a1[a1$role == "placebo", ]; tg <- a1[a1$role == "target", ]
pl$up <- pl$p_ri < 0.05 & pl$estimate > 0; pl$down <- pl$p_ri < 0.05 & pl$estimate < 0
tg$det <- det(tg$estimate, tg$p_ri, tg$expected_sign); tg$z <- tg$est_sd * tg$expected_sign  # target effect in the theorized direction
key <- function(x) paste(x$dataset, x$contrast, sep = "|")
best <- function(t) { t <- t[order(-t$z), ]; t[!duplicated(key(t)), c("dataset", "contrast", "det", "z")] }  # largest oriented est_sd
crowd <- function(p, t, sample) {
  x <- merge(p, best(t), by = c("dataset", "contrast"))
  ct <- suppressWarnings(cor.test(x$est_sd, x$z, method = "spearman", exact = FALSE))
  data.frame(sample = sample, n_tests = nrow(p), share_up = mean(p$up), share_down = mean(p$down), n_pairs = nrow(x),
             n_target_detected = sum(x$det), share_down_if_detected = mean(x$down[x$det]), share_down_if_not = mean(x$down[!x$det]),
             share_up_if_detected = mean(x$up[x$det]), share_up_if_not = mean(x$up[!x$det]),
             spearman = unname(ct$estimate), spearman_p = ct$p.value, n_contrasts = length(unique(key(x))))
}
pk <- unique(key(tg[tg$primary, ]))
cr <- rbind(crowd(pl, tg, "all contrasts"), crowd(pl[key(pl) %in% pk, ], tg[tg$primary, ], "primary contrasts"))
write.csv(cr, file.path(TAB, "placebo_crowding.csv"), row.names = FALSE)
cat(sprintf("\nCheck 3: reproduce -- all: %d tests, up %.1f%%, down %.1f%% (paper 475, 3.2, 12.2); primary: %d, up %.1f%%, down %.1f%% (paper 310, 2.9, 14.8)\n",
            cr$n_tests[1], 100 * cr$share_up[1], 100 * cr$share_down[1], cr$n_tests[2], 100 * cr$share_up[2], 100 * cr$share_down[2]))
print(cr, digits = 3, row.names = FALSE)
