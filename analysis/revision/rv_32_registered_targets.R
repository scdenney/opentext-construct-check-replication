# rv_32_registered_targets.R -- three gaps found by the pre-submission review. Post hoc,
# 24 Nov 2026, after the pre-submission review; rv_treatments.R, rv_06_effects.R and
# effects_a1.csv are left unchanged.
#   A. nwo26_exp3_comprehension: the three registered country-target contrasts (only
#      founding_charter was coded), estimated as rv_06 does -> tables/registered_targets_nwo26.csv
#   B. detection rates over alternative sets of target contrasts -> tables/contrast_sets.csv
#   C. chopra2024_newsdemand without the 257 rows flagged exclude = "True", which
#      read_harmonized() keeps because it matches only TRUE/"TRUE" -> tables/chopra_sensitivity.csv
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_32_registered_targets.R
source("analysis/revision/rv_config.R"); source("analysis/revision/rv_treatments.R")
src <- readLines("analysis/revision/rv_06_effects.R")   # measure_info(), contrasts_for(), expected_sign()
eval(parse(text = src[grep("^measure_info <-", src):(grep("^args <-", src) - 1)]))
TAB <- file.path(OUT, "tables"); dir.create(TAB, showWarnings = FALSE)
x  <- read.csv(file.path(OUT, "effects_a1.csv"), stringsAsFactors = FALSE)
xc <- read.csv(file.path(OUT, "effects_stmcov.csv"), stringsAsFactors = FALSE)
detected <- function(z) !is.na(z$p_ri) & z$p_ri < .05 & !is.na(z$estimate * z$expected_sign) & z$estimate * z$expected_sign > 0
key <- function(z) paste(z$dataset, z$model, z$arm, z$construct, z$contrast)

# Estimation sample and labels loaded as in rv_06: registered card, a1 lock, holdout split,
# read_harmonized(), then opencheck's logged reader oc_estimation_data() (ACCESS_LOG).
load_study <- function(id, purpose) {
  card <- oc_study_read(registered_cards()[[id]])
  lock <- oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json")))
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = purpose)
  meas <- list()
  for (mk in list.dirs(file.path(RESULTS, "a1", id), recursive = FALSE, full.names = FALSE)) {
    f <- file.path(RESULTS, "a1", id, mk, "measures.csv")
    if (file.exists(f)) meas[[mk]] <- read.csv(f, stringsAsFactors = FALSE)
  }
  yes_cols <- unique(unlist(lapply(meas, function(m) grep("^yes__", names(m), value = TRUE))))
  maj <- data.frame(id = e$id)   # cross-model majority vote, as in rv_06
  for (yc in yes_cols) {
    v <- matrix(sapply(meas, function(m) if (yc %in% names(m)) m[[yc]][match(e$id, m$id)] else rep(NA, nrow(e))), nrow = nrow(e))
    n <- rowSums(!is.na(v)); maj[[yc]] <- ifelse(n == 0, NA, as.integer(rowSums(v, na.rm = TRUE) > n / 2))
  }
  meas$majority <- maj
  list(card = card, targets = vapply(card$constructs$target, `[[`, "", "name"), e = e, d = d, s = s, lock = lock, meas = meas)
}

# The rv_06 loop for given contrasts on (a subset of) the estimation sample.
effects_on <- function(id, L, e, contrasts, models = names(L$meas), arms = NULL, constructs = NULL) {
  strata <- effect_strata(id, e); rows <- list()
  for (cn in contrasts) for (mk in intersect(models, names(L$meas))) {
    m <- L$meas[[mk]]
    for (col in grep("^(yes|pyes|rate|bt|stm|stmne|stmorig)__", names(m), value = TRUE)) {
      info <- measure_info(col)
      if ((!is.null(arms) && !info$arm %in% arms) || (!is.null(constructs) && !info$construct %in% constructs)) next
      y <- m[[col]][match(e$id, m$id)]; coverage <- mean(!is.na(y)); tr <- cn$treat
      if (coverage < 0.95 && info$arm != "D_topic_nonempty") next
      keep <- !is.na(tr) & !is.na(y)
      if (sum(tr[keep] == 1) < 20 || sum(tr[keep] == 0) < 20) next
      ef <- oc_effect(y[keep], tr[keep], strata = if (is.null(strata)) NULL else strata[keep], n_perm = 2000L, seed = 42L)
      sdy <- stats::sd(y[keep])
      rows[[length(rows) + 1]] <- data.frame(dataset = id, model = mk, arm = info$arm, construct = info$construct,
        role = if (info$construct %in% L$targets) "target" else "placebo", contrast = cn$name,
        primary = info$construct %in% cn$primary,
        expected_sign = expected_sign(L$card, info$construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L),
        ef, mean_control = mean(y[keep][tr[keep] == 0]), sd = sdy, est_sd = ef$estimate / sdy, se_sd = ef$se / sdy,
        coverage = coverage, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}
same_as_stored <- function(new) {   # stored rows: effects_a1.csv, stmcov in effects_stmcov.csv
  old <- rbind(x, xc)[match(key(new), key(rbind(x, xc))), ]
  ok <- !is.na(old$estimate)
  c(matched = sum(ok), of = nrow(new), max_diff = max(abs(c(new$estimate - old$estimate, new$se - old$se, new$p_ri - old$p_ri))[c(ok, ok, ok)]))
}

# --- A. nwo26_exp3_comprehension: registered country-target contrasts ---------------------
id <- "nwo26_exp3_comprehension"
L <- load_study(id, "rv_32: registered country-target contrasts (post hoc, 24 Nov 2026)")
v <- paste(L$e$country, L$e$cn_variant)
reg <- list(
  ctr("kr_v2_vs_v1_v3", arm_vs(v, "KR v2", c("KR v1", "KR v3")), "kr_indigenous_democracy", "registered: KR v2 vs v1 and v3"),
  ctr("de_v3_vs_v1_v2", arm_vs(v, "DE v3", c("DE v1", "DE v2")), "de_new_we", "registered: DE v3 vs v1 and v2"),
  ctr("us_v3_vs_v1_v2_v4", arm_vs(v, "US v3", c("US v1", "US v2", "US v4")), "us_unfinished_nation", "registered: US v3 vs v1, v2 and v4"))
cat("A. check: the coded charter contrast re-estimated here vs stored (matched, of, max |diff|):",
    same_as_stored(effects_on(id, L, L$e, contrasts_for(id, L$e, L$targets))), "\n")
A <- effects_on(id, L, L$e, reg)
write.csv(A, file.path(TAB, "registered_targets_nwo26.csv"), row.names = FALSE)
Ap <- A[A$primary & A$arm %in% c("A1_presence", "C_rating", "D_topic"), ]
Ap$detected <- detected(Ap)
print(Ap[, c("model", "arm", "construct", "estimate", "se", "p_ri", "n1", "n0", "detected")], digits = 3, row.names = FALSE)

# --- B. detection rates under alternative contrast sets ------------------------------------
MEAS <- list(ref_binary = c("qwen36_35b", "A1_presence"), ref_rating = c("qwen36_35b", "C_rating"),
             stm = c("stm", "D_topic"), stmcov = c("stmcov", "D_topic"), gemma4_e4b = c("gemma4_e4b", "A1_presence"))
pool <- rbind(x, xc, A)
ckey <- function(z) paste(z$dataset, z$construct, z$contrast)
base <- x[x$model == "qwen36_35b" & x$arm == "A1_presence" & x$primary & x$role == "target", c("dataset", "construct", "contrast", "n1", "n0")]
added <- rbind(unique(A[A$primary, c("dataset", "construct", "contrast")]),
  data.frame(dataset = c("schnabel2015_service", "schnabel2015_service", "chu2020_collegerank"),
             construct = c("religious_liberty", "owner_autonomy", "cost_affordability"),
             contrast = c("religious_vs_nonreligious", "self_employed_vs_chain", "top25_in_pair")))
secondary <- data.frame(dataset = c("cohen2019_collegemajor", rep("dill2022_kettlesofhawks", 2), "musgrave2026_sanctionswtp",
    rep("nomikos2019_audiencecosts", 2), rep("sagan2024_atomicarguments", 2), rep("scaduto2023_foodpolitics", 2),
    "scherer2015_screening", "ziegler2022_kane", "aks21_c3_unification"),
  contrast = c("no_debt_vs_debt", "target_civ_vs_eff9090", "eff9045_vs_eff9090", "efficacy_high_vs_low",
    "back_down_vs_engage_fail", "engage_fail_vs_stay_out", "precedent_vs_baseline", "effect_vs_baseline",
    "left_coded_vs_base", "right_coded_vs_base", "harms_vs_no_harms", "disunited_vs_control", "authoritarian_in_pair"))
is_secondary <- paste(base$dataset, base$contrast) %in% paste(secondary$dataset, secondary$contrast)
post_selected <- base$dataset %in% c("nyhan2024_targetedcorrections_seats2020", "nyhan2024_targetedcorrections_seats2022",
                                     "scaduto2023_foodpolitics", "aarslew2026_electoralintegrity", "sydnor2016_video_whynot")
small_bisgaard <- base$dataset %in% c("bisgaard2019_study3", "bisgaard2019_study4") &
  base$contrast == "pos_vs_neg_gov_supporters" & base$n1 + base$n0 < 300
stopifnot(nrow(base) == 205, sum(is_secondary) == 13, nrow(added) == 6, !any(ckey(added) %in% ckey(base)))
cat("B. removed: set 4", sum(post_selected), "rows; set 5", sum(small_bisgaard), "rows (",
    paste(unique(paste(base$dataset, base$n1 + base$n0)[small_bisgaard]), collapse = "; "), ")\n")
b0 <- base[, 1:3]
SETS <- list(
  "0_all_primary" = b0,
  "1_without_secondary_or_unnamed" = b0[!is_secondary, ],
  "2_plus_registered_directional" = rbind(b0, added),
  "3_both" = rbind(b0[!is_secondary, ], added),
  "4_without_post_treatment_selected_items" = b0[!post_selected, ],
  "5_without_small_bisgaard_gov_subgroups" = b0[!small_bisgaard, ])
B <- do.call(rbind, lapply(names(SETS), function(sn) do.call(rbind, lapply(names(MEAS), function(mn) {
  z <- pool[pool$model == MEAS[[mn]][1] & pool$arm == MEAS[[mn]][2], ]
  z <- z[match(ckey(SETS[[sn]]), ckey(z)), ]   # an unmatched topic-model contrast counts as undetected
  data.frame(set = sn, measure = mn, n_contrasts = nrow(z), n_matched = sum(!is.na(z$estimate)),
             n_detected = sum(detected(z)), rate = sum(detected(z)) / nrow(z))
}))))
write.csv(B, file.path(TAB, "contrast_sets.csv"), row.names = FALSE)
print(B, digits = 3, row.names = FALSE)

# --- C. chopra2024_newsdemand without the flagged rows -------------------------------------
id <- "chopra2024_newsdemand"
L <- load_study(id, "rv_32: sensitivity without exclude = 'True' rows (post hoc, 24 Nov 2026)")
flag <- function(z) tolower(as.character(z$exclude)) == "true"
in_D <- L$d$id %in% L$s$id[L$s$split == L$lock$split_k & L$s$fold == "D"]   # split manifest only
fl_D <- sum(flag(L$d)[in_D]); fl_E <- sum(flag(L$e))
run <- function(e) effects_on(id, L, e, Filter(function(cn) length(cn$primary) > 0, contrasts_for(id, e, L$targets)),
                              models = c("qwen36_35b", "stm", "stmcov"), arms = c("A1_presence", "D_topic"))
w <- run(L$e); wo <- run(L$e[!flag(L$e), ])
w <- w[w$primary, ]; wo <- wo[match(key(w), key(wo)), ]
cat("C. check: full-sample estimates vs stored (matched, of, max |diff|):", same_as_stored(w), "\n")
C <- data.frame(w[, c("dataset", "model", "arm", "construct", "contrast", "expected_sign")],
  estimate_all = w$estimate, se_all = w$se, p_ri_all = w$p_ri, n1_all = w$n1, n0_all = w$n0,
  estimate_excl = wo$estimate, se_excl = wo$se, p_ri_excl = wo$p_ri, n1_excl = wo$n1, n0_excl = wo$n0,
  flagged_discovery = fl_D, flagged_estimation = fl_E)
write.csv(C, file.path(TAB, "chopra_sensitivity.csv"), row.names = FALSE)
print(C[, -1], digits = 3, row.names = FALSE)
