# ==============================================================================
# rv_19_closed_outcomes.R -- does the study's own closed-ended outcome move on
# the contrasts where the open text does not?
# ==============================================================================
# A primary contrast that the open text does not recover can mean three things:
# the answers do not carry the construct, the text measure is too noisy at this
# sample size, or the treatment did little in this sample to begin with. The
# third can be checked where the harmonized file carries the closed-ended
# outcome (or manipulation check) that the study itself reports. For those
# studies this script estimates the SAME contrasts, with the same estimator and
# strata, on that closed-ended variable:
#   all  every respondent with a non-empty answer (discovery and estimation
#        folds together; no text is read and no measure depends on this)
#   E    the estimation fold only, the sample the text estimate uses
# The test is two-sided: the card records the expected direction of the
# construct in the text, not of the closed-ended item.
# The outcome of each study is the first closed-ended outcome named in its
# study card (outcomes: closed_ended) that the harmonized file carries. Studies
# whose file carries none are left out and counted. Conjoint studies in which
# the answer explains a profile choice are left out: the choice is the event
# the contrast conditions on, so it has no effect to estimate on these rows.
# Writes tables/closed_outcomes.csv and si_tables/closed_outcome_rows.tex.
# Run: LANG=en_US.UTF-8 OMP_NUM_THREADS=1 Rscript analysis/revision/rv_19_closed_outcomes.R
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")
suppressMessages(library(parallel))
src06 <- readLines("analysis/revision/rv_06_effects.R")
eval(parse(text = src06[grep("^contrasts_for <-", src06):(grep("^args <-", src06) - 1)]))
REF <- "qwen36_35b"
TEX <- "paper/revision/si_tables"

O <- function(col, kind = "outcome", recode = NULL) list(col = col, kind = kind, recode = recode)
MAP <- list(
  aarslew2026_electoralintegrity = list(O("pei", "manipulation check")),
  andre2022_primemech = list(O("pi_delta")),
  andre2024_stockmodels_arbitrage = list(O("news_congruent")),
  andre2024_stockmodels_arbitrage_w2 = list(O("news_congruent")),
  andre2024_stockmodels_attention = list(O("news_congruent")),
  andre2024_stockmodels_memory = list(O("news_congruent")),
  andre2024_stockmodels_pricecosts = list(O("news_congruent")),
  andre2025_macronarratives_mp = list(O("inflation_12_w2")),
  andre2025_macronarratives_provision = list(O("inflation_12_w2")),
  bolton2026_calves = list(O("animal_attitude")),
  bonilla2024_sourcecues_s3 = list(O("s1_agree")),
  bonilla2024_sourcecues_s4 = list(O("s2_agree")),
  braman2016_legitimacy = list(O("legitimacy")),
  broockman2026_housing = list(O("streamlining_dv_01")),
  bursztyn2023_dissent_defund = list(O("believe_donated")),
  calarco2019_vignette = list(O("approve")),
  clayton2023_backsliding = list(O("threat")),
  corstange2016_refugees = list(O("refugees")),
  darr2019_trumprussia = list(O("consequences_overall")),
  dill2022_kettlesofhawks = list(O("strike_pref")),
  ishizuka2015_parenting = list(O("rating")),
  johnson2021_algofair = list(O("chose", recode = c(algorithm = 1, status_quo = 0))),
  kertzer2024_perspectivetaking = list(O("policy_choice")),
  koblitz2012_mood = list(O("intend_decrease_q8")),
  krishnarajan2022_rationalizing = list(O("dem_rating")),
  kwon2025_productjourney = list(O("dv_joint")),
  leipziger2026_groupinequality = list(O("effort_luck")),
  levine2009_donate = list(O("amount_donated"), O("sign_up")),
  lupu2024_lawsofwar_gov = list(O("app_gov5")),
  melin2019_pay = list(O("salary")),
  mezzapelle2021_harassment = list(O("q3_believable")),
  miller2016_humanfinancialcosts = list(O("support")),
  mills2018_labtransparency = list(O("rating_honest")),
  muradova2022_empathy = list(O("post_attitude_ubi")),
  musgrave2026_sanctionswtp = list(O("sanctions_support")),
  nomikos2019_audiencecosts = list(O("approval")),
  nwo26_exp2 = list(O("task5_support")),
  nyhan2024_targetedcorrections_seats2020 = list(O("closed_post")),
  nyhan2024_targetedcorrections_seats2022 = list(O("closed_post")),
  pepin2016_couples = list(O("fairness")),
  pepin2016_couples_leisure = list(O("fairness")),
  peyton2022_imaginedcontact = list(O("imagined_dv")),
  pogacar2023_inactioninertia = list(O("choice")),
  porumbescu2023_blame = list(O("performance_trump")),
  quadlin2021_audit = list(O("interview_rec")),
  roos2024_subjectivity = list(O("comment_chance")),
  sagan2024_atomicarguments = list(O("prefer_strike_bin")),
  scaduto2023_foodpolitics = list(O("exp_ideology")),
  scherer2015_screening = list(O("want_test")),
  schnabel2015_service = list(O("allow_refuse")),
  sirovica2022_cowcalf = list(O("attitude")),
  spruill2022_legalreasonable_s1 = list(O("warmth")),
  spruill2022_legalreasonable_s2 = list(O("warmth")),
  sukin2024_patronreassurance = list(O("support")),
  sydnor2016_video = list(O("share")),
  sydnor2016_video_whynot = list(O("clip_civil", "manipulation check")),
  thomas2022_ubireframing_e1 = list(O("support")),
  thomas2022_ubireframing_e2 = list(O("support")),
  turnbulldugarte2022_surrogacy = list(O("support")),
  vogler2019_precarious = list(O("rating_heterosexual")),
  ward2026_localcandidates = list(O("manipulation_check", "manipulation check")),
  weisshaar2019_applicant = list(O("hire")),
  weston2021_elephant = list(O("support_important_have")),
  williamson2017_taxes = list(O("food_stamps_spending", recode = c(decrease = -1, same = 0, increase = 1))),
  yeung2022_ubiframing = list(O("support")),
  zarpli2023_turkeysanctions = list(O("policy_change"))
)
cards <- registered_cards()
stopifnot(all(names(MAP) %in% names(cards)))

one <- function(id) {
  card <- oc_study_read(cards[[id]])
  targets <- vapply(card$constructs$target, `[[`, "", "name")
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  lock <- oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json")))
  e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "closed-ended outcome on E; no text read")
  out <- list()
  for (fold in c("all", "E")) {
    x <- if (fold == "all") d else e
    st <- effect_strata(id, x)
    for (cn in contrasts_for(id, x, targets)) for (o in MAP[[id]]) {
      if (!o$col %in% names(x)) stop("column ", o$col, " not in the file")
      y <- x[[o$col]]
      if (!is.null(o$recode)) y <- unname(o$recode[as.character(y)])
      y <- suppressWarnings(as.numeric(y))
      keep <- !is.na(cn$treat) & !is.na(y)
      if (sum(cn$treat[keep] == 1) < 20 || sum(cn$treat[keep] == 0) < 20 || stats::sd(y[keep]) == 0) next
      ef <- oc_effect(y[keep], cn$treat[keep], strata = if (is.null(st)) NULL else st[keep], n_perm = 2000L, seed = 42L)
      out[[length(out) + 1]] <- data.frame(
        dataset = id, contrast = cn$name, fold = fold, outcome = o$col, kind = o$kind, estimate = ef$estimate,
        se = ef$se, p_ri = ef$p_ri, n1 = ef$n1, n0 = ef$n0, est_sd = ef$estimate / stats::sd(y[keep]), stringsAsFactors = FALSE)
      break   # the first outcome in the list that this contrast can be estimated on
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}

res <- mclapply(names(MAP), function(id) tryCatch(one(id), error = function(err) structure(conditionMessage(err), class = "try-error")),
                mc.cores = min(12L, detectCores()), mc.preschedule = FALSE)
bad <- vapply(res, inherits, NA, "try-error")
if (any(bad)) stop("Failed: ", paste(sprintf("%s (%s)", names(MAP)[bad], unlist(res[bad])), collapse = "; "))
cl <- do.call(rbind, res)
cl$sig <- cl$p_ri < 0.05

# the text result for the same contrasts: yes/no question, reference model
a1 <- read.csv(file.path(OUT, "effects_a1.csv"), stringsAsFactors = FALSE)
p <- a1[a1$primary & a1$role == "target" & a1$model == REF & a1$arm == "A1_presence", ]
p$rec <- p$p_ri < 0.05 & sign(p$estimate) == p$expected_sign
p$right_sign <- sign(p$estimate) == p$expected_sign
wide <- reshape(cl[, c("dataset", "contrast", "fold", "outcome", "kind", "est_sd", "p_ri", "sig", "n1", "n0")],
                idvar = c("dataset", "contrast", "outcome", "kind"), timevar = "fold", direction = "wide", sep = "_")
m <- merge(p[, c("dataset", "construct", "contrast", "estimate", "est_sd", "p_ri", "rec", "right_sign", "n1", "n0")], wide,
           by = c("dataset", "contrast"), all.x = TRUE, suffixes = c("_text", ""))
m$has_closed <- !is.na(m$sig_all)
write.csv(m, file.path(OUT, "tables", "closed_outcomes.csv"), row.names = FALSE)

k <- m[m$has_closed, ]
cat(sprintf("primary contrasts: %d in %d studies; with a closed-ended variable in the file: %d in %d studies (%d on a manipulation check)\n",
            nrow(m), length(unique(m$dataset)), nrow(k), length(unique(k$dataset)), sum(k$kind == "manipulation check")))
cat(sprintf("text recovery on these %d contrasts: %.3f; on the %d without: %.3f\n", nrow(k), mean(k$rec), sum(!m$has_closed), mean(m$rec[!m$has_closed])))
tab <- function(sig, lab) {
  cat(sprintf("\n%s\n", lab))
  print(table(text = ifelse(k$rec, "recovered", "missed"), closed = ifelse(sig, "significant", "not significant")))
  cat(sprintf("  closed-ended variable significant: %.3f of all %d; %.3f of the %d recovered; %.3f of the %d missed\n",
              mean(sig), length(sig), mean(sig[k$rec]), sum(k$rec), mean(sig[!k$rec]), sum(!k$rec)))
  cat(sprintf("  text recovery where the closed-ended variable is significant: %.3f (%d of %d); where it is not: %.3f (%d of %d)\n",
              mean(k$rec[sig]), sum(k$rec[sig]), sum(sig), mean(k$rec[!sig]), sum(k$rec[!sig]), sum(!sig)))
}
tab(k$sig_all, "Closed-ended variable, all respondents with an answer")
tab(k$sig_E, "Closed-ended variable, estimation fold only")
miss <- k[!k$rec, ]
cat(sprintf("\nmissed contrasts with a closed-ended variable: %d; closed-ended variable not significant either: %d (all respondents), %d (estimation fold); of the %d missed with the expected sign in the text, closed-ended not significant: %d\n",
            nrow(miss), sum(!miss$sig_all), sum(!miss$sig_E), sum(miss$right_sign), sum(!miss$sig_all & miss$right_sign)))
cat(sprintf("median absolute standardized effect: text %.3f, closed-ended (all respondents) %.3f; closed-ended larger in %d of %d\n",
            median(abs(k$est_sd)), median(abs(k$est_sd_all)), sum(abs(k$est_sd_all) > abs(k$est_sd)), nrow(k)))
o <- k[k$kind == "outcome", ]
cat(sprintf("outcomes only (no manipulation checks): %d contrasts; closed significant %.3f; text recovery where significant %.3f (%d of %d), where not %.3f (%d of %d)\n",
            nrow(o), mean(o$sig_all), mean(o$rec[o$sig_all]), sum(o$rec[o$sig_all]), sum(o$sig_all), mean(o$rec[!o$sig_all]), sum(o$rec[!o$sig_all]), sum(!o$sig_all)))

# SI rows: one line per cell of the two-by-two table
row <- function(lab, sel) sprintf("%s & %d & %d & %d & %s \\\\", lab, sum(sel), sum(k$rec[sel]), sum(!k$rec[sel]),
                                  fmt_num(mean(k$rec[sel])))
writeLines(enc2utf8(c(
  row("Closed-ended variable differs ($p<.05$), all respondents", k$sig_all),
  row("Closed-ended variable does not differ, all respondents", !k$sig_all),
  "\\addlinespace",
  row("Closed-ended variable differs ($p<.05$), estimation fold", k$sig_E),
  row("Closed-ended variable does not differ, estimation fold", !k$sig_E),
  "\\addlinespace",
  row("All contrasts with a closed-ended variable", rep(TRUE, nrow(k))))),
  file.path(TEX, "closed_outcome_rows.tex"), useBytes = TRUE)
