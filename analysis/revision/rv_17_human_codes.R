# ==============================================================================
# rv_17_human_codes.R -- the original authors' hand codes against the frozen
# yes/no labels
# ==============================================================================
# Thirty-two public archives carry hand codes of the open answers (columns
# human_* in the harmonized files). For 28 of them one code matches a target
# construct of the study card. No answer is coded anew here: the script compares
# the reference model's frozen yes/no label with the code the original team
# assigned, on the estimation fold.
#   agreement  Cohen's kappa, accuracy and AUC per (study, target); kappa between
#              two human coders where both are in the archive
#   effects    the primary contrast estimated on the human code and on the
#              model label, same answers, same estimator
# The topic-model arm (data/results/a1/<id>/stm/measures.csv) is compared on
# the answers where hand code, yes/no label and topic share are all present:
# stm__ gives answers with no word of the discovery vocabulary the topic's
# discovery mean, stmne__ leaves them missing (sensitivity). Where a target has
# no matched topic, the effect columns *_t repeat the two-measure comparison and
# the topic model counts as not recovered.
# The recode rules below were written from the cards' notes and from the code
# values in the discovery folds. Not used: janssen2026_england / _us (100 coded
# answers, treated respondents who chose one option), graeber2024_storiesmemory
# and ziegler2022_kane (no code for the target construct).
# Writes tables/human_codes_agreement.csv, tables/human_codes_effects.csv and
# paper/revision/si_tables/human_{agreement,effects}_rows.tex.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_17_human_codes.R
# ==============================================================================

source("analysis/revision/rv_config.R")
source("analysis/revision/rv_treatments.R")
src06 <- readLines("analysis/revision/rv_06_effects.R")
eval(parse(text = src06[grep("^contrasts_for <-", src06):(grep("^args <-", src06) - 1)])) # contrasts, signs
REF <- "qwen36_35b"
# STM_ARM=stmcov (added 22 Nov 2026): compare the STM covariate variant instead of
# the original topic arm; writes *_stmcov.csv only and leaves the SI tables alone.
STM_ARM <- Sys.getenv("STM_ARM", "stm")
TEX <- "paper/revision/si_tables"

num <- function(x) suppressWarnings(as.numeric(as.character(x)))
is1 <- function(col) function(d) as.integer(num(d[[col]]) == 1)
among <- function(col, values) function(d) ifelse(is.na(d[[col]]) | d[[col]] == "", NA_integer_, as.integer(as.character(d[[col]]) %in% values))
any1 <- function(cols) function(d) {
  m <- sapply(cols, function(c) num(d[[c]]))
  ifelse(rowSums(!is.na(m)) == 0, NA_integer_, as.integer(rowSums(m == 1, na.rm = TRUE) > 0))
}
has <- function(col, pattern) function(d) ifelse(is.na(d[[col]]) | d[[col]] == "", NA_integer_, as.integer(grepl(pattern, d[[col]])))
anycol <- function(cols, values) function(d) {
  m <- sapply(cols, function(c) as.character(d[[c]]))
  ifelse(rowSums(!is.na(m) & m != "") == 0, NA_integer_, as.integer(rowSums(matrix(m %in% values, nrow(d))) > 0))
}
M <- function(id, construct, h, h2 = NULL, coder = "researchers", match = "same construct") list(
  id = id, construct = construct, h = h, h2 = h2, coder = coder, match = match)

MAP <- list(
  M("andre2022_primemech", "firm_cost_channel", is1("human_int_costs")),
  M("andre2022_primemech", "demand_channel", is1("human_int_demand")),
  M("andre2025_macronarratives_mp", "monetary_policy_cause", is1("human_dagcode_monpol")),
  M("andre2025_macronarratives_mp", "energy_crisis_cause", is1("human_dagcode_energy")),
  M("andre2025_macronarratives_provision", "pent_up_demand_cause", is1("human_dagcode_pudem")),
  M("andre2025_macronarratives_provision", "energy_crisis_cause", is1("human_dagcode_energy")),
  M("andre2025_shallowmeritocracy_attention", "rate_shapes_effort", is1("human_endogeneity"), match = "closest code"),
  M("andre2025_shallowmeritocracy_cf", "disadvantage_shaped_effort", any1(c("human_endogeneity", "human_counterfactual")), match = "closest code"),
  M("bisgaard2019_study1", "incumbent_responsibility", among("human_c1raw", c("2", "3")), among("human_c2raw", c("2", "3"))),
  M("bisgaard2019_study1", "other_actor_responsibility", among("human_c1raw", c("1", "2")), among("human_c2raw", c("1", "2"))),
  M("bisgaard2019_study3", "incumbent_responsibility", among("human_c1raw", c("2", "3")), among("human_c2raw", c("2", "3"))),
  M("bisgaard2019_study3", "other_actor_responsibility", among("human_c1raw", c("1", "2")), among("human_c2raw", c("1", "2"))),
  M("bisgaard2019_study4", "incumbent_responsibility", among("human_c1raw", c("2", "3")), among("human_c2raw", c("2", "3"))),
  M("bisgaard2019_study4", "other_actor_responsibility", among("human_c1raw", c("1", "2")), among("human_c2raw", c("1", "2"))),
  M("bonilla2024_sourcecues_s3", "rejects_as_racist", among("human_c1_code2", "-2"), among("human_c2_code2", "-2"), match = "closest code"),
  M("bonilla2024_sourcecues_s4", "rejects_as_bigoted", among("human_c1_code2", "-2"), among("human_c2_code2", "-2"), match = "closest code"),
  M("bursztyn2023_dissent_aux5", "social_cover", has("human_hand_coding", "rationale")),
  M("busby2019_study1", "conspiring_elite", is1("human_badelite_r"), is1("human_badelite_n")),
  M("busby2019_study1", "good_people", is1("human_goodpeople_r"), is1("human_goodpeople_n")),
  M("busby2019_study2", "conspiring_elite", is1("human_badelite_r"), is1("human_badelite_n")),
  M("busby2019_study2", "good_people", is1("human_goodpeople_r"), is1("human_goodpeople_n")),
  M("chopra2024_newsdemand", "outlet_bias_complaint", among("human_bias_motive_class", "bias")),
  M("chopra2024_newsdemand_exp3", "political_bias_attribution", is1("human_bias")),
  M("chopra2024_newsdemand_exp3", "balanced_reporting_attribution", is1("human_balanced")),
  M("davies2013_audiencecosts", "inconsistency_credibility", among("human_basic_code", "Shouldn't have been inconsistent")),
  M("glazier2021_selfcoding", "societal_attribution", has("human_GEDO", "2"), coder = "respondents"),
  M("glazier2021_selfcoding", "individual_attribution", has("human_GEDO", "1"), coder = "respondents"),
  M("hartnett2022_whytrump", "fraud_illegitimacy", anycol(c("human_tres11", "human_tres22", "human_tres33"), c("Voter Fraud", "Distrust Election"))),
  M("kwon2025_productjourney", "partner_valuation_clarity", is1("human_mr_main")),
  M("landrum2025_mechexplain", "noticing_logical_flaw", is1("human_notice")),
  M("pogacar2023_inactioninertia", "against_current_deal", is1("human_self_any_against"), coder = "respondents"),
  M("roos2024_subjectivity", "subjective_opinion_marking", is1("human_coder1"), is1("human_coder2")),
  M("sagan2024_atomicarguments", "law_reference", is1("human_law")),
  M("sagan2024_atomicarguments", "precedent_reference", is1("human_precedent")),
  M("sagan2024_atomicarguments", "effectiveness_reference", is1("human_effectiveness")),
  M("sperber2024_zambia", "religious_identity", is1("human_religion")),
  M("thomas2022_ubireframing_e1", "freedom_mention", is1("human_freedom")),
  M("thomas2022_ubireframing_e1", "security_mention", is1("human_security")),
  M("thomas2022_ubireframing_e2", "freedom_mention", is1("human_freedom")),
  M("tripp2026_refugees", "involuntary_humanitarian", any1(c("human_Push_any", "human_No_choice_any", "human_In_need_any")), match = "several codes combined"),
  M("tripp2026_refugees", "threat_fairness_politics", any1(c("human_Threatening_any", "human_Politics_any", "human_Crime_ambiguous_any")), match = "several codes combined"),
  M("ward2026_localcandidates", "neighborhood_attachment", among("human_code", c("PI", "MX")), match = "closest code")
)

kappa2 <- function(a, b) {
  ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]
  if (length(a) < 20) return(NA_real_)
  po <- mean(a == b); pe <- mean(a) * mean(b) + (1 - mean(a)) * (1 - mean(b))
  if (pe >= 1) NA_real_ else (po - pe) / (1 - pe)
}
auc <- function(score, y) {
  ok <- !is.na(score) & !is.na(y); score <- score[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(rank(score)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

cards <- registered_cards()
agree <- list(); eff <- list()
for (id in unique(vapply(MAP, `[[`, "", "id"))) {
  card <- oc_study_read(cards[[id]])
  targets <- vapply(card$constructs$target, `[[`, "", "name")
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  e <- oc_estimation_data(d, s, oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json"))), log = ACCESS_LOG,
                          purpose = "human-code comparison")
  yn <- read.csv(file.path(RESULTS, "a1", id, REF, "measures.csv"))
  stm_f <- file.path(RESULTS, "a1", id, STM_ARM, "measures.csv")
  tm <- if (file.exists(stm_f)) read.csv(stm_f, colClasses = c(id = "character")) else NULL
  strata_all <- effect_strata(id, e)
  cns <- contrasts_for(id, e, targets)
  for (m in Filter(function(x) x$id == id, MAP)) {
    h <- m$h(e)
    y <- yn[[paste0("yes__", m$construct)]][match(e$id, yn$id)]
    py <- yn[[paste0("pyes__", m$construct)]][match(e$id, yn$id)]
    topic <- lapply(c(stm = "stm", stmne = "stmne"), function(pre) {
      v <- tm[[paste0(pre, "__", m$construct)]]
      if (is.null(v)) NULL else v[match(e$id, tm$id)]
    })
    ok <- !is.na(h) & !is.na(y)
    topic_agree <- do.call(cbind, lapply(names(topic), function(v) {
      sc <- topic[[v]]
      k <- if (is.null(sc)) rep(FALSE, length(h)) else ok & !is.na(py) & !is.na(sc)
      r <- data.frame(n = sum(k), auc_pyes = NA_real_, auc = NA_real_, r_yes = NA_real_, r_pyes = NA_real_, r = NA_real_)
      if (any(k)) r[1, -1] <- c(auc(py[k], h[k]), auc(sc[k], h[k]), suppressWarnings(c(cor(h[k], y[k]), cor(h[k], py[k]), cor(h[k], sc[k]))))
      setNames(r, paste0(names(r), "_", v))
    }))
    agree[[length(agree) + 1]] <- cbind(data.frame(
      dataset = id, construct = m$construct, coder = m$coder, match = m$match, n = sum(ok), n_E = nrow(e),
      share_human = mean(h[ok]), share_model = mean(y[ok]), kappa = kappa2(h, y), accuracy = mean(h[ok] == y[ok]),
      auc = auc(py, h), kappa_humans = if (is.null(m$h2)) NA_real_ else kappa2(h, m$h2(e)), stringsAsFactors = FALSE),
      has_topic = !is.null(topic$stm), topic_agree)
    for (cn in cns) {
      if (!m$construct %in% cn$primary) next
      keep <- ok & !is.na(cn$treat)
      if (sum(cn$treat[keep] == 1) < 20 || sum(cn$treat[keep] == 0) < 20) next
      st <- if (is.null(strata_all)) NULL else strata_all[keep]
      eh <- oc_effect(h[keep], cn$treat[keep], strata = st, n_perm = 2000L, seed = 42L)
      em <- oc_effect(y[keep], cn$treat[keep], strata = st, n_perm = 2000L, seed = 42L)
      sgn <- expected_sign(card, m$construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L)
      # hand code, label and topic share on the rows where all three are present
      three <- do.call(cbind, lapply(names(topic), function(v) {
        sc <- topic[[v]]
        k3 <- if (is.null(sc)) keep else keep & !is.na(sc)
        if (!is.null(sc) && (sum(cn$treat[k3] == 1) < 20 || sum(cn$treat[k3] == 0) < 20)) { sc <- NULL; k3 <- keep }
        same <- identical(k3, keep)
        st3 <- if (is.null(strata_all)) NULL else strata_all[k3]
        eh3 <- if (same) eh else oc_effect(h[k3], cn$treat[k3], strata = st3, n_perm = 2000L, seed = 42L)
        em3 <- if (same) em else oc_effect(y[k3], cn$treat[k3], strata = st3, n_perm = 2000L, seed = 42L)
        et <- if (is.null(sc)) list(estimate = NA_real_, p_ri = NA_real_)
              else oc_effect(sc[k3], cn$treat[k3], strata = st3, n_perm = 2000L, seed = 42L)
        sdt <- if (is.null(sc)) NA_real_ else stats::sd(sc[k3])
        sfx <- if (v == "stm") "_t" else "_ne"
        r <- data.frame(has = !is.null(sc), n1 = eh3$n1, n0 = eh3$n0, est_human = eh3$estimate, p_human = eh3$p_ri,
                        est_model = em3$estimate, p_model = em3$p_ri)
        names(r) <- c(paste0("has_", v), paste0(names(r)[-1], sfx))
        r[[paste0("est_", v)]] <- et$estimate; r[[paste0("p_", v)]] <- et$p_ri
        r[[paste0("sd_", v)]] <- sdt; r[[paste0("est_", v, "_sd")]] <- et$estimate / sdt
        r
      }))
      eff[[length(eff) + 1]] <- cbind(data.frame(
        dataset = id, construct = m$construct, contrast = cn$name, coder = m$coder, match = m$match, expected_sign = sgn,
        n1 = eh$n1, n0 = eh$n0, est_human = eh$estimate, p_human = eh$p_ri, est_model = em$estimate, p_model = em$p_ri,
        stringsAsFactors = FALSE), three)
    }
  }
}
agree <- do.call(rbind, agree); eff <- do.call(rbind, eff)
rec <- function(est, p) !is.na(p) & p < 0.05 & sign(est) == eff$expected_sign
eff$rec_human <- eff$p_human < 0.05 & sign(eff$est_human) == eff$expected_sign
eff$rec_model <- eff$p_model < 0.05 & sign(eff$est_model) == eff$expected_sign
for (sfx in c("_t", "_ne")) for (v in c("human", "model")) eff[[paste0("rec_", v, sfx)]] <- rec(eff[[paste0("est_", v, sfx)]], eff[[paste0("p_", v, sfx)]])
eff$rec_stm <- rec(eff$est_stm, eff$p_stm)
eff$rec_stmne <- rec(eff$est_stmne, eff$p_stmne)
sfx <- if (STM_ARM == "stm") "" else paste0("_", STM_ARM)
write.csv(agree, file.path(OUT, "tables", paste0("human_codes_agreement", sfx, ".csv")), row.names = FALSE)
write.csv(eff, file.path(OUT, "tables", paste0("human_codes_effects", sfx, ".csv")), row.names = FALSE)
if (STM_ARM == "stm") {

esc <- function(x) gsub("_", "\\_\\allowbreak{}", x, fixed = TRUE)
star <- function(est, p) ifelse(is.na(est), "--", sprintf("%s%s", fmt_num(est), ifelse(!is.na(p) & p < 0.05, "*", "")))
writeLines(sprintf("\\texttt{%s} & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\", esc(agree$dataset), plain_id(agree$construct),
                   ifelse(agree$coder == "respondents", "R", ifelse(agree$match == "same construct", "A", "A$^\\dagger$")),
                   format(agree$n, big.mark = ","), fmt_num(agree$share_human), fmt_num(agree$share_model), fmt_num(agree$kappa),
                   fmt_num(agree$auc), fmt_num(agree$auc_stm), fmt_num(agree$kappa_humans)), file.path(TEX, "human_agreement_rows.tex"))
writeLines(sprintf("\\texttt{%s} & %s & %s & %s & %s & %s & %s \\\\", esc(eff$dataset), plain_id(eff$construct), plain_id(eff$contrast),
                   ifelse(eff$expected_sign > 0, "+", "$-$"), star(eff$est_human, eff$p_human), star(eff$est_model, eff$p_model),
                   star(eff$est_stm_sd, eff$p_stm)),
           file.path(TEX, "human_effects_rows.tex"))
}

print(agree[, c("dataset", "construct", "coder", "match", "n", "share_human", "share_model", "kappa", "auc", "kappa_humans")], row.names = FALSE, digits = 2)
print(eff[, c("dataset", "construct", "contrast", "expected_sign", "n1", "n0", "est_human", "p_human", "est_model", "p_model")], row.names = FALSE, digits = 2)
res <- agree[agree$coder == "researchers", ]
cat(sprintf("\nagreement: %d targets in %d studies (%d coded by researchers); kappa median %.2f, IQR %.2f-%.2f (researchers: %.2f; same construct only: %.2f, n = %d); AUC median %.2f\n",
            nrow(agree), length(unique(agree$dataset)), nrow(res), median(agree$kappa, na.rm = TRUE),
            quantile(agree$kappa, .25, na.rm = TRUE), quantile(agree$kappa, .75, na.rm = TRUE), median(res$kappa, na.rm = TRUE),
            median(res$kappa[res$match == "same construct"], na.rm = TRUE), sum(res$match == "same construct"), median(agree$auc, na.rm = TRUE)))
two <- agree[!is.na(agree$kappa_humans), ]
cat(sprintf("two human coders in the archive: %d targets; human-human kappa median %.2f; model-human kappa on the same targets median %.2f\n",
            nrow(two), median(two$kappa_humans), median(two$kappa, na.rm = TRUE)))
cat(sprintf("effects: %d primary contrasts in %d studies; recovered with the human code %d, with the model label %d, with both %d, with neither %d; same sign %d; r between estimates %.2f\n",
            nrow(eff), length(unique(eff$dataset)), sum(eff$rec_human), sum(eff$rec_model), sum(eff$rec_human & eff$rec_model),
            sum(!eff$rec_human & !eff$rec_model), sum(sign(eff$est_human) == sign(eff$est_model)), cor(eff$est_human, eff$est_model)))

# Topic model against the hand codes. AUC and correlations on the answers where
# hand code, label, probability of yes and topic share are all present; effects
# on the rows where hand code, label and topic share are all present.
topic_summary <- function(v) {
  a <- agree[agree[[paste0("n_", v)]] > 0, ]
  sfx <- if (v == "stm") "_t" else "_ne"
  x <- eff[eff[[paste0("has_", v)]], ]
  rc <- function(d, w) sum(d[[paste0("rec_", w)]])
  line <- function(d, lab) sprintf("%s: %d contrasts in %d studies; recovered with the hand code %d, the model label %d, the topic share %d; topic and hand code both %d; topic share with the hand code's sign %d",
    lab, nrow(d), length(unique(d$dataset)), rc(d, paste0("human", sfx)), rc(d, paste0("model", sfx)), rc(d, v),
    sum(d[[paste0("rec_", v)]] & d[[paste0("rec_human", sfx)]]), sum(sign(d[[paste0("est_", v)]]) == sign(d[[paste0("est_human", sfx)]]), na.rm = TRUE))
  c(sprintf("%s (%s): %d of %d targets in %d studies have a matched topic; on their answers (median %d per target): median AUC, probability of yes %.2f, topic share %.2f; median r with the hand code: yes/no label %.2f, probability of yes %.2f, topic share %.2f",
            v, if (v == "stm") "answers without a known word get the topic's discovery mean" else "answers without a known word left out",
            nrow(a), nrow(agree), length(unique(a$dataset)), as.integer(median(a[[paste0("n_", v)]])),
            median(a[[paste0("auc_pyes_", v)]], na.rm = TRUE), median(a[[paste0("auc_", v)]], na.rm = TRUE),
            median(a[[paste0("r_yes_", v)]], na.rm = TRUE), median(a[[paste0("r_pyes_", v)]], na.rm = TRUE), median(a[[paste0("r_", v)]], na.rm = TRUE)),
    paste("  effects,", line(x, "contrasts with a matched topic")),
    paste("  effects,", line(eff, "all primary contrasts (topic not recovered where none is matched)")))
}
cat("\n", paste(c(topic_summary("stm"), topic_summary("stmne")), collapse = "\n"), "\n", sep = "")
