# ==============================================================================
# andre2025_shallowmeritocracy.R -- harmonized files for the cards
#   andre2025_shallowmeritocracy_attention  (main study, fairness.sav)
#   andre2025_shallowmeritocracy_cf         (counterfactual study)
#   andre2025_shallowmeritocracy_cfadv      (advantaged counterfactual study)
#   andre2025_shallowmeritocracy_costs      (effort costs study)
#   andre2025_shallowmeritocracy_vignettes  (vignette study, waves 1 and 2)
# ==============================================================================
# Andre, P. (2025), "Shallow Meritocracy", Review of Economic Studies 92(2),
# 772-807; replication package Zenodo 10591832 (see MANIFEST.md).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/andre2025_shallowmeritocracy.R
#
# Inputs: data/external/andre2025_shallowmeritocracy/raw/data/*.sav and
# explanations.csv, unchanged from the package. Exclusions replicate the
# author's cleaning scripts in raw/code/data_cleaning/ (cited per study below);
# the package code itself is not run.
#
# Not built (no card; see the drafter's report): origin of circumstances and
# bonus studies (no stated expectation), leisure time study (replication of the
# main contrast; no stated expectation), disappointment study (its PAP states a
# null and it has no open item of its own: `reason` in fairness_expectations.sav
# is the vignette-study item, used here), rationale study (no randomization).
#
# Blindness: no text value is printed; only counts. Run with stderr to a file.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw <- "data/external/andre2025_shallowmeritocracy/raw/data"
out_dir <- "data/external/andre2025_shallowmeritocracy"

rd <- function(f) as.data.frame(zap_labels(read_spss(file.path(raw, f))))
nonempty <- function(x) !is.na(x) & nchar(trimws(x)) > 0
instr_pre <- function(s, pages = c("1", "2a", "2b", "3", "4", "5")) {
  # time_instr_pre_treatment (seconds on the instruction pages before the
  # treatment variation): pages 1-5 in clean_wide.R l. 78 and
  # clean_wide_counterfactual*.R; pages 1-6 in clean_wide_context.R l. 71.
  rowSums(s[, paste0("time_instr_", pages, "_Page_Submit")])
}

# ---- Human codes (main study and counterfactual study) ----------------------
# explanations.csv: the author's hand codes, ';'-separated (read.csv2), joined
# on RID = ResponseId. Recodes and dummies as in
# code/data_cleaning/clean_explanations.R, ll. 9-29 (typo fixes othr/other ->
# other, misunderstanding -> misunderstand, payment -> accept; one dummy per
# code via grepl). NA = response not coded. Covers EU, UU, UU_debias of the
# main study and UU, CL, CH of the counterfactual study (1,872 rows).
hc <- read.csv2(file.path(raw, "explanations.csv"), as.is = TRUE,
                encoding = "UTF-8")
names(hc)[1] <- "RID"                          # first name carries a UTF-8 BOM
stopifnot(!anyDuplicated(hc$RID))
codes <- c("effort", "equality", "accept", "endogeneity", "counterfactual",
           "need", "misunderstand", "other")
hc$code <- gsub("othr|other ", "other", hc$code)
hc$code <- gsub("misunderstanding", "misunderstand", hc$code)
hc$code <- gsub("payment", "accept", hc$code)
stopifnot(all(setdiff(unique(trimws(unlist(strsplit(hc$code, ",|, ")))), NA)
              %in% codes))
for (k in codes) {
  hc[[paste0("human_", k)]] <- ifelse(!is.na(hc$code), 1L * grepl(k, hc$code),
                                      NA_integer_)
}
hc_cols <- paste0("human_", codes)
add_human <- function(d) {
  m <- hc[match(d$id, hc$RID), hc_cols]
  rownames(m) <- NULL
  cbind(d, m)
}

# ---- Writer with the brief's checks ------------------------------------------
write_card <- function(d, card_id, arm_order) {
  d <- d[nonempty(d$text), ]                   # rows with empty text are left out
  d$text <- trimws(d$text)
  # An answer that is exactly "NA" is read back as missing by read.csv (its
  # default na.strings), so it cannot survive the round trip; it carries no
  # content and is treated as empty. One such answer exists (advantaged
  # counterfactual study); the count is printed below.
  n_na <- sum(d$text == "NA")
  d <- d[d$text != "NA", ]
  d$dataset <- card_id
  d$country <- "US"
  d$lang <- "en"
  first <- c("id", "dataset", "country", "lang", "text", "exclude", "treatment")
  d <- d[, c(first, setdiff(names(d), first))]
  stopifnot(!anyDuplicated(d$id), all(d$treatment %in% arm_order))
  f <- file.path(out_dir, paste0("harmonized_", card_id, ".csv"))
  write.csv(d, f, row.names = FALSE, fileEncoding = "UTF-8")
  back <- read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
  esc <- sum(lengths(regmatches(readLines(f, encoding = "UTF-8", warn = FALSE),
                                gregexpr("<U\\+", readLines(f, encoding = "UTF-8", warn = FALSE)))))
  cat("\n==", card_id, "==\n")
  cat("rows:", nrow(d), " excluded:", sum(d$exclude),
      " dropped literal 'NA' answers:", n_na, "\n")
  cat("non-excluded rows per arm:\n")
  print(table(factor(d$treatment[!d$exclude], levels = arm_order)))
  cat("re-read identical text:", identical(back$text, d$text),
      " rows re-read:", nrow(back), "\n")
  cat("<U+ escapes in file:", esc, "\n")
  stopifnot(identical(back$text, d$text), esc == 0)
  invisible(d)
}

# ==============================================================================
# 1. Main study (fairness.sav): attention card
# ==============================================================================
# Exclusions, code/data_cleaning/clean_wide.R:
#   l. 87  keep only if the last redistribution decision page was submitted
#          (time_redist8 = raw time_redist_2_last_Page_Submit after the renames
#          of ll. 36-60; the time_redist_1_* columns are dropped at l. 25);
#   l. 91  instruction time before the treatment page >= 30 s;
#   l. 96  first row per Lucid id (rid -> LID);
#   l. 99  attention-arm rows from the May 2020 pilot (date contains "-05-").
# Arms, l. 194: treatment_all = first two letters of `treatment`, plus
# "_debias" when debiasing == 1. EU = control, UU = treatment, UU_debias =
# attention condition; EE / UE / UE_debias = the "equal rates" versions.
s <- rd("fairness.sav")
s$date <- substr(as.character(s$StartDate), 1, 10)
s$pilot <- grepl("-05-", s$date)
s$debiasing <- as.character(s$debiasing)
ip <- instr_pre(s)
keep <- !is.na(s$time_redist_2_last_Page_Submit) & !is.na(ip) & ip >= 30
k_idx <- which(keep)
k_idx <- k_idx[!duplicated(s$rid[k_idx])]
k_idx <- k_idx[!(s$debiasing[k_idx] %in% "1" & s$pilot[k_idx])]
s$author_keep <- seq_len(nrow(s)) %in% k_idx
s <- s[!is.na(s$treatment) & s$treatment != "", ]   # rows with no arm have no text
short <- substr(s$treatment, 1, 2)
arm_all <- ifelse(s$debiasing %in% "1", paste0(short, "_debias"), short)
arm_map <- c(EU = "control", UU = "treatment", UU_debias = "attention",
             EE = "eqrates_control", UE = "eqrates_treatment",
             UE_debias = "eqrates_attention")
stopifnot(all(arm_all %in% names(arm_map)))
d1 <- data.frame(
  id = s$ResponseId, text = as.character(s$explanation),
  exclude = !s$author_keep, treatment = unname(arm_map[arm_all]),
  # Order of the advantaged (A) and disadvantaged (D) worker on screen
  # (suffix _AD / _DA); for EE the suffix is the realized rate (high / low).
  # Randomized with the arm (codebook.xlsx, `treatment`).
  worker_order = tolower(sub("^[A-Z]{2}_", "", s$treatment)),
  pilot = s$pilot,
  stringsAsFactors = FALSE)
d1 <- add_human(d1)
write_card(d1, "andre2025_shallowmeritocracy_attention", unname(arm_map))

# ==============================================================================
# 2-3. Counterfactual and advantaged counterfactual studies
# ==============================================================================
# Exclusions, code/data_cleaning/clean_wide_counterfactual.R ll. 86-97 and
# clean_wide_counterfactual_adv.R ll. 86-95: last decision page submitted,
# pre-treatment instruction time >= 30 s, first row per rid. No pilot rule.
# Arms (`treatment`): UU = no information, CL = low counterfactual, CH = high
# counterfactual. In the counterfactual study the information is about what
# worker B would have done at $0.50; in the advantaged study it is about what
# worker A would have done at $0.10 (CL there: A would have done as little as
# B; CH: A would not have changed), per the June 2022 PAP and the .qsf.
# The `code` column in these .sav files is a numeric completion code (51
# distinct values), not a hand code; the author drops it (l. 27 / l. 25).
cf_build <- function(file, card_id, human) {
  s <- rd(file)
  ip <- instr_pre(s)
  keep <- !is.na(s$time_redist_2_last_Page_Submit) & !is.na(ip) & ip >= 30
  k_idx <- which(keep)
  k_idx <- k_idx[!duplicated(s$rid[k_idx])]
  s$author_keep <- seq_len(nrow(s)) %in% k_idx
  s <- s[s$treatment %in% c("UU", "CL", "CH"), ]
  arm <- c(UU = "none", CL = "low", CH = "high")
  d <- data.frame(id = s$ResponseId, text = as.character(s$explanation),
                  exclude = !s$author_keep, treatment = unname(arm[s$treatment]),
                  stringsAsFactors = FALSE)
  if (human) d <- add_human(d)
  write_card(d, card_id, c("none", "low", "high"))
}
cf_build("fairness_counterfactual.sav", "andre2025_shallowmeritocracy_cf", TRUE)
cf_build("fairness_counterfactual_adv.sav", "andre2025_shallowmeritocracy_cfadv", FALSE)

# ==============================================================================
# 4. Effort costs study (context_costs.sav)
# ==============================================================================
# Exclusions, code/data_cleaning/clean_wide_context.R: the three July 2023
# files are stacked in the order origins, costs, bonus (l. 7-10, rbind.fill)
# before cleaning, so the duplicate rule (l. 88, first row per PROLIFIC_PID)
# runs across all three studies; replicated here. Also l. 80 (post-experiment
# questionnaire page submitted, time_peq) and l. 85 (instruction time on
# pages 1-6 >= 30 s).
# Arm: `condition`. context_highcosts = both workers called the task "tedious
# and tiresome"; context_lowcosts = "exciting and entertaining" (display logic
# in surveys/effort_costs_study.qsf). The 2023 PAP and Appendix C.7 print the
# two labels the other way round; the main text (Section 5.4) and the survey
# agree with the coding used here. treatment = 1 for high costs (tedious).
# cf_arm: worker B's counterfactual in scenarios 1-3 (CH / CL), randomized
# independently of `condition` (PAP 2023, Table 1).
ctx <- lapply(c("context_origins.sav", "context_costs.sav", "context_bonus.sav"),
              function(f) {
                x <- rd(f)
                data.frame(study = f, ResponseId = x$ResponseId,
                           PROLIFIC_PID = x$PROLIFIC_PID,
                           time_peq = x$time_peq_Page_Submit,
                           ip = instr_pre(x, c("1", "2a", "2b", "3", "4", "5", "6")),
                           condition = x$condition, cf = x$treatment,
                           text = as.character(x$explanation),
                           stringsAsFactors = FALSE)
              })
ctx <- do.call(rbind, ctx)
keep <- !is.na(ctx$time_peq) & !is.na(ctx$ip) & ctx$ip >= 30
k_idx <- which(keep)
k_idx <- k_idx[!duplicated(ctx$PROLIFIC_PID[k_idx])]
ctx$author_keep <- seq_len(nrow(ctx)) %in% k_idx
cc <- ctx[ctx$study == "context_costs.sav", ]
# Rows without a `condition` never reached the randomizer (no assignment).
no_arm <- !cc$condition %in% c("context_highcosts", "context_lowcosts")
cat("\ncosts study: rows without assignment", sum(no_arm), "of which with text",
    sum(no_arm & nonempty(cc$text)), "\n")
stopifnot(sum(no_arm & nonempty(cc$text)) == 0)
cc <- cc[!no_arm, ]
d4 <- data.frame(id = cc$ResponseId, text = cc$text, exclude = !cc$author_keep,
                 treatment = 1L * (cc$condition == "context_highcosts"),
                 cf_arm = tolower(cc$cf), stringsAsFactors = FALSE)
write_card(d4, "andre2025_shallowmeritocracy_costs", c(0L, 1L))

# ==============================================================================
# 5. Vignette study (fairness_expectations.sav = wave 1, fairness_vignettes.sav
#    = wave 2)
# ==============================================================================
# code/data_cleaning/clean_vignettes.R: both waves stacked, wave 1 first
# (ll. 10-15); keep if the feedback page was submitted (l. 51); first row per
# rid (l. 54). Analysis exclusion (code/data_analysis/reg_vignettes.R ll.
# 26-29; paper fn. 28): mean time on the vignettes a respondent saw >= 60 s,
# where a vignette's time is the sum of its pages 1, 2 and 4
# (clean_vignettes.R ll. 82-85, 113-116).
# Arm: `treatment2`: treatment = baseline (no counterfactual), counterfactual_
# low, counterfactual_high (BlockRandomizer, even presentation, in both .qsf).
# Wave 1 respondents first made the disappointment-study decision
# (treatment_exp, cross-randomized) and then saw 2 of 4 vignettes; wave 2
# respondents saw all 4. vig_* flag which vignettes were shown (law = the
# crime vignette, not reported in the paper).
vig <- c("hardwork", "promotion", "law", "startup")
w1 <- rd("fairness_expectations.sav"); w1$wave <- 1L
w2 <- rd("fairness_vignettes.sav");    w2$wave <- 2L
w2$treatment_exp <- NA_character_
cols <- c("ResponseId", "rid", "wave", "treatment2", "treatment_exp", "reason",
          "time_feedback_Page_Submit", paste0("redist_", vig, "_2"),
          as.vector(outer(paste0("time_", vig, "_"), paste0(c(1, 2, 4), "_Page_Submit"), paste0)))
v <- rbind(w1[, cols], w2[, cols])
keep <- !is.na(v$time_feedback_Page_Submit)
k_idx <- which(keep)
k_idx <- k_idx[!duplicated(v$rid[k_idx])]
shown <- sapply(vig, function(g) !is.na(v[[paste0("redist_", g, "_2")]]))
vt <- sapply(vig, function(g) rowSums(v[, paste0("time_", g, "_", c(1, 2, 4), "_Page_Submit")]))
vt[!shown] <- NA
time_avg <- rowMeans(vt, na.rm = TRUE)
v$author_keep <- seq_len(nrow(v)) %in% k_idx & !is.na(time_avg) & time_avg >= 60
arm <- c(treatment = "none", counterfactual_low = "low",
         counterfactual_high = "high")
v <- v[v$treatment2 %in% names(arm), ]
shown <- sapply(vig, function(g) !is.na(v[[paste0("redist_", g, "_2")]]))
d5 <- data.frame(id = v$ResponseId, text = as.character(v$reason),
                 exclude = !v$author_keep, treatment = unname(arm[v$treatment2]),
                 wave = v$wave, treatment_exp = v$treatment_exp,
                 vig_hardwork = shown[, "hardwork"], vig_promotion = shown[, "promotion"],
                 vig_law = shown[, "law"], vig_startup = shown[, "startup"],
                 stringsAsFactors = FALSE)
write_card(d5, "andre2025_shallowmeritocracy_vignettes", c("none", "low", "high"))
