# ==============================================================================
# scherer2015_screening.R -- harmonized file for card scherer2015_screening
# ==============================================================================
# TESS study scherer712 (L. Scherer), "Interest in Cancer Screening Tests That
# Lack Benefits: The Role of Cancer Worry, Perceived Cancer Risk, and Perceived
# Test Risk", fielded by GfK KnowledgePanel 12/2015-4/2016, N = 2,171.
# Reported in Scherer, Valentine, Patel, Baker & Fagerlin (2019), "A bias for
# action in cancer screening?", J. Exp. Psychol.: Applied 25(2): 149-161,
# doi:10.1037/xap0000177.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/scherer2015_screening.R
#
# Input: data/external/scherer2015_screening/raw/TESS_204_Scherer_Client.sav
# Output: data/external/scherer2015_screening/harmonized_scherer2015_screening.csv
#
# Text item: Q11 "Please explain your reason for answering the last question as
# you did. Why did you decide to get the screening test, or not get the
# screening test? List any and all reasons that were important in making your
# decision." (questionnaire TESS3_204_Scherer_QNR_V6_final.docx), directly after
# Q10 (would you get this screening test). QF1 (general comments, end of
# survey) is not used.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/scherer2015_screening/raw/TESS_204_Scherer_Client.sav"
out_csv  <- "data/external/scherer2015_screening/harmonized_scherer2015_screening.csv"
card_id  <- "scherer2015_screening"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 2171, !anyDuplicated(d$CaseID), !anyNA(d$CaseID))

# ---- Treatment ----------------------------------------------------------------
# XTESS204 (questionnaire, sample variables): 1 High Risk/Present,
# 2 High Risk/Absent, 3 Low Risk/Present, 4 Low Risk/Absent, 5 Control.
#   Risk: arms 1-2 are told their lifetime cancer risk is 20% ("significantly
#   higher" than the 13% average), arms 3-4 that it is 6% ("lower") after
#   filling in risk-factor items Q1-Q2 (Display1). The risk is fabricated
#   (debriefing). Control (5) gets no risk items and no risk feedback.
#   Harms: "Present" (1, 3) adds the list of physical, emotional and
#   financial harms of unnecessary treatment to the test description
#   (Display2a/2b, "[IF XTESS204 = 1 OR 3]"); Absent (2, 4) and Control (5)
#   do not see it.
# Mapping checks (closed items, not text): Q5 perceived risk 5-7 is chosen
# more often in arms 1-2 than 3-4, and Q9 test riskiness 6-7 more often in
# arms 1 and 3 than in 2 and 4 (printed below).
arm_code <- as.integer(as.vector(d$XTESS204))
stopifnot(identical(as.vector(table(arm_code)), c(435L, 433L, 448L, 429L, 426L)))
arm <- c("high_harms", "high_noharms", "low_harms", "low_noharms", "control")[arm_code]
risk  <- c("high", "high", "low", "low", NA)[arm_code]
harms <- c(1L, 0L, 1L, 0L, 0L)[arm_code]
q5 <- as.integer(as.vector(d$Q5)); q9 <- as.integer(as.vector(d$Q9))
cat("Share Q5 >= 5 (high perceived risk) by arm:\n")
print(round(tapply(q5 >= 5, arm, mean), 3))
cat("Share Q9 >= 6 (test very risky) by arm:\n")
print(round(tapply(q9 >= 6, arm, mean), 3))

# ---- Text ---------------------------------------------------------------------
# Q11 is plain character; refusals are flagged in Q11_Refused, not coded in
# the text. A text equal to "-1" (GfK refusal code) is treated as empty.
x <- as.character(as.vector(d$Q11))
x[is.na(x)] <- ""
cat("Q11 values equal to '-1':", sum(trimws(x) == "-1"), "\n")
x[trimws(x) == "-1"] <- ""
txt <- trimws(gsub("\r\n?", "\n", x))   # CR / CRLF -> \n (read.csv does not round-trip \r)

# ---- Exclusions ---------------------------------------------------------------
# None. The file is the fielded sample (2,171); the proposal restricts
# eligibility to ages 40-70, applied at sampling (checked below). No
# exclusion rule for this item is documented in the archive.
age <- as.integer(d$PPAGE)
cat("Age range:", range(age), "\n")

q10 <- as.integer(as.vector(d$Q10))
q10[q10 < 0] <- NA                       # -1 = refused

h <- data.frame(
  id        = as.character(d$CaseID),    # GfK CaseID
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  treatment = arm,                       # five arms, see Treatment above
  risk_feedback = risk,                  # factor 1: "high" / "low" / NA (control)
  harms_shown   = harms,                 # factor 2: 1 = detailed harms listed
  cancer_type = ifelse(as.integer(as.vector(d$PPGENDER)) == 1, "prostate", "breast"),  # by respondent sex, not randomized
  want_test = as.integer(q10 == 1),      # Q10, the decision Q11 explains (1 = would get the test)
  perceived_risk_q5 = ifelse(q5 < 0, NA, q5),
  worry_q6  = ifelse(as.integer(as.vector(d$Q6)) < 0, NA, as.integer(as.vector(d$Q6))),
  test_risky_q9 = ifelse(q9 < 0, NA, q9),
  weight    = as.numeric(d$weight),      # not used
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text)))
cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
h <- h[h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
cat("Non-excluded rows per arm:\n")
print(table(h$treatment[!h$exclude]))
cat("Risk factor x harms factor:\n"); print(table(h$risk_feedback, h$harms_shown, useNA = "ifany"))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
