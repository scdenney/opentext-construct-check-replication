# ==============================================================================
# mezzapelle2021_harassment.R -- harmonized file for card mezzapelle2021_harassment
# ==============================================================================
# Mezzapelle, J. L., & Reiman, A.-K. TESS study mezzapelle1439 (fielded
# 09/2021-11/2021 by NORC AmeriSpeak, N = 2,113). Article: Mezzapelle & Reiman
# (2024), Psychology of Women Quarterly, doi:10.1177/03616843241252658, Study 2;
# preprint https://osf.io/s968w (read: Introduction end, Study 2 Method).
# Data: OSF https://osf.io/fxcn4/ (TESS archive).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/mezzapelle2021_harassment.R
#
# Input:  data/external/mezzapelle2021_harassment/raw/_9089.0106__TESS__Mezzapelle_Data.sav
# Output: data/external/mezzapelle2021_harassment/harmonized_mezzapelle2021_harassment.csv
#
# Design (fielded questionnaire raw/_9089.0106__TESS__Mezzapelle_vFINALprog.docx,
# EXP_1; P_EXP value labels): 2 x 2 between-subjects, claimant Michelle's race
# (Black / White) x gender identity (cisgender / transgender) shown in the
# demographics section of a redacted HR incident report image.
#   P_EXP 1 = Black cisgender, 2 = Black transgender, 3 = White cisgender,
#   4 = White transgender (checked against the value labels below).
# Q1 recall check (8 options); respondents who answer wrongly are shown the
# correct identity (Q1_CORRECT) before Q2. Q2 is shown to everyone:
#   "Based on the information you read in the incident report filed by
#   Michelle, what exactly do you think John did or said to Michelle? Please
#   tell us in a few words. While many possibilities may come to mind, please
#   only write down the one most likely thing that you think John did."
#
# Exclusions (preprint, Study 2 Method, Participants): "As preregistered,
# participants were excluded from analysis if they provided an incoherent
# response to an open-ended question assessing assumed form of sexual
# harassment. The final sample included 2,088 participants." The 25 rows are a
# content judgment on the text and cannot be identified without reading it:
# NOT reproduced; exclude is FALSE for all rows. The authors did not exclude
# recall-check failures (exploratory only); q1_recall_correct is kept.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/mezzapelle2021_harassment/raw/_9089.0106__TESS__Mezzapelle_Data.sav"
out_csv  <- "data/external/mezzapelle2021_harassment/harmonized_mezzapelle2021_harassment.csv"
card_id  <- "mezzapelle2021_harassment"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 2113, !anyDuplicated(d$CaseId))

# ---- Cell -> factor mapping, checked against the value labels ----------------
exp <- as.integer(d$P_EXP)
stopifnot(!anyNA(exp), all(exp %in% 1:4))
labs <- attr(d$P_EXP, "labels")
stopifnot(identical(unname(names(labs)[match(1:4, labs)]),
                    c("Black Cisgender Woman", "Black Transgender Woman",
                      "White Cisgender Woman", "White Transgender Woman")))
race     <- c("black", "black", "white", "white")[exp]
identity <- c("cisgender", "transgender", "cisgender", "transgender")[exp]

# Q1 recall: correct answer per cell (questionnaire Q1 options 2, 4, 6, 8).
q1 <- as.integer(zap_labels(d$Q1))
q1_correct <- ifelse(q1 %in% 1:8, as.integer(q1 == c(2L, 4L, 6L, 8L)[exp]), NA_integer_)

# ---- Text ---------------------------------------------------------------------
txt <- as.character(zap_labels(d$Q2))
txt[is.na(txt)] <- ""
txt <- trimws(gsub("\r\n?", "\n", txt))   # line breaks normalized for CSV round trip
# 77/98/99 are the CAWI non-response codes (value labels of Q2); a cell holding
# only such a code is a non-answer and is treated as empty.
n_code <- sum(txt %in% c("77", "98", "99"))
cat("Q2 cells holding only a 77/98/99 code (treated as empty):", n_code, "\n")
txt[txt %in% c("77", "98", "99")] <- ""
stopifnot(all(validUTF8(txt)))

l7 <- function(v) { v <- as.integer(zap_labels(v)); v[!v %in% 1:7] <- NA; v }
q8 <- as.integer(zap_labels(d$Q8)); q8[!q8 %in% 1:8] <- NA
h <- data.frame(
  id        = as.character(d$CaseId),
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,                       # see header: content-based rule not reproduced
  # 1 = claimant listed as a transgender woman, 0 = cisgender woman
  treatment = as.integer(identity == "transgender"),
  claimant_race = race,                    # independently randomized factor
  p_exp     = exp,
  q1_recall_correct = q1_correct,          # pre-text recall check (1 = correct); not an exclusion
  weight    = as.numeric(d$WEIGHT),        # kept as a design column; not to be used
  # Closed outcomes, after the text (Q3-Q5 in randomized order), 1-7 agree.
  q3_believable = l7(d$Q3), q4_credible = l7(d$Q4), q5_truthful = l7(d$Q5),
  q6_harm = l7(d$Q6), q7_compensation = l7(d$Q7),
  q8_punishment = q8,                      # 1 informal warning ... 8 termination
  stringsAsFactors = FALSE
)

n_empty <- sum(h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = transgender):\n")
print(table(h$treatment[!h$exclude]))
cat("Arm x claimant race:\n"); print(table(h$treatment, h$claimant_race))
cat("Recall check correct, by arm:\n"); print(table(h$treatment, h$q1_recall_correct, useNA = "ifany"))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
