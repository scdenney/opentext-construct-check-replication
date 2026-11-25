# ==============================================================================
# grace2019_refusal.R -- harmonized file for card grace2019_refusal
# ==============================================================================
# Grace, M. K., & Doan, L. TESS study grace1119 (fielded 2019 by NORC
# AmeriSpeak, N = 5,028), "Factors Affecting Public Opinion on Transgender
# Medical Care Refusal". Article: Doan, L., & Grace, M. K. (2022), American
# Sociological Review 87(2): 275-302, doi:10.1177/00031224221082233 (closed
# access; not read).
# Data: OSF https://osf.io/hu45w/ (TESS archive).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/grace2019_refusal.R
#
# Input:  data/external/grace2019_refusal/raw/PN8041_TESS048_Grace_Client_File.sav
# Output: data/external/grace2019_refusal/harmonized_grace2019_refusal.csv
#
# Design (questionnaire raw/8041.048_TSS048_Grace_v4clean.docx, DISPLAY_VIN and
# the P_EXP preload table; proposal raw/Grace1119.pdf pp. 4-6): 2 (trans woman,
# TW / trans man, TM) x 4 (Black, White, Latinx, Asian name) x 2 (doctor's
# rationale: inadequate training, IT / religious objections, RB), 16 cells in
# P_EXP, blocked on the health-care-worker oversample (P_OVER).
#   P_EXP 1-4  = TW, IT;  5-8  = TW, RB;  9-12 = TM, IT;  13-16 = TM, RB;
#   within each block of four: Black, White, Latinx, Asian.
# The builder checks this against the SAS value labels (P_EXP) and against
# DOV_NAME (the patient name piped into Q1-Q5), which must map one-to-one.
#
# Text item Q2 (asked only if Q1 was answered, "[SHOW IF Q1=1,2,3,4]"):
#   "Why do you [Q1 answer] that Dr. Smith should be [allowed to refuse /
#   required] to treat [name]? In your own words, please write a few sentences
#   explaining why you feel this way."
# The wording switch (RND_01, randomized in the script, questionnaire RND table)
# is NOT in the archive, so the direction of Q1/Q5 agreement cannot be recovered.
#
# Exclusions: the article reports N = 4,876 (abstract), not reproduced from the
# archive (Q1 valid 5,004; Q1, Q3, Q5 valid 4,979; the same without the 21
# respondents identifying as transgender or neither 4,958). The rule is in the
# closed-access article and was not seen, so no row is excluded on it; all
# rows carry a randomized P_EXP, so exclude is FALSE throughout.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/grace2019_refusal/raw/PN8041_TESS048_Grace_Client_File.sav"
out_csv  <- "data/external/grace2019_refusal/harmonized_grace2019_refusal.csv"
card_id  <- "grace2019_refusal"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 5028, !anyDuplicated(d$CaseId))

# ---- Cell -> factor mapping, checked against the value labels ----------------
exp <- as.integer(d$P_EXP)
stopifnot(!anyNA(exp), all(exp %in% 1:16))
labs <- attr(d$P_EXP, "labels")
lab_of <- setNames(names(labs), labs)[as.character(1:16)]
race_lv <- c("black", "white", "latinx", "asian")
cells <- data.frame(
  p_exp     = 1:16,
  gender    = rep(c("trans_woman", "trans_man"), each = 8),
  rationale = rep(rep(c("inadequate_training", "religious_objection"), each = 4), 2),
  race      = rep(race_lv, 4)
)
code <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
expect <- paste(ifelse(cells$race == "latinx", "Latinx", code(cells$race)),
                ifelse(cells$gender == "trans_woman", "TW", "TM"),
                ifelse(cells$rationale == "inadequate_training", "IT", "RB"), sep = "-")
stopifnot(identical(unname(lab_of), expect))
# DOV_NAME (1 Tyra, 2 Heidi, 3 Camila, 4 Mai, 5 Tyrone, 6 Scott, 7 Carlos,
# 8 Minh; questionnaire "COMPUTE DOV_NAME") must be fixed by gender x race.
dov <- as.integer(d$DOV_NAME)
key <- paste(cells$gender, cells$race)[exp]
stopifnot(all(rowSums(table(key, dov) > 0) == 1), all(colSums(table(key, dov) > 0) == 1))

# ---- Text ---------------------------------------------------------------------
txt <- as.character(zap_labels(d$Q2))
txt[is.na(txt)] <- ""
# Line breaks normalized for a clean CSV round trip (\r\n and lone \r -> \n).
txt <- trimws(gsub("\r\n?", "\n", txt))
# Q2 carries the CAWI codes 77/98/99 as value labels (SAS format $Q2F); a cell
# holding only such a code is a non-answer and is treated as empty.
n_code <- sum(txt %in% c("77", "98", "99"))
cat("Q2 cells holding only a 77/98/99 code (treated as empty):", n_code, "\n")
txt[txt %in% c("77", "98", "99")] <- ""
stopifnot(all(validUTF8(txt)))

num <- function(v) { v <- as.integer(zap_labels(v)); v[!v %in% 1:4] <- NA; v }
h <- data.frame(
  id        = as.character(d$CaseId),
  dataset   = card_id,
  country   = "US",
  lang      = "en",                       # questionnaire: "English only", CAWI only
  text      = txt,
  exclude   = FALSE,                      # see header: article rule not available
  # 1 = doctor cites religious objections (RB), 0 = inadequate training (IT)
  treatment = as.integer(cells$rationale[exp] == "religious_objection"),
  patient_gender = cells$gender[exp],     # independently randomized
  patient_race   = cells$race[exp],       # independently randomized (name cue)
  p_exp          = exp,                   # 16-cell condition as fielded
  oversample_hcw = as.integer(zap_labels(d$P_OVER)),  # blocking: 1 = health care professional
  weight         = as.numeric(d$WEIGHT),  # kept as a design column; not to be used
  # Q1/Q5: 1 strongly disagree ... 4 strongly agree with "[allowed to refuse /
  # required] to treat"; wording arm RND_01 not deposited, so not directional.
  # Q1 is the answer piped into the Q2 prompt. 77/98/99 -> NA.
  q1_agree_wording_unknown = num(d$Q1),
  q3_rationale_primary     = num(d$Q3),   # agree the stated reason is the true one
  q5_agree_after_referral_wording_unknown = num(d$Q5),
  stringsAsFactors = FALSE
)

n_empty <- sum(h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = religious objection):\n")
print(table(h$treatment[!h$exclude]))
cat("Arm x patient gender:\n"); print(table(h$treatment, h$patient_gender))
cat("Arm x patient race:\n");   print(table(h$treatment, h$patient_race))
cat("Arm x oversample (blocking):\n"); print(table(h$treatment, h$oversample_hcw))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
