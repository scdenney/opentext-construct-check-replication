# ==============================================================================
# melin2019_pay.R -- harmonized file for card melin2019_pay
# ==============================================================================
# TESS study melin1066 (TESS 041, AmeriSpeak, fielded 05/07/2019-11/04/2019),
# J. Melin, "Testing a Theory of Hybrid Femininity". No published article
# found. Data: OSF https://osf.io/tbrfh/ (TESS archive, public).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/melin2019_pay.R
#
# Input:  data/external/melin2019_pay/raw/8041.41_TESS_041_Melin.sav
#         (SPSS, windows-1252; haven re-encodes to UTF-8. Q8 is a 1,530-byte
#         very long string, which haven joins; longest answer 747 bytes.)
# Output: data/external/melin2019_pay/harmonized_melin2019_pay.csv
#
# Design (questionnaire raw/8041.041_TESS041_Melin_v5clean.docx, preload
# P_41): 2 resumes x 3 target jobs, one cell per respondent:
#   1 Military/Automotive, 2 Military/Wedding, 3 Military/Consulting,
#   4 Cosmetics/Automotive, 5 Cosmetics/Wedding, 6 Cosmetics/Consulting.
# Resume 1 (P_41 1-3): "U.S. Army Project Manager (Active-duty Enlisted)";
# resume 2 (P_41 4-6): "Cosmetics Project Manager"; name (Amy Decker),
# education, volunteer work and skills are identical (resume images in the
# questionnaire). Q8 follows the salary, bonus and status ratings (Q1-Q7):
# "What is your overall assessment regarding how much to pay the applicant for
# the [job] position? In your own words, please write a few sentences
# explaining why you feel this way." Same wording in both resume arms.
#
# Exclusions: the proposal (raw/Melin1066.pdf) states none for the main study
# (its pilot dropped manipulation-check failures; that rule is not carried to
# the TESS study). exclude = FALSE throughout; the manipulation check (Q9) is
# kept as a column. Weights kept, not used.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/melin2019_pay/raw/8041.41_TESS_041_Melin.sav"
out_csv  <- "data/external/melin2019_pay/harmonized_melin2019_pay.csv"
card_id  <- "melin2019_pay"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 1682, !anyDuplicated(d$CaseId), all(d$P_41 %in% 1:6))

# ---- Text -------------------------------------------------------------------------
# Q8 value labels: 77 DON'T KNOW, 98 SKIPPED ON WEB, 99 REFUSED (questionnaire:
# "Please code refusals in CAWI: 98 IMPLICIT REFUSAL, WEB SKIP"). Rows whose
# whole answer is one of these codes are skips, set to empty and dropped.
txt <- trimws(as.character(zap_labels(d$Q8)))
stopifnot(all(validUTF8(txt)))
# Windows line breaks (CR LF) inside answers become LF: read.csv drops the CR,
# so the file would not re-read to identical text otherwise.
cat("Answers with CR line breaks normalized to LF:", sum(grepl("\r", txt, fixed = TRUE)), "\n")
txt <- gsub("\r\n?", "\n", txt)
skip <- txt %in% c("77", "98", "99")
cat("Q8 skip codes (77/98/99) set to empty:", sum(skip), "\n")
txt[skip] <- ""

# ---- Design -----------------------------------------------------------------------
p <- as.integer(d$P_41)
resume <- ifelse(p <= 3, "military", "cosmetics")
job    <- c("automotive", "wedding", "consulting")[(p - 1) %% 3 + 1]
# Q9 manipulation check: 1 Military, 2 Cosmetics/beauty, 98 skipped.
q9 <- as.integer(zap_labels(d$Q9))
mc_pass <- ifelse(q9 %in% 1:2, as.integer((q9 == 1) == (resume == "military")), NA)
num <- function(v) { v <- as.numeric(zap_labels(v)); v[v %in% c(77, 98, 99)] <- NA; v }

h <- data.frame(
  id          = d$CaseId,
  dataset     = card_id,
  country     = "US",
  lang        = "en",
  text        = txt,
  exclude     = FALSE,
  # 1 = military (hypermasculine) occupational history, 0 = cosmetics (hyperfeminine)
  treatment   = as.integer(resume == "military"),
  # independently randomized target job (male-dominated / female-dominated / neutral)
  target_job  = job,
  cell        = p,
  # closed outcomes (pre-text): recommended salary (USD, slider 35,000-225,000)
  # and bonus (0-67,500); status and gender perceptions
  salary      = num(d$Q1),
  bonus       = num(d$Q2),
  committed   = num(d$Q3),
  capable     = num(d$Q4_1),
  selfconfident = num(d$Q4_2),
  competent   = num(d$Q4_3),
  reliable    = num(d$Q5),
  leader      = num(d$Q6),
  masculine   = num(d$Q7_1),
  feminine    = num(d$Q7_2),
  mc_pass     = mc_pass,
  weight      = as.numeric(d$WEIGHT),
  stringsAsFactors = FALSE)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -----------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = military):\n"); print(table(h$treatment[!h$exclude]))
cat("Arm x target job:\n"); print(table(h$target_job, h$treatment))
cat("Manipulation check pass x arm (NA = skipped):\n")
print(table(mc_pass = h$mc_pass, arm = h$treatment, useNA = "ifany"))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
