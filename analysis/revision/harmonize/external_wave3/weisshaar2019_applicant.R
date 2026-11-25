# ==============================================================================
# weisshaar2019_applicant.R -- harmonized file for card weisshaar2019_applicant
# ==============================================================================
# TESS study weisshaar1206 (TESS 054, AmeriSpeak, fielded 11/11/2019-
# 02/27/2020), K. Weisshaar and K. Chavez, "An Imperfect Match? How Gender and
# Race Influence Perceptions of Job Applicants by Qualification Levels".
# Data: OSF https://osf.io/j986z/ (TESS archive, public). No published
# article of this experiment found (the authors list a related working paper).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/weisshaar2019_applicant.R
#
# Input:  data/external/weisshaar2019_applicant/raw/PN8041.054_TESS054_Weisshaar_20200226.sav
# Output: data/external/weisshaar2019_applicant/harmonized_weisshaar2019_applicant.csv
#
# Design (raw/TESS-1206.R1_description.docx; fielded questionnaire
# raw/8041.054_TESS_054_Weisshaar_v3clean.docx): employed adults (EMPLOY1 = 1;
# others screened out) are told recruiters judged that an applicant for an
# "Expert Accountant Level II" job "meets / does not meet the job
# requirements" (P_QUALIF_TREAT 1/2), then see a cover letter and resume that
# are identical except for the name (P_NAME_TREAT: 1 DeAndre Washington,
# 2 Ebony Washington, 3 Jake Walsh, 4 Katelyn Walsh), rate hireability (HIRE),
# rate six areas needing "more emphasis" (COM, CAP, LEAD, RESP, ASSERT, FIT,
# random order), and then answer STRENGTHS_WEAKNESSES: "In your own words, how
# would you describe the strengths and weaknesses of the job applicant?"
# (same wording in all cells), followed by the race/gender recall check
# (MANIPULATION). 2 x 4 cells, equal probability, preloaded.
#
# Exclusions: the proposal states none (the wiki results summary analyzes
# respondents 30+ and controls for correct race recall; neither is an
# exclusion rule). exclude = FALSE throughout. Weights kept, not used.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/weisshaar2019_applicant/raw/PN8041.054_TESS054_Weisshaar_20200226.sav"
out_csv  <- "data/external/weisshaar2019_applicant/harmonized_weisshaar2019_applicant.csv"
card_id  <- "weisshaar2019_applicant"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 1814, !anyDuplicated(d$CaseId),
          all(d$P_NAME_TREAT %in% 1:4), all(d$P_QUALIF_TREAT %in% 1:2))

# ---- Language ----------------------------------------------------------------------
# SURV_LANG (interview language); questionnaire header lists English only.
cat("Interview language (SURV_LANG):\n"); print(table(as_factor(d$SURV_LANG), useNA = "ifany"))

# ---- Text -------------------------------------------------------------------------
# Value labels: 77 DON'T KNOW, 98 SKIPPED ON WEB, 99 REFUSED. Whole-answer codes
# are skips and set to empty. CR LF line breaks become LF (read.csv drops CR).
txt <- trimws(as.character(zap_labels(d$STRENGTHS_WEAKNESSES)))
stopifnot(all(validUTF8(txt)))
skip <- txt %in% c("77", "98", "99")
cat("Skip codes (77/98/99) set to empty:", sum(skip), "\n")
txt[skip] <- ""
cat("Answers with CR line breaks normalized to LF:", sum(grepl("\r", txt, fixed = TRUE)), "\n")
txt <- gsub("\r\n?", "\n", txt)

# ---- Design -----------------------------------------------------------------------
arm <- c("black_man", "black_woman", "white_man", "white_woman")[d$P_NAME_TREAT]
num <- function(v) { v <- as.numeric(zap_labels(v)); v[v %in% c(77, 98, 99)] <- NA; v }
# MANIPULATION (after the text): 1 White male, 2 White female, 4 Black male,
# 5 Black female; correct recall = the arm's own race and gender.
recall_code <- c(black_man = 4, black_woman = 5, white_man = 1, white_woman = 2)[arm]
manip <- num(d$MANIPULATION)

h <- data.frame(
  id            = d$CaseId,
  dataset       = card_id,
  country       = "US",
  lang          = "en",
  text          = txt,
  exclude       = FALSE,
  # name-signaled race x gender of the applicant
  treatment     = arm,
  applicant_race   = ifelse(d$P_NAME_TREAT <= 2, "black", "white"),
  applicant_gender = ifelse(d$P_NAME_TREAT %in% c(1, 3), "man", "woman"),
  # independently randomized: 1 = told the applicant meets the job
  # requirements, 0 = does not meet them
  qualified     = as.integer(d$P_QUALIF_TREAT == 1),
  # closed outcomes asked before the text (1-7)
  hire          = num(d$HIRE),
  com = num(d$COM), cap = num(d$CAP), lead = num(d$LEAD),
  resp = num(d$RESP), assert = num(d$ASSERT), fit = num(d$FIT),
  # recall check asked after the text
  recall_correct = ifelse(is.na(manip), NA, as.integer(manip == recall_code)),
  weight        = as.numeric(d$WEIGHT),
  stringsAsFactors = FALSE)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -----------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
cat("Arm x qualified:\n"); print(table(h$treatment, h$qualified))
cat("Recall correct x arm:\n"); print(table(h$recall_correct, h$treatment, useNA = "ifany"))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
