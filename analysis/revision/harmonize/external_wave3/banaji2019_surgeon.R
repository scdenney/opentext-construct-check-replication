# ==============================================================================
# banaji2019_surgeon.R -- harmonized file for card banaji2019_surgeon
# ==============================================================================
# TESS study banajiS39, "Do Americans Think Surgeon = Male in 2019?"
# (M. Banaji), fielded by NORC AmeriSpeak 2019 in a combined instrument with
# TESS 042 (Vogler). Published as Study 2 of Morehouse, Kurdi, Hakim & Banaji
# (2022), Current Research in Ecological and Social Psychology 3: 100044,
# doi:10.1016/j.cresp.2022.100044.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/banaji2019_surgeon.R
#
# Input: data/external/banaji2019_surgeon/raw/8041.038_TESS_038_Banaji.sav
#   (TESS/NORC deposit, one row per respondent, N = 3,010).
# Output: data/external/banaji2019_surgeon/harmonized_banaji2019_surgeon.csv
#
# Text item: B1 "How could this be? Please give a brief answer to the riddle."
# (questionnaire 8041.038.042_Banaji__Vogler_v7clean.docx, item B1), typed
# directly after the riddle screen, on which P_CONDITION set the child's
# gender. The answer to the riddle is itself the authors' outcome.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/banaji2019_surgeon/raw/8041.038_TESS_038_Banaji.sav"
out_csv  <- "data/external/banaji2019_surgeon/harmonized_banaji2019_surgeon.csv"
card_id  <- "banaji2019_surgeon"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 3010, !anyDuplicated(d$CaseId), !anyNA(d$CaseId))
stopifnot(all(as.vector(d$SURV_LANG) == 1))   # 1 = English for every row

# ---- Treatment ----------------------------------------------------------------
# P_CONDITION (preload, randomly assigned by NORC statistics; questionnaire
# "Custom survey-specific preloads"). Riddle text by condition, questionnaire
# section "BEGIN TESS 038 Banaji":
#   1 = Father + Son (classic riddle), 2 = Father + Daughter, 3 = Father + Child.
# Arm sizes 1002 / 1008 / 1000.
cond <- as.integer(as.vector(d$P_CONDITION))
stopifnot(identical(as.vector(table(cond)), c(1002L, 1008L, 1000L)))
arm <- c("son", "daughter", "child")[cond]

# ---- Text ---------------------------------------------------------------------
# B1 is a labelled character variable; codebook (s990_TESS_038_Banaji_Codebook.xlsx,
# rows for B1) gives "77" DON'T KNOW, "98" SKIPPED ON WEB, "99" REFUSED.
# These codes are not answers: they become empty text and the row is dropped.
b1 <- as.character(as.vector(d$B1))
b1_code <- b1 %in% names(attr(d$B1, "labels")) | b1 %in% attr(d$B1, "labels")
cat("B1 rows holding a missing-value code (77/98/99):", sum(b1_code), "\n")
txt <- trimws(gsub("\r\n?", "\n", b1))   # CR / CRLF -> \n (read.csv does not round-trip \r)
txt[b1_code] <- ""

# ---- Exclusions ---------------------------------------------------------------
# Authors' rule: only respondents with no prior exposure to the riddle are
# analyzed (B2 == 3 "No"). OSF wiki summary ("Outcomes": "only participants who
# reported having no prior exposure to the riddle (variable: B2) were
# included"); Morehouse et al. 2022, section 3.1.1: "1657 participants met
# this inclusion criterion". B2 is asked AFTER the riddle answer
# (questionnaire item order B1, B2), so the rule conditions on a
# post-treatment item; the builder follows the authors and keeps B2 as a
# column. Rows with B2 = 1 (yes), 2 (yes, don't remember) or 98 (skipped)
# are excluded.
b2 <- as.integer(as.vector(d$B2))
stopifnot(sum(b2 == 3) == 1657)
excl <- b2 != 3

h <- data.frame(
  id        = as.character(d$CaseId),     # NORC CaseId
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = excl,
  treatment = arm,                        # "son" / "daughter" / "child"
  heard_before = b2,                      # B2: 1 yes, 2 yes but don't remember answer, 3 no, 98 skipped
  # B4 (after B1): 1 men naturally more suited, 2 equally suited, 3 women more
  # suited; 98 skipped. Display order reversed by RND_02, codes unchanged.
  essentialism_b4 = as.integer(as.vector(d$B4)),
  family_mother_doctor = as.integer(as.vector(d$B5_4)),   # B5_4 "Yes, my mother"
  weight    = as.numeric(d$WEIGHT),       # post-stratification weight; not used
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
cat("Arm x heard_before (all rows with text):\n")
print(table(h$treatment, h$heard_before))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
