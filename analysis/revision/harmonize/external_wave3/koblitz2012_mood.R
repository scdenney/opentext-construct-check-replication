# ==============================================================================
# koblitz2012_mood.R -- harmonized file for card koblitz2012_mood
# ==============================================================================
# TESS study koblitz279 (A. Koblitz; TESS2 115 "Ferrer"), "Self-Affirmation and
# Affect: Effects on Implementation Intentions", fielded by Knowledge Networks
# 3/2012-8/2012 among US women who drink alcohol, N = 1,161.
# 2 (self-affirmation vs no affirmation) x 5 (recalled emotion: happiness,
# sadness, anger, hope, neutral room description) between-subjects design.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/koblitz2012_mood.R
#
# Input: data/external/koblitz2012_mood/raw/TESS2_115_Ferrer_Client.sav
# Output: data/external/koblitz2012_mood/harmonized_koblitz2012_mood.csv
#
# Text item: Q9 "If you have any interest in altering your alcohol consumption
# in the next 7 days, how will you go about it? Please list below any steps
# that you think you will take to reduce your alcohol consumption."
# (questionnaire TESSS2_115_-_Ferrer_Survey.docx), the implementation-
# intention item, asked after the alcohol and breast cancer message and Q8
# (intention to decrease drinking). Asked in all ten arms. The other text
# items (Q1/Q1A recall, Q6A/Q7A value essays) are the manipulations
# themselves and are not used.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/koblitz2012_mood/raw/TESS2_115_Ferrer_Client.sav"
out_csv  <- "data/external/koblitz2012_mood/harmonized_koblitz2012_mood.csv"
card_id  <- "koblitz2012_mood"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 1161, !anyDuplicated(d$CaseID), !anyNA(d$CaseID))
stopifnot(all(as.vector(d$PPGENDER) == 2))   # women only (proposal p. 5)

# ---- Treatment ----------------------------------------------------------------
# XTESS115 (questionnaire Table 1, "Research Design - Experimental Conditions"):
#   1 Happiness/SA, 2 Sadness/SA, 3 Angry/SA, 4 Hopeful/SA, 5 Neutral/SA,
#   6 Happiness/no-SA, 7 Sadness/no-SA, 8 Angry/no-SA, 9 Hopeful/no-SA,
#   10 Neutral/no-SA.
# SA = write about the value most important to you (Q6/Q6A, arms 1-5);
# no-SA = write why your least important value could matter to someone else
# (Q7/Q7A, arms 6-10). Emotion = recall the happiest/saddest/angriest/most
# hopeful time (Q1, arms 1-4, 6-9) or describe a room in your house (Q1A,
# arms 5, 10). Verified below: the value-essay and recall items are answered
# only in the arms the questionnaire routes to them.
a <- as.integer(as.vector(d$XTESS115))
stopifnot(identical(as.vector(table(a)), c(133L, 111L, 126L, 118L, 113L, 110L, 104L, 115L, 118L, 113L)))
answered <- function(v) { y <- as.character(as.vector(d[[v]])); !is.na(y) & trimws(y) != "" }
stopifnot(!any(answered("Q6A") & a > 5), !any(answered("Q7A") & a <= 5),
          !any(answered("Q1A") & !(a %in% c(5, 10))), !any(answered("Q1") & a %in% c(5, 10)))
affirm  <- as.integer(a <= 5)
emotion <- c("happiness", "sadness", "anger", "hope", "neutral")[(a - 1) %% 5 + 1]
arm     <- paste0(emotion, "_", ifelse(affirm == 1, "sa", "nosa"))

# Mood check Q2-Q5 (1-7, -1 refused), mean by recalled emotion, for the record.
for (v in c("Q2", "Q3", "Q4", "Q5")) {
  z <- as.numeric(as.vector(d[[v]])); z[z < 0] <- NA
  cat(v, attr(d[[v]], "label"), "\n"); print(round(tapply(z, emotion, mean, na.rm = TRUE), 2))
}

# ---- Text ---------------------------------------------------------------------
# Q9 is plain character; refusals are flagged in Q9_Refused, not coded in the
# text. A text equal to "-1" (refusal code) is treated as empty.
x <- as.character(as.vector(d$Q9))
x[is.na(x)] <- ""
cat("Q9 values equal to '-1':", sum(trimws(x) == "-1"), "\n")
x[trimws(x) == "-1"] <- ""
txt <- trimws(gsub("\r\n?", "\n", x))   # CR / CRLF -> \n (read.csv does not round-trip \r)

# ---- Exclusions ---------------------------------------------------------------
# None. The file is the fielded sample of eligible women (drank alcohol in
# the past month; screener S1/S2). No exclusion rule is given in the proposal
# or questionnaire.

q8 <- as.integer(as.vector(d$Q8)); q8[q8 < 0] <- NA

h <- data.frame(
  id        = as.character(d$CaseID),    # Knowledge Networks CaseID
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  treatment = arm,                       # ten arms, "<emotion>_sa" / "<emotion>_nosa"
  affirmation = affirm,                  # factor 1: 1 = self-affirmation (arms 1-5)
  emotion   = emotion,                   # factor 2: recalled emotion (neutral = room description)
  intend_decrease_q8 = q8,               # Q8, 1-7, asked just before Q9
  drink_freq_s1 = as.integer(as.vector(d$S1)),   # screener, pre-treatment
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
cat("Affirmation x emotion:\n"); print(table(h$affirmation, h$emotion))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
