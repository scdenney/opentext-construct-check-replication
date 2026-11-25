# ==============================================================================
# vogler2019_precarious.R -- harmonized file for card vogler2019_precarious
# ==============================================================================
# Vogler, S., & Petsko, C. TESS study voglerS43 (TESS 042, fielded 2019 by
# NORC AmeriSpeak, N = 3,010). Article: Petsko, C. D., & Vogler, S. (2023),
# Personality and Social Psychology Bulletin, doi:10.1177/01461672221143839,
# Experiment 2 (accepted version, UNC repository doi:10.17615/2q9a-pe94; read:
# Method sections and notes).
# Data: OSF https://osf.io/4nyuc/ (TESS archive).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/vogler2019_precarious.R
#
# Input:  data/external/vogler2019_precarious/raw/8041.042_TESS_042_Vogler.sav
# Output: data/external/vogler2019_precarious/harmonized_vogler2019_precarious.csv
#
# Design (fielded questionnaire raw/8041.038.042_Banaji__Vogler_v7clean.docx,
# "BEGIN TESS 042 Vogler"; value labels): 2 x 2 x 2 between-subjects.
#   P_BEHAV  1 = same-sex encounter ("Homosexual"), 2 = opposite-sex ("Heterosexual")
#   P_GENDER 1 = man, 2 = woman;  P_RACE 1 = White, 2 = Black (name cue)
#   DOV_NAME: one of 15 names drawn per race x gender cell (questionnaire name
#   table: 1-15 White men, 16-30 White women, 31-45 Black men, 46-60 Black
#   women; checked below). Article note 7: "Due to a programming error, the
#   name 'Jermaine' was accidentally used in place of the name 'Aisha'" (name
#   57, Black women); the authors keep these rows. Flagged in name_error.
# Sequence: story -> V1 (three 0-100 sliders: heterosexual, homosexual,
#   bisexual) -> V2 (text): "If you had to guess, why do you think [name]
#   behaved the way [he/she] did in the story you just read?" Shown to all.
# The Vogler block followed the TESS 038 Banaji block in the same survey.
#
# Exclusions: none. The article analyzes the "entire sample" (Experiment 2,
# Participants, and note 7); the open item was "for exploratory purposes" and
# "not analyzed". exclude is FALSE for all rows.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/vogler2019_precarious/raw/8041.042_TESS_042_Vogler.sav"
out_csv  <- "data/external/vogler2019_precarious/harmonized_vogler2019_precarious.csv"
card_id  <- "vogler2019_precarious"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 3010, !anyDuplicated(d$CaseId))

lab_ok <- function(v, nm) identical(names(attr(d[[v]], "labels"))[match(1:2, attr(d[[v]], "labels"))], nm)
stopifnot(lab_ok("P_BEHAV", c("Homosexual", "Heterosexual")),
          lab_ok("P_GENDER", c("Man", "Woman")), lab_ok("P_RACE", c("White", "Black")))
beh <- as.integer(d$P_BEHAV); gen <- as.integer(d$P_GENDER); rac <- as.integer(d$P_RACE)
stopifnot(all(beh %in% 1:2), all(gen %in% 1:2), all(rac %in% 1:2))

# Name drawn must sit in the race x gender column of the name table.
nm <- as.integer(d$DOV_NAME)
col <- (rac - 1L) * 2L + gen            # 1 WM, 2 WW, 3 BM, 4 BW
stopifnot(all(nm >= (col - 1L) * 15L + 1L & nm <= col * 15L))

# Survey language (TESS brief: count Spanish interviews if any)
cat("Survey language:\n"); print(table(as_factor(d$SURV_LANG), useNA = "ifany"))
lang <- ifelse(as.integer(d$SURV_LANG) == 1L, "en", NA_character_)
lang_lab <- attr(d$SURV_LANG, "labels")
stopifnot(names(lang_lab)[lang_lab == 1] == "English")

# ---- Text ---------------------------------------------------------------------
txt <- as.character(zap_labels(d$V2))
txt[is.na(txt)] <- ""
txt <- trimws(gsub("\r\n?", "\n", txt))
n_code <- sum(txt %in% c("77", "98", "99"))
cat("V2 cells holding only a 77/98/99 code (treated as empty):", n_code, "\n")
txt[txt %in% c("77", "98", "99")] <- ""
stopifnot(all(validUTF8(txt)))

sl <- function(v) { v <- as.numeric(zap_labels(d[[v]])); v[!(v >= 0 & v <= 100)] <- NA; v }
h <- data.frame(
  id        = as.character(d$CaseId),
  dataset   = card_id,
  country   = "US",
  lang      = lang,
  text      = txt,
  exclude   = FALSE,
  # 1 = casual encounter with someone of the same sex, 0 = opposite sex
  treatment = as.integer(beh == 1L),
  target_gender = c("man", "woman")[gen],     # independently randomized (moderator in H1)
  target_race   = c("white", "black")[rac],   # independently randomized
  name_id   = nm,
  name_error = nm == 57L,                     # "Jermaine" shown for a Black woman (article note 7)
  rating_heterosexual = sl("V1A"),            # 0-100 certainty, before the text
  rating_homosexual   = sl("V1B"),
  rating_bisexual     = sl("V1C"),
  weight    = as.numeric(d$WEIGHT),           # kept as a design column; not to be used
  stringsAsFactors = FALSE
)
stopifnot(!anyNA(h$lang))

n_empty <- sum(h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = same-sex encounter):\n")
print(table(h$treatment[!h$exclude]))
cat("Arm x target gender:\n"); print(table(h$treatment, h$target_gender))
cat("Arm x target race:\n");   print(table(h$treatment, h$target_race))
cat("Name error rows:", sum(h$name_error), "\n")
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
