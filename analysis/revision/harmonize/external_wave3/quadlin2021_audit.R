# ==============================================================================
# quadlin2021_audit.R -- harmonized file for card quadlin2021_audit
# ==============================================================================
# Quadlin, N. "Which College Graduates are Perceived as Trainable for Today's
# Work World? Perceptions of Race, Gender, Achievement, and the Liberal Arts."
# TESS study quadlin1348 (TESS 096), NORC AmeriSpeak, employed US adults
# (EMPLOY1 = paid employee; others terminated), N = 4,891, fielded Jun-Nov 2021.
# OSF https://osf.io/mebw9/ (proposal: component qvr78, Quadlin1348.pdf).
#
# Design (fielded questionnaire raw/8041.096_TESS_Quadlin_vFINAL_simple.docx;
# description raw/TESS-1348.R1_description.docx): one resume, 96 conditions
# (P_CONDITION) = 8 names (race x gender: FIRSTNAME) x 4 majors (MAJOR:
# Communications, Computer Science = "applied"; English, Physics = "liberal
# arts", description table) x 3 GPAs (2.70 / 3.25 / 3.80). Then Q1 interview
# recommendation, Q2A-E trait grid, Q3 expected tenure, Q4 class difficulty
# (restates major and GPA), Q5 open overall assessment, Q6 recall of grades.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/quadlin2021_audit.R
# Input:  data/external/quadlin2021_audit/raw/TESS096_Quadlin.sav
# Output: data/external/quadlin2021_audit/harmonized_quadlin2021_audit.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_dir <- "data/external/quadlin2021_audit/raw"
out_csv <- "data/external/quadlin2021_audit/harmonized_quadlin2021_audit.csv"
card_id <- "quadlin2021_audit"

d <- read_sav(file.path(raw_dir, "TESS096_Quadlin.sav"))
stopifnot(nrow(d) == 4891, !anyDuplicated(d$CaseId))
lab <- function(x) as.character(as_factor(x, levels = "labels"))

# Text. Q5 carries value labels 77/98/99 (DON'T KNOW / SKIPPED ON WEB /
# REFUSED); an answer equal to a code is a skip and is set empty. CRLF line
# breaks are normalized to LF (read.csv drops CR).
txt <- as.character(zap_labels(d$Q5))
Encoding(txt) <- "UTF-8"
txt <- trimws(ifelse(is.na(txt), "", txt))
txt <- gsub("\r\n?", "\n", txt)
stopifnot(all(validUTF8(txt)))
is_code <- txt %in% c("77", "98", "99")
cat("Text equal to a missing-value code (set empty):", sum(is_code), "\n")
txt[is_code] <- ""

cat("Interview language:\n"); print(table(lab(d$SURV_LANG)))

# ---- Factors -----------------------------------------------------------------
major <- lab(d$MAJOR)
stopifnot(all(major %in% c("Communications", "Computer Science", "English", "Physics")))
field <- ifelse(major %in% c("English", "Physics"), "liberal_arts", "applied")  # description, table
first <- lab(d$FIRSTNAME)
name_group <- c(Michael = "white_man", Ashley = "white_woman", DeAndre = "black_man",
                Ebony = "black_woman", Carlos = "hispanic_man", Mariana = "hispanic_woman",
                Daniel = "asian_man", Amy = "asian_woman")[first]          # description, table
stopifnot(!anyNA(name_group))
gpa <- substr(lab(d$GPA), 1, 4)
stopifnot(all(gpa %in% c("2.70", "3.25", "3.80")))
# P_CONDITION is consistent with the factors (questionnaire table: name blocks
# of 12 in the order Amy ... Michael; within a name, major blocks of 3; GPA
# cycles low/moderate/high).
pc <- as.numeric(d$P_CONDITION)
stopifnot(all(c("2.70", "3.25", "3.80")[(pc - 1) %% 3 + 1] == gpa),
          all(c("Communications", "Computer Science", "English", "Physics")[((pc - 1) %/% 3) %% 4 + 1] == major))

# ---- Exclusions ----------------------------------------------------------------
# None. The screener already terminated non-employees (questionnaire EMPLOY1);
# the proposal states no further exclusions; all rows are weighted completes.
num <- function(x, ok) { x <- as.numeric(x); x[!x %in% ok] <- NA; x }

h <- data.frame(
  id        = as.integer(d$CaseId),
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  treatment = as.integer(field == "liberal_arts"),  # 1 = English/Physics, 0 = Communications/Computer Science
  major     = major,
  applicant_name = first,
  applicant_race = sub("_.*", "", name_group),
  applicant_gender = sub(".*_", "", name_group),
  gpa       = gpa,
  condition = pc,
  interview_rec = num(d$Q1, 0:10),    # closed outcome: 0 not at all likely ... 10 very likely (value labels)
  trainable = num(d$Q2A, 1:7),
  competent = num(d$Q2B, 1:7),
  warm      = num(d$Q2C, 1:7),
  weight    = as.numeric(d$WEIGHT),   # not used
  stringsAsFactors = FALSE
)

cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
h <- h[h$text != "", ]
write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks --------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
k <- h[!h$exclude, ]
cat("Non-excluded rows, liberal arts (1) vs applied (0):\n"); print(table(k$treatment))
cat("By major:\n"); print(table(k$major))
cat("Field x applicant race:\n"); print(table(k$treatment, k$applicant_race))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
