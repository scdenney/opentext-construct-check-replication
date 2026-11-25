# ==============================================================================
# powell2016_restroom.R -- harmonized file for card powell2016_restroom
# ==============================================================================
# Powell, B., Doan, L., & Quadlin, N. TESS short study powellS4, "Factors
# Affecting Attitudes toward Transgender Bathroom Use" (fielded April-May 2017
# by NORC AmeriSpeak as one of three combined TESS short studies, N = 2,034).
# Related article (perceptions of sex, not the restroom item): Doan, Quadlin &
# Powell (2019), Socius, doi:10.1177/2378023119852015 (not read).
# Data: OSF https://osf.io/y5mt3/ (TESS archive).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/powell2016_restroom.R
#
# Input:  data/external/powell2016_restroom/raw/PowellS4.dta
# Output: data/external/powell2016_restroom/harmonized_powell2016_restroom.csv
#
# Assignment variables (codebook raw/TESS 002_POWELL_CODEBOOK.xlsx, "EXP:"
# preloads). The manifest named PAGESTAT alone; the design has three
# independently randomized factors (proposal raw/PowellS4.pdf, Research Design):
#   PGENID   1 = trans man / trans boy, 2 = trans woman / trans girl
#            (stored as character "1"/"2"; the Stata value labels are broken,
#            so the codes are taken from the codebook)
#   PAGESTAT 1 = adult (36), 2 = teenager (16)
#   PGENCONF 1 = conforming ("Confirming" in the codebook), 2 = nonconforming,
#            3 = ambiguous, 4 = unspecified (stored as character)
# All 16 cells are filled (107-138 each; checked below).
#
# Restroom item Q1 exists in four versions in the file (the fielded
# questionnaire is not in raw/; versions read from the codebook labels):
#   Q1A   "local store", options men's first      (adults and some teens)
#   Q1A_1 "local store", options women's first    (adults and some teens)
#   Q1B   "at school",   boy's / girl's restroom  (teens only)
#   Q1B_1 "local store", boy's / girl's restroom  (teens only; label as stored)
# Teens split about evenly between the men's/women's and boy's/girl's
# versions on the same field days (2017-04-18 to 2017-05-26), so the split is
# randomized within the teen arm; no assignment variable is deposited.
# q1_version records the version answered.
#
# Text item Q2A_OE: "Why do you think that Casey should use [Q1 INSERT]?"
# (codebook label; the proposal's wording adds "In your own words, please write
# a few sentences explaining why you feel this way and how strongly you feel
# this way"). Q2A is 1 = answer given, 98 = skipped on web.
#
# Exclusions: none stated for the restroom item in the proposal; the related
# article was not read. exclude is FALSE for all rows (all have assignment).
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/powell2016_restroom/raw/PowellS4.dta"
out_csv  <- "data/external/powell2016_restroom/harmonized_powell2016_restroom.csv"
card_id  <- "powell2016_restroom"

d <- read_dta(raw_file)
stopifnot(nrow(d) == 2034, !anyDuplicated(d$CaseId))

gid  <- as.character(zap_labels(d$PGENID))
age  <- as.integer(zap_labels(d$PAGESTAT))
conf <- as.character(zap_labels(d$PGENCONF))
stopifnot(all(gid %in% c("1", "2")), all(age %in% 1:2), all(conf %in% c("1", "2", "3", "4")))
stopifnot(identical(unname(attr(d$PAGESTAT, "labels")), c(1, 2)),
          identical(names(attr(d$PAGESTAT, "labels")), c("Adult", "Teenager")))
cells <- table(gid, age, conf)
cat("Cells (gender identity x age x conformity), min / max:", min(cells), "/", max(cells), "\n")
stopifnot(all(cells > 100))

# Q1 version actually answered (each row has exactly one, or none if skipped)
has <- sapply(c("Q1A", "Q1A_1", "Q1B", "Q1B_1"), function(v) !is.na(d[[v]]))
stopifnot(all(rowSums(has) <= 1))
q1_version <- c("store_men_first", "store_women_first", "school_boy_girl", "store_boy_girl")[
  ifelse(rowSums(has) == 1, max.col(has), NA)]
stopifnot(all(!grepl("boy_girl", q1_version) | age == 2, na.rm = TRUE))
# Restroom chosen, harmonized (Q1A and Q1B/Q1B_1: 1 = men's/boy's, 2 = women's/
# girl's; Q1A_1 reverses: 1 = women's, 2 = men's; 3 = other; 98 = skipped).
q1 <- function(v) as.integer(zap_labels(d[[v]]))
pick <- ifelse(has[, "Q1A"], q1("Q1A"), ifelse(has[, "Q1A_1"], c(2L, 1L, 3L)[match(q1("Q1A_1"), 1:3)],
        ifelse(has[, "Q1B"], q1("Q1B"), ifelse(has[, "Q1B_1"], q1("Q1B_1"), NA_integer_))))
restroom <- c("male", "female", "other")[match(pick, 1:3)]
# Perceived sex Q3A (1 male, 2 female) / Q3B (1 female, 2 male), 3 other.
q3 <- ifelse(!is.na(d$Q3A), q1("Q3A"), ifelse(!is.na(d$Q3B), c(2L, 1L, 3L)[match(q1("Q3B"), 1:3)], NA_integer_))
perceived_sex <- c("male", "female", "other")[match(q3, 1:3)]

# ---- Text ---------------------------------------------------------------------
txt <- as.character(d$Q2A_OE)
txt[is.na(txt)] <- ""
txt <- trimws(gsub("\r\n?", "\n", txt))
n_code <- sum(txt %in% c("77", "98", "99"))
cat("Q2A_OE cells holding only a 77/98/99 code (treated as empty):", n_code, "\n")
txt[txt %in% c("77", "98", "99")] <- ""
stopifnot(all(validUTF8(txt)))
stopifnot(all((txt != "") == (as.integer(zap_labels(d$Q2A)) %in% 1L)))

h <- data.frame(
  id        = as.character(d$CaseId),
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  # 1 = Casey is a trans woman / trans girl (PGENID 2), 0 = trans man / boy (1)
  treatment = as.integer(gid == "2"),
  age_group = c("adult", "teen")[age],                                   # independently randomized
  conformity = c("conforming", "nonconforming", "ambiguous", "unspecified")[as.integer(conf)],  # independently randomized
  q1_version = q1_version,           # randomized wording within teens; post-hoc from item filled
  restroom_choice = restroom,        # Q1, piped into the text prompt (post-treatment)
  perceived_sex = perceived_sex,     # Q3, after the text
  rnd_sect = as.integer(zap_labels(d$RND_SECT)),  # order of the three combined TESS studies
  weight    = as.numeric(d$WEIGHT1), # kept as a design column; not to be used
  stringsAsFactors = FALSE
)

n_empty <- sum(h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = trans woman):\n")
print(table(h$treatment[!h$exclude]))
cat("Arm x age group:\n");  print(table(h$treatment, h$age_group))
cat("Arm x conformity:\n"); print(table(h$treatment, h$conformity))
cat("Age group x Q1 version:\n"); print(table(h$age_group, h$q1_version, useNA = "ifany"))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
