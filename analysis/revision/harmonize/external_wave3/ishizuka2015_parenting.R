# ==============================================================================
# ishizuka2015_parenting.R -- harmonized files for cards
#   ishizuka2015_parenting         (Q3, why the respondent rated the parent so)
#   ishizuka2015_parenting_advise  (Q1, how the respondent would advise the parent)
# ==============================================================================
# TESS study ishizuka661 (TESS3 191), "Preferences or Constraints? Education,
# Gender, and Parenting Attitudes", P. Ishizuka; fielded by GfK 15 May - 24 Sep
# 2015 on parents of children under 18 (main sample + augment of poor parents).
# Article: Ishizuka, P. (2019). Social class, gender, and contemporary parenting
# standards in the United States: Evidence from a national survey experiment.
# Social Forces 98(1): 31-58, doi:10.1093/sf/soy107 (not read).
# Data: OSF https://osf.io/bbehq/ (TESS archive, public).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/ishizuka2015_parenting.R
#
# Input:  data/external/ishizuka2015_parenting/raw/TESS3_191_Ishizuka_Client.sav
# Output: data/external/ishizuka2015_parenting/harmonized_ishizuka2015_parenting.csv
#         data/external/ishizuka2015_parenting/harmonized_ishizuka2015_parenting_advise.csv
#
# Reading the strings. haven::read_sav cannot parse this file ("Unknown
# error"). foreign::read.spss can, but it does not join SPSS very long strings:
# the file dictionary (very long string record, subtype 14) declares Q1 and Q3
# as 3,072-byte strings, stored as 13 segment variables each (Q1, Q10-Q19,
# Q1A, Q1B; Q3, Q30-Q39, Q3A, Q3B; widths 12 x 255 + 48). foreign returns each
# segment as its own column, so reading only `Q1` cuts every answer at 255
# bytes. Each full segment carries 255 bytes of text (ReadStat,
# readstat_sav_read.c, drops 1 of the 256 slot bytes per non-final segment;
# confirmed here: bytes 253-255 of the first segment are never padding in rows
# whose second segment is used). The builder pastes the segments back in file
# order, as raw bytes, then checks UTF-8. No answer reaches the 3,072-byte
# limit (checked below), so no answer is truncated in the deposit itself.
#
# Questionnaire (raw/TESS3_191_-_Ishizuka_Questionnaire_Final.docx):
#   parent gender is assigned once per respondent (within the XTESS191
#   education x gender block) and holds for both vignettes. Vignette 1: one of
#   6 situations x child gender, no parent behavior described; Q1 "How would
#   you advise [name] to respond in this situation?" (text box, same page).
#   Vignette 2: a situation from a different domain x concerted cultivation
#   (CC) or natural growth (NG) response x child gender; Q2 rating (excellent
#   ... poor); Q3 "What was it about [name]'s parenting that led you to rate it
#   as [Q2 answer]?" (text box, next screen with the vignette shown again).
#
# Exclusions: the proposal (raw/Ishizuka661.pdf) names none; TESS files are
# the fielded sample, so exclude = FALSE for every row with an assignment.
# Weights are kept as a design column and not used.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(foreign))

raw_file <- "data/external/ishizuka2015_parenting/raw/TESS3_191_Ishizuka_Client.sav"
out_dir  <- "data/external/ishizuka2015_parenting"

d <- suppressWarnings(read.spss(raw_file, to.data.frame = FALSE, reencode = FALSE,
                                use.value.labels = FALSE))
stopifnot(length(d$CaseID) == 3642, !anyDuplicated(d$CaseID))

# ---- Join very long string segments ------------------------------------------------
join_segments <- function(stem) {
  segs <- c(stem, paste0(stem, 0:9), paste0(stem, c("A", "B")))
  stopifnot(all(segs %in% names(d)))
  full <- segs[-length(segs)]
  # every non-final segment must come back as exactly 255 bytes (no trimming)
  for (s in full) stopifnot(all(nchar(d[[s]], type = "bytes") == 255))
  x <- do.call(paste0, lapply(segs, function(s) d[[s]]))
  Encoding(x) <- "UTF-8"
  stopifnot(all(validUTF8(x)))
  x <- trimws(x)
  nb <- nchar(x, type = "bytes")
  cat(stem, ": answers longer than 255 bytes (cut by a 255-byte reader):",
      sum(nb > 255), "| longest (bytes):", max(nb),
      "| at the 3,072-byte limit:", sum(nb >= 3072), "\n")
  stopifnot(all(nb < 3072))
  x
}
q1 <- join_segments("Q1")
q3 <- join_segments("Q3")

# ---- Design variables ----------------------------------------------------------------
# DOV_VIGNETTE01 value labels (merged first-vignette code, 1-24): within each
# situation s, codes 4(s-1)+1..4 = MOTHER/SON, MOTHER/DAUGHTER, FATHER/SON,
# FATHER/DAUGHTER.
v1 <- d$DOV_VIGNETTE01
stopifnot(all(v1 %in% 1:24))
sit1    <- (v1 - 1) %/% 4 + 1
par1    <- ifelse((v1 - 1) %% 4 < 2, "mother", "father")
child1  <- ifelse(v1 %% 2 == 1, "son", "daughter")
# DOV_VIGNETTE02 value labels (merged second-vignette code, 1-48): within each
# situation s, codes 8(s-1)+1..4 = CONCERTED CULTIVATION MOTHER/SON,
# MOTHER/DAUGHTER, FATHER/SON, FATHER/DAUGHTER; +5..8 = NATURAL GROWTH, same order.
v2 <- d$DOV_VIGNETTE02
stopifnot(all(v2 %in% 1:48))
sit2    <- (v2 - 1) %/% 8 + 1
cc2     <- as.integer((v2 - 1) %% 8 < 4)
par2    <- ifelse((v2 - 1) %% 4 < 2, "mother", "father")
child2  <- ifelse(v2 %% 2 == 1, "son", "daughter")

# Checks against the separate assignment variables (DOV_Q1_PARENT,
# DOV_Q2Q3_PARENT: 1 = Mother, 2 = Father) and the questionnaire rules.
stopifnot(all(par1 == ifelse(d$DOV_Q1_PARENT == 1, "mother", "father")),
          all(par2 == ifelse(d$DOV_Q2Q3_PARENT == 1, "mother", "father")),
          all(par1 == par2))
# Domains (proposal Table 1 / questionnaire exclusion rule): situations 1-2,
# 3-4, 5-6; the second vignette comes from a different domain.
stopifnot(all((sit1 - 1) %/% 2 != (sit2 - 1) %/% 2))
cat("First/second situations in different domains: TRUE\n")

nm_mom <- c("Michelle", "Kim", "Nicole", "Angela", "Vanessa", "Alicia")
nm_dad <- c("Michael", "Kevin", "Brandon", "Anthony", "David", "Russell")
name1 <- ifelse(par1 == "mother", nm_mom[d$DOV_MOMNAME1], nm_dad[d$DOV_DADNAME1])
name2 <- ifelse(par2 == "mother", nm_mom[d$DOV_MOMNAME2], nm_dad[d$DOV_DADNAME2])
stopifnot(!anyNA(name1), !anyNA(name2))

# XTESS191 randomization block: 1 female college+, 2 male college+,
# 3 female less than college, 4 male less than college (questionnaire Samvar).
block <- c("female_college", "male_college", "female_noncollege",
           "male_noncollege")[d$XTESS191]
# XISHIZUKA: 1 main sample, 2 augment (poor parents, fpl100 = 1).
sample_ <- c("main", "augment_poor")[d$XISHIZUKA]
# Q2: 1 excellent ... 6 poor, -1 refused (kept as NA).
rating <- ifelse(d$Q2 %in% 1:6, d$Q2, NA)

common <- data.frame(
  id = d$CaseID, country = "US", lang = "en", stringsAsFactors = FALSE)

# ---- Card 1: Q3 (CC vs NG second vignette) -----------------------------------------
h3 <- data.frame(
  id            = common$id,
  dataset       = "ishizuka2015_parenting",
  country       = "US",
  lang          = "en",
  text          = q3,
  exclude       = FALSE,
  # 1 = concerted cultivation (intensive) behavior, 0 = natural growth
  treatment     = cc2,
  situation     = sit2,
  parent_gender = par2,
  child_gender  = child2,
  parent_name   = name2,
  block         = block,
  sample        = sample_,
  # closed outcome, 1 = excellent ... 6 = poor; post-treatment, inserted in Q3
  rating        = rating,
  weight        = d$weight,
  stringsAsFactors = FALSE)

# ---- Card 2: Q1 (mother vs father, first vignette) ---------------------------------
h1 <- data.frame(
  id            = common$id,
  dataset       = "ishizuka2015_parenting_advise",
  country       = "US",
  lang          = "en",
  text          = q1,
  exclude       = FALSE,
  # 1 = vignette parent is a mother, 0 = father
  treatment     = as.integer(par1 == "mother"),
  situation     = sit1,
  child_gender  = child1,
  parent_name   = name1,
  block         = block,
  sample        = sample_,
  weight        = d$weight,
  stringsAsFactors = FALSE)

write_checked <- function(h, card) {
  out_csv <- file.path(out_dir, paste0("harmonized_", card, ".csv"))
  n_empty <- sum(is.na(h$text) | h$text == "")
  h <- h[!is.na(h$text) & h$text != "", ]
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n==", card, "==\nRows with empty text dropped:", n_empty,
      "| rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
  cat("Non-excluded rows per arm:\n"); print(table(treatment = h$treatment[!h$exclude]))
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  stopifnot(identical(r$text, h$text))
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n")
  stopifnot(n_esc == 0)
  invisible(h)
}

h3w <- write_checked(h3, "ishizuka2015_parenting")
cat("Arm x situation (Q3 card):\n"); print(table(h3w$situation, h3w$treatment))
cat("Arm x parent gender (Q3 card):\n"); print(table(h3w$parent_gender, h3w$treatment))
cat("Arm x child gender (Q3 card):\n"); print(table(h3w$child_gender, h3w$treatment))
h1w <- write_checked(h1, "ishizuka2015_parenting_advise")
cat("Arm x situation (Q1 card):\n"); print(table(h1w$situation, h1w$treatment))
cat("Arm x child gender (Q1 card):\n"); print(table(h1w$child_gender, h1w$treatment))
