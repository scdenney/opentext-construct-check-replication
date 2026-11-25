# ==============================================================================
# hopkins2013_conjoint.R -- harmonized file for card hopkins2013_conjoint
# ==============================================================================
# Hopkins, D. J. "Do Voting Criteria Differ across the Levels of the U.S.
# Federal System? A Conjoint Experiment to Understand Voter Decision-making."
# TESS study hopkins365 (TESS3 145), GfK KnowledgePanel, adults in CA, FL, NY,
# TX, fielded Feb-Mar 2014. OSF https://osf.io/c38xe/.
#
# Design (questionnaire raw/TESS3_145_Hopkins_Survey.docx): respondents are
# randomized to an office (XTESS145: 1 President, 2 Governor, 3 Mayor) and see
# seven paired candidate tables (Table1..Table7), each profile with 7 randomized
# attributes (Appendix A): A1 party/ideology, A2 issue (taxes, schools, crime,
# health care, spending, global warming), A3 issue (abortion, same-sex marriage,
# guns), A4 religion, A5 income, A6 race/ethnicity (white p = .5), A7 gender.
# The open item Q4 "Why do you prefer this candidate? Please answer in one
# sentence." (200-character textbox) is asked "only on the first pairing viewed
# by each respondent" (questionnaire, before Q4), on the same page as the Table1
# choice Q1 (Candidate 1 vs 2) and ratings Q2/Q3. So the text refers to Table1:
# A*_Can1_Table1 and A*_Can2_Table1, and "this candidate" is the Q1 choice.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/hopkins2013_conjoint.R
# Input:  data/external/hopkins2013_conjoint/raw/Tess3_145_Hopkins_Client.sav
# Output: data/external/hopkins2013_conjoint/harmonized_hopkins2013_conjoint.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_dir <- "data/external/hopkins2013_conjoint/raw"
out_csv <- "data/external/hopkins2013_conjoint/harmonized_hopkins2013_conjoint.csv"
card_id <- "hopkins2013_conjoint"

d <- read_sav(file.path(raw_dir, "Tess3_145_Hopkins_Client.sav"))
stopifnot(nrow(d) == 551, !anyDuplicated(d$CaseID))
lab <- function(x) as.character(as_factor(x, levels = "labels"))

txt <- d$Q4
Encoding(txt) <- "UTF-8"
txt <- trimws(ifelse(is.na(txt), "", txt))
stopifnot(all(validUTF8(txt)))

# ---- Treatment: office arm (respondent level, equal probability) ------------
# XTESS145 value labels 1 President, 2 Governor, 3 Mayor (questionnaire samvar).
office <- c("1" = "president", "2" = "governor", "3" = "mayor")[as.character(as.numeric(d$XTESS145))]
stopifnot(!anyNA(office))

# ---- Task-1 profiles: chosen vs other ----------------------------------------
# Q1: 1 = Candidate 1, 2 = Candidate 2, refused otherwise (value labels).
attrs <- c(party = "A1", issue_economic = "A2", issue_social = "A3",
           religion = "A4", income = "A5", race = "A6", gender = "A7")
c1 <- sapply(attrs, function(a) lab(d[[paste0(a, "_Can1_Table1")]]))
c2 <- sapply(attrs, function(a) lab(d[[paste0(a, "_Can2_Table1")]]))
stopifnot(!anyNA(c1), !anyNA(c2))
q1 <- as.numeric(d$Q1)
choice <- ifelse(q1 %in% c(1, 2), q1, NA)
chosen <- c1; other <- c2
sw <- which(choice == 2)
chosen[sw, ] <- c2[sw, ]; other[sw, ] <- c1[sw, ]
chosen[is.na(choice), ] <- NA; other[is.na(choice), ] <- NA
colnames(chosen) <- paste0("chosen_", names(attrs))
colnames(other)  <- paste0("other_",  names(attrs))

# Ratings (1-7) of the chosen and the other candidate, Q2 (Cand 1) / Q3 (Cand 2).
r1 <- as.numeric(d$Q2); r2 <- as.numeric(d$Q3)
r1[!r1 %in% 1:7] <- NA; r2[!r2 %in% 1:7] <- NA
rating_chosen <- ifelse(choice == 1, r1, r2)
rating_other  <- ifelse(choice == 1, r2, r1)

# ---- Exclusions --------------------------------------------------------------
# (a) Non-qualified completes: the GfK response-rate report
#     (raw/TESS3_145_Hopkins_-_Response_Rate_Report.doc) states "cases that had
#     refused less than 30% of the questions were kept in the final data for
#     weighting". These 19 cases carry no weight1/weight2; the final sample is
#     532 (OSF study summary "Sample size: 532"). Excluded.
nonqual <- is.na(d$weight1)
stopifnot(sum(nonqual) == 19, identical(nonqual, is.na(d$weight2)))
# (b) Q1 refused: no preferred candidate, so "this candidate" has no referent
#     and chosen/other are undefined (12 cases; 7 with text). Excluded.
#     Assignment (office, Table1 pair) is still randomized for these rows.
q1_refused <- is.na(choice)
exclude <- nonqual | q1_refused
cat("Excluded: non-qualified", sum(nonqual), "| Q1 refused", sum(q1_refused),
    "| either", sum(exclude), "\n")

h <- data.frame(
  id        = as.integer(d$CaseID),   # TESS/GfK CaseID
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = exclude,
  treatment = unname(office),         # office arm: president / governor / mayor
  task      = 1L,                     # Q4 refers to Table1 (first pairing)
  chosen_candidate = choice,          # 1 = Candidate 1 (left), 2 = Candidate 2
  chosen, other,
  rating_chosen = rating_chosen, rating_other = rating_other,
  order_choice_rating = lab(d$DOV_ORDER21),  # choice-first vs rating-first
  resp_party7 = as.numeric(d$XPARTY7),       # 1 Strong Rep ... 7 Strong Dem, 9 missing
  state     = lab(d$PPSTATEN),
  weight1   = as.numeric(d$weight1),         # condition-level weight (not used)
  stringsAsFactors = FALSE, check.names = FALSE
)

# Rows with empty text are dropped (shared brief); Q4_Refused marks 24 of them.
cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
h <- h[h$text != "", ]
write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks ------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
k <- h[!h$exclude, ]
cat("Non-excluded rows per office arm:\n"); print(table(k$treatment))
pd <- as.integer(k$chosen_party != k$other_party)
cat("Non-excluded rows, Table1 party/ideology label differs (1) or same (0):\n"); print(table(pd))
dr <- function(x) ifelse(grepl("Democrat", x), "D", ifelse(grepl("Republican", x), "R", "I"))
cat("Non-excluded rows, Table1 pair has one Democrat and one Republican:",
    sum(paste0(dr(k$chosen_party), dr(k$other_party)) %in% c("DR", "RD")), "\n")
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
