# ==============================================================================
# johnson2021_algofair.R -- harmonized file for card johnson2021_algofair
# ==============================================================================
# Johnson, R. A., & Zhang, S. (2021). Optimizing Schools? Public Perceptions of
# Algorithmic versus Status Quo Prioritization in K-12 Schooling. TESS study
# johnson1389 (TESS 094), NORC AmeriSpeak, general population plus parent
# oversample, fielded 10/22-12/10/2021. OSF https://osf.io/v3bfk/.
# Design: 4 status-quo methods x 3 district ethnoracial contexts = 12 cells.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/johnson2021_algofair.R
#
# Input:  data/external/johnson2021_algofair/raw/TESS094_Johnson_FinalData.dta
# Output: data/external/johnson2021_algofair/harmonized_johnson2021_algofair.csv
#
# Text item: Q4 "Explain why you think [INSERT RESPONSE AT Q3] is fairer than
# [INSERT RESPONSE THAT WAS NOT SELECTED AT Q3]." (questionnaire
# raw/_8041.094__TESS_Johnson_vFINALsimple.docx; shown if Q3 = 1, 2). Q2 (the
# comprehension item, "explain ... what it means for the school counselor to
# use a predictive model") is not carded.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/johnson2021_algofair/raw/TESS094_Johnson_FinalData.dta"
out_csv  <- "data/external/johnson2021_algofair/harmonized_johnson2021_algofair.csv"
card_id  <- "johnson2021_algofair"

d <- read_dta(raw_file)
stopifnot(nrow(d) == 5911, !anyDuplicated(d$CaseId))
num <- function(v) as.integer(zap_labels(v))
cond <- num(d$P_CONDITION)
stopifnot(all(cond %in% 1:12))

# ---- Cell -> factor mapping (questionnaire, "Question 1" grid; .dta value labels)
# 1-3 counselor discretion, 4-6 parent requests, 7-9 simple rule (test score and
# family income cutoff), 10-12 weighted lottery; within each method:
# majority Black and Hispanic, integrated, majority White.
sq_lab  <- c("counselor_discretion", "parent_requests", "simple_rule", "weighted_lottery")
ctx_lab <- c("majority_black_hispanic", "integrated", "majority_white")
status_quo <- sq_lab[(cond - 1L) %/% 3L + 1L]
context    <- ctx_lab[(cond - 1L) %% 3L + 1L]
labs <- names(attr(d$P_CONDITION, "labels"))
stopifnot(all(mapply(function(s, c, l)
  grepl(c(counselor_discretion = "Counselor discretion", parent_requests = "Parent requests",
          simple_rule = "Simple rule", weighted_lottery = "Weighted lottery")[s], l, fixed = TRUE) &&
  grepl(c(majority_black_hispanic = "Majority Black and Hispanic", integrated = "Integrated",
          majority_white = "Majority White")[c], l, fixed = TRUE),
  status_quo, context, labs[cond])))

# ---- Text -----------------------------------------------------------------------
# Encoding: the .dta (Stata 118) declares UTF-8, but 648 Q4 strings hold
# Windows-1252 bytes (0x92 apostrophe, 0x93/0x94 quotes, 0x85 ellipsis, ...),
# and no string holds a multi-byte UTF-8 character. Strings that are not valid
# UTF-8 are converted from CP1252; ASCII strings are unchanged.
# Missing-value codes: NORC stores the item's code 98 (SKIPPED ON WEB) as the
# string "98" in 118 cells; set to empty. 52 respondents who skipped Q3 were
# not asked Q4 (empty).
# Line breaks normalized: \r\n and lone \r become \n (read.csv round-trip).
txt <- as.character(zap_labels(d$Q4))
bad <- !is.na(txt) & !validUTF8(txt)
cat("Q4 strings converted from CP1252:", sum(bad), "\n")
txt[bad] <- iconv(txt[bad], "CP1252", "UTF-8")
stopifnot(!anyNA(txt[bad]))
txt <- trimws(gsub("\r\n?", "\n", txt))
is_code <- txt %in% c("77", "98", "99")
cat("Q4 cells holding a missing-value code:", sum(is_code), "\n")
txt[is_code] <- NA

# ---- Exclusions -----------------------------------------------------------------
# QUAL = 9 "Invalid Complete" (268 rows): not among the 5,643 interviews NORC
# delivered (raw/8041.094_TESS_Johnson_Project_Report.pdf, p. 2, "NORC
# collected 5,643 interviews--4,308 general population ... and 1,335 parent
# oversample"; QUAL = 1 gives exactly 4,308 + 1,335), carry no weight, and have
# a median duration of 1 minute (7 for valid completes). The study page also
# gives N = 5,643. No other exclusion is stated in the proposal.
qual <- num(d$QUAL)
stopifnot(sum(qual == 1) == 5643, sum(qual == 1 & num(d$P_OVER) == 1) == 4308)
exclude <- qual != 1

nn <- function(v, ok) { v <- num(v); ifelse(v %in% ok, v, NA) }   # 77/98/99 -> NA
h <- data.frame(
  id        = as.character(d$CaseId),
  dataset   = card_id,
  country   = "US",
  lang      = "en",                          # SURV_LANG = 1 (English) for all rows
  text      = txt,
  exclude   = exclude,
  treatment = status_quo,                    # status-quo method the algorithm is contrasted with
  context   = context,                       # district ethnoracial composition, randomized independently
  condition = cond,                          # P_CONDITION 1-12
  chose     = ifelse(num(d$Q3) == 1, "status_quo", ifelse(num(d$Q3) == 2, "algorithm", NA)),  # Q3; sets Q4 wording
  certainty = nn(d$Q5, 1:5),                 # 1 status quo definitely fairer ... 5 model definitely fairer
  faster    = nn(d$Q6, 1:2),                 # 1 status quo, 2 model saves most time
  after_update = nn(d$Q8, 1:2),              # after bias update: 1 model, 2 status quo
  parent_status = nn(d$QS1, 1:3),            # 1 K-12 parent, 2 former/other parent, 3 never
  oversample = as.integer(num(d$P_OVER) == 2),
  party7    = nn(d$PartyID7, 1:7),
  weight_genpop = as.numeric(d$WEIGHT_Genpop),   # kept as design columns; not used
  weight_parent = as.numeric(d$WEIGHT_Parent),
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text[!is.na(h$text)])))
cat("Rows with empty text dropped:", sum(is.na(h$text) | h$text == ""), "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -----------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (status-quo method):\n")
print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows, method x context:\n")
print(table(h$treatment[!h$exclude], h$context[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
