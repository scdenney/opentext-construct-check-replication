# ==============================================================================
# andre2022_primemech.R -- harmonized file for card andre2022_primemech
# ==============================================================================
# Andre, Pizzinelli, Roth & Wohlfart (2022), "Subjective Models of the
# Macroeconomy", REStud 89(6); replication package Zenodo 5838445.
# Wave 4 household survey (Lucid, February 2021, N = 1,521): priming study on
# the interest rate vignette. Arms: control / costs prime / demand prime.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/andre2022_primemech.R
#
# Inputs (data/external/andre2022_primemech/raw/, copied unchanged from the
# package; see MANIFEST.md):
#   prime_text.dta             package out/data/prime_text.dta: the authors'
#                              analysis sample with treatment and hand codes.
#   prime_qualtrics_export.csv package data/prime.csv: raw Qualtrics export.
#
# Why two files: the authors wrote prime_text.dta with foreign::write.dta(
# version = 10) (code/data_cleaning/associations/clean_prime_text_2.R, l. 75),
# which caps strings at 244 bytes. 107 of the 1,521 outcome answers are cut
# off there. The text is therefore taken from the raw export, matched on
# ResponseId; the builder checks that every dta string equals the export
# string or is its 244-byte prefix.
#
# Output: data/external/andre2022_primemech/harmonized_andre2022_primemech.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/andre2022_primemech/raw"
out_csv <- "data/external/andre2022_primemech/harmonized_andre2022_primemech.csv"
card_id <- "andre2022_primemech"

# ---- 1. Authors' analysis sample, design and hand codes ----------------------
# encoding = "latin1": haven fails to parse this R-written (format 114) file
# with its default encoding, presumably because the 244-byte cut splits
# multi-byte characters. Only ASCII columns are taken from it (checked below);
# its text columns are not used.
d <- read_dta(file.path(raw_dir, "prime_text.dta"), encoding = "latin1")
d <- as.data.frame(zap_labels(d))
stopifnot(nrow(d) == 1521, !anyDuplicated(d$ResponseId))

# The dta already applies every exclusion in the authors' cleaning
# (code/data_cleaning/associations/clean_prime.R, ll. 57-69): no Lucid rid,
# survey not reached the rise page (time_int_rise missing), control respondents
# without the end-of-survey prime questions, duplicated rids, and the 1% / 99%
# duration trims. The authors' regressions (code/data_analysis/associations/
# reg_prime.R) use all 1,521 rows with no further exclusion, so exclude = FALSE
# for every row. One control row has Finished = 0 (Progress 97, the respondent
# stopped after the outcome page); the authors keep it, and so do we.
keep_cols <- c("ResponseId", "treatment", "treatment_control", "pi_delta",
               "costs_rise", "demand_rise", "int_costs", "int_demand",
               "int_labor", "mechanism", "approach", "variable", "reviewer")
for (v in keep_cols[vapply(d[keep_cols], is.character, logical(1))]) {
  stopifnot(!any(grepl("[^\x01-\x7f]", d[[v]], useBytes = TRUE)))
}

# ---- 2. Full text from the raw Qualtrics export ------------------------------
# Same read as the authors (clean_prime.R, l. 17 and l. 20): read.csv, drop
# the two Qualtrics header rows. na.strings = character(0) so that an answer
# typed as "NA" stays text (the authors' read would turn it into NA; none
# occurs in the analysis sample, checked below).
q <- read.csv(file.path(raw_dir, "prime_qualtrics_export.csv"), as.is = TRUE,
              na.strings = character(0), encoding = "UTF-8")
q <- q[-(1:2), c("ResponseId", "treatment", "int_open", "int_open_prime")]
stopifnot(all(d$ResponseId %in% q$ResponseId), !anyDuplicated(q$ResponseId))
q <- q[match(d$ResponseId, q$ResponseId), ]
stopifnot(all(q$treatment == d$treatment))

# Outcome text: int_open (control; "Your thoughts") or int_open_prime (costs
# and demand arms; "Your thoughts in step 2"). This is the authors' `open`
# (clean_prime.R, l. 87). The other arm's column must be empty.
ctl <- d$treatment == "control"
stopifnot(all(ifelse(ctl, q$int_open_prime, q$int_open) == ""))
text_full <- ifelse(ctl, q$int_open, q$int_open_prime)
Encoding(text_full) <- "UTF-8"
stopifnot(all(validUTF8(text_full)))

# Check against the authors' (truncated) column: equal, or a 244-byte prefix.
dta_open <- foreign::read.dta(file.path(raw_dir, "prime_text.dta"),
                              convert.factors = FALSE)$open
same <- dta_open == text_full
cut244 <- !same & nchar(dta_open, type = "bytes") == 244 &
  startsWith(text_full, dta_open)
cat("Text check vs prime_text.dta: identical", sum(same), "| truncated at 244 bytes",
    sum(cut244), "| mismatched", sum(!same & !cut244), "\n")
stopifnot(all(same | cut244))

# ---- 3. Assemble --------------------------------------------------------------
h <- data.frame(
  id        = d$ResponseId,                 # authors' respondent id (Qualtrics)
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = trimws(text_full),
  exclude   = FALSE,                        # see section 1
  treatment = d$treatment,                  # "control" / "costs" / "demand"
  # Closed-ended outcome: predicted change in inflation, -1 ... +1 p.p.
  # (pi_int_rise; clean_prime.R, l. 82). Asked on the same page, before the text.
  pi_delta  = d$pi_delta,
  # Step-1 answers (1 = firms' costs rise / demand rises). In the primed arms
  # these are the prime itself, asked before the text; in the control arm they
  # were asked at the end of the survey, after the text, to a random half each
  # (control_question). Post-treatment for control: use only for heterogeneity.
  costs_rise  = d$costs_rise,
  demand_rise = d$demand_rise,
  control_question = ifelse(ctl, d$treatment_control, NA_character_),
  # Hand codes (one coder per response; Online Appendix F, fn. 10). Groups from
  # code/specs/narratives_coded.json; built in clean_prime_text_2.R, ll. 37-43.
  human_int_costs  = as.integer(d$int_costs),   # Firms' costs (+)
  human_int_demand = as.integer(d$int_demand),  # Product demand (-)
  human_int_labor  = as.integer(d$int_labor),   # Labor demand (-)
  human_mechanism  = as.integer(d$mechanism),   # response type "mechanism"
  human_approach   = d$approach,                # cleaned response-type codes
  human_variable   = d$variable,                # cleaned mechanism-element codes
  human_reviewer   = d$reviewer,                # coder initial (R / W / P)
  stringsAsFactors = FALSE
)
# Responses not coded as "mechanism" carry the literal string "NA" in
# `variable` (clean_prime_text_1.R, l. 138 pastes the split of an NA code);
# store them as "" (no mechanism element coded).
h$human_approach[is.na(h$human_approach) | h$human_approach == "NA"] <- ""
h$human_variable[is.na(h$human_variable) | h$human_variable == "NA"] <- ""
stopifnot(all((h$human_variable != "") == (h$human_mechanism == 1)))

# Rows with empty text are dropped (shared brief); none expected.
n_empty <- sum(h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- 4. Checks ----------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
cat("Non-excluded rows per arm:\n")
print(table(h$treatment[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
