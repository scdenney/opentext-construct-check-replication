# ==============================================================================
# williamson2017_taxes.R -- harmonized file for card williamson2017_taxes
# ==============================================================================
# Williamson, V. "'The Taxpayer Gap': Perceptions of the Taxpaying Population
# and Opposition to Welfare Spending." TESS study williamson859 (TESS 003),
# NORC AmeriSpeak, web only, English only, 1,527 interviews, fielded
# 31 Mar - 24 Apr 2017 (field report raw/8041.003 TESS Williamson Field Report
# v2.pdf, p. 2). OSF https://osf.io/bsd7j/. Publication reporting the
# experiment: Williamson, V. S. (2018). Who are "The Taxpayers"? The Forum
# 16(3): 399-418 (abstract read; full text not accessed).
#
# Design (fielded questionnaire raw/[8041.003] TESS Williamson_v3clean.docx;
# proposal raw/Williamson859.pdf, PDF pp. 5-13): GROUP preload, "RANDOM
# ASSIGNMENT TO ONE OF THE THREE CONDITIONS, IF POSSIBLE BALANCED BY PARTY ID":
#   1 = COND1 federal income tax information (Q2 chart + attention check),
#   2 = COND2 all taxes information (Q3), 3 = CONTROL internet-use chart (Q4).
# Then, for all respondents: Q5 % of adults who pay taxes, Q6 are you a
# taxpayer, Q7 "What are you glad that tax dollars pay for?" (textbox),
# Q8 "What are you upset that tax dollars pay for?" (textbox; this card),
# Q9A-C food stamps / Social Security / military spending, Q10 efficacy,
# Q11A-B fair share.
#
# Exclusions: none stated in the proposal; exclude = FALSE throughout. Every
# row has a GROUP value (1,527 rows). Weights kept, not used.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/williamson2017_taxes.R
# Input:  data/external/williamson2017_taxes/raw/TESS003_Williamson_27July17.sav
# Output: data/external/williamson2017_taxes/harmonized_williamson2017_taxes.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_file <- "data/external/williamson2017_taxes/raw/TESS003_Williamson_27July17.sav"
out_csv  <- "data/external/williamson2017_taxes/harmonized_williamson2017_taxes.csv"
card_id  <- "williamson2017_taxes"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 1527, !anyDuplicated(d$CaseId))
code_na <- function(x, codes = c(77, 98, 99, 998)) { x <- as.numeric(x); x[x %in% codes | x < 0] <- NA; x }

# Text. Questionnaire: "Please code refusals in CAWI: 98 IMPLICIT REFUSAL, WEB
# SKIP"; the item flag Q8 (TEXTBOX ENTRY / Skipped on Web) marks the 6 skips.
# A text cell equal to a skip code ("98", "99", "-1", "77") is treated as empty
# (none occur in Q8_OE). Other digit-only answers are kept as answers.
txt <- as.character(zap_labels(d$Q8_OE))
Encoding(txt) <- "UTF-8"
txt <- trimws(ifelse(is.na(txt), "", txt))
txt <- gsub("\r\n?", "\n", txt)        # CRLF -> LF so the file re-reads identically
stopifnot(all(validUTF8(txt)))
is_code <- txt %in% c("98", "99", "-1", "77")
cat("Text equal to a skip code (set empty):", sum(is_code), "\n")
txt[is_code] <- ""
cat("Other digit-only answers kept:", sum(grepl("^-?[0-9]+$", txt)), "\n")

# English only, web only (questionnaire header; SURV_MODE all web).
stopifnot(all(as.numeric(d$SURV_MODE) == as.numeric(d$SURV_MODE[1])))

# Treatment: GROUP value labels 1 COND1, 2 COND2, 3 CONTROL; COND1/COND2 are
# the FIT and AT conditions (questionnaire: SHOW IF GROUP=1 federal income tax
# chart, GROUP=2 all-taxes chart, GROUP=3 internet chart).
g <- as.numeric(d$GROUP)
stopifnot(all(g %in% 1:3))
arm <- c("federal_income_tax", "all_taxes", "control")[g]

# Attention check after the chart (correct: Q2 = 3 "-7", Q3 = 3 "19",
# Q4 = 3 "74"); post-treatment, kept as a design column, not an exclusion.
chk <- ifelse(g == 1, as.numeric(d$Q2), ifelse(g == 2, as.numeric(d$Q3), as.numeric(d$Q4)))
attention_correct <- ifelse(chk %in% 1:4, chk == 3, NA)

h <- data.frame(
  id        = as.integer(d$CaseId),          # AmeriSpeak CaseId
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  treatment = arm,
  attention_correct = attention_correct,
  pct_pay_taxes = code_na(d$Q5),             # Q5 slider 0-100; 998 skipped
  taxpayer   = c("yes", "no")[match(as.numeric(d$Q6), 1:2)],
  food_stamps_spending = c("increase", "decrease", "same")[match(as.numeric(d$Q9A), 1:3)],
  social_security_spending = c("increase", "decrease", "same")[match(as.numeric(d$Q9B), 1:3)],
  military_spending = c("increase", "decrease", "same")[match(as.numeric(d$Q9C), 1:3)],
  pid1      = as.character(as_factor(d$PID1)),   # possible blocking variable (questionnaire)
  party_id7 = code_na(d$PartyID7),           # 1 strong Dem ... 7 strong Rep; -1 unknown -> NA
  hh_income = code_na(d$INCOME),             # 1-18 brackets; subgroup factor in the proposal
  weight    = as.numeric(d$WEIGHT1),         # not used
  stringsAsFactors = FALSE
)

cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
h <- h[h$text != "", ]
write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks --------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
