# ==============================================================================
# kwon2025_productjourney.R -- harmonized file for card kwon2025_productjourney
# ==============================================================================
# Kwon, Liu & Haws (2026), "It takes one to buy but two to say goodbye",
# Journal of Marketing Research 63(1): 127-148, doi:10.1177/00222437251361007.
# Data: Harvard Dataverse doi:10.7910/DVN/R6GB0B, CC BY-NC-ND 4.0. Research
# use is permitted; the licence forbids distributing derivatives, so the
# harmonized CSV stays under data/ (not version-controlled) and must not be
# posted in the replication archive. This script may be shared.
#
# Study 4A (preregistered AsPredicted #222638): 2-arm between-subjects,
# acquisition (buy a mug) vs disposal (donate a mug), Prolific respondents
# living with a spouse or romantic partner, N = 603. Open text: thought
# listing right after the scenario (S4A Stimuli.pdf, "Set-up + Thought-listing").
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/kwon2025_productjourney.R
#
# Input: data/external/kwon2025_productjourney/raw/S4A_Data.sav (the deposited
#   SPSS original, fetched via the Dataverse queue, batch w3card_kwon_01).
#   The Dataverse .tab ingest (raw/S4A_Data.tab) is NOT used: it splits long
#   strings into 8-character-named segments (THOUGHT, THOUGE, ...), and its
#   THOUGHT column is shorter than the original for 73 of 603 answers.
# Output: data/external/kwon2025_productjourney/harmonized_kwon2025_productjourney.csv
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/kwon2025_productjourney/raw/S4A_Data.sav"
out_csv  <- "data/external/kwon2025_productjourney/harmonized_kwon2025_productjourney.csv"
card_id  <- "kwon2025_productjourney"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 603, !anyDuplicated(d$ID), !anyNA(d$ID))

# ---- Treatment ----------------------------------------------------------------
# IV_disposal1purchase0, label "IV (decision stage: 0=acquisition, 1=disposal)";
# used as the IV in S4A Syntax.sps (LOGISTIC REGRESSION ... IV_disposal1purchase0).
tr <- as.integer(d$IV_disposal1purchase0)
stopifnot(all(tr %in% 0:1), identical(as.vector(table(tr)), c(302L, 301L)))

# ---- Text ---------------------------------------------------------------------
# `thought` is the authors' coalesce of thought_acquisition (QID408) and
# thought_disposal (QID418) (variable label). Verify that each row's text sits
# in the column of its own arm and equals `thought`.
own <- ifelse(tr == 1, d$thought_disposal, d$thought_acquisition)
other <- ifelse(tr == 1, d$thought_acquisition, d$thought_disposal)
stopifnot(all(own == d$thought), all(is.na(other) | trimws(other) == ""))

# ---- Exclusions ---------------------------------------------------------------
# Preregistration Q6: "All participants who complete the study will be
# included in analysis"; the file holds the 603 analysed completers and all
# analyses in S4A Syntax.sps and S4A Syntax_For R.R use every row. The
# preregistered robustness check without "gibberish" answers has no flag in
# the data and would need reading the text, so exclude = FALSE for all rows.

# ---- Country ------------------------------------------------------------------
# Not recorded in the file. Prolific sample; the scenario names a Goodwill
# store (a US/Canadian chain). "US" is an assumption, flagged in the card.

lab0 <- function(x) as.integer(zap_labels(x))

h <- data.frame(
  id        = as.character(d$ID),                  # authors' participant ID
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  # CR / CRLF become \n (none present); otherwise unchanged apart from trimming.
  text      = trimws(gsub("\r\n?", "\n", d$thought)),
  exclude   = FALSE,                               # see Exclusions above
  treatment = tr,                                  # 1 = disposal, 0 = acquisition
  # Closed-ended outcome asked after the text: 0 = solo, 1 = joint decision.
  dv_joint  = lab0(d$DV),
  # Human codes of this text (S4A Coding Guide.pdf; hypothesis-blind coders per
  # README). MR_final 1 = no/very low, 2 = implicit, 3 = explicit desire for
  # clarity about the partner's valuation. MR_main = MR_final >= 2 and
  # MR_robust = MR_final == 3 (checked below). MP and MP_item1-3 are the
  # respondents' self-codes of their own text (1-7).
  human_mr_main   = lab0(d$MR_main),
  human_mr_robust = lab0(d$MR_robust),
  human_mr_final  = lab0(d$MR_final),
  human_mr_coder1 = lab0(d$MR_coder1),
  human_mr_coder2 = lab0(d$MR_coder2),
  human_self_mp    = as.numeric(d$MP),
  human_self_item1 = lab0(d$MP_item1),
  human_self_item2 = lab0(d$MP_item2),
  human_self_item3 = lab0(d$MP_item3),
  stringsAsFactors = FALSE
)
stopifnot(all(h$human_mr_main == (h$human_mr_final >= 2)),
          all(h$human_mr_robust == (h$human_mr_final == 3)))

stopifnot(all(validUTF8(h$text)))
n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
cat("Non-excluded rows per arm (1 = disposal):\n")
print(table(h$treatment[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
