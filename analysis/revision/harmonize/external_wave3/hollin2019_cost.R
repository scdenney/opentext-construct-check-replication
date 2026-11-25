# ==============================================================================
# hollin2019_cost.R -- harmonized file for card hollin2019_cost
# ==============================================================================
# TESS study hollin1121 (I. Hollin), "Price Disclosure Laws for
# Direct-to-Consumer Pharmaceutical Advertising", fielded by NORC AmeriSpeak
# 2019. Published as Hollin & Ball (2022), Exploratory Research in Clinical and
# Social Pharmacy 8: 100180, doi:10.1016/j.rcsop.2022.100180.
# 3 (price transparency: no price, list price, price plus) x 3 (health
# condition: DVT/PE, diabetes, RA) between-subjects design, N = 2,138.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/hollin2019_cost.R
#
# Input: data/external/hollin2019_cost/raw/PN8041.052_TESS052_Hollin_Final_20191231.sav
# Output: data/external/hollin2019_cost/harmonized_hollin2019_cost.csv
#
# Text item: question 3 of the instrument (TESS-1121.R2_description.docx,
# "Questions", item 3), asked right after the out-of-pocket cost estimate Q2,
# in one of two branches:
#   Q3A "Please explain why you think it would cost you that amount."
#       (shown if Q2 was a dollar amount)
#   Q3B "Please explain why you do not know what it would cost you."
#       (shown if Q2 was "I don't know")
# No row has text in both. The two branches are combined into one text column
# (q3_branch records which), as the authors did (Hollin & Ball 2022, section
# 2.5: the free text "provided reasons for respondents' cost estimates or
# inability to provide a guess"). Combining avoids selecting on Q2, which is
# post-treatment: the share answering Q3A rises with price disclosure.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/hollin2019_cost/raw/PN8041.052_TESS052_Hollin_Final_20191231.sav"
out_csv  <- "data/external/hollin2019_cost/harmonized_hollin2019_cost.csv"
card_id  <- "hollin2019_cost"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 2138, !anyDuplicated(d$CaseId), !anyNA(d$CaseId))
stopifnot(all(as.vector(d$SURV_LANG) == 1))   # 1 = English for every row

# ---- Treatment ----------------------------------------------------------------
# P_TRANSP (value labels in the .sav and codebook): 1 No price (control),
# 2 List price only, 3 Price plus (list price + typical cost to patients).
# P_CONDITION: 1 DVT/PE, 2 Diabetes, 3 RA. Both assigned by NORC ("DATA ONLY");
# proposal Table 1 (arms 1-9 = condition x transparency).
# Check on the mapping: Q7_2 (recalled "Price" among the ad's contents) is
# selected by 56 of 697 in arm 1 vs 307 of 718 and 319 of 723 in arms 2-3.
tr  <- as.integer(as.vector(d$P_TRANSP))
cnd <- as.integer(as.vector(d$P_CONDITION))
stopifnot(identical(as.vector(table(tr)), c(697L, 718L, 723L)))
rec_price <- as.integer(as.vector(d$Q7_2))
stopifnot(sum(rec_price[tr == 1] == 1) == 56, sum(rec_price[tr == 2] == 1) == 307,
          sum(rec_price[tr == 3] == 1) == 319)

# ---- Text ---------------------------------------------------------------------
# Q3A / Q3B are labelled character variables; codebook codes "77" DON'T KNOW,
# "98" SKIPPED ON WEB, "99" REFUSED are not answers and become empty.
clean_txt <- function(v) {
  x <- as.character(as.vector(v))
  x[is.na(x)] <- ""
  code <- x %in% attr(v, "labels")
  cat("  missing-value codes:", sum(code), "\n")
  x[code] <- ""
  trimws(gsub("\r\n?", "\n", x))   # CR / CRLF -> \n (read.csv does not round-trip \r)
}
cat("Q3A\n"); a <- clean_txt(d$Q3A)
cat("Q3B\n"); b <- clean_txt(d$Q3B)
stopifnot(!any(a != "" & b != ""))
txt    <- ifelse(a != "", a, b)
branch <- ifelse(a != "", "A", ifelse(b != "", "B", NA))

# ---- Closed outcome -----------------------------------------------------------
# Q2 out-of-pocket estimate in dollars; 77777 = "I don't know",
# 99998 = skipped, 99999 = refused (value labels). No outlier trimming here:
# the authors trimmed above the 95th percentile per condition only for the
# cost-estimate analysis (section 2.5).
q2 <- as.numeric(as.vector(d$Q2))
q2_dk <- q2 == 77777
q2[q2 %in% c(77777, 99998, 99999)] <- NA

# ---- Exclusions ---------------------------------------------------------------
# None. The archive is the authors' analytic sample of 2,138 (section 3.1).
# For the text, the authors dropped "missing and irrelevant responses" (section
# 3.2, 1,898 kept): "irrelevant" is a content judgment that cannot be
# reproduced without reading the text, so exclude = FALSE for every row.

h <- data.frame(
  id        = as.character(d$CaseId),     # NORC CaseId
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  treatment = c("control", "list_price", "price_plus")[tr],
  condition = c("dvt_pe", "diabetes", "ra")[cnd],   # independently randomized factor
  q3_branch = branch,                     # A = explained a dollar estimate, B = explained not knowing
  q2_cost_estimate = q2,                  # dollars; NA if don't know / skipped / refused
  q2_dont_know = q2_dk,
  recalled_price = rec_price,             # Q7_2, asked after the text
  weight    = as.numeric(d$WEIGHT),       # post-stratification weight; not used
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
cat("Arm x condition:\n"); print(table(h$treatment, h$condition))
cat("Arm x branch (A = gave an amount):\n"); print(table(h$treatment, h$q3_branch))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
