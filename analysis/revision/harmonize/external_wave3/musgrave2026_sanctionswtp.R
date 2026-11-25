# ==============================================================================
# musgrave2026_sanctionswtp.R -- harmonized file for card musgrave2026_sanctionswtp
# ==============================================================================
# Source: Musgrave, P. (2026). Pay Any Price? Measuring Americans' Willingness-to-Pay
# for Sanctions on Russia and China. Foreign Policy Analysis 22(3), orag014,
# doi:10.1093/fpa/orag014. Data: Harvard Dataverse doi:10.7910/DVN/KCJFCI (CC0 1.0),
# 2022 MTurk/CloudResearch experiment (July 7-8, 2022). Pre-registered at
# osf.io/b74te (Online Appendix H).
#
# Item: sanctionsDVtext (codebook var. 54, "Sanctions Evaluation (Free Text)").
# Online Appendix I, item 13: "Please briefly explain why you voted that way."
# Asked of everyone after the efficacy quotation, the referendum vote at the
# initial price (item 11) and the certainty item (item 12).
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/musgrave2026_sanctionswtp.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "musgrave2026_sanctionswtp"
CARD <- "musgrave2026_sanctionswtp"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# WTP_2022_Russia_Ukraine.dta is the deposited Stata original (Dataverse file
# 13626704, ?format=original), used instead of the .tab ingest, whose multi-line
# answers break naive parsing. Codebook header: obs 2,030, ID unique.
d <- read_dta(file.path(RAW, "WTP_2022_Russia_Ukraine.dta"))
stopifnot(nrow(d) == 2030, !anyDuplicated(d$ID), !anyNA(d$ID))

# Randomized factors (codebook vars. 84 and 88/128; pre-registration q7, q14):
# price1 in {0.10, 0.25, 0.50, 1.00, 2.00} (393/402/421/427/387) and efficacy
# high/low (993/1,037). Neither is missing.
price <- as.numeric(d$price1)
eff   <- as.character(d$efficacy)
stopifnot(!anyNA(price), all(price %in% c(.1, .25, .5, 1, 2)),
          all(eff %in% c("high", "low")),
          identical(as.integer(eff == "high"), as.integer(d$efficacy_num)),
          sum(eff == "high") == 993)
price_lab <- c("0.1" = "usd010", "0.25" = "usd025", "0.5" = "usd050",
               "1" = "usd100", "2" = "usd200")
arm <- unname(price_lab[as.character(price)])
stopifnot(!anyNA(arm))

# Text: whitespace runs (including line breaks) collapsed and trimmed. A literal
# "NA" (from an earlier R export, per the searcher's count) is treated as empty.
# Encoding repair: in the deposited .dta every non-ASCII answer (49) is UTF-8 text
# that was once decoded as Mac Roman and re-encoded (e.g. a right single quote
# stored as the three characters U+201A U+00C4 U+00F4; the same damage shows in
# efficacytext, codebook var. 89). Re-encoding those strings to Mac Roman and
# reading the bytes as UTF-8 restores them; checked by count: all 49 convert, all
# are valid UTF-8 afterwards, and the only non-ASCII characters left are U+2014,
# U+2019, U+201C and U+201D.
fix_macroman <- function(x) {
  m <- grepl("[^\001-\177]", x, useBytes = TRUE)
  y <- iconv(x[m], "UTF-8", "macintosh")
  stopifnot(!anyNA(y)); Encoding(y) <- "UTF-8"; stopifnot(all(validUTF8(y)))
  x[m] <- y
  x
}
clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))
  x <- fix_macroman(x)
  trimws(gsub("[[:space:]]+", " ", x))
}
text <- clean_text(d$sanctionsDVtext)
cat("Literal 'NA' answers treated as empty:", sum(text == "NA"), "\n")
text[text == "NA"] <- ""

# Closed outcomes. sanctionsDV_num: 1 = "Yes, maintain", 0 = "No" or "Don't know"
# (article, Design section: "Don't know" coded no; codebook var. 120).
# sanctionsDVconfidence_num: certainty 0-10 (var. 121). pid3: 0 Dem, 1 Ind, 2 Rep,
# measured before the vignette (var. 92). driver: commutes by car (var. 103), the
# author's H3 moderator.
num <- function(x) as.numeric(zap_labels(x))

# Encoding counts (rows with text), before writing.
enc_counts <- function(x) c(
  rows = length(x),
  non_ascii = sum(grepl("[^\001-\177]", x, useBytes = TRUE)),
  replacement_char = sum(grepl("�", x, fixed = TRUE)),
  mojibake = sum(grepl("â€|Ã[\u0080-¿]|‚Ä", x)))

cat("Per price arm x efficacy: assigned, non-empty text\n")
lv <- unname(price_lab)
print(table(price = factor(arm, lv), efficacy = eff))
print(table(price = factor(arm, lv)[nzchar(text)], efficacy = eff[nzchar(text)]))

# Exclusions: none. The article's analyses use the full file (WTP_2022_Figure3-6.R
# l. 26-27: data.experiment = full read; data.confidence, confidence >= 8, is a
# robustness subsample only). The pre-registration (H.4.2) uses attention checks
# and speeding only for robustness. Every row has both randomized factors.
h <- data.frame(id = as.character(d$ID), dataset = CARD, country = "US", lang = "en",
                text = text, exclude = FALSE, treatment = arm,
                efficacy_high = as.integer(eff == "high"), price1 = price,
                sanctions_support = num(d$sanctionsDV_num),
                confidence = num(d$sanctionsDVconfidence_num),
                pid3 = num(d$pid3), driver = num(d$driver),
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

cat("\nEncoding counts before writing:\n"); print(enc_counts(h$text))
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per price arm:\n")
print(table(treatment = factor(h$treatment[!h$exclude], lv)))
cat("Non-excluded rows per efficacy arm (1 = high):\n")
print(table(efficacy_high = h$efficacy_high[!h$exclude]))
cat("Primary contrast: high price (usd100, usd200) =",
    sum(!h$exclude & h$treatment %in% c("usd100", "usd200")),
    "; low price (usd010, usd025) =",
    sum(!h$exclude & h$treatment %in% c("usd010", "usd025")), "\n")
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
cat("Encoding counts after re-read:\n"); print(enc_counts(r$text))
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
