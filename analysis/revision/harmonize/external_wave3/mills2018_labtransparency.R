# ==============================================================================
# mills2018_labtransparency.R -- harmonized file for card mills2018_labtransparency
# ==============================================================================
# Mills, Han, Robbins & Weary (2018), "Institutional transparency improves
# public perception of lab animal technicians and support for animal research",
# PLoS ONE 13(2): e0193262, doi:10.1371/journal.pone.0193262.
# Data: figshare doi:10.6084/m9.figshare.5755308.v1 (CC BY 4.0).
# 3 (species: cows, dogs, mice) x 2 (facility transparency: low, high) vignette,
# MTurk, August-September 2016, N = 550 (the authors' final sample).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/mills2018_labtransparency.R
#
# Input: data/external/mills2018_labtransparency/raw/l.xlsx (figshare file
#   "Lab Transparency Data_Figshare.xlsx", unchanged). One sheet, one row per
#   participant, one header row (Qualtrics id + question wording).
# Output: data/external/mills2018_labtransparency/harmonized_mills2018_labtransparency.csv
#
# Text item: Q115 "Please explain your answer." after Q77 "How honest is Cathy?"
# (the third of six rating + explanation pairs; article section 2.2, Table 1).
# The other five explanation items (Q113 warm, Q114 trustworthy, Q116
# intelligent, Q117 competent, Q118 confident) are not used by this card.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(readxl))

raw_file <- "data/external/mills2018_labtransparency/raw/l.xlsx"
out_csv  <- "data/external/mills2018_labtransparency/harmonized_mills2018_labtransparency.csv"
card_id  <- "mills2018_labtransparency"

d <- as.data.frame(read_excel(raw_file, sheet = 1, col_types = "text"))
stopifnot(nrow(d) == 550, ncol(d) == 39)

# Columns are taken by position and checked against their header wording.
hdr <- names(d)
stopifnot(hdr[1] == "Participant",
          identical(hdr[3:8], c("cows_lab", "dogs_lab", "mice_lab",
                                "cows_farm", "dogs_farm", "mice_farm")),
          startsWith(hdr[13], "Q77 - How honest is Cathy?"),
          startsWith(hdr[14], "Q115 - Please explain your answer."),
          grepl("How transparent do you believe the facility is", gsub("\\s+", " ", hdr[28])),
          startsWith(hdr[34], "Q128 - 50 States"),
          startsWith(hdr[35], "Q130 - Have you lived in the United States"))
stopifnot(!anyDuplicated(d$Participant), !anyNA(d$Participant))

# ---- Treatment ----------------------------------------------------------------
# Exactly one of the six cell dummies is non-missing per row.
cells <- !is.na(d[, 3:8])
stopifnot(all(rowSums(cells) == 1))
cell <- hdr[3:8][max.col(cells)]

# "lab" = LOW transparency, "farm" = HIGH transparency. Verified two ways:
# (1) cell sizes match article Table 2 one-to-one (LT Dog 93, LT Cow 95,
#     LT Mouse 91, HT Dog 83, HT Cow 90, HT Mouse 98 = dogs_lab 93, cows_lab 95,
#     mice_lab 91, dogs_farm 83, cows_farm 90, mice_farm 98);
# (2) the transparency check Q73 averages 2.3 in the lab cells and 5.1 in the
#     farm cells, as reported for low vs high in article section 3.1
#     (2.3 +/- 1.56 vs 5.1 +/- 1.51). The names follow the authors' two model
#     facilities (rodent lab in a basement vs dairy research farm; Introduction).
# The vignette itself does not say lab or farm: it varies only the six
# bracketed transparency phrases (article section 2.2).
low_transp <- grepl("_lab$", cell)
q73 <- as.numeric(d[[28]])
m_low <- mean(q73[low_transp]); m_high <- mean(q73[!low_transp])
cat(sprintf("Transparency check Q73: lab cells %.2f, farm cells %.2f\n", m_low, m_high))
stopifnot(abs(m_low - 2.3) < 0.05, abs(m_high - 5.1) < 0.05)
stopifnot(identical(as.vector(table(cell)[hdr[3:8]]), c(95L, 93L, 91L, 90L, 83L, 98L)))

# ---- Exclusions ---------------------------------------------------------------
# The file holds the authors' final sample of 550 (article section 3: 614
# recruited, minus incomplete 8, failed attention check 44, invariant
# responses 12). No further exclusion is reproducible: the authors also left
# out of the qualitative analysis responses "that did not answer the question"
# (section 2.3), a content judgment that cannot be reproduced without reading
# the text, so exclude = FALSE for every row.

# ---- Country ------------------------------------------------------------------
# The article (section 2.1) reports 474 US and 76 Canadian residents, but the
# file has no country column. Q128 (US state) has code 53 for 72 rows, all of
# whom answered 2 on Q130 ("lived in the US for most of your life"); code 53 is
# probably "I do not live in the US", but no codebook is deposited and 72 is
# not 76. country is therefore "US" (recruitment frame) for all rows, and the
# raw indicator is kept as non_us_q128 (unverified).
q128 <- as.integer(d[[34]])

h <- data.frame(
  id        = d$Participant,                       # authors' participant id
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  # Q115, after "How honest is Cathy?". CR / CRLF line breaks become \n
  # (read.csv does not round-trip \r); no other change to the text.
  text      = trimws(gsub("\r\n?", "\n", d[[14]])),
  exclude   = FALSE,                               # see Exclusions above
  treatment = as.integer(low_transp),              # 1 = low transparency, 0 = high
  # Independently randomized second factor (article section 2.2).
  species   = sub("_(lab|farm)$", "", cell),       # "cows" / "dogs" / "mice"
  # Closed-ended ratings, 1-7 (Table 1). rating_honest is the rating the text
  # explains; warm and trustworthy complete the warmth scale.
  rating_honest      = as.integer(d[[13]]),
  rating_warm        = as.integer(d[[9]]),
  rating_trustworthy = as.integer(d[[11]]),
  transparency_check = as.integer(d[[28]]),        # Q73, 1-7, asked after the text
  non_us_q128        = q128 == 53,                 # see Country above
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text)))
n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
cat("Non-excluded rows per arm (1 = low transparency):\n")
print(table(h$treatment[!h$exclude]))
cat("Arm x species:\n")
print(table(h$treatment, h$species))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
