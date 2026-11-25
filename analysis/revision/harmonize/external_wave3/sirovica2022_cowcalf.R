# ==============================================================================
# sirovica2022_cowcalf.R -- harmonized file for card sirovica2022_cowcalf
# ==============================================================================
# Sirovica, L. V., Ritter, C., Hendricks, J., Weary, D. M., Gulati, S., & von
# Keyserlingk, M. A. G. (2022). Public attitude toward and perceptions of dairy
# cattle welfare in cow-calf management systems differing in type of social
# and maternal contact. Journal of Dairy Science 105(4): 3248-3268,
# doi:10.3168/jds.2021-21344.
# Data: Borealis doi:10.5683/SP3/GQ8DUY (CC0 1.0).
# One factor, four cow-calf management systems, between subjects; Canada
# (MTurk, Sep-Oct 2019, n = 307) and US (Prime Panels, Feb-Mar 2020, n = 1,487).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/sirovica2022_cowcalf.R
#
# Inputs (Borealis originals; .tab names are the download names, the content is
# the deposited CSV, sha256 matches ?format=original):
#   raw/Final_CAUSdataset.tab (S2; authors' cleaned sample, 1,794 rows)
#   raw/CAUSdataset_with_IDs_and_constructs.tab (S4; S2 plus anonymous ID and
#     constructs, exported from the S3 SAS code)
# Output: data/external/sirovica2022_cowcalf/harmonized_sirovica2022_cowcalf.csv
#
# Text item: `Qual`, "Please explain your views about this cow-calf management
# system." (Readme variable 2; article, Survey Design), directly after the
# three attitude items (agree, appropriate, acceptable).
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(data.table))

raw_dir <- "data/external/sirovica2022_cowcalf/raw"
out_csv <- "data/external/sirovica2022_cowcalf/harmonized_sirovica2022_cowcalf.csv"
card_id <- "sirovica2022_cowcalf"

s2 <- fread(file.path(raw_dir, "Final_CAUSdataset.tab"), encoding = "UTF-8")
s4 <- fread(file.path(raw_dir, "CAUSdataset_with_IDs_and_constructs.tab"), encoding = "UTF-8")
stopifnot(nrow(s2) == 1794, nrow(s4) == 1794)

# ---- Id -----------------------------------------------------------------------
# S4 adds the anonymous ID used to label quotes (S3 SAS code: ID =
# cats(Country, _n_), i.e. country + row number of S2). S4 is S2 in the same
# row order: checked on every shared closed-ended column below. The text is
# taken from S2 because S4's Qual is cut by the SAS export (S4 max 489
# characters vs S2 max 1,179; 22 rows differ).
for (v in c("System", "Agree", "Appropriate", "Acceptable", "Country", "numCountry",
            "Health_Calf", "Feelings_Calf", "NatlLife_Calf", "Duration_seconds"))
  stopifnot(identical(s2[[v]], s4[[v]]))
stopifnot(!anyDuplicated(s4$ID))
cat("S4 Qual is a prefix of S2 Qual in", sum(startsWith(s2$Qual, s4$Qual)), "of", nrow(s2), "rows\n")

# ---- Arms ---------------------------------------------------------------------
# Readme variable 1: SI separated and individually housed; SG separated and
# group housed; FC separated and kept with foster cow; NS not separated and
# group housed (article Table 1 for the vignette text).
stopifnot(all(s2$System %in% c("SI", "SG", "FC", "NS")))

# ---- Exclusions ----------------------------------------------------------------
# None. The deposited S2 is the authors' analytic sample after their attention
# check and duplicate removal (Participant Recruitment and Selection: 307 CA +
# 1,487 US = 1,794), and all its rows enter their analyses.

h <- data.frame(
  id        = s4$ID,                              # authors' anonymous ID (S4)
  dataset   = card_id,
  country   = s2$Country,                         # CA / US (Readme variable 39)
  lang      = "en",
  # 0 answers hold CR or LF (checked when drafting); trimws only.
  text      = trimws(s2$Qual),
  exclude   = FALSE,
  treatment = tolower(s2$System),                 # si, sg, fc, ns
  # Attitude items (1-7, Readme 4-6) and the authors' attitude construct
  # (S3 SAS code: (Agree + Appropriate + Acceptable)/3); all asked before the text.
  agree = s2$Agree, appropriate = s2$Appropriate, acceptable = s2$Acceptable,
  attitude = s4$attitude,
  stringsAsFactors = FALSE
)
stopifnot(isTRUE(all.equal(h$attitude, (h$agree + h$appropriate + h$acceptable) / 3)))

stopifnot(all(validUTF8(h$text)), !any(grepl("[\r\n]", h$text)))
n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n")
print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows, arm x country:\n")
print(table(h$treatment[!h$exclude], h$country[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
