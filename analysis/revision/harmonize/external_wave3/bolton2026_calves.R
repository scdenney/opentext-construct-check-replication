# ==============================================================================
# bolton2026_calves.R -- harmonized file for card bolton2026_calves
# ==============================================================================
# Bolton, S. E., Sirovica, L. V., & von Keyserlingk, M. A. G. (2026). When is
# the problem truly "solved"? Australian public attitudes toward future
# scenarios addressing early life killing of surplus dairy calves. Journal of
# Dairy Science 109(5): 5347-5355, doi:10.3168/jds.2025-28099.
# Data: Borealis doi:10.5683/SP3/0CVJRZ (CC BY 4.0).
# 2 (pasture / feedlot) x 2 (industry commitment / none) vignette, Australia
# (Pureprofile panel), December 2022; 1,000 completed, 999 analyzed.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/bolton2026_calves.R
#
# Inputs (Borealis originals; the .tab names are the download names, the
# content is the deposited CSV, sha256 matches ?format=original):
#   raw/S4_QualitativeDataset.tab  (1,000 rows: Treatment, Participant ID and
#     the three explanation items; authors' qualitative file, UTF-8)
#   raw/S2_QuantitativeDataset.tab (Qualtrics export, 2 header rows + 1,000
#     rows; arm in the block column prefix, e.g. PasNoCom9.1)
# Output: data/external/bolton2026_calves/harmonized_bolton2026_calves.csv
#
# Text item: "<arm>9.4 Please provide an explanation for your responses
# above:" after the three items on the life led by the animals (S1 survey,
# each treatment block; article Table 2). S4 column "Animal qual response".
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(data.table))

raw_dir <- "data/external/bolton2026_calves/raw"
out_csv <- "data/external/bolton2026_calves/harmonized_bolton2026_calves.csv"
card_id <- "bolton2026_calves"

q <- fread(file.path(raw_dir, "S4_QualitativeDataset.tab"), encoding = "UTF-8",
           colClasses = "character")
s <- fread(file.path(raw_dir, "S2_QuantitativeDataset.tab"), encoding = "UTF-8",
           colClasses = "character", header = TRUE)
s <- s[-(1:2)]                       # Qualtrics rows 2-3: wording and ImportId
stopifnot(nrow(q) == 1000, nrow(s) == 1000)
ne <- function(v) !is.na(v) & trimws(v) != ""

# ---- Arms ---------------------------------------------------------------------
# S4 Treatment labels; the four S2 block prefixes match the S1 survey blocks
# (checked against the vignette text in S1: Past./no comm. = PasNoCom, etc.;
# article Table 1). Readme: 1 PasNoCom, 2 PasCom, 3 FLNoCom, 4 FLCom.
map <- c(PastureNoCommit = "PasNoCom", PastureCommit = "PasCom",
         FLNoCommit = "FLNoCom", FLCommit = "FLCom")
stopifnot(all(q$Treatment %in% names(map)))
q$arm <- unname(map[q$Treatment])
arms <- unname(map)
shown <- sapply(arms, function(a) ne(s[[paste0(a, "9.1")]]))
stopifnot(all(rowSums(shown) == 1))
s$arm <- arms[max.col(shown)]
stopifnot(identical(as.vector(table(q$arm)[arms]), as.vector(table(s$arm)[arms])))

# ---- Text ---------------------------------------------------------------------
# Each S4 answer is stored as "<Participant ID>: <answer>" (the label the
# authors used for quotes; Methods). The prefix is removed. S4 is used for the
# text, not S2: S2's text columns hold mojibake in 96 animal answers (UTF-8
# read as Windows-1252, e.g. curly apostrophes), S4 is clean UTF-8.
pid <- q[["Participant ID"]]
stopifnot(!anyDuplicated(pid), all(startsWith(q[["Animal qual response"]], paste0(pid, ":"))))
strip <- function(t, id) trimws(substring(t, nchar(id) + 2))
txt <- strip(q[["Animal qual response"]], pid)

# ---- Link S4 to S2 (for the authors' exclusion and the closed items) -----------
# No shared id. Rows are linked on arm + the three answers reduced to lower-case
# ASCII letters (insensitive to the S2 mojibake); the link must be one-to-one.
nz <- function(x) gsub("[^a-z]", "", tolower(iconv(x, "UTF-8", "ASCII", sub = "")))
g <- function(k) vapply(seq_len(nrow(s)), function(i) s[[paste0(s$arm[i], "9.", k)]][i], "")
k2 <- paste(s$arm, nz(g(4)), nz(g(9)), nz(g(19)))
k4 <- paste(q$arm, nz(txt), nz(strip(q[["Industry qual response"]], pid)),
            nz(strip(q[["Farmers qual response"]], pid)))
stopifnot(!anyDuplicated(k2))
m <- match(k4, k2)
stopifnot(!anyNA(m), !anyDuplicated(m))
cat("S4 rows linked one-to-one to S2:", sum(!is.na(m)), "\n")
s <- s[m]

# ---- Exclusion ----------------------------------------------------------------
# Article Methods and Readme: "One respondent was removed from the sample due to
# missing demographic data, resulting in a final sample of 999." In S2 exactly
# one row has an empty Q2 (age) and all other demographic items filled.
miss_dem <- !ne(s$Q2) | !ne(s$Q3) | !ne(s$Q4) | !ne(s$Q5) | !ne(s$Q6) | !ne(s$Q7)
stopifnot(sum(miss_dem) == 1)
# All 1,000 passed the attention check (Q99 = "Agree") and finished (S2).
stopifnot(all(s$Q99 == "Agree"), all(s$Finished == "TRUE"))

# ---- Closed items on the animals' life (S3 SAS code, lines 120-144) ------------
lik <- function(v, lv) { x <- match(v, lv); stopifnot(!anyNA(x)); x }
agree_lv  <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree",
               "Somewhat agree", "Agree", "Strongly agree")
approp_lv <- c("Completely inappropriate", "Inappropriate", "Slightly inappropriate",
               "Neither inappropriate nor appropriate", "Slightly appropriate",
               "Appropriate", "Completely appropriate")
accept_lv <- c("Totally unacceptable", "Unacceptable", "Somewhat unacceptable",
               "Neither unacceptable nor acceptable", "Somewhat acceptable",
               "Acceptable", "Totally acceptable")
a1 <- lik(g(1), agree_lv); a2 <- lik(g(2), approp_lv); a3 <- lik(g(3), accept_lv)

h <- data.frame(
  id        = pid,                                  # authors' S4 Participant ID
  dataset   = card_id,
  country   = "AU",
  lang      = "en",
  text      = txt,
  exclude   = miss_dem,
  # 1 = feedlot (pens, no access to grass), 0 = pasture (open paddocks, grass)
  treatment = as.integer(q$arm %in% c("FLNoCom", "FLCom")),
  # Independently randomized second factor: industry commitment to end early
  # life slaughter (1) vs "has occurred for decades" (0).
  commitment = as.integer(q$arm %in% c("PasCom", "FLCom")),
  arm       = tolower(q$arm),
  animal_agree = a1, animal_approp = a2, animal_accept = a3,
  animal_attitude = (a1 + a2 + a3) / 3,             # authors' construct (Methods)
  response_id = s$ResponseId,                       # Qualtrics id in S2
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text)))
n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = feedlot):\n")
print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows, feedlot x commitment:\n")
print(table(feedlot = h$treatment[!h$exclude], commitment = h$commitment[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
