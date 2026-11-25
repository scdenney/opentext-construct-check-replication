# ==============================================================================
# ces2018_uma.R -- harmonized file for card ces2018_uma_juror
# ==============================================================================
# Source data: Nteta, T. (2022). CCES 2018, Team Module of University of
# Massachusetts Amherst (UMA). Harvard Dataverse doi:10.7910/DVN/HWGUH4 (CC0 1.0).
# Paper reporting the juror experiment: Rice, D., Rhodes, J. H., & Nteta, T.
# (2022). Same as it ever was? The impact of racial resentment on white juror
# decision-making. Journal of Politics 84(2), doi:10.1086/715991.
#
# Only one card is built from this module. The veil item (UMA410 x UMA407_treat)
# failed inclusion criterion (ii) (no author statement found) and gets no file.
#
# Treatment mapping, read from the survey code in raw/UMA18_post.txt (the code
# that displayed the pages, not inferred from order):
#   lines 153-181: page VignetteA_pg if Vignette_treat in [1] -> Bradley Schwartz,
#     prosecution paragraph first; B [2] -> Jamal Gaines, prosecution first;
#     C [3] -> Bradley Schwartz, defense first; D [4] -> Jamal Gaines, defense first.
#   lines 188-191: UMA405_insert = "Bradley Schwartz" if Vignette_treat in [1,3],
#     "Jamal Gaines" if in [2,4] (name piped into UMA405 and UMA406).
#   line 196: UMA406 asked if UMA404 (verdict) answered; wording pipes the verdict.
#   line 93: Vignette_treat assigned with equal weights [1,1,1,1].
# Confirmed by the authors' replication code (doi:10.7910/DVN/LEV1RR,
#   replication-code.r lines 22-25): drVignette = 1 if Vignette_treat is
#   "Version B" or "Version D" (codes 2 and 4), i.e. Jamal Gaines; the order flip
#   is not treated as a separate treatment. The script applies no race filter; the
#   deposited analysis file is presumably already restricted to white respondents
#   (paper title; not checked, the file was not downloaded).
# Racial resentment: pre-wave grid UMA305a-c (raw/UMA18_pre.txt lines 381-389),
#   1 = strongly agree ... 5 = strongly disagree. a and b are pro-Black wording
#   (agree = low resentment), c is resentful wording (agree = high resentment).
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/ces2018_uma.R
# ==============================================================================
suppressPackageStartupMessages(library(data.table))

DS   <- "ces2018_uma"
RAW  <- file.path("data/external", DS, "raw")
CARD <- "ces2018_uma_juror"
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# The .tab is the deposited tab-separated original (not a Dataverse ingest of
# another format). Read everything as character so no answer is coerced.
d <- fread(file.path(RAW, "CCES18_UMA_OUTPUT_vv.tab"), colClasses = "character",
           encoding = "UTF-8", na.strings = NULL)
stopifnot(nrow(d) == 1000, !anyDuplicated(d$caseid))

# Empty text: blank, or the export's not-asked placeholders. Respondents who did
# not take the post wave (tookpost == 1, 141 rows) have a blank Vignette_treat and
# no answer. Rows with empty text are kept out (brief).
clean_text <- function(x) { x <- gsub("[\r\n\t]+", " ", x); x <- trimws(x); x[is.na(x)] <- ""
  x[x %in% c("-1", "__NA__", "-99", ".")] <- ""; x }
num <- function(x) suppressWarnings(as.numeric(x))

vt <- num(d$Vignette_treat)
rr_items <- cbind(a = num(d$UMA305a), b = num(d$UMA305b), c = num(d$UMA305c))
rr_items[!(rr_items %in% 1:5)] <- NA
# Resentment scored 0-1, high = more resentful: for a and b disagreeing is
# resentful ((x - 1) / 4); for c agreeing is resentful ((5 - x) / 4).
rr <- rowMeans(cbind((rr_items[, "a"] - 1) / 4, (rr_items[, "b"] - 1) / 4, (5 - rr_items[, "c"]) / 4))

h <- data.frame(
  id = d$caseid, dataset = CARD, country = "US", lang = "en",
  text = clean_text(d$UMA406),
  # Exclusions: (1) no randomized assignment (none survive the empty-text drop);
  # (2) non-white respondents: the article analyses white respondents only
  # (title and abstract, "white juror decision-making"); race == 1 is White in the
  # CCES profile coding (UMA18_post.txt uses `race in [1]` for white-only items).
  exclude = is.na(vt) | d$race != "1",
  treatment = as.integer(vt %in% c(2, 4)),                  # 1 = Jamal Gaines, 0 = Bradley Schwartz
  order = ifelse(vt %in% c(1, 2), "prosecution_first", ifelse(vt %in% c(3, 4), "defense_first", NA)),
  vignette_treat = vt,
  white = d$race == "1",
  racial_resentment = round(rr, 4),                          # 0-1, mean of UMA305a-c (NA if any item missing)
  rr_high = as.integer(rr > 0.5),                            # above the scale midpoint (pre-specified split)
  pid3 = num(d$pid3),
  verdict = ifelse(d$UMA404 == "1", "guilty", ifelse(d$UMA404 == "2", "not_guilty", NA)),  # closed-ended UMA404
  sentence_months = num(d$UMA405),                           # 0-60 slider UMA405
  stringsAsFactors = FALSE)
h$sentence_months[!is.na(h$sentence_months) & (h$sentence_months < 0 | h$sentence_months > 60)] <- NA
h$exclude[is.na(h$exclude)] <- TRUE
cat("Rows before dropping empty text:", nrow(h), "\n")
h <- h[nzchar(h$text), ]
stopifnot(!anyDuplicated(h$id), all(!is.na(h$vignette_treat)))
# No human codes: none deposited.

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Excluded rows (non-white):", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (0 = Bradley Schwartz, 1 = Jamal Gaines) by order:\n")
print(table(treatment = h$treatment[!h$exclude], order = h$order[!h$exclude]))
cat("Non-excluded rows per arm by racial resentment above midpoint (NA = item missing):\n")
print(table(treatment = h$treatment[!h$exclude], rr_high = h$rr_high[!h$exclude], useNA = "ifany"))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"), na.strings = "")
cat("Answers that are the literal string NA:", sum(h$text == "NA"), "\n")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
