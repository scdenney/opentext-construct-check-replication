# ==============================================================================
# ces2022_uma.R -- harmonized file for card ces2022_uma_juror
# ==============================================================================
# Source data: Nteta, T. (2024). CES 2022, Team Module of the University of
# Massachusetts, Amherst (UMA). Harvard Dataverse doi:10.7910/DVN/AV7AEO (CC0 1.0).
# Paper reporting the juror experiment (Study 1): Rhodes, J., Nteta, T., & Rice, D.
# (2025). Partisan bias in juror decision-making. Journal of Law & Empirical
# Analysis 2(2), 308-323, doi:10.1177/2755323X251366728. Replication archive
# doi:10.7910/DVN/2LZDER.
#
# Only one card is built from this module. The refugee-visa (UMA324 x UMA323rand)
# and deportation (UMA411 x UMA410rand) experiments failed inclusion criterion (ii)
# (no author statement about the 2022 manipulations found); no file is written.
#
# Treatment mapping (UMA403rand, "Vignette 1".."Vignette 4" in the questionnaire,
# raw/UMA22_post.docx, block "UMA403 randomization" and page p_UMA403):
#   1 = conservative defendant (Fox News, "crazy right-winger"), prosecution first
#   2 = conservative defendant, defense first
#   3 = progressive defendant (MSNBC, "crazy liberal"), prosecution first
#   4 = progressive defendant, defense first
# The questionnaire lists the four page versions in this order without display
# conditions; see MAPPING_SOURCE below for how the order was confirmed.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/ces2022_uma.R
# ==============================================================================
suppressPackageStartupMessages(library(data.table))

DS   <- "ces2022_uma"
RAW  <- file.path("data/external", DS, "raw")
CARD <- "ces2022_uma_juror"
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))
# How far the mapping is verified: the questionnaire export lists the page versions
# without display conditions, and the module codebook was not available. The
# authors' replication archive (doi:10.7910/DVN/2LZDER) is unpublished (API 404 on
# 2026-11-17). Supporting evidence: (a) in the same export, the labelled refugee
# randomization UMA323rand (A Control, B Voluntary, C Forced) matches the order in
# which its page versions are listed; (b) closed-ended verdicts (UMA404) show the
# copartisan pattern the article reports in both parties only if {1,2} are the
# conservative arms. The order labels (prosecution/defense first) rest on the
# listing order alone and are used only as a stratum.
MAPPING_SOURCE <- "questionnaire listing order; partisanship split consistent with reported verdict pattern; order labels unverified"

d <- fread(file.path(RAW, "CCES22_UMA_OUTPUT_vv.tab"), colClasses = "character",
           encoding = "UTF-8", na.strings = NULL)
stopifnot(nrow(d) == 1000, !anyDuplicated(d$caseid))

# Empty text: blank, or the export's not-asked placeholder "-1". The 179 rows with
# a blank UMA403rand are the respondents who did not take the post wave
# (tookpost == 1); their UMA406 holds only "-1", so they carry no answer and drop
# out here. (MANIFEST.md said these rows "still hold text"; corrected there.)
clean_text <- function(x) { x <- gsub("[\r\n\t]+", " ", x); x <- trimws(x); x[is.na(x)] <- ""
  x[x %in% c("-1", "__NA__", "-99", ".")] <- ""; x }
num <- function(x) suppressWarnings(as.numeric(x))

arm  <- num(d$UMA403rand)
pid3 <- num(d$pid3)                       # pre-wave: 1 Democrat, 2 Republican, 3 Independent, 4 Other, 5 Not sure
pid7 <- num(d$pid7)                       # 1-3 Democrat (incl. leaners), 4 Independent, 5-7 Republican, 8 Not sure
treatment <- ifelse(arm %in% c(1, 2), 1L, ifelse(arm %in% c(3, 4), 0L, NA_integer_))  # 1 = conservative (Fox) defendant
# Copartisan defendant: Republicans with the conservative defendant, Democrats with
# the progressive one; NA for independents, others, not sure. Party from pid3
# (the paper speaks of "self-identified party"); pid7 kept for a leaner variant.
copartisan <- ifelse(pid3 == 2, treatment, ifelse(pid3 == 1, 1L - treatment, NA_integer_))

h <- data.frame(
  id = d$caseid, dataset = CARD, country = "US", lang = "en",
  text = clean_text(d$UMA406),
  # Exclusions: rows with no randomized assignment (none survive the empty-text
  # drop). The paper uses all 821 post-wave respondents (Study 1: Data, footnote
  # on 821 responses); no further exclusions.
  exclude = is.na(arm),
  treatment = treatment,
  copartisan = copartisan,
  order = ifelse(arm %in% c(1, 3), "prosecution_first", ifelse(arm %in% c(2, 4), "defense_first", NA)),
  uma403rand = arm,
  pid3 = pid3, pid7 = pid7,
  verdict = ifelse(d$UMA404 == "1", "guilty", ifelse(d$UMA404 == "2", "not_guilty", NA)),  # closed-ended UMA404
  sentence_months = num(d$UMA405),                                                         # 0-60 slider UMA405
  stringsAsFactors = FALSE)
h$sentence_months[!is.na(h$sentence_months) & (h$sentence_months < 0 | h$sentence_months > 60)] <- NA
cat("Treatment mapping source:", MAPPING_SOURCE, "\n")
cat("Rows before dropping empty text:", nrow(h), "\n")
h <- h[nzchar(h$text), ]
stopifnot(!anyDuplicated(h$id), !any(h$exclude))
# No human codes: the paper's GPT-4o thematic summaries are not deposited per answer.

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (1 = conservative/Fox, 0 = progressive/MSNBC) by order:\n")
print(table(treatment = h$treatment[!h$exclude], order = h$order[!h$exclude]))
cat("Non-excluded rows: copartisan defendant (1) vs counter-partisan (0), NA = not a pid3 partisan:\n")
print(table(copartisan = h$copartisan[!h$exclude], pid3 = h$pid3[!h$exclude], useNA = "ifany"))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"), na.strings = "")
cat("Answers that are the literal string NA:", sum(h$text == "NA"), "\n")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
