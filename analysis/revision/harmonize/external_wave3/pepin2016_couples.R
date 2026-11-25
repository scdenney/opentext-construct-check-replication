# ==============================================================================
# pepin2016_couples.R -- harmonized files for cards pepin2016_couples (B05)
#                        and pepin2016_couples_leisure (B07)
# ==============================================================================
# Pepin, J. R. "Public Perceptions of Money in Families." TESS study pepin791
# (TESS3 217), GfK KnowledgePanel, US adults, N = 4,020, fielded 18 Jul -
# 10 Aug 2016 (response-rate report). OSF https://osf.io/cug34/.
# Published: Pepin (2019), Journal of Marriage and Family 81:361-379.
#
# Design (questionnaire raw/310.209.00988.1_TESS3217_..._v5.00.docx, Appendix A):
# a vignette couple "Michelle and Anthony, both 31", 24 blocks (XTESS217):
#   marital status: blocks 1-12 "are married", 13-24 "live together"
#   duration:       3 years in 1-6 and 13-18, 7 years in 7-12 and 19-24
#   parental:       "no children" in the first three blocks of each six,
#                   "one child together" in the last three
#   earnings:       (block - 1) %% 3 == 0 Anthony $2,800 / Michelle $1,200;
#                   == 1 Michelle $2,800 / Anthony $1,200; == 2 each $2,000
# Mapping checked below against the DOV_* insert variables.
# B04: a decision about an item (DOV_ITEM: home, mattress, vacation) that one
# partner (DOV_PERSON_B04, random) "ultimately decides since they can't agree";
# fairness 1-4; B05 open: "Please explain why you indicated this decision is
# [B04 answer]" (30-second delay, 2-3 sentences).
# B06: a shared activity (DOV_ACTIVITY: weekend activities, movie) where one
# partner (DOV_PERSON_B06, random) usually accepts the other's opinion;
# fairness 1-4; B07 open: same wording, explains B06.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/pepin2016_couples.R
# Input:  data/external/pepin2016_couples/raw/TESS3_217_Pepin_Client.sav
# Output: data/external/pepin2016_couples/harmonized_pepin2016_couples.csv (B05)
#         data/external/pepin2016_couples/harmonized_pepin2016_couples_leisure.csv (B07)
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_dir <- "data/external/pepin2016_couples/raw"
d <- read_sav(file.path(raw_dir, "TESS3_217_Pepin_Client.sav"))
stopifnot(nrow(d) == 4020, !anyDuplicated(d$CaseID))
lab <- function(x) as.character(as_factor(x, levels = "labels"))

# ---- Block -> factor mapping ---------------------------------------------------
b <- as.numeric(d$XTESS217)
stopifnot(all(b %in% 1:24))
marital  <- ifelse(b <= 12, "married", "cohabiting")
duration <- ifelse(((b - 1) %/% 6) %% 2 == 0, "3_years", "7_years")
parental <- ifelse(((b - 1) %% 6) < 3, "no_children", "one_child")
earnings <- c("man_higher", "woman_higher", "equal")[(b - 1) %% 3 + 1]
# Verify against the inserted-text variables (value labels hold the inserts).
rs <- lab(d$DOV_RELSTAT); rd <- lab(d$DOV_RELDUR); ps <- lab(d$DOV_PARENTST); er <- lab(d$DOV_EARNINGS)
stopifnot(all(grepl("married", rs) == (marital == "married")),
          all(grepl("7", rd) == (duration == "7_years")),
          all(grepl("child together|one child", ps, ignore.case = TRUE) == (parental == "one_child")),
          all(grepl("each earn", er) == (earnings == "equal")),
          all(grepl("Anthony earns \\$2,800", er) == (earnings == "man_higher")))

# ---- Exclusions ----------------------------------------------------------------
# None: all 4,020 are qualified, weighted completes (response-rate report,
# qualification rate 100%; weight non-missing); the proposal states none.

build <- function(item, fair, person, topic_var, topic_name, card_id) {
  txt <- d[[item]]
  Encoding(txt) <- "UTF-8"
  txt <- trimws(ifelse(is.na(txt), "", txt))
  txt <- gsub("\r\n?", "\n", txt)
  stopifnot(all(validUTF8(txt)))
  f <- as.numeric(d[[fair]]); f[!f %in% 1:4] <- NA
  dec <- tolower(lab(d[[person]]))          # partner who decides (B04) / gives in (B06)
  h <- data.frame(
    id        = as.integer(d$CaseID),
    dataset   = card_id,
    country   = "US",
    lang      = "en",
    text      = txt,
    exclude   = FALSE,
    treatment = earnings,                 # man_higher / woman_higher / equal
    marital_status = marital,
    duration  = duration,
    parental_status = parental,
    block     = b,
    person    = dec,
    topic     = lab(d[[topic_var]]),
    fairness  = f,                        # 1 very fair ... 4 very unfair (closed outcome)
    weight    = as.numeric(d$weight),     # not used
    stringsAsFactors = FALSE
  )
  names(h)[names(h) == "person"] <- if (item == "B05") "decider" else "partner_who_gives_in"
  names(h)[names(h) == "topic"]  <- topic_name
  out_csv <- sprintf("data/external/pepin2016_couples/harmonized_%s.csv", card_id)
  cat("\n==", card_id, "(", item, ") ==\n")
  cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
  h <- h[h$text != "", ]
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Rows written:", nrow(h), "\n")
  k <- h[!h$exclude, ]
  cat("Non-excluded rows per earnings arm:\n"); print(table(k$treatment))
  cat("Earnings x marital status:\n"); print(table(k$treatment, k$marital_status))
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  stopifnot(identical(r$text, h$text))
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n")
  stopifnot(n_esc == 0)
}

build("B05", "B04", "DOV_PERSON_B04", "DOV_ITEM", "decision_item", "pepin2016_couples")
build("B07", "B06", "DOV_PERSON_B06", "DOV_ACTIVITY", "activity", "pepin2016_couples_leisure")
