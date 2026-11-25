# ==============================================================================
# zarpli2023_turkeysanctions.R -- harmonized file for card zarpli2023_turkeysanctions
# ==============================================================================
# Source: Zarpli, O. (2023). Sanctions and target public opinion: Experimental
# evidence from Turkey. International Interactions 49(3), 424-453,
# doi:10.1080/03050629.2023.2184814. Data: Harvard Dataverse doi:10.7910/DVN/KQ3Q2D
# (CC0 1.0). Pre-registered at EGAP (article, footnote 1).
#
# Item: QID54. Article p. 441 and footnote 6: asked after the policy-change item
# (QID22/policy_change), which follows the vignette: "In 1-2 sentences, please
# explain why you responded the way you did to the previous question" (English
# rendering in the article; the Turkish wording is not in the archive).
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/zarpli2023_turkeysanctions.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "zarpli2023_turkeysanctions"
CARD <- "zarpli2023_turkeysanctions"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# Survey_Data.dta is the deposited Stata original of the Dataverse ingest
# `Survey Data.tab` (fetched as original). no_typos.csv (the authors' typo-corrected
# text used by Turkish_Sentiment_Analysis.Rmd) is not used: it has no respondent id,
# 1,218 rows against 1,219, and recodes blanks to "-99", so it cannot be linked
# row by row with certainty; the original answers are kept.
d <- read_dta(file.path(RAW, "Survey_Data.dta"))
stopifnot(nrow(d) == 1219, !anyDuplicated(d$ResponseId), all(d$UserLanguage == "TR"))

# Arms: FL_5_DO (Qualtrics flow randomizer). Main_analysis.do lines 16-31 and
# article Table 1 / pp. 440-441: Control = background only; Treatment1 = official
# goals = real goals (certain); Treatment2 = Treatment1 + economic cost;
# Treatment3 = experts doubt the real goal (uncertain); Treatment4 = Treatment3 +
# economic cost. Check: Table 2 (non-control, complete covariates) has N = 970,
# reproduced below.
arm_map <- c(Control = "control", Treatment1 = "certain", Treatment2 = "certain_cost",
             Treatment3 = "uncertain", Treatment4 = "uncertain_cost")
arm <- unname(arm_map[d$FL_5_DO])
stopifnot(!anyNA(arm))
cc <- complete.cases(d[, c("policy_change", "progov", "age", "female", "edu_college4", "US1", "Russia1")])
stopifnot(sum(arm != "control" & cc) == 970)   # article Table 2, N = 970

# Text: QID54, stored as UTF-8 in the .dta (validUTF8 checked); whitespace runs
# collapsed and trimmed. Blanks are non-answers (the "-99" code appears only in
# no_typos.csv).
clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))
  trimws(gsub("[\r\n\t]+", " ", x))
}
text <- clean_text(d$QID54)
stopifnot(!any(text == "-99"))

# Design columns: uncertain (Treatments 3-4) and cost (Treatments 2 and 4), the two
# randomized factors among the four treatment arms (NA for control); progov
# (government supporter, asked before the treatment, article p. 443; moderator in
# H2); policy_change (closed outcome, 1 strongly oppose ... 5 strongly support).
uncertain <- ifelse(arm == "control", NA_integer_, as.integer(arm %in% c("uncertain", "uncertain_cost")))
cost      <- ifelse(arm == "control", NA_integer_, as.integer(arm %in% c("certain_cost", "uncertain_cost")))

# Encoding counts (rows with text), before writing.
enc_counts <- function(x) c(
  rows = length(x),
  turkish_letters = sum(grepl("[çğıöşüÇĞİÖŞÜ]", x)),
  replacement_char = sum(grepl("�", x, fixed = TRUE)),
  mojibake = sum(grepl("Ã[\u0080-¿]|Ä[°±\u009f]|Å[\u009f\u009e]|â€", x)))

cat("Per arm: assigned, non-empty QID54\n")
lv <- c("control", "certain", "certain_cost", "uncertain", "uncertain_cost")
print(data.frame(arm = lv, assigned = as.vector(table(factor(arm, lv))),
                 text = as.vector(tapply(nzchar(text), factor(arm, lv), sum))))

# Exclusions: none. Main_analysis.do drops no respondent (models restrict to
# non-control arms or listwise-delete missing covariates only); the Rmd drops only
# non-answers. 181 rows are Qualtrics Finished = False but all have an arm and
# policy_change; those without an answer drop out below as empty text.
h <- data.frame(id = d$ResponseId, dataset = CARD, country = "TR", lang = "tr",
                text = text, exclude = FALSE, treatment = arm,
                uncertain = uncertain, cost = cost,
                progov = as.integer(d$progov), policy_change = as.integer(d$policy_change),
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

cat("\nEncoding counts before writing:\n"); print(enc_counts(h$text))
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm:\n")
print(table(treatment = factor(h$treatment[!h$exclude], lv)))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
cat("Encoding counts after re-read:\n"); print(enc_counts(r$text))
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
