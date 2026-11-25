# ==============================================================================
# bursztyn2023_dissent_defund.R -- harmonized file for card bursztyn2023_dissent_defund
# ==============================================================================
# Source: Bursztyn, L., Egorov, G., Haaland, I., Rao, A., & Roth, C. (2023).
# Justifying dissent. QJE 138(3), 1403-1451, doi:10.1093/qje/qjad007.
# Data doi:10.7910/DVN/YVUIFF (CC0 1.0).
#
# Lower-credibility receiver experiment on defunding the police: "Auxiliary
# Experiment 7 (defund, lower credibility)" in the replication code
# (code/analysis/twitter-receiver-analysis.R, line 6); "Auxiliary Experiment 6"
# in the NBER working paper w29730 (Section 3.2.3, Appendix E.10).
# Conventions follow the wave-2 builder for bursztyn2023_dissent_exp2
# (analysis/revision/harmonize/external_wave2/dataverse/harmonize.R, lines 57-65):
# the file has no respondent id, so id = <card id>_<row number in the raw file>;
# rows without a randomized condition are kept with exclude = TRUE.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/bursztyn2023_dissent_defund.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "bursztyn2023_dissent"
CARD <- "bursztyn2023_dissent_defund"
RAW  <- file.path("data/external", DS, "raw", "replication folder", "data", "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

d <- read_dta(file.path(RAW, "defund-nocredible-receiver-deidentified.dta"))
stopifnot(nrow(d) == 512)
cond <- as.character(d$condition)

# Exclusion: code/cleaning/receiver-clean.do, line 8 (loop over "defund-nocredible"):
# 'drop if !inlist(condition, "excuse", "noexcuse")' -- pre-randomization
# attriters (6 rows). Kept here with exclude = TRUE, as in the wave-2 exp2 file.
excl <- !(cond %in% c("excuse", "noexcuse"))

# Treatment: receiver-clean.do lines 15-18, excuse = condition == "excuse"
# (Cover: the matched participant's Tweet says "Before joining, I was shown
# this article ..."); noexcuse = No Cover ("After joining ...").
treat <- ifelse(cond == "excuse", 1L, ifelse(cond == "noexcuse", 0L, NA_integer_))

# Closed-ended outcomes, coded as in receiver-clean.do:
# line 11: attrit = 1 if donation == "NA" | q212 == "NA" (post-randomization
#          attriters; flagged, not dropped by the authors);
# line 47: believe_donated = strpos(q212, "Yes") (guess that the matched
#          participant donated $5 to the NAACP);
# line 49: bonus = strpos(donation, "Yes") (authorized the $1 bonus).
# strpos returns 0 for "NA", so attriters are 0 on both, as in the authors' data.
q212 <- as.character(d$q212); don <- as.character(d$donation)
attrit          <- as.integer(q212 %in% "NA" | don %in% "NA")
believe_donated <- as.integer(grepl("Yes", q212, fixed = TRUE))
bonus           <- as.integer(grepl("Yes", don, fixed = TRUE))

clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))       # checked: all valid UTF-8, no mojibake markers
  trimws(gsub("[\r\n\t]+", " ", x))
}
text <- clean_text(d$openended)

cat("Raw rows per condition: assigned, non-empty text, post-randomization attriters\n")
arms <- c("excuse", "noexcuse", "NA")
cc <- ifelse(excl, "NA", cond)
print(data.frame(condition = arms,
                 rows      = as.vector(table(factor(cc, arms))),
                 text      = as.vector(tapply(nzchar(text), factor(cc, arms), sum)),
                 attrit    = as.vector(tapply(attrit == 1, factor(cc, arms), sum))))

h <- data.frame(id = paste0(CARD, "_", seq_len(nrow(d))), dataset = CARD, country = "US", lang = "en",
                text = text, exclude = excl, treatment = treat,
                believe_donated = believe_donated, bonus = bonus, attrit = attrit,
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)
# No human codes: the authors did not hand-code this item.

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (1 = Cover/excuse, 0 = No Cover/noexcuse):\n")
print(table(treatment = h$treatment[!h$exclude]))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"), na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
