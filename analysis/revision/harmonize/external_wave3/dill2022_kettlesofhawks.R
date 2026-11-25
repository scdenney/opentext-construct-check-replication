# ==============================================================================
# dill2022_kettlesofhawks.R -- harmonized file for card dill2022_kettlesofhawks
# ==============================================================================
# Source: Dill, J., Sagan, S. D., & Valentino, B. A. (2022). Kettles of hawks:
# Public opinion on the nuclear taboo and noncombatant immunity in the United
# States, United Kingdom, France, and Israel. Security Studies 31(1), 1-31,
# doi:10.1080/09636412.2022.2038663. Data: Harvard Dataverse doi:10.7910/DVN/PAY8IA
# (CC0 1.0). YouGov online samples, June-July 2018.
#
# Item: one reason per respondent, from two choice-specific items (online
# appendix, Appendix 2): nuke_reason_open ("In your own words, please briefly
# describe the single most important reason you preferred the nuclear strike.")
# for nuclear preferrers (strike_pref 3-4) and conv_reason_open (same, "the
# conventional strike") for conventional preferrers (strike_pref 1-2). The article
# describes them as one question put to all subjects ("we asked all subjects to
# briefly describe in their own words 'the single most important reason they
# preferred' the nuclear or conventional strike", section on open responses,
# p. 21 of the ORA copy). The two are pooled into one answer per respondent.
# no_attack_reason_open_A is NOT used: it was asked only of respondents who ranked
# "do not attack" first (Appendix 2), a post-treatment subset.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/dill2022_kettlesofhawks.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "dill2022_kettlesofhawks"
CARD <- "dill2022_kettlesofhawks"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# The deposited Stata original (.dta, release 118) is used; Dataverse labels it
# .tab but it is not an ingest.
d <- as.data.frame(zap_labels(read_dta(file.path(RAW,
  "Dill_Sagan_Valentino_Kettle_of_Hawks_Replication_Stata_Data_10-10-2021.dta"))))
stopifnot(nrow(d) == 12292)

# 1. Drop the country == 5 block. Value label 5 = "AVERAGE"; the do-file uses it
# as the four-country average (replication_do_file_10-10-2021.do, lines 84-86:
# "compare means of 4-country average ...", test on 5.country), and restricts
# other models with "country!=5" (lines 50-72, 92-152). Evidence that it is a
# duplicate: its 6,146 caseids are exactly the 6,146 real caseids, and 120 of 122
# columns are identical row by row; only caseid_with_average (renumbered 1-6146)
# and weight_new_IL_therm_any / FR_muslim differ (checked below).
real <- d[d$country != 5, ]; avg <- d[d$country == 5, ]
stopifnot(nrow(real) == 6146, !anyDuplicated(real$caseid),
          setequal(avg$caseid, real$caseid))
m <- match(avg$caseid, real$caseid)
cmp <- setdiff(names(d), c("country", "country_ascending", "caseid_with_average"))
same <- vapply(cmp, function(v) identical(as.vector(real[[v]][m]), as.vector(avg[[v]])), TRUE)
cat("AVERAGE block: columns identical to the matched real rows:", sum(same), "of", length(cmp),
    "; differ:", paste(cmp[!same], collapse = ", "), "\n")
stopifnot(sum(same) >= length(cmp) - 2)
d <- real

# 2. Experiment A only (experiment == 1; Experiment B and the control group did not
# read the strike story). Value labels: experiment 1 = A; country 1 USA, 2 UK,
# 3 FRANCE, 4 ISRAEL; treatment_A 1 effectiveness 90/90, 2 effectiveness 90/45,
# 3 nuclear 100k deaths, 4 target civilians, 5 Christian 100k deaths.
d <- d[d$experiment == 1, ]
stopifnot(nrow(d) == 3320, !anyNA(d$treatment_A), all(d$treatment_A %in% 1:5))

# 3. Text. Rows not asked an item hold the placeholder "__NA__" (6 characters):
# it is the value every conventional preferrer holds in nuke_reason_open_A.
ph <- unique(d$nuke_reason_open_A[d$strike_pref_A %in% 1:2])
stopifnot(length(ph) == 1, nchar(ph) == 6)
clean_text <- function(x) {
  x[is.na(x) | x == ph] <- ""
  # 594 answers (561 French, 33 US/UK) hold single Windows-1252 bytes that
  # are invalid UTF-8; these strings are converted from Windows-1252; all other
  # strings are valid UTF-8 and left as they are.
  bad <- !validUTF8(x)
  x[bad] <- iconv(x[bad], "windows-1252", "UTF-8")
  stopifnot(!anyNA(x), all(validUTF8(x)))
  trimws(gsub("[[:space:]]+", " ", x))
}
nk <- clean_text(d$nuke_reason_open_A)
cv <- clean_text(d$conv_reason_open_A)
stopifnot(!any(nzchar(nk) & nzchar(cv)))
text <- ifelse(nzchar(nk), nk, cv)
item <- ifelse(nzchar(nk), "nuclear", ifelse(nzchar(cv), "conventional", NA_character_))
# The item shown follows the closed choice (strike_pref_A 1-2 conventional, 3-4 nuclear).
has <- !is.na(item)
stopifnot(all(item[has] == ifelse(d$strike_pref_A[has] %in% 3:4, "nuclear", "conventional")))

# 4. Israel. The Israeli answers were written in Hebrew; in the deposited file every
# Hebrew letter is stored as "_" (U+005F): of the code points in the 834 Israeli
# answers, 91,110 are "_" and only 4 answers contain any Latin letter (checked
# blind, counts only). The text is unrecoverable, so Israeli rows are left out.
il <- d$country == 4 & has
il_cp <- unlist(lapply(text[il], utf8ToInt))
cat("Israel: answers", sum(il), "; share of '_' among code points:",
    round(mean(il_cp == 95L), 3), "\n")
stopifnot(mean(il_cp == 95L) > 0.8)

# Encoding counts (rows with text).
enc_counts <- function(x) c(
  rows = length(x), non_ascii = sum(grepl("[^\x01-\x7f]", x, useBytes = TRUE)),
  replacement_char = sum(grepl("�", x, fixed = TRUE)),
  mojibake = sum(grepl("Ã|â€|Â", x)))

# Exclusions: the authors drop no Experiment A respondent (do-file: all models use
# experiment==1 & country!=5; the Christian arm 5 is left out of Figure 2 but
# reported in Appendix 4). No row has a missing assignment. exclude is FALSE for all.
arm_map <- c(`1` = "eff9090", `2` = "eff9045", `3` = "nuc100k", `4` = "target_civ",
             `5` = "christian")
ctry <- c(`1` = "US", `2` = "GB", `3` = "FR")
lang <- c(`1` = "en", `2` = "en", `3` = "fr")   # French sample answered in French
                                               # (Appendix 3: French and Hebrew were
                                               # translated before coding; the file
                                               # holds the originals)
keep <- has & d$country %in% 1:3
k <- as.character(d$country[keep])

# Closed-ended "most important reason" menus (nuke_reasons_A: 4 options;
# conv_reasons_A: 6 options; value labels in the .dta). These are respondents'
# closed choices, not codes of the open text: one value per respondent, YouGov
# "skipped"/"not asked" codes, and categories that differ from the Appendix 3 code
# book (no radiation category). Kept as design columns, not human_*.
closed_reason <- ifelse(item == "nuclear", paste0("nuc", d$nuke_reasons_A),
                        paste0("conv", d$conv_reasons_A))
closed_reason[grepl("NA$", closed_reason)] <- NA

h <- data.frame(id = as.character(d$caseid[keep]), dataset = CARD,
                country = unname(ctry[k]), lang = unname(lang[k]),
                text = text[keep], exclude = FALSE,
                treatment = unname(arm_map[as.character(d$treatment_A[keep])]),
                item = item[keep], strike_pref = as.integer(d$strike_pref_A[keep]),
                closed_reason = closed_reason[keep],
                weight = as.numeric(d$weight[keep]),
                stringsAsFactors = FALSE)
stopifnot(all(nzchar(h$text)))

cat("Encoding counts before writing:\n"); print(enc_counts(h$text))
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
lv <- unname(arm_map)
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (by country):\n")
print(table(treatment = factor(h$treatment[!h$exclude], lv), country = h$country[!h$exclude]))
cat("Non-excluded rows per arm (by item):\n")
print(table(treatment = factor(h$treatment[!h$exclude], lv), item = h$item[!h$exclude]))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "",
              colClasses = c(id = "character"))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
cat("Encoding counts after re-read:\n"); print(enc_counts(r$text))
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
