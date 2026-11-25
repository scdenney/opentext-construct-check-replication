# ==============================================================================
# chopra2024_newsdemand_extra.R -- harmonized file for card chopra2024_newsdemand_exp3
# ==============================================================================
# Source: Chopra, F., Haaland, I., & Roth, C. (2024). The demand for news:
# Accuracy concerns versus belief confirmation motives. The Economic Journal.
# Replication package 10.5281/zenodo.21244470 (CC-BY-4.0).
#
# Experiment 3 ("Mechanism experiment: Interpretation of treatment", main text
# section sec:mechanism_experiment; AsPredicted #87947; February 2022).
# Not built here: the study-purpose probe `purpose` in study1.dta/study2.dta
# fails criterion (ii) (no author expectation that the bias treatment changes
# it; it is used only to argue against experimenter demand). See the report.
#
# Conventions follow the wave-2 builder for chopra2024_newsdemand
# (analysis/revision/harmonize/external_wave2/econ/harmonize.py, lines 20-37):
# id = exp<k>_<responseid>, here exp3_<responseid>; treatment 1 = biased;
# exclude = TRUE for rows outside the authors' analysis sample.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/chopra2024_newsdemand_extra.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "chopra2024_newsdemand"
CARD <- "chopra2024_newsdemand_exp3"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

d  <- read_dta(file.path(RAW, "open_ended.dta"))              # Experiment 3 raw (ReadMe.md, data table)
hc <- read_dta(file.path(RAW, "open_ended_handcoded.dta"))    # authors' hand codes, 388 rows
stopifnot(nrow(d) == 463, !anyDuplicated(d$responseid), nrow(hc) == 388, !anyDuplicated(hc$prolific_pid))

clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))
  trimws(gsub("[\r\n\t]+", " ", x))
}
# Text: one open item per respondent, stored in three columns by arm and topic
# (Experimental Instructions.pdf, H.4.1-H.4.4): open_unbiased_wage (no bias,
# minimum wage bill), open_biased_wage (right-wing bias, minimum wage bill),
# open_ended_health (both healthcare arms; the reported statistics are piped in
# from `report`, "and" vs "but not"). Checked: no row has more than one.
t1 <- clean_text(d$open_unbiased_wage); t2 <- clean_text(d$open_biased_wage); t3 <- clean_text(d$open_ended_health)
stopifnot(all(nzchar(t1) + nzchar(t2) + nzchar(t3) <= 1))
text <- paste0(t1, t2, t3)

# Treatment: `treatment` biased / unbiased (empty for 8 rows that stopped before
# assignment; none has text). Topic: `article` wage / health, randomized
# independently of treatment (main text, Design paragraph: half minimum wage
# bill, half Republican healthcare plan). The bias is right-wing for wage and
# left-wing for health (Table 1, Experiment 3 row).
arm   <- as.character(d$treatment)
topic <- as.character(d$article)
stopifnot(all(arm %in% c("biased", "unbiased", "")), all(topic[arm != ""] %in% c("wage", "health")))
vote  <- as.character(as_factor(d$vote))

# Exclusions. The analysis sample is 388 respondents, "240 Biden voters and 148
# Trump voters" (main text, sec:mechanism_experiment, first paragraph); the
# authors' cleaning do-file is not in the package. Rule used here: exclude rows
# without assignment, incomplete responses (finished != 1; Qualtrics flag) and
# respondents who did not vote for Biden or Trump in 2020 (`vote`).
# Checked below: the kept set equals the 388 hashed Prolific ids of the authors'
# hand-coded file (and of data/open_ended_cleaned.dta inside the Zenodo zip).
excl <- arm == "" | as.numeric(d$finished) != 1 | !(vote %in% c("Joe Biden", "Donald Trump"))
stopifnot(setequal(d$prolific_pid[!excl], hc$prolific_pid), !anyDuplicated(d$prolific_pid[!excl]))

# Human codes: `motives` (one code per response: balanced, biased, cognitive
# constraints, delegation, entertainment, other), merged on hashed prolific_pid.
# human_bias / human_balanced = the two pre-specified categories (main text,
# Hand-coding paragraph; open_bias/open_balanced in open_ended_cleaned.dta).
m <- hc$motives[match(d$prolific_pid, hc$prolific_pid)]
m[excl] <- NA

cat("Raw rows: arm x topic, non-empty text, excluded\n")
print(table(arm = ifelse(arm == "", "(none)", arm), topic = ifelse(topic == "", "(none)", topic),
            text = nzchar(text), excluded = excl))

h <- data.frame(id = paste0("exp3_", d$responseid), dataset = CARD, country = "US", lang = "en",
                text = text, exclude = excl,
                treatment = ifelse(arm == "biased", 1L, ifelse(arm == "unbiased", 0L, NA_integer_)),
                topic = topic, vote_2020 = ifelse(vote %in% c("Joe Biden", "Donald Trump"), vote, "other or none"),
                human_motives = m,
                human_bias = as.integer(m == "biased"), human_balanced = as.integer(m == "balanced"),
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (1 = biased, 0 = unbiased), by topic:\n")
print(table(topic = h$topic[!h$exclude], treatment = h$treatment[!h$exclude]))
cat("Human codes merged:", sum(!is.na(h$human_motives)), "\n")
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"), na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)

# Respondents shared with card chopra2024_newsdemand (Experiments 1-2), by
# hashed Prolific id. The ids of the two files cannot be joined directly
# (responseid differs by survey); the paper says previous participants were excluded.
p1 <- read_dta(file.path(RAW, "study1.dta"), col_select = "prolific_pid")$prolific_pid
p2 <- read_dta(file.path(RAW, "study2.dta"), col_select = "prolific_pid")$prolific_pid
k  <- d$prolific_pid[!excl]
cat("Analysis-sample respondents whose hashed Prolific id is also in study1.dta:", sum(k %in% p1),
    "; study2.dta:", sum(k %in% p2), "\n")
