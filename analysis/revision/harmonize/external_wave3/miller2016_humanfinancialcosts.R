# ==============================================================================
# miller2016_humanfinancialcosts.R -- harmonized file for card
# miller2016_humanfinancialcosts
# ==============================================================================
# Miller, C., & Barber, B. S. IV (2016). It's only money: Do voters treat human
# and financial sunk costs the same? Journal of Peace Research 53(1), 116-129.
# doi:10.1177/0022343315608934. Data: JPR replication archive at PRIO
# ("Miller & Barber 2016 replication.zip"); the archive page states the files
# "are made freely available and no special permission is required to use them".
#
# MTurk experiment (appendix C). US respondents read a vignette about a
# friendly country facing an al-Qaeda-backed insurgency; the US sends (or keeps
# sending) troops or weapons. Cells: cost type (human: troops; financial:
# weaponry) x timing (prospective; sunk: "14"/"4500 US troops have been killed
# and $20 million spent" or "$124 million"/"$33.3 billion has been spent") x
# level (low, appendix C.1; high, appendix C.2). After the 1-10 approval rating
# the optional item "If you like, tell us more about why you chose the answer
# you did." (`Written_Response`).
#
# Low and high are two separate MTurk samples, merged in the deposited file:
# appendix A reports "Low Sunk Costs" and "High Sunk Costs" subject counts
# separately, Tables B1-B3 report a "Low Cost Experiment" and a "High Cost
# Experiment", and the file codes the two with different columns (Low_Sunk vs
# High_Sunk; arm code `treatmen` only in the low sample, `treatment` only in the
# high sample). Arms were assigned within each sample; level was not
# randomized against the arms, so `level` is kept as a stratum.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/miller2016_humanfinancialcosts.R
#
# Input (data/external/miller2016_humanfinancialcosts/raw/, unchanged):
#   Combined_MTurk_Survey_Results.dta  deposited Stata original (no .tab ingest)
#   Tables_%26_Figures_Final.R         authors' script (cited "tf.R l. N")
# KnowledgeNetworks.dta is not used (about 110 open answers per item).
# Output: data/external/miller2016_humanfinancialcosts/harmonized_miller2016_humanfinancialcosts.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/miller2016_humanfinancialcosts/raw"
out_csv <- "data/external/miller2016_humanfinancialcosts/harmonized_miller2016_humanfinancialcosts.csv"
card_id <- "miller2016_humanfinancialcosts"

d <- read_dta(file.path(raw_dir, "Combined_MTurk_Survey_Results.dta"))
stopifnot(nrow(d) == 1658, !anyDuplicated(d$responseid))

# ---- 1. Design ----------------------------------------------------------------
# Cell from the eight dummies, as in tf.R ll. 12-30 (Treatment2).
cells <- c(Human_Prospective_Low_Dum = "human_prospective.low",
           Finance_Prospective_Low_Dum = "financial_prospective.low",
           Human_Sunk_Low_Dum = "human_sunk.low",
           Finance_Sunk_Low_Dum = "financial_sunk.low",
           Human_Prospective_High_Dum = "human_prospective.high",
           Finance_Prospective_High_Dum = "financial_prospective.high",
           Human_Sunk_High_Dum = "human_sunk.high",
           Finance_Sunk_High_Dum = "financial_sunk.high")
m <- sapply(names(cells), function(v) as.numeric(d[[v]]) %in% 1)
stopifnot(all(rowSums(m) <= 1))
cell <- rep(NA_character_, nrow(d))
for (v in names(cells)) cell[m[, v]] <- cells[[v]]
treatment <- sub("\\..*$", "", cell)        # human_prospective, human_sunk, ...
level     <- sub("^.*\\.", "", cell)        # low / high (sample)
level[is.na(cell)] <- NA
cost_type <- sub("_.*$", "", treatment)     # human / financial
timing    <- sub("^.*_", "", treatment)     # prospective / sunk
# Cross-check sample columns: low cells have Low_Sunk == 1, high cells High_Sunk == 1.
stopifnot(all(as.numeric(d$Low_Sunk)[level %in% "low"] == 1),
          all(as.numeric(d$High_Sunk)[level %in% "high"] == 1))

# ---- 2. Exclusions ------------------------------------------------------------
# tf.R ll. 65-67 ("FIXED THIS MISTAKE"): data <- subset(data, Total_Time > 1);
# data <- subset(data, finished == 1). Kept counts reproduce the N of appendix
# Table B3 (378 low, 358 high in the human-sunk vs financial-sunk models).
# Rows with no cell recorded (21, all without text) have no assignment.
exclude <- !(as.numeric(d$finished) %in% 1) |
  !(!is.na(d$Total_Time) & as.numeric(d$Total_Time) > 1) |
  is.na(cell)
stopifnot(sum(!exclude & cell %in% c("human_sunk.low", "financial_sunk.low")) == 378,
          sum(!exclude & cell %in% c("human_sunk.high", "financial_sunk.high")) == 358)

# ---- 3. Text --------------------------------------------------------------------
text <- as.character(d$Written_Response)
text[is.na(text)] <- ""
text <- trimws(gsub("[\r\n\t]+", " ", text))
stopifnot(all(validUTF8(text)))

df <- data.frame(
  id = as.character(d$responseid), dataset = card_id, country = "US", lang = "en",
  text = text, exclude = exclude, treatment = treatment,
  cost_type = cost_type, timing = timing, level = level,
  # 1-10 approval of the American decision, asked before the open item
  # (variable label of `support`; tf.R uses Military_Support, same values).
  support = as.numeric(d$Military_Support),
  stringsAsFactors = FALSE)

cat("All rows:", nrow(df), "| no cell recorded:", sum(is.na(cell)),
    "| authors' exclusions (finished/Total_Time):", sum(exclude & !is.na(cell)), "\n")
ok0 <- !df$exclude
cat("Non-excluded: assigned and answered (non-empty text), by cell:\n")
tab <- rbind(assigned = table(cell[ok0]), answered = table(cell[ok0 & nzchar(df$text)]))
print(rbind(tab, pct = round(100 * tab["answered", ] / tab["assigned", ], 1)))
cat("Answers at the Stata str244 limit (possibly truncated):", sum(nchar(df$text) >= 244), "\n")
# Keep rows with empty text out (brief). An answer that is exactly "NA" is
# treated as empty (as in nomikos2019_audiencecosts.R): read.csv would turn it
# into a missing value, and it carries no reason.
cat("Answers exactly 'NA' dropped:", sum(df$text == "NA"), "\n")
df <- df[nzchar(df$text) & df$text != "NA", ]
rownames(df) <- NULL

# ---- 4. Write and check ---------------------------------------------------------
write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
ok <- !df$exclude
cat("Rows written:", nrow(df), "| excluded:", sum(df$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(df$treatment[ok]))
cat("Non-excluded rows per arm x level:\n"); print(table(df$treatment[ok], df$level[ok]))
cat("Non-excluded rows, cost type / timing:\n")
print(table(df$cost_type[ok])); print(table(df$timing[ok]))
back <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read identical text:", identical(back$text, df$text), "\n")
n_esc <- sum(grepl("<U+", readLines(out_csv, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(back$text, df$text), n_esc == 0)
