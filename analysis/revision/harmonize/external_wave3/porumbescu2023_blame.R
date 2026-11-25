# ==============================================================================
# porumbescu2023_blame.R -- harmonized file for card porumbescu2023_blame
# ==============================================================================
# Porumbescu, G. A., Moynihan, D. P., Anastasopoulos, L. J., & Olsen, A. L.
# (2023). When blame avoidance backfires: Responses to performance framing and
# outgroup scapegoating during the COVID-19 pandemic. Governance, 36(3),
# 779-803. doi:10.1111/gove.12701
# Data: Harvard Dataverse doi:10.7910/DVN/Z9XXOW, CC0 1.0.
#
# Design (article, Setting and Research Design; OSF preregistration k28sz):
# 2 x 2 between-subjects ("Chinese Virus" vs "COVID-19" x negative vs positive
# framing of testing capacity) plus a baseline arm, one Qualtrics randomizer
# (FL_21_DO). Open item Q22: "In the United States, who has done a bad job in
# responding to the pandemic? Why?" (article, Measured Variables), asked after
# the vignette and the closed blame items (preregistered materials, OSF file
# "Performance_Attribution_Revised Materials June 5..docx").
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/porumbescu2023_blame.R
#
# Input (data/external/porumbescu2023_blame/raw/, unchanged):
#   COVID Data June 27 Control.tab     baseline arm, 287 rows
#   COVID Data June 27 Treatments.tab  four treatment arms, 1,152 rows
# Output: data/external/porumbescu2023_blame/harmonized_porumbescu2023_blame.csv
# Blindness: no text value is printed; only counts.
#
# Parsing. These are Dataverse ingests of the authors' CSV files. Every line has
# exactly 39 tab-separated fields; strings are wrapped in double quotes and an
# embedded quote is written \" . read.delim()/read.table() do not understand
# the \" escape and silently merge 37 treatment rows into neighbours (they
# return 1,116 rows and one all-NA row, the "1,403" in the first manifest). The
# file is therefore split on tabs line by line: 287 + 1,152 = 1,439 rows, the
# article's usable N (Sample section). The ingest has no U+FFFD characters.
# ==============================================================================

raw_dir <- "data/external/porumbescu2023_blame/raw"
out_csv <- "data/external/porumbescu2023_blame/harmonized_porumbescu2023_blame.csv"
card_id <- "porumbescu2023_blame"

read_tab <- function(f) {
  L <- readLines(file.path(raw_dir, f), encoding = "UTF-8", warn = FALSE)
  stopifnot(all(validUTF8(L)))
  # sentinel keeps trailing empty fields
  p <- lapply(strsplit(paste0(L, "\t@END@"), "\t", fixed = TRUE), function(z) z[-length(z)])
  stopifnot(all(lengths(p) == 39))
  m <- do.call(rbind, p)
  hdr <- m[1, ]; m <- m[-1, , drop = FALSE]
  q <- grepl('^".*"$', m)
  m[q] <- gsub('\\"', '"', substr(m[q], 2, nchar(m[q]) - 1), fixed = TRUE)
  stopifnot(all(grepl("^-?[0-9.]*$", m[!q])))   # unquoted cells are numbers or empty
  m[!q & m == ""] <- NA
  d <- as.data.frame(m, stringsAsFactors = FALSE)
  names(d) <- hdr
  d
}
ctl <- read_tab("COVID Data June 27 Control.tab")
trt <- read_tab("COVID Data June 27 Treatments.tab")
stopifnot(nrow(ctl) == 287, nrow(trt) == 1152, identical(names(ctl), names(trt)))
d <- rbind(ctl, trt)
stopifnot(nrow(d) == 1439)                       # article, Sample: 1439 usable

# ---- 1. Assignment ---------------------------------------------------------------
# Condition (deposit): 1 baseline, 2 Chinese virus + negative, 3 Chinese virus
# + positive, 4 COVID-19 + negative, 5 COVID-19 + positive. Cross-checked
# against the randomizer's display-order field FL_21_DO and the dummies
# Chinese / Negative (NA in the baseline file).
arm_of <- c(`1` = "baseline", `2` = "chinese_negative", `3` = "chinese_positive",
            `4` = "covid_negative", `5` = "covid_positive")
fl_of <- c(Control = "baseline", `ChineseVirus+Negative` = "chinese_negative",
           `ChineseVirus+Positive` = "chinese_positive", `COVID+Negative` = "covid_negative",
           `COVID+Positive` = "covid_positive")
arm <- unname(arm_of[d$Condition])
stopifnot(!anyNA(arm), identical(arm, unname(fl_of[d$FL_21_DO])))
chinese <- as.integer(d$Chinese); negative <- as.integer(d$Negative)
tr <- arm != "baseline"
stopifnot(all(is.na(chinese[!tr])), all(is.na(negative[!tr])),
          identical(chinese[tr], as.integer(grepl("^chinese", arm[tr]))),
          identical(negative[tr], as.integer(grepl("negative$", arm[tr]))))

# ---- 2. Exclusions ---------------------------------------------------------------
# The authors dropped incomplete, too fast (< 180 s), suspicious-IP and
# duplicate-IP responses before deposit (article, Sample); the deposit holds
# exactly their 1,439 and their scripts use the files whole (Governance
# Hypothesis 1 Final.R, l. 5-21). Three rows have a non-US IP country
# (Colombia, Kenya, Thailand) and stay in, as in the authors' N; flagged in
# ip_country. The preregistration (q20) reports results with and without
# respondents failing the two manipulation checks; the checks are asked after
# Q22 (post-treatment), so no row is dropped and the pass flags are design
# columns. Every row has a randomized arm, so exclude is FALSE throughout.
exclude <- rep(FALSE, nrow(d))

# ---- 3. Text -----------------------------------------------------------------------
txt <- trimws(gsub("[\r\n\t]+", " ", d$Q22))

# ---- 4. Design columns ---------------------------------------------------------------
num <- function(v) as.numeric(d[[v]])
# Manipulation checks (materials docx, end of survey). Q33: "How did the prompt
# you read refer to the virus?" (Chinese Virus / COVID - 19 / Did not say);
# correct answer: Chinese Virus in arms 2-3, COVID - 19 in arms 4-5, Did not say
# in the baseline (the baseline paragraph says "a new and potentially dangerous
# virus"). Q32: name of the agency in the prompt; correct: Trump Administration.
virus_key <- c(baseline = "Did not say", chinese_negative = "Chinese Virus",
               chinese_positive = "Chinese Virus", covid_negative = "COVID - 19",
               covid_positive = "COVID - 19")
df <- data.frame(
  id = c(sprintf("c%03d", seq_len(nrow(ctl))), sprintf("t%04d", seq_len(nrow(trt)))),
  # no respondent id in the deposit: file (c = Control, t = Treatments) + row order
  dataset = card_id, country = "US", lang = "en",
  text = txt, exclude = exclude,
  treatment = arm,
  virus_name = ifelse(tr, ifelse(chinese == 1, "chinese", "covid"), NA),
  framing = ifelse(tr, ifelse(negative == 1, "negative", "positive"), NA),
  condition = as.integer(d$Condition),
  # Ideology and party: asked after Q22 in the preregistered materials order
  # (possibly post-treatment). Conservative = 1 for slightly/conservative/very
  # conservative (Q9), the authors' moderator for H1, H3, H5.
  conservative = as.integer(d$Conservative), ideology_con = as.integer(d$Ideology_con),
  republican = as.integer(d$Republican), democrat = as.integer(d$Democrat),
  # closed outcomes asked before Q22
  performance_trump = num("Performance"), severity = num("Severity"),
  blame_trump = num("Trump Blame"), blame_chinese_gov = num("Chinese Gov Blame"),
  blame_governors = num("Govs Blame"), blame_cdc = num("CDC Blame"),
  ethnic_group_responsible = d$Q28,
  check_virus = d$Q33, check_virus_pass = d$Q33 == virus_key[arm],
  check_agency = d$Q32, check_agency_pass = d$Q32 == "Trump Administration",
  ip_country = d[["${e://Field/IP_countryName}"]],
  stringsAsFactors = FALSE)
df <- df[!is.na(df$text) & nzchar(df$text), ]
rownames(df) <- NULL

# ---- 5. Write and check ------------------------------------------------------------
write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
ok <- !df$exclude
cat("Rows:", nrow(df), "of", nrow(d), "| excluded:", sum(df$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(df$treatment[ok]))
cat("Virus-name contrast (pooled over framing):\n"); print(table(df$virus_name[ok]))
cat("Framing contrast (pooled over virus name):\n"); print(table(df$framing[ok]))
cat("Manipulation checks passed, by arm (virus / agency):\n")
print(rbind(virus = tapply(df$check_virus_pass, df$treatment, sum, na.rm = TRUE),
            agency = tapply(df$check_agency_pass, df$treatment, sum, na.rm = TRUE)))
cat("Answers under 20 characters:", sum(nchar(df$text) < 20), "\n")
back <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read identical text:", identical(back$text, df$text), "\n")
n_esc <- sum(grepl("<U+", readLines(out_csv, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(back$text, df$text), n_esc == 0)
