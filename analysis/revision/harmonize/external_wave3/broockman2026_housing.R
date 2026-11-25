# ==============================================================================
# broockman2026_housing.R -- harmonized file for card broockman2026_housing
# ==============================================================================
# Broockman, D. E., Elmendorf, C. S., & Kalla, J. L. (forthcoming 2026). The
# Symbolic Politics of Housing. American Political Science Review.
# Data: Harvard Dataverse doi:10.7910/DVN/WARHEN (v1), CC BY 4.0 (data).
#
# Prolific follow-up survey, February 2026 ("Streamlining follow-up:
# Controlled details"; archive README). Respondents read a by-right permitting
# vignette and chose between "[Big real estate developers | Small local
# homebuilders] should be able to build apartments that comply with the clear
# and specific rules the government made in advance" and "Local government
# officials should always be able to reject a proposed apartment development"
# (or Don't know). The submitting group (stream_group_submit) and the policy
# detail (project_char: 0 generic, 1 detailed specs) are crossed. The next
# block (Study3.docx, block "Explain") repeats both options, states the
# respondent's choice, and asks "Please explain why you selected this answer."
# One row per respondent.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/broockman2026_housing.R
#
# Input (data/external/broockman2026_housing/raw/, unchanged):
#   developer_followup_2026.csv         design, closed DV, thermometers
#   developer_followup_explain_RAW.csv  anon_id, explain (open text)
#   developer_followup_coded.csv        Gemini 3 Flash codes of the open text
#                                       (LLM output, kept as llm_* columns)
#   03_developer_followup.R             authors' analysis (cited "03 l. N")
# Output: data/external/broockman2026_housing/harmonized_broockman2026_housing.csv
# Blindness: no text value is printed; only counts. Warnings go to stderr.
# ==============================================================================

suppressPackageStartupMessages(library(data.table))

raw_dir <- "data/external/broockman2026_housing/raw"
out_csv <- "data/external/broockman2026_housing/harmonized_broockman2026_housing.csv"
card_id <- "broockman2026_housing"

d  <- fread(file.path(raw_dir, "developer_followup_2026.csv"), encoding = "UTF-8")
ex <- fread(file.path(raw_dir, "developer_followup_explain_RAW.csv"), encoding = "UTF-8")
lc <- fread(file.path(raw_dir, "developer_followup_coded.csv"), encoding = "UTF-8")
stopifnot(nrow(d) == 2615, !anyDuplicated(d$anon_id), !anyDuplicated(ex$anon_id),
          all(ex$anon_id %in% d$anon_id))
# The archive README lists 2,647 rows for the RAW file; the deposited file has
# 2,598 (one per text answer, all of which join to the main file).
cat("Main rows:", nrow(d), "| text rows:", nrow(ex), "| Gemini-coded rows:", nrow(lc), "\n")

# ---- 1. Design --------------------------------------------------------------
# Treatment: 1 = "Big real estate developers", 0 = "Small local homebuilders"
# (codebook; 03 l. 128 sets homebuilders as the reference level).
treat <- ifelse(d$stream_group_submit == "Big real estate developers", 1L,
         ifelse(d$stream_group_submit == "Small local homebuilders", 0L, NA_integer_))
cat("Rows with no randomized submitter (stream_group_submit missing):", sum(is.na(treat)), "\n")

# Closed DV, recoded as in 03 ll. 47-52: 1 by-right (option 1), 0.5 Don't
# know, 0 local officials may reject (option 2). The raw export holds the
# unpiped option text ("${e://Field/stream_group_submit} should be able ...").
dv  <- d$streamlining_dv
dv01 <- ifelse(grepl("should be able to build apartments", dv), 1,
        ifelse(dv %in% "Don't know", 0.5,
        ifelse(grepl("should always be able to reject", dv), 0, NA_real_)))
choice <- ifelse(dv01 %in% 1, "by_right", ifelse(dv01 %in% 0, "officials_reject",
          ifelse(dv01 %in% 0.5, "dont_know", NA_character_)))

# STRATUM: project_char. Its arms are unequal (854 generic vs 1,748 detailed
# among the 2,602 assigned rows, about 1:2), which a 50/50 draw cannot produce,
# and no filter explains it (all 2,615 rows are completes; only 13 lack an
# assignment). The archive does not document the randomizer, so the share of
# "Big real estate developers" may differ by stratum (52.5% generic vs 48.6%
# detailed; chi-square p = 0.07). Effects must block on project_char.
# Exclusions: the authors keep rows with a non-missing DV (03 l. 37; 04 l. 9).
# Rows with no randomized submitter are also excluded (all 13 also lack the DV).
exclude <- is.na(dv01) | is.na(treat)

# Pre-treatment thermometers (block "therms - housing" precedes the
# "Streamlining" block in Study3.docx): therms_housing_1 small local
# homebuilders, _2 big real estate developers (codebook). Kept raw (0-100);
# the authors impute NA as 50 for covariate adjustment (03 l. 116), not here.
des <- data.frame(
  anon_id = d$anon_id, exclude = exclude, treatment = treat,
  project_char = as.integer(d$project_char),
  choice = choice, streamlining_dv_01 = dv01,
  therm_big_developers = as.numeric(d$therms_housing_2),
  therm_small_homebuilders = as.numeric(d$therms_housing_1),
  stringsAsFactors = FALSE)

# ---- 2. Text ------------------------------------------------------------------
# The deposited text is valid UTF-8. The authors strip non-ASCII characters
# before analysis (03 l. 29); the original characters are kept here. Line
# breaks and tabs are collapsed to one space; the authors' place/ZIP redaction
# tokens ([PLACE_REDACTED] etc.) are left as deposited.
stopifnot(all(validUTF8(ex$explain[!is.na(ex$explain)])))
txt <- data.frame(anon_id = ex$anon_id,
                  text = trimws(gsub("[\r\n\t]+", " ", ex$explain)), stringsAsFactors = FALSE)
df <- merge(des, txt, by = "anon_id", all = FALSE)
df <- df[!is.na(df$text) & nzchar(df$text), ]

# ---- 3. Gemini codes (LLM output, NOT human codes) --------------------------
# developer_followup_coded.csv: Gemini 3 Flash batch codes (README "LLM coding
# pipelines"; classify_developer_open_ends.py). Prefixed llm_ so they are never
# read as human validation codes.
lcols <- c("affect_group_based", "instrumental_policy", "process_rights",
           "uncertain_vague", "other_reasoning", "mentions_developer", "developer_valence")
llm <- as.data.frame(lc[, c("anon_id", lcols), with = FALSE])
names(llm) <- c("anon_id", paste0("llm_", lcols))
df <- merge(df, llm, by = "anon_id", all.x = TRUE)
stopifnot(!anyDuplicated(df$anon_id))

df$id <- as.character(df$anon_id); df$anon_id <- NULL
df$dataset <- card_id; df$country <- "US"; df$lang <- "en"
core <- c("id", "dataset", "country", "lang", "text", "exclude", "treatment",
          "project_char", "choice", "streamlining_dv_01",
          "therm_big_developers", "therm_small_homebuilders")
df <- df[order(as.numeric(df$id)), c(core, setdiff(names(df), core))]
rownames(df) <- NULL

# ---- 4. Write and check ---------------------------------------------------------
write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
ok <- !df$exclude
cat("Rows:", nrow(df), "| excluded:", sum(df$exclude), "\n")
cat("Non-excluded rows per arm (1 = Big real estate developers):\n"); print(table(df$treatment[ok]))
cat("Non-excluded rows, treatment x project_char (stratum):\n"); print(table(treatment = df$treatment[ok], project_char = df$project_char[ok]))
cat("Answering among assigned with a DV, by arm (text / assigned):\n")
asg <- des[!des$exclude, ]
print(rbind(assigned = table(asg$treatment), with_text = table(df$treatment[ok])))
cat("Choice by arm among answers (counts):\n"); print(table(choice = df$choice[ok], treatment = df$treatment[ok]))
cat("Choice by arm x project_char among answers (counts):\n")
print(ftable(table(project_char = df$project_char[ok], treatment = df$treatment[ok], choice = df$choice[ok])))
back <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read identical text:", identical(back$text, df$text), "\n")
n_esc <- sum(grepl("<U+", readLines(out_csv, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(back$text, df$text), n_esc == 0)
