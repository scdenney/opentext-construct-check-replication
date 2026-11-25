# ==============================================================================
# nomikos2019_audiencecosts.R -- harmonized file for card nomikos2019_audiencecosts
# ==============================================================================
# Nomikos, W. G., & Sambanis, N. (2019). What is the mechanism underlying
# audience costs? Incompetence, belligerence, and inconsistency. Journal of
# Peace Research 56(4), 575-588. doi:10.1177/0022343319839456. Data: JPR
# replication archive at PRIO ("Nomikos and Sambanis.zip"); the archive page
# states "These files are made freely available and no special permission is
# required to use them" and asks for a citation to the article.
#
# Experiment 1 (MTurk; online appendix D.1 and E). US respondents read a
# vignette (a country invades its neighbor; randomized regime type, motive,
# strength and harm to the US), then one of four presidential actions (treat):
# stay out, threaten and back down, threaten and engage and fail, threaten and
# engage and succeed. After approval items they answered "In your words, why
# did you feel the way that you did about the U.S. president's actions in the
# hypothetical scenario that you were presented (limit 100 words)?"
# (`feelings`; appendix D.1, last approval item).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/nomikos2019_audiencecosts.R
#
# Input (data/external/nomikos2019_audiencecosts/raw/, unchanged):
#   exp1.dta                         deposited Stata original
#   nomikos_sambanis_replication.R   authors' script (cited "repl.R l. N")
# Output: data/external/nomikos2019_audiencecosts/harmonized_nomikos2019_audiencecosts.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/nomikos2019_audiencecosts/raw"
out_csv <- "data/external/nomikos2019_audiencecosts/harmonized_nomikos2019_audiencecosts.csv"
card_id <- "nomikos2019_audiencecosts"

d <- read_dta(file.path(raw_dir, "exp1.dta"))
stopifnot(nrow(d) == 1764)
# No respondent id is deposited: id = row order in exp1.dta ("r0001" ...).
id <- sprintf("r%04d", seq_len(nrow(d)))

# ---- 1. Design --------------------------------------------------------------
# treat has no value labels. Map from the file's own indicator labels:
#   treat_so_engage "1 indicates staying out, 0 indicates engagement" -> 1 for
#   treat 0, 0 for treats 2-3; treat_bd_engage "1 indicates backing down, 0
#   indicates engagement" -> 1 for treat 1; treat_se_fe "1 indicates successful engagement, 0 indicates failed" -> 0 for
#   treat 2, 1 for treat 3. Cross-check: approval share (appyes) is 0.419 in
#   treat 2 and 0.695 in treat 3, the Engage-and-Fail and Engage-and-Succeed
#   values in appendix G.1; repl.R ll. 59-87 (Table III) orders 0, 1, 3, 2.
treat <- as.numeric(d$treat)
stopifnot(all(treat %in% 0:3))
stopifnot(all(d$treat_so_engage[treat == 0] == 1), all(d$treat_bd_engage[treat == 1] == 1),
          all(d$treat_so_engage[treat %in% 2:3] == 0),
          all(d$treat_se_fe[treat == 2] == 0), all(d$treat_se_fe[treat == 3] == 1))
treatment <- c("stay_out", "back_down", "engage_fail", "engage_succeed")[treat + 1]

# Vignette factors (appendix D.1 "Vignette"), randomized independently of treat.
# 0/1 codes kept as deposited: the file has no value labels, so which level is
# "dictator"/"power and resources"/"hurt" is not recorded and is not guessed.
regime <- as.numeric(d$regime)   # "Was invader democracy or dictatorship?"
motive <- as.numeric(d$motive)   # "Why did country invade?"
hurt   <- as.numeric(d$hurt)     # "Would a victory by the invader hurt the U.S.?"

# Exclusions: none. repl.R uses every row of exp1 (Tables II-III, ll. 52-87);
# atn_check (favorite-color attention check) is not used to drop anyone.
exclude <- rep(FALSE, nrow(d))

# ---- 2. Text ------------------------------------------------------------------
text <- as.character(d$feelings)
text[is.na(text)] <- ""
text <- trimws(gsub("[\r\n\t]+", " ", text))
stopifnot(all(validUTF8(text)))

df <- data.frame(
  id = id, dataset = card_id, country = "US", lang = "en", text = text,
  exclude = exclude, treatment = treatment,
  regime = regime, motive = motive, hurt = hurt,
  approval = as.numeric(d$approval),   # 0-6 approval of the president (variable label)
  appyes = as.numeric(d$appyes),       # 1 = approves (repl.R Table III outcome)
  atn_check = as.numeric(d$atn_check),
  stringsAsFactors = FALSE)

cat("All rows:", nrow(df), "\n")
cat("Answered (non-empty text) / assigned, by arm:\n")
print(rbind(assigned = table(df$treatment), answered = table(df$treatment[nzchar(df$text)])))
cat("Answers exactly 'NA' dropped:", sum(df$text == "NA"), "\n")
df <- df[nzchar(df$text) & df$text != "NA", ]
rownames(df) <- NULL

# ---- 3. Write and check ---------------------------------------------------------
write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
ok <- !df$exclude
cat("Rows written:", nrow(df), "| excluded:", sum(df$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(df$treatment[ok]))
cat("Non-excluded rows per arm x regime / motive:\n")
print(table(treatment = df$treatment[ok], regime = df$regime[ok]))
print(table(treatment = df$treatment[ok], motive = df$motive[ok]))
back <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read identical text:", identical(back$text, df$text), "\n")
n_esc <- sum(grepl("<U+", readLines(out_csv, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(back$text, df$text), n_esc == 0)
