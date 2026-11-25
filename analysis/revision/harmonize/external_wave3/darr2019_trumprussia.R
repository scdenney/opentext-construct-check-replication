# ==============================================================================
# darr2019_trumprussia.R -- harmonized file for card darr2019_trumprussia
# ==============================================================================
# Darr, J. P., Kalmoe, N. P., Searles, K., Sui, M., Pingree, R. J., Watson,
# B. R., Bryanov, K., & Santia, M. (2019). Collision with collusion: Partisan
# reaction to the Trump-Russia scandal. Perspectives on Politics, 17(3),
# 772-787. doi:10.1017/S1537592719001075
# Data: Harvard Dataverse doi:10.7910/DVN/XSGYWA (v1), CC0 1.0.
#
# Design (article, "Data: The Portal Panel" and "Methods"): US MTurk workers
# used a custom news portal as their main news source for one week (June
# 12-16, 2017). Five binary story-mix factors were randomized in a full
# factorial (32 cells): russia (more vs less Trump-Russia scandal stories; the
# factor the card is about), immigration (more immigration, less health care),
# intraparty, factcheck, defenseOfJourn. A pre-test (wave 1) preceded portal
# access; the post-test (wave 2) followed the portal's closing.
# Open item (article, "Methods"): "respondents provided up to five perceived
# consequences of a Trump presidency, and then assessed those consequences on
# a five-point scale". Text column openended_5con; the overall rating is
# openended_overall ("Taken as a whole, how good or bad do you think the
# consequences of a Trump Presidency will be?", online appendix B). It is the
# respondent's own closed rating, not a code of the text.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/darr2019_trumprussia.R
#
# Input (data/external/darr2019_trumprussia/raw/, unchanged):
#   Portal2_Russia.dta   long panel, 2 rows (wave 1, wave 2) per subjectid
#   TrumpRussia_PoP.do   authors' analysis (cited "do l. N")
# Output: data/external/darr2019_trumprussia/harmonized_darr2019_trumprussia.csv
# Blindness: no text value is printed; only counts. Warnings go to stderr.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/darr2019_trumprussia/raw"
out_csv <- "data/external/darr2019_trumprussia/harmonized_darr2019_trumprussia.csv"
card_id <- "darr2019_trumprussia"

d <- read_dta(file.path(raw_dir, "Portal2_Russia.dta"))
stopifnot(nrow(d) == 3660, length(unique(d$subjectid)) == 1830,
          all(table(d$subjectid) == 2), all(d$wave %in% 1:2))
# Factors are respondent-level and constant across the two waves.
for (f in c("russia", "immigration", "intraparty", "factcheck", "defenseOfJourn"))
  stopifnot(all(tapply(as.vector(d[[f]]), d$subjectid, function(z) length(unique(z)) == 1)))

# ---- 1. Keep the post-test only ----------------------------------------------
# Wave 1 answers were given before portal access (pre-treatment) and are not
# part of the contrast. Wave 2 is the post-test.
w2 <- d[d$wave == 2, ]
w1 <- d[d$wave == 1, ]

# Exclusion: the authors' analytic sample is respondents who completed the
# post-test; dropout = 1 if prc is missing (do l. 486-487; appendix table A9).
exclude <- is.na(w2$prc)

# ---- 2. Text -------------------------------------------------------------------
# openended_5con is one string per respondent (the up-to-five consequences as
# deposited). The deposited .dta is the original Stata file (no .tab ingest
# used). Line breaks and tabs are collapsed to one space.
x <- as.character(w2$openended_5con)
stopifnot(all(validUTF8(x[!is.na(x)])))
txt <- trimws(gsub("[\r\n\t]+", " ", x))

df <- data.frame(
  id = as.character(w2$subjectid), dataset = card_id, country = "US", lang = "en",
  text = txt, exclude = exclude,
  treatment = as.integer(w2$russia),                 # 1 more, 0 less Trump-Russia news
  immigration = as.integer(w2$immigration),          # 1 more immigration, less health care
  intraparty = as.integer(w2$intraparty),
  factcheck = as.integer(w2$factcheck),
  defenseOfJourn = as.integer(w2$defenseOfJourn),
  condition = as.integer(w2$condition),              # 32-cell factorial id
  # pid_three: pre-test party ID, value labels not deposited. From the pre-test
  # cross-tab with the consequences rating, 0 is Republican and 1 Democrat
  # (inferred, not documented); the do-file's Rep_dum/Dem_dum are not in the deposit.
  pid_three = as.integer(w1$pid_three[match(w2$subjectid, w1$subjectid)]),
  # Closed outcomes, post-test. consequences_overall: 0-4 as deposited;
  # 0 = Very good ... 4 = Very bad (inferred: correlation -0.88 with job
  # approval in wave 1; the appendix lists the options Very good ... Very bad).
  consequences_overall = as.numeric(w2$openended_overall),
  trump_job_appr = as.numeric(w2$trump_job_appr),    # 1-7, 7 = strongly approve (appendix B)
  consequences_overall_pre = as.numeric(w1$openended_overall[match(w2$subjectid, w1$subjectid)]),
  russia_unique_stories = as.numeric(w2$russiaUnique), # exposure dose (post-treatment)
  stringsAsFactors = FALSE)
df <- df[!is.na(df$text) & nzchar(df$text), ]
df <- df[order(as.numeric(df$id)), ]
rownames(df) <- NULL

# ---- 3. Write and check ----------------------------------------------------------
write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
ok <- !df$exclude
cat("Rows:", nrow(df), "| excluded (no post-test, prc missing):", sum(df$exclude), "\n")
cat("Non-excluded rows per arm (1 = more Trump-Russia stories):\n"); print(table(df$treatment[ok]))
cat("Non-excluded rows, treatment x immigration:\n"); print(table(treatment = df$treatment[ok], immigration = df$immigration[ok]))
cat("Non-excluded rows, treatment x pid_three:\n"); print(table(treatment = df$treatment[ok], pid_three = df$pid_three[ok]))
cat("Post-test completers per arm (text / completed):\n")
print(rbind(completed = table(as.integer(w2$russia)[!is.na(w2$prc)]), with_text = table(df$treatment[ok])))
cat("Pre-test (wave 1) non-empty answers, not in file:",
    sum(!is.na(w1$openended_5con) & nzchar(trimws(as.character(w1$openended_5con)))), "\n")
back <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read identical text:", identical(back$text, df$text), "\n")
n_esc <- sum(grepl("<U+", readLines(out_csv, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(back$text, df$text), n_esc == 0)
