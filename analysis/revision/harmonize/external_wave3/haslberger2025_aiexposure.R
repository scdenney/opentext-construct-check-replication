# ==============================================================================
# haslberger2025_aiexposure.R -- harmonized file for card haslberger2025_aiexposure
# ==============================================================================
# Source: Haslberger, M., Gingrich, J., & Bhatia, J. (2025). Rage against the
# machine? Generative AI exposure, subjective risk, and policy preferences.
# Journal of European Public Policy, doi:10.1080/13501763.2025.2554903.
# Accepted manuscript read (Birkbeck eprint 56079) and the pre-analysis plan
# (OSF dgh2u, AI_Survey_PAP.pdf).
# Data: Harvard Dataverse doi:10.7910/DVN/YOSGEL (CC0 1.0). UK YouGov online
# experiment, July 2023, 1,041 working-age ChatGPT account holders: treatment
# (encouraged to use ChatGPT for three work-like tasks, 504) vs control
# (instructed not to use it, 537).
#
# Item: `conclusion` in textdata.rds, "Please briefly tell us in your own words
# what you think is most important when it comes to dealing with the effects of
# artificial intelligence on the labour market." Last item of the survey, after
# the closed risk, attitude and social-policy blocks (article Table 2; PAP item 6).
#
# textdata.rds has 1,003 rows: the authors already removed 38 answers they judged
# ChatGPT-written (1 control, 37 treatment; article Appendix E). Those rows are
# not in the deposit and cannot be flagged here. data.rds (closed outcomes) has
# no id column, so closed outcomes cannot be linked to the answers.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/haslberger2025_aiexposure.R
# ==============================================================================
DS   <- "haslberger2025_aiexposure"
CARD <- "haslberger2025_aiexposure"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

t <- as.data.frame(readRDS(file.path(RAW, "textdata.rds")))
stopifnot(nrow(t) == 1003, !anyDuplicated(t$id),
          all(t$treatment %in% c("Control", "Treatment")),
          all((t$treatment == "Treatment") == (t$ntreatment == 1)))

x <- as.character(t$conclusion); x[is.na(x)] <- ""
stopifnot(all(validUTF8(x)))

h <- data.frame(
  id = t$id,                                   # authors' respondent id
  dataset = CARD, country = "GB", lang = "en",
  text = trimws(gsub("[[:space:]]+", " ", x)),
  exclude = FALSE,                             # the authors' exclusion (ChatGPT-written
                                               # answers, Appendix E) is already applied in
                                               # the deposit; no other drop in
                                               # 01_main_analysis.R lines 517-533 (stm drops
                                               # rows with missing degree only for its model)
  treatment = as.integer(t$ntreatment),        # 1 = encouraged to use ChatGPT
  female = as.integer(t$female),               # pre-treatment covariates used in the
  degree = as.integer(t$degree),               # authors' STM (lines 783-801)
  age = t$age,
  prof_man = as.integer(t$prof_man),
  stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief): 1 control row

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (1 = ChatGPT encouraged):\n")
print(table(h$treatment[!h$exclude]))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
