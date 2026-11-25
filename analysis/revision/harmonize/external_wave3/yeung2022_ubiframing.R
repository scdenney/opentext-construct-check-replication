# ==============================================================================
# yeung2022_ubiframing.R -- harmonized file for card yeung2022_ubiframing
# ==============================================================================
# Source data: Yeung, E. S. F. (2022). Can conservatives be persuaded? Framing
# effects on support for universal basic income in the US. Political Behavior,
# doi:10.1007/s11109-022-09824-z. Harvard Dataverse doi:10.7910/DVN/VCHMLD
# (CC0 1.0). Raw Qualtrics exports final1.csv (soft launch) and final2.csv
# (main wave), merged as in the author's cleaning_raw_data.R (lines 17-22).
#
# PRIVACY: the exports carry IPAddress, LocationLatitude, LocationLongitude,
# Recipient* fields, ExternalReference, the panel id `rid`, `zip` and
# timestamps. None of them is copied into the harmonized file and none is
# printed. Only the columns listed in `h` below leave this script.
#
# Blind builder: never prints open-text values, only counts. Warnings and
# stderr should be sent to a file. Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/yeung2022_ubiframing.R
# ==============================================================================
suppressPackageStartupMessages(library(data.table))
CARD <- "yeung2022_ubiframing"
RAW  <- file.path("data/external", CARD, "raw")
OUT  <- file.path("data/external", CARD, paste0("harmonized_", CARD, ".csv"))

# Read as character. Rows 1-2 after the header are Qualtrics label rows
# (question text and ImportId), dropped as in cleaning_raw_data.R lines 18, 20.
read_wave <- function(f, wave) {
  d <- fread(file.path(RAW, f), colClasses = "character", encoding = "UTF-8",
             na.strings = NULL, header = TRUE)[-(1:2)]
  setnames(d, make.unique(names(d)))   # `age` appears twice (survey item, panel field)
  d[, wave := wave]
  d
}
d <- rbind(read_wave("final1.csv", "soft_launch"), read_wave("final2.csv", "main"))
stopifnot(!anyDuplicated(d$ResponseId))

num <- function(x) suppressWarnings(as.numeric(x))
clean_text <- function(x) { x <- gsub("[\r\n\t]+", " ", x); x[is.na(x)] <- ""; trimws(x) }

# Support: raw 1-7, recoded 0-6 (cleaning_raw_data.R line 91).
support <- num(d$support) - 1
# The open item has three versions, routed by the support answer (stm.R lines
# 22-24: favor if support 4-6, neutral if 3, oppose if 0-2). Each respondent saw
# exactly one version (checked below), so the versions are pooled into `text`.
# prompt_version is POST-TREATMENT (it depends on the support answer).
fav <- clean_text(d$open.ended.favor); opp <- clean_text(d$open.ended.oppose); neu <- clean_text(d$open.ended.neutral)
n_versions <- nzchar(fav) + nzchar(opp) + nzchar(neu)
stopifnot(all(n_versions <= 1))
prompt_version <- ifelse(support >= 4, "favor", ifelse(support == 3, "neutral", ifelse(support >= 0, "oppose", NA)))
text <- ifelse(prompt_version == "favor", fav, ifelse(prompt_version == "neutral", neu,
               ifelse(prompt_version == "oppose", opp, "")))
text[is.na(text)] <- ""
# Every non-empty answer sits in the column its support band routes to.
stopifnot(sum(nzchar(text)) == sum(n_versions))

# Arms: group 0 control, 1 equalizing opportunity, 2 limiting government
# (cleaning_raw_data.R lines 87-88; appendix figure labels).
arm <- c("0" = "control", "1" = "opportunity", "2" = "limit_government")[d$group]

# Conservatism score (cleaning_raw_data.R lines 110-125): one point per
# conservative answer to policy1-10; conservative = score >= 5 (preregistered).
# Measured AFTER the open item: post-treatment.
cons_codes <- c(policy1 = "2", policy2 = "1", policy3 = "2", policy4 = "2", policy5 = "1",
                policy6 = "2", policy7 = "2", policy8 = "2", policy9 = "2", policy10 = "2")
cons_score <- Reduce(`+`, lapply(names(cons_codes), function(v) as.integer(d[[v]] == cons_codes[[v]])))
# Attention (cleaning_raw_data.R lines 104-106): screener1 passed if left blank,
# screener2 passed if "3,5"; 0-2. Both screeners precede randomization.
attention <- as.integer(d$screener1 == "") + as.integer(d$screener2 == "3,5")

h <- data.frame(
  id = d$ResponseId, dataset = CARD, country = "US", lang = "en",
  text = text,
  # Exclusion: respondents who did not finish the survey (Finished != 1),
  # dropped by the author (cleaning_raw_data.R line 22); and rows with no arm.
  # Inattentive respondents are kept, as in the main analysis (main.R, Model 3
  # on the full sample; attention is used only for stratified checks).
  exclude = d$Finished != "1" | is.na(arm),
  treatment = arm,
  prompt_version = prompt_version,            # post-treatment, see above
  support = support,                          # 0-6, closed-ended outcome
  ideo = num(d$ideo),                         # self-reported ideology, raw code, PRE-treatment
  self_conservative = as.integer(d$ideo %in% c("4", "5", "6")),  # appendix.R line 217
  cons_score = cons_score,                    # 0-10, post-treatment
  conservative = as.integer(cons_score >= 5), # preregistered threshold, post-treatment
  attention = attention,
  wave = d$wave,
  stringsAsFactors = FALSE)
h$exclude[is.na(h$exclude)] <- TRUE

cat("Rows in the merged exports:", nrow(h), "\n")
h <- h[nzchar(h$text), ]                      # keep rows with empty text out (brief)
stopifnot(!anyDuplicated(h$id))
# No human codes: none deposited.
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks ----
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Excluded rows (did not finish / no arm):", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude], useNA = "ifany"))
cat("Non-excluded rows per arm and wave:\n"); print(table(h$treatment[!h$exclude], h$wave[!h$exclude]))
cat("Non-excluded rows per arm and prompt version (post-treatment):\n")
print(table(h$treatment[!h$exclude], h$prompt_version[!h$exclude]))
cat("Non-excluded rows per arm, self-reported conservatives (pre-treatment):\n")
print(table(h$treatment[!h$exclude], h$self_conservative[!h$exclude]))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Answers that are the literal string NA:", sum(h$text == "NA"), "\n")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
