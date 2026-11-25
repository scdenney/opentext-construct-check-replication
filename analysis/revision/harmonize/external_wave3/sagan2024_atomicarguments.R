# ==============================================================================
# sagan2024_atomicarguments.R -- harmonized file for card sagan2024_atomicarguments
# ==============================================================================
# Source: Sagan, S. D., & Valentino, B. A. (2025). Atomic Arguments and
# Counter-Arguments: How Exposure to Conflicting Information Influences American
# Public Support for the Use of Nuclear Weapons. International Studies Quarterly
# 69(1), sqae154, doi:10.1093/isq/sqae154. Data: Harvard Dataverse
# doi:10.7910/DVN/KUF6CH (CC0 1.0). US online survey, 2020-11-10 to 2020-12-07.
#
# Item: one reason per respondent, from two choice-specific items
# (Full_Survey_Question_Wording.pdf): prefer_strike_open ("You said you preferred
# to launch the nuclear strike against the Iranian city. Briefly describe the
# single most important reason why you preferred this option.") for strike
# preferrers, prefer_ground_open (same, for continuing the ground war) for ground
# preferrers. No respondent answered both. The registered comparison pools the two.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/sagan2024_atomicarguments.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "sagan2024_atomicarguments"
CARD <- "sagan2024_atomicarguments"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# The deposited Stata original (.dta, fetched with format=original) is used, not
# the Dataverse .tab ingest, which replaces some characters in the text with U+FFFD.
d <- as.data.frame(read_dta(file.path(RAW, "ISQ_replication_data_Atomic_Arguments_2024-08-24.dta")))
stopifnot(nrow(d) == 3600, !anyDuplicated(d$caseid), all(d$consent == 1), all(d$US_citizen == 1))

# Arms. ISQ_treatments 1-7 (value labels in the .dta: baseline, strike illegal,
# strike illegal (counter), set precedent, set precedent (counter),
# effectiveness (90), effectiveness (counter 45)); blank = the separate salient-information arm
# (salient_info_treatment non-missing only there). Mapping from the
# arm-specific manipulation checks (each asked in exactly one arm; checked below)
# and the article's Research Design section: 1 = baseline (JCS present two
# options, n = 800), 2 = legal message (JCS conclude the strike would violate
# international laws of armed conflict), 3 = legal counter-message (JCS disagree
# on legality), 4 = precedent message (strike would end the tradition of
# non-use), 5 = precedent counter-message, 6 = effectiveness message (90 percent
# chance Iran surrenders within 24 hours), 7 = effectiveness counter-message
# (45 percent chance of success).
chk <- c(`2` = "manipulation_illegal", `3` = "manipulation_illegal_counter",
         `4` = "manipulation_precedent", `5` = "manipulation_precedent_counter",
         `6` = "manipulation_effect", `7` = "manipulation_effect_counter")
for (a in names(chk)) {
  asked <- !is.na(d[[chk[[a]]]])
  stopifnot(all(asked == (d$ISQ_treatments %in% as.integer(a))))
}
stopifnot(all(is.na(d$ISQ_treatments) == !is.na(d$salient_info_treatment)))
arm_map <- c(`1` = "baseline", `2` = "illegal", `3` = "illegal_counter",
             `4` = "precedent", `5` = "precedent_counter", `6` = "effect",
             `7` = "effect_counter")
stopifnot(identical(unname(attr(d$ISQ_treatments, "labels")), as.double(1:7)))
arm <- unname(arm_map[as.character(as.integer(d$ISQ_treatments))])
arm[is.na(d$ISQ_treatments)] <- "salient_info"

# Text. Rows not asked an item hold a constant 6-character placeholder (MANIFEST);
# it is identified as the value every strike preferrer holds in prefer_ground_open
# and is treated as empty. Whitespace runs collapsed and trimmed.
ph <- unique(d$prefer_ground_open[d$prefer_strike_bin %in% 1])
stopifnot(length(ph) == 1, nchar(ph) == 6)
clean_text <- function(x) {
  x[is.na(x) | x == ph] <- ""
  # 145 answers (36 strike, 109 ground) hold single Windows-1252 bytes 0x91-0x94
  # (curly quotes and apostrophes) that are invalid UTF-8; the .tab ingest turned
  # them into U+FFFD. These rows are converted from Windows-1252; all others are
  # valid UTF-8 and left as they are.
  bad <- !validUTF8(x)
  x[bad] <- iconv(x[bad], "windows-1252", "UTF-8")
  stopifnot(!anyNA(x), all(validUTF8(x)))
  trimws(gsub("[[:space:]]+", " ", x))
}
s <- clean_text(d$prefer_strike_open)
g <- clean_text(d$prefer_ground_open)
stopifnot(!any(nzchar(s) & nzchar(g)))
text <- ifelse(nzchar(s), s, g)
item <- ifelse(nzchar(s), "strike", ifelse(nzchar(g), "ground", NA_character_))
# The item shown follows the closed choice (prefer_strike_bin: 1-3 ground, 4-6 strike).
stopifnot(all(item[!is.na(item)] == ifelse(d$prefer_strike_bin[!is.na(item)] == 1, "strike", "ground")))

# Encoding counts (rows with text).
enc_counts <- function(x) c(
  rows = length(x), non_ascii = sum(grepl("[^\x01-\x7f]", x, useBytes = TRUE)),
  replacement_char = sum(grepl("�", x, fixed = TRUE)),
  mojibake = sum(grepl("Ã|â€|Â", x)))

# Exclusions. The .do file (Figure 4, lines 26-45) runs logit ... i.ISQ_treatments,
# which drops the salient-information arm (ISQ_treatments missing); that arm also
# answered the three-item information list before the choice (different
# instrument). It is kept in the file with exclude = TRUE. No other rows are
# dropped by the authors (all rows consented and are US citizens).
exclude <- arm == "salient_info"

# Human codes: the authors' binary codes of the reason (three undergraduate
# coders scored 0/1/2, a code is 1 when the three scores sum to 3 or more; article,
# section "Measuring Availability and Salience ..."). Coded for every respondent
# (0 where no answer). Kept as human_*.
h <- data.frame(id = as.integer(d$caseid), dataset = CARD, country = "US", lang = "en",
                text = text, exclude = exclude, treatment = arm,
                item = item, prefer_strike = as.integer(d$prefer_strike),
                prefer_strike_bin = as.integer(d$prefer_strike_bin),
                weight = as.numeric(d$weight),
                human_law = as.integer(d$open_law_3to6),
                human_civilians = as.integer(d$open_civs_3to6),
                human_precedent = as.integer(d$open_prec_3to6),
                human_effectiveness = as.integer(d$open_effect_3to6),
                human_troops = as.integer(d$open_troops_3to6),
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)
stopifnot(!anyNA(h[, grep("^human_", names(h))]))

cat("Encoding counts before writing:\n"); print(enc_counts(h$text))
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
lv <- c("baseline", "illegal", "illegal_counter", "precedent", "precedent_counter",
        "effect", "effect_counter", "salient_info")
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (by item):\n")
print(table(treatment = factor(h$treatment[!h$exclude], lv), item = h$item[!h$exclude]))
# na.strings = "": one answer is the literal string NA, which the default would read as missing.
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
cat("Encoding counts after re-read:\n"); print(enc_counts(r$text))
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
