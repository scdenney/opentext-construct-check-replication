# ==============================================================================
# thomas2022_ubireframing.R -- harmonized files for cards
#   thomas2022_ubireframing_e1 (Experiment 1) and thomas2022_ubireframing_e2
#   (Experiment 2)
# ==============================================================================
# Source: Thomas, C. C., Walton, G. M., Reinhart, E. C., & Markus, H. R. (2023).
# Mitigating welfare-related prejudice and partisanship among U.S. conservatives
# with moral reframing of a universal basic income policy. Journal of
# Experimental Social Psychology 105, 104424, doi:10.1016/j.jesp.2022.104424.
# Data: public OSF project osf.io/kn6s4 (no licence field; article "Data
# availability: Data and study materials are shared on the project OSF page"):
# ubi1_data.csv (Experiment 1), ubi2_data.csv (Experiment 2). No analysis code
# is deposited. ubi3_data.csv (Experiment 3) has no open-text answers.
#
# Item: `values`, "Does this policy reflect your values? Please describe why or
# why not." (Study Materials for Experiments 1-3.pdf, p. 3 "Manipulation check";
# article 2.1.3.1 and 3.1.3.1), asked directly after the message in every arm.
#
# Arm crosswalk (`condition`; article 2.1.2 and 3.1.2):
#   E1: "control" (Basic Income, policy details only), "freedom" (Financial
#       Freedom), "security" (Social Security for All) -> kept as is.
#   E2: "control" (Universal Basic Income), "freedom" (Financial Freedom),
#       "prosperity" (Freedom for Prosperity, the article's "freedom plus")
#       -> "control", "freedom", "freedom_plus".
#
# Human codes (study materials p. 3-4 and 7; SM Table S1.4, Sections 1.6, 2.3):
#   val.lib = presence (1) / absence (0) of the theme of freedom -> human_freedom
#   val.sec = presence (1) / absence (0) of the theme of security (E1 only)
#             -> human_security
# One code per answer: two RAs double-coded 10% rounds and resolved
# discrepancies (5 rounds in E1, 7 in E2), then each coded half of the rest; the
# deposit does not say which answers were double-coded.
#
# Blind builder: never prints open-text values, only counts. Send warnings and
# stderr to a file. Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/thomas2022_ubireframing.R
# ==============================================================================
suppressPackageStartupMessages(library(data.table))
DS  <- "thomas2022_ubireframing"
RAW <- file.path("data/external", DS, "raw")

# ubi2_data.csv is not valid UTF-8: typographic quotes were mangled into
# byte sequences before deposit (counted by byte pattern only, never printed):
#   \x8a\x97\xc8 (35, between letters) and _\xdc_ (14, between letters) stand
#   for a right single quote / apostrophe; \x8a\x97\x93 (1) and \x8a\x97\x9d,
#   _\xdc\x9d (1 each) for double quotes. They are replaced by ASCII ' and ".
#   ubi1_data.csv is pure ASCII. Any other byte above 0x7f stops the script.
repair_bytes <- function(x) {
  x[is.na(x)] <- ""
  Encoding(x) <- "bytes"
  x <- gsub("\x8a\x97\xc8", "'", x, useBytes = TRUE)
  x <- gsub("_\xdc_", "'", x, useBytes = TRUE)
  x <- gsub("\x8a\x97[\x93\x9d]", "\"", x, useBytes = TRUE)
  x <- gsub("_\xdc\x9d", "\"", x, useBytes = TRUE)
  stopifnot(!any(grepl("[\x80-\xff]", x, useBytes = TRUE)))
  Encoding(x) <- "unknown"
  x
}
# Line breaks (CRLF and bare CR inside answers) and runs of spaces -> one space.
clean_text <- function(x) trimws(gsub("[[:space:]]+", " ", repair_bytes(x)))

read_exp <- function(f) fread(file.path(RAW, f), encoding = "unknown",
                              na.strings = c("", "NA"))

build <- function(d, card, arm_map, n_expected) {
  # The deposits hold the authors' final analytic samples (article 2.1.1: 628
  # after dropping 14 who did not answer "Yes" to "should we use your data";
  # article 3.1.1: 701 after dropping 13 such cases and 175 non-conservatives,
  # ideology score of 4 or less; Preregistration deviations.docx: duplicates
  # also removed). The checks below confirm that no excluded case remains:
  # debrief_3 has one value throughout (the retained "Yes"), and in E2 every
  # polit.quant is above 4. Hence exclude is FALSE for every row. The attention
  # check is not an exclusion ("we retained all participants", article 2.2.2).
  stopifnot(nrow(d) == n_expected, !anyDuplicated(d$ID), all(nzchar(d$ID)),
            length(unique(d$debrief_3)) == 1,
            all(d$condition %in% names(arm_map)))
  arm <- unname(arm_map[d$condition])
  h <- data.frame(
    id = d$ID, dataset = card, country = "US", lang = "en",
    text = clean_text(d$values),
    exclude = is.na(arm),
    treatment = arm,
    # Closed-ended items, all asked after the open item (study materials p. 4
    # "Question Order"; E2 p. 8 "same order"): support 1-7 (primary outcome),
    # fit = mean of fit_1-fit_3 (moral fit, 1-5). Ideology polit.quant = mean of
    # social and economic self-placement 1-7, asked with the demographics at
    # the end of the survey (post-treatment; the E1 moderator and E2 sample rule).
    support = as.numeric(d$support),
    moral_fit = as.numeric(d$fit),
    ideology = as.numeric(d$polit.quant),
    # Attention check (eligibility conditions; correct answer 3, materials p. 4).
    attention_pass = as.integer(d$policy_check_3 == 3),
    human_freedom = as.integer(d$val.lib),
    stringsAsFactors = FALSE)
  if ("val.sec" %in% names(d)) h$human_security <- as.integer(d$val.sec)
  h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)
  h
}

write_check <- function(h, card) {
  out <- file.path("data/external", DS, paste0("harmonized_", card, ".csv"))
  write.csv(h, out, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  cat(sprintf("\n%s: rows = %d, columns = %s\n", out, nrow(h),
              paste(names(h), collapse = ",")))
  cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
  cat("Human codes = 1 per arm (non-excluded):\n")
  hc <- grep("^human_", names(h), value = TRUE)
  print(sapply(hc, function(v) tapply(h[[v]][!h$exclude], h$treatment[!h$exclude], sum, na.rm = TRUE)))
  cat("Missing human codes:", sapply(hc, function(v) sum(is.na(h[[v]]))), "\n")
  cat("Rows with <= 2 characters of text, per arm:\n")
  print(table(factor(h$treatment[nchar(h$text) <= 2], levels = sort(unique(h$treatment)))))
  cat("Non-ASCII answers:", sum(grepl("[^\x01-\x7f]", h$text, useBytes = TRUE)), "\n")
  r <- read.csv(out, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  n_esc <- sum(grepl("<U+", readLines(out, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
  cat("Lines with <U+ escapes:", n_esc, "\n")
  stopifnot(identical(r$text, h$text), n_esc == 0)
}

# Experiment 1 (not preregistered; full sample, liberals and conservatives).
d1 <- read_exp("ubi1_data.csv")
h1 <- build(d1, "thomas2022_ubireframing_e1",
            c(control = "control", freedom = "freedom", security = "security"), 628)
write_check(h1, "thomas2022_ubireframing_e1")

# Experiment 2 (preregistered, osf.io/gwru2; conservatives only).
d2 <- read_exp("ubi2_data.csv")
stopifnot(all(d2$polit.quant > 4))   # article 3.1.1: non-conservatives (<= 4) excluded
h2 <- build(d2, "thomas2022_ubireframing_e2",
            c(control = "control", freedom = "freedom", prosperity = "freedom_plus"), 701)
write_check(h2, "thomas2022_ubireframing_e2")
