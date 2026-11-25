# ==============================================================================
# muradova2022_empathy.R -- harmonized file for card muradova2022_empathy
# ==============================================================================
# Source: Muradova, L., & Arceneaux, K. (2022). Reflective political reasoning:
# Political disagreement and empathy. European Journal of Political Research
# 61(3), 740-761, doi:10.1111/1475-6765.12490.
# Data: Harvard Dataverse doi:10.7910/DVN/QYMM1Z (CC0 1.0): 0_data_for_analyses_.dta
# (Stata 118 original) and 1_empirical_analyses_and_appendix.do. UK online
# survey (Dynata, March 2019), 2,014 rows.
#
# Item: cog_comp_bi, "You indicated that you [oppose/favour] introducing a basic
# income scheme in the UK. Why do you think so? Please justify your choice in one
# paragraph (about 4 or 5 sentences)." (accepted manuscript, Online Appendix C).
# The authors' main outcome (cc_score, Table 1 Models 1-2). cog_comp_ad is the
# same item about legalizing assisted dying (spillover outcome, cc_spillover);
# it shares the same randomized contrast and is not used. pt_essay_1/2 (the
# perspective-taking essay) and control_essay (weekend essay) are the treatments.
#
# Arm crosswalk (variable tratamiento; .do lines 8-10; PAP naming per Appendix H.1):
#   tratamiento 2 "perspectivetaking"        -> "perspective_taking" (paper: empathy treatment)
#   tratamiento 1 "exposure_to_disagreement" -> "disagreement" (paper: "placebo",
#                 read the counter-attitudinal Sarah vignette, no writing task)
#   tratamiento 3 "placebo"                  -> "control" (paper: control; wrote
#                 about their last weekend, no vignette)
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/muradova2022_empathy.R
# ==============================================================================
DS   <- "muradova2022_empathy"
CARD <- "muradova2022_empathy"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

d <- haven::read_dta(file.path(RAW, "0_data_for_analyses_.dta"))
stopifnot(nrow(d) == 2014, !anyDuplicated(d$id), all(nzchar(d$id)))

# Repair double-encoded UTF-8 in the deposit: about 110 answers carry curly
# quotes, ellipses and pound signs stored as UTF-8 bytes re-read as Latin-1
# (e.g. U+00E2 U+0080 U+0099 for a right single quote, U+00C2 U+00A3 for a
# pound sign; found by counting code points only). A string is converted back
# only if all its characters fit Latin-1 and the bytes then form valid UTF-8,
# so genuine single accented letters are left unchanged.
fix_double_utf8 <- function(x) {
  cand <- grepl("[\u0080-ÿ]", x)
  for (i in which(cand)) {
    b <- iconv(x[i], "UTF-8", "latin1")
    if (is.na(b)) next
    Encoding(b) <- "bytes"; b <- rawToChar(charToRaw(b)); Encoding(b) <- "UTF-8"
    if (validUTF8(b)) x[i] <- b
  }
  x
}
clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))
  x <- fix_double_utf8(x)
  trimws(gsub("[[:space:]]+", " ", x))
}

arm <- c(`1` = "disagreement", `2` = "perspective_taking", `3` = "control")[
  as.character(as.integer(d$tratamiento))]
pre  <- as.integer(d$pre_bi_at)      # destring pre_bi_at (.do line 77); 1 strongly oppose .. 6 strongly favour
post <- as.integer(d$policy_at_bi)   # destring policy_at_bi (.do line 78), measured after the treatment

h <- data.frame(
  id = d$id, dataset = CARD, country = "GB", lang = "en",
  text = clean_text(d$cog_comp_bi),
  # exclude: the authors drop no respondents. All 2,014 rows enter the score
  # summaries (.do line 46; the control-group mean 24.9034 used to standardize
  # at line 48 is the mean over all 708 control rows). The Table 1 Ns (932,
  # 967) come from listwise deletion on regression covariates (ideology is
  # missing for 569 rows), not from a sample rule, so those rows are kept.
  # Rows with a missing assignment would be excluded; there are none.
  exclude = is.na(arm),
  treatment = unname(arm),
  # Prior position on UBI (pre-treatment). The vignette Sarah always argued the
  # opposite side: favour = pre_bi_at > 3 (.do Appendix I comment "greater than
  # 3 are in favour and less than 4 against"; manuscript, Research design).
  prior_favour = as.integer(pre > 3),
  pre_attitude_ubi = pre,
  post_attitude_ubi = post,
  # Display order of the randomized post-treatment attitude battery (assisted
  # dying, basic income, NHS; Appendix C "the order was randomized").
  order_policy = as.character(d$order_policy),
  # Authors' automated scores of this text (LIWC, not human codes): cc_score is
  # the Owens-Wedeking cognitive complexity formula (Appendix D); wordcount is
  # the LIWC word count (Appendix F).
  auto_complexity = as.numeric(d$cc_score),
  auto_wordcount = as.numeric(d$wordcount),
  stringsAsFactors = FALSE)

stopifnot(!any(h$exclude), !anyNA(h$prior_favour))
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

enc_counts <- function(x) c(
  rows = length(x), non_ascii = sum(grepl("[^\x01-\x7f]", x, useBytes = TRUE)),
  replacement_char = sum(grepl("�", x, fixed = TRUE)),
  mojibake = sum(grepl("Ã|â€|Â", x)))
cat("Encoding counts before writing:\n"); print(enc_counts(h$text))
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows per arm by prior position (1 = favour):\n")
print(with(h[!h$exclude, ], table(treatment, prior_favour)))
cat("Rows with 1-2 characters of text, per arm:\n")
print(table(h$treatment[nchar(h$text) <= 2]))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
