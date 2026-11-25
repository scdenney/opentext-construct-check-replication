# Harmonized file for card wang2026_aifollowup (OSF 9wnfv, "When AI Asks Back").
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/wang2026_aifollowup.R
# Reads only data/external/wang2026_aifollowup/raw/; writes
# data/external/wang2026_aifollowup/harmonized_wang2026_aifollowup.csv.
# Prints counts only; never prints text values (blindness rule).

raw_dir <- "data/external/wang2026_aifollowup/raw"
out <- "data/external/wang2026_aifollowup/harmonized_wang2026_aifollowup.csv"
rd <- function(f) read.csv(file.path(raw_dir, f), encoding = "UTF-8", fileEncoding = "UTF-8-BOM",
                           check.names = FALSE, stringsAsFactors = FALSE)

# Deposited originals: all 603 submissions plus the authors' case-level cleaning flags.
r <- rd("raw603_deidentified.csv")
fl <- rd("exclusion_flags_603.csv")
a <- rd("analysis_ready_516.csv")   # authors' analytic sample, used for derived variables
stopifnot(nrow(r) == 603, nrow(fl) == 603, nrow(a) == 516,
          !anyDuplicated(r$case_id), setequal(r$case_id, fl$case_id))

d <- merge(r, fl[, c("case_id", "included_final")], by = "case_id", sort = FALSE)
d <- d[order(d$case_id), ]

# Exclusion: the authors' outcome-blind screening (README "Sample cleaning"; reproduce_summary.py
# lines 23-31): completion time outside 180-1500 s, failed follow-up-strategy recognition, failed
# symptom-scenario recognition, failed instructed-response check, or Q30 != 5. included_final == 1
# reproduces the 516-case analytic sample (checked below).
d$exclude <- d$included_final != 1
stopifnot(setequal(d$case_id[!d$exclude], a$case_id))

# Treatment: condition_id 1-6 (codebook row condition_id): 1/4 = open-ended, 2/5 = fixed
# structured, 3/6 = context-tailored structured follow-up; 1-3 headache, 4-6 urinary/lower-abdominal.
stopifnot(all(d$condition_id %in% 1:6))
d$treatment <- c("open", "fixed", "tailored")[(d$condition_id - 1) %% 3 + 1]
d$symptom <- ifelse(d$condition_id <= 3, "headache", "urinary")
# Cross-check against the authors' derived labels in the analytic file.
chk <- merge(d[, c("case_id", "treatment", "symptom")],
             a[, c("case_id", "followup_design", "symptom_context")], by = "case_id")
stopifnot(all(chk$treatment == c("Open-ended" = "open", "Fixed structured" = "fixed",
                                 "Context-tailored structured" = "tailored")[chk$followup_design]),
          all(chk$symptom == ifelse(chk$symptom_context == "Headache", "headache", "urinary")))

# Closed-ended outcome and the authors' text-validity flag come from the analytic file
# (README "Derived focal variables"): supplementation_willingness_mean = mean of Q16 items 1-7;
# open_text_valid = nonblank, not numeric-only, >= 10 Chinese/Latin/digit characters. Rows with
# open_text_valid == 0 are NOT excluded here: the authors keep them in the sample and score their
# elaboration as 0 (open_text_elaboration_fullsample); the column lets the lead apply the rule.
d <- merge(d, a[, c("case_id", "supplementation_willingness_mean", "open_text_valid")],
           by = "case_id", all.x = TRUE, sort = FALSE)
d <- d[order(d$case_id), ]

# Respondent self-coding (Q15 "what information did your reply contain", multi-select). Kept raw:
# the option labels are not in codebook.csv, so it is not usable as a human code yet.
d$selfcode_info_raw <- d$open_text_info_categories

d$text <- trimws(d$open_text_next_message)
d$text[is.na(d$text)] <- ""
cat("rows (603 file):", nrow(d), " excluded by authors:", sum(d$exclude),
    " non-empty text among excluded:", sum(d$exclude & nzchar(d$text)), "\n")
# Text in the analytic file must equal the text in the 603-row original.
ta <- trimws(a$open_text_next_message); ta[is.na(ta)] <- ""
stopifnot(identical(ta[order(a$case_id)], d$text[!d$exclude]))

# Keep rows with empty text out (brief).
h <- d[nzchar(d$text), ]
h <- data.frame(
  id = h$case_id, dataset = "wang2026_aifollowup", country = "CN", lang = "zh-Hans",
  text = h$text, exclude = h$exclude, treatment = h$treatment,
  symptom = h$symptom, condition_id = h$condition_id,
  supplementation_willingness_mean = h$supplementation_willingness_mean,
  author_text_valid = h$open_text_valid, selfcode_info_raw = h$selfcode_info_raw,
  stringsAsFactors = FALSE)

write.csv(h, out, row.names = FALSE, fileEncoding = "UTF-8")

# Checks.
cat("rows written:", nrow(h), "\n")
cat("non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
cat("non-excluded rows per arm x symptom:\n"); print(table(h$treatment[!h$exclude], h$symptom[!h$exclude]))
cat("rows containing CJK code points:", sum(grepl("[一-鿿]", h$text)), "of", nrow(h),
    "; U+FFFD:", sum(grepl("�", h$text, fixed = TRUE)), "\n")
cat("author_text_valid == 0 among rows written:", sum(h$author_text_valid == 0), "\n")
back <- read.csv(out, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("re-read text identical:", identical(back$text, h$text), "\n")
cat("'<U+' escapes in file:", sum(grepl("<U+", readLines(out, encoding = "UTF-8", warn = FALSE), fixed = TRUE)), "\n")
