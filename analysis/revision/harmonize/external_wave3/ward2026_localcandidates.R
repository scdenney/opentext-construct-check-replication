# ==============================================================================
# ward2026_localcandidates.R -- harmonized file for card ward2026_localcandidates
# ==============================================================================
# Ward, A. J. "Place-Based Identity and the Demand for Local Candidates"
# (replication materials, file prefix ward_jop; GitHub
# albert-ward/replication-place-based-identity-local-candidates, HEAD 45fe4ce,
# 2026-10-30; data CC0 1.0 per README). Prolific US survey experiment,
# 16-18 April 2025, N = 4,803, preregistered osf.io/mwbcu (registration 782p4).
# Respondents were randomized to a neighborhood-identity writing prime
# (treatment) or an individual-identity writing prime (control); the written
# reflection is the manipulation. A three-round candidate conjoint and the
# belonging manipulation check follow.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/ward2026_localcandidates.R
#
# Input (data/external/ward2026_localcandidates/raw/, unchanged; MANIFEST.md):
#   ward_jop_localcandidates_exp_data.csv  one row per participant, before
#                                          exclusions (README, codebook sec. 2)
#   opentext_coding_prompt.txt             authors' LLM coding prompt; used only
#                                          to flag the answers it quotes
# Output: data/external/ward2026_localcandidates/harmonized_ward2026_localcandidates.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

raw_dir  <- "data/external/ward2026_localcandidates/raw"
out_csv  <- "data/external/ward2026_localcandidates/harmonized_ward2026_localcandidates.csv"
card_id  <- "ward2026_localcandidates"

# ---- 1. Read ------------------------------------------------------------------
# Same NA convention as the authors (readr read_csv defaults "" and "NA";
# Rmd l. 46). The file was written by readr::write_csv (data preparation
# script), which writes missing values as bare NA, so an answer typed as "NA"
# cannot be told apart from a missing one; no cell holds a quoted "NA", and
# every row has exactly one reflection (checked in section 2).
d <- read.csv(file.path(raw_dir, "ward_jop_localcandidates_exp_data.csv"),
              as.is = TRUE, check.names = FALSE, encoding = "UTF-8",
              na.strings = c("", "NA"))
stopifnot(nrow(d) == 4803, !anyDuplicated(d$ResponseId))

tr_txt <- d$treatment; ct_txt <- d$control
Encoding(tr_txt) <- "UTF-8"; Encoding(ct_txt) <- "UTF-8"
stopifnot(all(validUTF8(tr_txt[!is.na(tr_txt)])), all(validUTF8(ct_txt[!is.na(ct_txt)])))

# ---- 2. Treatment -------------------------------------------------------------
# The authors define assignment as having a treatment reflection:
# treatment_group = !is.na(treatment) (Rmd ll. 53-55; codebook l. 138). The
# file holds no separate Qualtrics randomizer field. Each row must have exactly
# one of the two reflections (checked), so no row lacks an assignment.
has_tr <- !is.na(d$treatment); has_ct <- !is.na(d$control)
stopifnot(!any(has_tr & has_ct), all(has_tr | has_ct))
treat <- as.integer(has_tr)        # 1 = neighborhood prime, 0 = individual prime

# ---- 3. Authors' exclusions (Rmd ll. 240-270; codebook l. 150) ----------------
# readr trims surrounding whitespace by default, so the authors' nchar() is on
# trimmed text; same here.
nc_tr <- nchar(trimws(tr_txt)); nc_ct <- nchar(trimws(ct_txt))
# short_text: bottom decile of reflection length, by arm: < 124 characters
# (treatment) or < 84 (control) (Rmd ll. 240-254). Selection on the text itself.
short_text <- (has_tr & nc_tr < 124) | (has_ct & nc_ct < 84)
# Duration ceiling: mean + 3 SD computed on the long conjoint data (Rmd
# ll. 259-262), which has 12 rows for every respondent (3 rounds x 2 tasks x 2
# profiles, ll. 160-166); rep(..., 12) reproduces that sd exactly.
dur <- d[["Duration (in seconds)"]]
dur_long <- rep(dur, each = 12)
threshold <- mean(dur_long, na.rm = TRUE) + 3 * sd(dur_long, na.rm = TRUE)
# Retained iff all filter() conditions are TRUE (dplyr drops NA), Rmd l. 270:
# duration >= 60, duration <= threshold, gpt_answer == 0 (hand flag for chatbot
# use, codebook l. 114), short_text == 0, reCAPTCHA > 0.4, attention check
# answered "Strongly disagree".
keep <- dur >= 60 & dur <= threshold & d$gpt_answer == 0 & !short_text &
  d$Q_RecaptchaScore > 0.4 & d$attention_check == "Strongly disagree"
keep[is.na(keep)] <- FALSE
exclude <- !keep
cat("Duration threshold (s):", round(threshold, 1), "\n")
cat("Excluded by criterion (may overlap): short", sum(dur < 60, na.rm = TRUE),
    "| long", sum(dur > threshold, na.rm = TRUE),
    "| gpt_answer", sum(d$gpt_answer != 0, na.rm = TRUE),
    "| short_text", sum(short_text),
    "| recaptcha", sum(d$Q_RecaptchaScore <= 0.4, na.rm = TRUE),
    "| attention", sum(d$attention_check != "Strongly disagree" |
                         is.na(d$attention_check)), "\n")

# ---- 4. Design columns --------------------------------------------------------
# pid_lean: pid2 where asked, else pid1 (Rmd ll. 57-62); routes the party
# primary shown in the conjoint. Pre-treatment.
pid_lean <- ifelse(is.na(d$pid2), d$pid1, d$pid2)
# Closed-ended outcome: belonging item, asked after the conjoint; 5 = strongly
# agree ... 1 = strongly disagree, Don't know = NA (Rmd ll. 461-470).
mc_map <- c("Strongly agree" = 5, "Agree" = 4, "Neither agree nor disagree" = 3,
            "Disagree" = 2, "Strongly disagree" = 1, "Don't know" = NA)
stopifnot(all(is.na(d$manipulation_check_1) | d$manipulation_check_1 %in% names(mc_map)))
manipulation_check <- unname(mc_map[d$manipulation_check_1])

# quoted_in_coding_prompt: TRUE for answers quoted (first 60 characters,
# whitespace- and case-normalized) as "Response:" examples in the authors'
# opentext_coding_prompt.txt. These were read by the wave-3 searcher and
# partly by the card drafter (see card provenance.contamination).
pl <- readLines(file.path(raw_dir, "opentext_coding_prompt.txt"),
                encoding = "UTF-8", warn = FALSE)
q <- sub('^Response: "(.*)"\\s*$', "\\1", grep('^Response: "', pl, value = TRUE))
norm <- function(x) tolower(trimws(gsub("\\s+", " ", x)))
keys <- substr(norm(q), 1, 60)
txt_all <- norm(ifelse(has_tr, tr_txt, ct_txt))
quoted <- Reduce(`|`, lapply(keys, function(k) grepl(k, txt_all, fixed = TRUE)))
cat("Prompt 'Response:' quotes:", length(keys), "| matching rows:", sum(quoted),
    "(treatment arm:", sum(quoted & has_tr), ")\n")

# ---- 5. Assemble --------------------------------------------------------------
h <- data.frame(
  id        = d$ResponseId,             # Qualtrics ResponseId (authors' id)
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = trimws(ifelse(has_tr, tr_txt, ct_txt)),
  exclude   = exclude,
  treatment = treat,
  pid_lean  = pid_lean,
  manipulation_check = manipulation_check,
  quoted_in_coding_prompt = quoted,
  # Author's hand codes, validation sample of 303 treatment reflections only
  # (codebook ll. 132-133; Rmd l. 1051). PI / PD / MX / UNUSABLE; negative 0/1.
  # LLM codes (llm_code, negative, pi_claims, pd_claims) and the free-text
  # flag / human_flag notes are deliberately not carried over.
  human_code     = d$human_code,
  human_negative = d$human_negative,
  stringsAsFactors = FALSE
)
stopifnot(all(is.na(h$human_code) | h$treatment == 1),
          sum(!is.na(h$human_code)) == 303)

# Rows with empty text are dropped (shared brief).
n_empty <- sum(h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- 6. Checks ----------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
cat("Non-excluded rows per arm (1 = neighborhood prime):\n")
print(table(h$treatment[!h$exclude]))
cat("Human-coded rows, non-excluded:", sum(!is.na(h$human_code) & !h$exclude), "\n")
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
