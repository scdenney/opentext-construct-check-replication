# ==============================================================================
# bonilla2024_sourcecues.R -- harmonized files for cards bonilla2024_sourcecues_s3
# and bonilla2024_sourcecues_s4
# ==============================================================================
# Source: Bonilla, T., Filindra, A., & Lajevardi, N. (2022). How source cues
# shape evaluations of group-based derogatory political messages. Journal of
# Politics 84(4), 1979-1996, doi:10.1086/720308. Data doi:10.7910/DVN/2CVG5E
# (CC0 1.0), folder Replication/Study3_4.
#
# Studies 3 and 4 are ONE survey of 870 Black respondents (Lucid, 2019) with two
# rounds per respondent (questionnaire study3_4_questionaire.docx, blocks
# "Response to statement round 1" and "... round 2"):
#   round 1 (Study 3): treat1 = White/Black candidate x neutral/derogatory
#     statement about Black people; open item S1_reason.
#   round 2 (Study 4): treat2 = White/Muslim candidate x neutral/derogatory
#     statement about Muslims; open item S2_reason.
# Both cards therefore share respondents (same responseid in both files).
# Round-2 wording is not independent of round 1: derog2 = 1 - derog1 for every
# respondent with a round-2 assignment (each saw one derogatory and one neutral
# statement). Round-2 speaker (White vs Muslim) is independent of round 1.
#
# Text: taken from the raw Qualtrics export study3_4_data.tsv (UTF-16LE), not
# from the .dta files: the .dta copies differ from the export only in the 72
# (S1) and 53 (S2) answers with non-ASCII characters (curly apostrophes and
# quotes, two emoji), where the Stata conversion changed the characters.
# Assignment, closed-ended items and human codes come from the authors'
# study3_cleaned_coded.dta and study4_cleaned_coded.dta (and
# study4_data_coded_v2.dta for the second round of Study 4 codes), joined on
# responseid.
#
# Blind builder: never prints open-text values or coder notes, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/bonilla2024_sourcecues.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS  <- "bonilla2024_sourcecues"
RAW <- file.path("data/external", DS, "raw")
num <- function(x) as.numeric(zap_labels(x))
clean_text <- function(x) { x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x); trimws(x) }

raw <- read.delim(file.path(RAW, "study3_4_data.tsv"), fileEncoding = "UTF-16LE",
                  quote = "\"", stringsAsFactors = FALSE, check.names = FALSE)
s3  <- read_dta(file.path(RAW, "study3_cleaned_coded.dta"))
s4  <- read_dta(file.path(RAW, "study4_cleaned_coded.dta"))
v2  <- read_dta(file.path(RAW, "study4_data_coded_v2.dta"))
stopifnot(nrow(s3) == 870, !anyDuplicated(s3$responseid), setequal(s3$responseid, s4$responseid),
          setequal(s3$responseid, v2$responseid), all(s3$responseid %in% raw$ResponseId))
s4 <- s4[match(s3$responseid, s4$responseid), ]
v2 <- v2[match(s3$responseid, v2$responseid), ]
rw <- raw[match(s3$responseid, raw$ResponseId), ]

# Arm labels from the value labels of treat1/treat2 (study3_cleaned_coded.dta):
# treat1 1 White Neutral, 2 White Derogator, 3 Black Neutral, 4 Black Derogator;
# treat2 1 White Neutral, 2 White Derogator, 3 Muslim Neutral, 4 Muslim Derogator.
arm1 <- c("white_neutral", "white_derog", "black_neutral", "black_derog")[num(s3$treat1)]
arm2 <- c("white_neutral", "white_derog", "muslim_neutral", "muslim_derog")[num(s3$treat2)]
# Consistency with the authors' dummies. study3_4_cleanup.do (fetched to
# data/dvq/outbox/card_bonilla_01/, lines 60-109; not in raw/) sets treat1/treat2 from which awareness item was shown, then black1,
# derog1, derog2 from them, and drops rows with no treat1. white2 is set to 1 for
# every row (a typo in the do-file), so the round-2 speaker comes from treat2.
stopifnot(all((num(s3$treat1) %in% 3:4) == (s3$black1 == 1)),
          all((num(s3$treat1) %in% c(2, 4)) == (s3$derog1 == 1)),
          all(is.na(s3$treat2) == is.na(s3$derog2)),
          all((num(s3$treat2) %in% c(2, 4)) == (s3$derog2 == 1), na.rm = TRUE))

common <- function(card) data.frame(id = s3$responseid, dataset = card, country = "US",
                                    lang = "en", stringsAsFactors = FALSE)

# ---- Study 3 (round 1) -------------------------------------------------------
# Exclusions: none. All 870 rows of the authors' analysis file have treat1.
# Human codes, round-1 instructions (coding_instructions1.docx; written for the
# S2 item but applied with the same columns to S1, per its first paragraph):
#   code1 -1 pushback against the statement / 0 ambivalent / 1 support
#   code2 -2 clear pushback because inappropriate or racist ... 2 clear endorsement
#   code3 1 = mentions discrimination against Black people
#   code4 1 = mentions discrimination against other stigmatized groups
#   code5 1 = mentions discrimination against Muslims
#   code6 1 = mentions inclusivity (candidate thinking positively or collectively
#             about different races)
#   nonsense 1 = incoherent text (coder 1's sheet column "Nonsense" is filled for
#             only 271 rows; coder 2's S1_code_Nonsense_2 for 828).
# Suffix _1 = coder 1 (study3_coder1.dta), _2 = coder 2 (study3_coder2.dta).
h3 <- cbind(common("bonilla2024_sourcecues_s3"),
  text = clean_text(rw$S1_reason), exclude = FALSE, treatment = arm1,
  speaker_black = as.integer(s3$black1 == 1), derog = as.integer(s3$derog1 == 1),
  s1_agree = num(s3$s1_agree),             # 1 strongly agree ... 5 strongly disagree (no 3)
  s1_appropriate = num(s3$s1_appropriate), # 1 very appropriate ... 4 very inappropriate
  human_c1_code1 = num(s3$S1_code1_1), human_c1_code2 = num(s3$S1_code2_1),
  human_c1_code3 = num(s3$S1_code3_1), human_c1_code4 = num(s3$S1_code4_1),
  human_c1_code5 = num(s3$S1_code5_1), human_c1_code6 = num(s3$S1_code6_1),
  human_c1_nonsense = num(s3$Nonsense),
  human_c2_code1 = num(s3$S1_code1_2), human_c2_code2 = num(s3$S1_code2_2),
  human_c2_code3 = num(s3$S1_code3_2), human_c2_code4 = num(s3$S1_code4_2),
  human_c2_code5 = num(s3$S1_code5_2), human_c2_code6 = num(s3$S1_code6_2),
  human_c2_nonsense = num(s3$S1_code_Nonsense_2),
  stringsAsFactors = FALSE)

# ---- Study 4 (round 2) -------------------------------------------------------
# Exclusions: 56 respondents have no treat2 (no round-2 assignment); the authors
# drop them for Study 4 ("drop if treat2==." before saving study4_cleaned.dta,
# study3_4_cleanup.do line 240). None has S2 text, so they also fall out with the empty-text rule.
# Human codes: first round of coding uses the same six codes as Study 3 (S2_*,
# coding_instructions1.docx); second round ("v2", coding_instructions2.docx,
# study4_data_coded_v2.dta) uses new codes:
#   code0 nonsense; code4 mentions other groups besides Muslims, code4a its tone
#   (-1/0/1); code5 mentions Muslims, code5a tone toward Muslims (-1/0/1);
#   code6 empathetic toward Muslims; code7 too vague to extract meaning.
#   (code8 "statement inappropriate" is described in the instructions but is not
#   in the deposited file.) v2 coder 2's code4 is stored as text; converted here.
h4 <- cbind(common("bonilla2024_sourcecues_s4"),
  text = clean_text(rw$S2_reason), exclude = is.na(s3$treat2), treatment = arm2,
  speaker_muslim = ifelse(is.na(s3$treat2), NA, as.integer(num(s3$treat2) %in% 3:4)),
  derog = ifelse(is.na(s3$treat2), NA, as.integer(s3$derog2 == 1)),
  round1_arm = arm1,                        # round-1 assignment (carry-over check)
  s2_agree = num(s3$s2_agree), s2_appropriate = num(s3$s2_appropriate),
  human_c1_code1 = num(s4$S2_code1_1), human_c1_code2 = num(s4$S2_code2_1),
  human_c1_code3 = num(s4$S2_code3_1), human_c1_code4 = num(s4$S2_code4_1),
  human_c1_code5 = num(s4$S2_code5_1), human_c1_code6 = num(s4$S2_code6_1),
  human_c2_code1 = num(s4$S2_code1_2), human_c2_code2 = num(s4$S2_code2_2),
  human_c2_code3 = num(s4$S2_code3_2), human_c2_code4 = num(s4$S2_code4_2),
  human_c2_code5 = num(s4$S2_code5_2), human_c2_code6 = num(s4$S2_code6_2),
  human_c2_nonsense = num(s4$S2_code_Nonsense_2),
  human_v2c1_nonsense = num(v2$code0_1), human_v2c1_othergroup = num(v2$code4_1),
  human_v2c1_othergroup_tone = num(v2$code4a_1), human_v2c1_muslim = num(v2$code5_1),
  human_v2c1_muslim_tone = num(v2$code5a_1), human_v2c1_empathy = num(v2$code6_1),
  human_v2c1_vague = num(v2$code7_1),
  human_v2c2_nonsense = num(v2$code0_2),
  human_v2c2_othergroup = suppressWarnings(as.numeric(as.character(v2$code4_2))),
  human_v2c2_othergroup_tone = num(v2$code4a_2), human_v2c2_muslim = num(v2$code5_2),
  human_v2c2_muslim_tone = num(v2$code5a_2), human_v2c2_empathy = num(v2$code6_2),
  human_v2c2_vague = num(v2$code7_2),
  stringsAsFactors = FALSE)
# The "Notes" column of coder 1 (study4_cleaned_coded.dta) is numeric 0/1 and
# undocumented; coder notes (free text) are not carried over.

# ---- write and check ---------------------------------------------------------
check_write <- function(h, card) {
  out <- file.path("data/external", DS, paste0("harmonized_", card, ".csv"))
  cat("\n=====", card, "\nRows before dropping empty text:", nrow(h), "\n")
  h <- h[nzchar(h$text), ]                 # rows with empty text are kept out (brief)
  stopifnot(!anyDuplicated(h$id))
  write.csv(h, out, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  cat(sprintf("%s: rows = %d\ncolumns = %s\n", out, nrow(h), paste(names(h), collapse = ",")))
  cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
  back <- read.csv(out, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character(0))
  cat("Re-read text identical:", identical(back$text, h$text), "\n")
  cat("'<U+' escapes in file:", sum(grepl("<U+", readLines(out, encoding = "UTF-8"), fixed = TRUE)), "\n")
  hc <- grep("^human_", names(h), value = TRUE)
  cat("Non-missing human codes (non-excluded rows):\n")
  print(sapply(h[!h$exclude, hc], function(z) sum(!is.na(z))))
  invisible(h)
}
check_write(h3, "bonilla2024_sourcecues_s3")
check_write(h4, "bonilla2024_sourcecues_s4")
