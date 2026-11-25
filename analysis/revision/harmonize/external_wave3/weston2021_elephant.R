# ==============================================================================
# weston2021_elephant.R -- harmonized file for card weston2021_elephant
# ==============================================================================
# Weston, Mills & von Keyserlingk (2021), "Your happiness or mine: influence of
# affective states and level of contact on public perceptions of elephant
# tourism", Animal Welfare 30(3): 279-293, doi:10.7120/09627286.30.3.005.
# Data: figshare doi:10.6084/m9.figshare.11607354.v2 (CC BY 4.0).
# 2 (elephant feels excellent / feels terrible) x 2 (low / high contact with
# tourists) vignette, MTurk, US, January 2019; 918 recruited, 590 analyzed.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/weston2021_elephant.R
#
# Input: data/external/weston2021_elephant/raw/UFAW_elephant_manuscript_data.xlsx
#   (figshare "UFAW elephant manuscript_data (4) (1).xlsx", unchanged). One
#   sheet; row 1 = Qualtrics ids (several duplicated), row 2 = question wording
#   (Qualtrics second header row), rows 3-920 = 918 participants.
# Output: data/external/weston2021_elephant/harmonized_weston2021_elephant.csv
#
# Text item: column 19 (Q18) "Please explain your above answers:" (article,
# Materials and methods, Study design), after the eight Likert items.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(readxl))

raw_file <- "data/external/weston2021_elephant/raw/UFAW_elephant_manuscript_data.xlsx"
out_csv  <- "data/external/weston2021_elephant/harmonized_weston2021_elephant.csv"
card_id  <- "weston2021_elephant"

x <- as.data.frame(read_excel(raw_file, sheet = 1, col_names = FALSE,
                              col_types = "text", .name_repair = "minimal"))
stopifnot(nrow(x) == 920, ncol(x) == 26)
ids <- as.character(x[1, ]); wording <- as.character(x[2, ])
d <- x[-(1:2), ]; names(d) <- paste0("c", 1:26)
ne <- function(v) !is.na(v) & trimws(v) != ""

# ---- Vignette blocks -> arms ---------------------------------------------------
# The file has no condition column. Each vignette was a separate Qualtrics
# block of two questions: the vignette text with "I have read and understood
# the above information" (value 1 when shown) and the manipulation check
# "Do you believe that Molly spends all her time feeling [excellent/terrible]?".
# The mapping below is read from the question wording in header row 2 (the
# instrument itself), matched to the vignette texts in article Table 1:
#   cols 3-4  (Q22/Q23): "not allowed to touch or ride her" ... "feeling excellent" -> FE/LC
#   cols 5-6  (Q24/Q28): "not allowed to touch or ride her" ... "feeling terrible"  -> FT/LC
#   cols 7-8  (Q25/Q29): "allowed to touch and ride her"    ... "feeling excellent" -> FE/HC
#   cols 9-10 (Q27/Q30): "allowed to touch and ride her"    ... "feeling terrible"  -> FT/HC
blocks <- data.frame(vig = c(3, 5, 7, 9), chk = c(4, 6, 8, 10),
                     feel = c("excellent", "terrible", "excellent", "terrible"),
                     contact = c("low", "low", "high", "high"))
for (b in seq_len(nrow(blocks))) {
  w <- wording[blocks$vig[b]]
  stopifnot(grepl(paste0("feeling ", blocks$feel[b]), w, fixed = TRUE),
            grepl(if (blocks$contact[b] == "low") "not allowed to touch or ride her"
                  else "Tourists are allowed to touch and ride her", w, fixed = TRUE),
            grepl(paste0("feeling ", blocks$feel[b], "?"), wording[blocks$chk[b]], fixed = TRUE))
}
stopifnot(startsWith(wording[19], "Please explain your above answers"),
          identical(ids[11:18], c("Q14", "Q15", "Q16", "Q17", "Q27", "Q28", "Q29", "Q30")),
          startsWith(wording[26], "Have you participated in elephant tourism"))

shown <- sapply(blocks$vig, function(j) ne(d[[paste0("c", j)]]))
stopifnot(all(rowSums(shown) <= 1))
blk <- ifelse(rowSums(shown) == 1, max.col(shown), NA_integer_)
cat("Rows with no vignette block shown (no randomized assignment):", sum(is.na(blk)), "\n")

# Manipulation check answer from the shown block. Qualtrics codes 1 = Yes,
# 2 = No (no codebook deposited; the coding is confirmed by the exclusion count
# below, which reproduces the article's 227 only if 2 = failed).
chk_mat <- sapply(blocks$chk, function(j) d[[paste0("c", j)]])
manip <- vapply(seq_len(nrow(d)), function(i)
  if (is.na(blk[i])) NA_character_ else chk_mat[i, blk[i]], character(1))

# ---- Exclusions (article, Results and discussion, Quantitative findings) ------
# "After excluding incomplete surveys (n = 69), failed manipulation checks
# (n = 227) ... and incomplete qualitative responses (n = 32) ..., the sample
# consisted of 590 participants."
#  (a) incomplete survey: not all 8 Likert items and all 7 demographic items
#      answered. 849 complete = 918 - 69 (reproduced exactly).
#  (b) failed manipulation check: answer 2 among complete rows: 227 (exact).
#  (c) incomplete qualitative responses (n = 32): a judgment on the content of
#      the text (Mazzola et al. 2011); all 622 remaining rows have non-empty
#      text, so these 32 cannot be identified without reading the answers.
#      NOT reproduced: the non-excluded set is 622 rows, the authors' 590 plus
#      32 rows they dropped on content.
likert_ok <- rowSums(sapply(11:18, function(j) ne(d[[paste0("c", j)]]))) == 8
demog_ok  <- rowSums(sapply(20:26, function(j) ne(d[[paste0("c", j)]]))) == 7
complete  <- likert_ok & demog_ok
failed    <- complete & manip %in% "2"
cat("Incomplete:", sum(!complete), "| failed manipulation check among complete:",
    sum(failed), "| retained:", sum(complete & !failed), "\n")
stopifnot(sum(!complete) == 69, sum(failed) == 227)
stopifnot(all(!is.na(blk[complete])))
exclude <- !complete | failed | is.na(blk)

# ---- Id -----------------------------------------------------------------------
# ResponseId is filled in 743 rows only (missing in all 69 incomplete rows and
# in 106 complete ones). id = ResponseId where present, else "row" + the row
# number in the sheet (header rows counted, so the first participant is row3).
rid <- d$c1
id <- ifelse(ne(rid), rid, paste0("row", seq_len(nrow(d)) + 2))
stopifnot(!anyDuplicated(id))

num <- function(j) as.integer(d[[paste0("c", j)]])
h <- data.frame(
  id        = id,
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  # Line breaks normalized: 9 answers hold CR or CRLF, which read.csv does not
  # round-trip; \r\n and lone \r become \n. No other change to the text.
  text      = trimws(gsub("\r\n?", "\n", d$c19)),
  exclude   = exclude,
  # 1 = elephant feels terrible (FT), 0 = feels excellent (FE)
  treatment = ifelse(is.na(blk), NA_integer_, as.integer(blocks$feel[blk] == "terrible")),
  # Independently randomized second factor: level of contact with tourists.
  contact   = ifelse(is.na(blk), NA_character_, blocks$contact[blk]),
  manip_check = as.integer(manip),               # 1 = yes (passed), 2 = no
  # Likert items, 1-7 as stored (Table 2). The Methods give 1 = strongly
  # disagree, 7 = strongly agree; the Figure 1 caption says the reverse. The
  # FE > FT effect on these items fits the Methods coding; kept as stored.
  support_important_have = num(11), support_important_support = num(12),
  support_pay = num(13), support_visit = num(14),
  emo_wellbeing = num(15), emo_exciting = num(16), emo_elated = num(17), emo_happy = num(18),
  prior_elephant_tourism = num(26),              # Q22 demographic, asked after the text; codes undocumented
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text[!is.na(h$text)])))
n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = feels terrible):\n")
print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows, arm x contact:\n")
print(table(h$treatment[!h$exclude], h$contact[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
