# ==============================================================================
# hahn2024_oeq.R -- harmonized files for cards hahn2024_oeq_pos, hahn2024_oeq_neg
# ==============================================================================
# Hahn, Kroehne & Merk (2024), "Improving and analyzing open-ended survey
# responses", Zeitschrift fuer Psychologie 232(3), 171-180,
# doi:10.1027/2151-2604/a000566. Data and analysis code: Hahn & Merk (2024),
# PsychArchives doi:10.23668/psycharchives.14490 (CC-BY 4.0).
#
# Course evaluation of abiturma math tutoring courses (several German cities,
# October 2017 - February 2018). 2 x 2 field experiment, students randomly
# assigned (article, Method > Procedure): position of the two open items on
# the teacher (beginning vs end of the survey) x instruction (motivating vs
# neutral).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/hahn2024_oeq.R
#
# Input (data/external/hahn2024_oeq/raw/, unchanged from PsychArchives):
#   Data_Long_Format.csv  long format, one row per completed survey x question.
#                         The authors' RDA.qmd reads it as
#                         data/eva_data_OEQexperimentI.csv (RDA.qmd, chunk
#                         "Import data"). Same columns; the four arm sizes
#                         (distinct completed_survey_id per survey_id: 788 /
#                         775 / 804 / 816) equal the article's cell sizes
#                         (Method > Data Description), and the total (3,183)
#                         equals the article's N.
#
# Outputs:
#   data/external/hahn2024_oeq/harmonized_hahn2024_oeq_pos.csv  (question 47)
#   data/external/hahn2024_oeq/harmonized_hahn2024_oeq_neg.csv  (question 48)
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(quanteda)
})

raw_csv <- "data/external/hahn2024_oeq/raw/Data_Long_Format.csv"
out_dir <- "data/external/hahn2024_oeq"

# ---- 1. Read ------------------------------------------------------------------
# The authors read with readr::read_csv, which turns "" and "NA" into NA.
# fread turns "NA" into NA and keeps "" as ""; empty strings are dropped below,
# so the set of answers kept is the same as the authors' drop_na() set.
d <- fread(raw_csv, encoding = "UTF-8", na.strings = "NA")
stopifnot(nrow(d) == 131161, !anyDuplicated(d$unique_identifier))

# ---- 2. Arms --------------------------------------------------------------------
# RDA.qmd, chunk "Import data":
#   position    = ifelse(survey_id %in% c(2,3), "at the beginning", "at the end")
#   instruction = ifelse(survey_id %in% c(2,5), "motivating instruction",
#                                                "neutral instruction")
# Check against the article's cell sizes (Method > Data Description):
# beginning + motivating 788, beginning + neutral 775, end + motivating 816,
# end + neutral 804.
n_arm <- d[, .(n = uniqueN(completed_survey_id)), keyby = survey_id]
stopifnot(identical(n_arm$survey_id, 2:5),
          identical(n_arm$n, c(788L, 775L, 804L, 816L)))

# Unit of randomization: the article says students were randomly assigned;
# the data agree (206 of 287 classes contain all four survey versions, and
# only 3 contain one). Printed here as a count.
arms_per_class <- d[, .(k = uniqueN(survey_id)), by = class_id]
cat("Classes by number of survey versions present (1/2/3/4):\n")
print(table(arms_per_class$k))

# ---- 3. One file per open item ----------------------------------------------------
build <- function(qid, card_id, extra_stop) {
  q <- d[question_id == qid]
  # One row per completed survey for this item (checked).
  stopifnot(!anyDuplicated(q$completed_survey_id))
  txt <- q$answer_text
  stopifnot(all(validUTF8(txt[!is.na(txt)])))

  # Authors' text set: drop_na() on answer_text (RDA.qmd, chunks "Corpus und
  # DTM (positive feedback)" / "(negative feedback)"). We also drop answers
  # that are empty after trimming (shared brief).
  keep <- !is.na(txt) & nchar(trimws(txt)) > 0
  cat(card_id, ": NA answers", sum(is.na(txt)),
      "| empty after trim", sum(!is.na(txt) & !keep), "\n")
  q <- q[keep]; txt <- txt[keep]

  # Authors' STM preprocessing (same chunks): punctuation, numbers, URLs,
  # symbols and Snowball German stopwords removed (plus "vielleicht",
  # "bisschen" and its sharp-s spelling for the negative item), Latin-script
  # tokens kept; documents with an empty DTM row are dropped from the STM only
  # (rowSums(dtm) > 0), not from the BERTopic analysis. The flag is kept as a
  # design column, NOT as an exclusion: it depends on the answer's wording,
  # which is post-treatment (e.g. a one-word "nothing" answer is a stopword).
  toks <- tokens(txt, remove_punct = TRUE, remove_numbers = TRUE,
                 remove_url = TRUE, remove_symbols = TRUE)
  toks <- tokens_remove(toks, stopwords("de", source = "snowball"))
  if (length(extra_stop)) toks <- tokens_remove(toks, pattern = extra_stop)
  toks <- tokens_keep(toks, pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")
  stm_empty <- rowSums(dfm(toks)) == 0

  h <- data.frame(
    id        = q$completed_survey_id,   # authors' survey id: one per completed
                                         # questionnaire (respondent_hash is not
                                         # unique: 16 hashes completed 2 surveys)
    dataset   = card_id,
    country   = "DE",
    lang      = "de",
    text      = trimws(txt),
    # No respondent-level exclusion in the authors' code: the surface models
    # use every row of items 47/48, the topic models every non-NA answer.
    exclude   = FALSE,
    # 1 = open items at the end of the survey (survey_id 4, 5),
    # 0 = at the beginning (survey_id 2, 3). RDA.qmd "Import data".
    treatment = as.integer(q$survey_id %in% c(4, 5)),
    # Second randomized factor: 1 = motivating instruction (survey_id 2, 5),
    # 0 = neutral (3, 4). RDA.qmd "Import data".
    cue       = as.integer(q$survey_id %in% c(2, 5)),
    survey_id = q$survey_id,
    # Grouping (not randomization units): class, course, teacher. The authors'
    # models use random intercepts for teacher_id and respondent_hash.
    class_id  = q$class_id,
    course_id = q$course_id,
    teacher_id = q$teacher_id,
    respondent_hash = q$respondent_hash,
    authors_stm_empty = stm_empty,
    stringsAsFactors = FALSE
  )
  stopifnot(all(h$text != ""), !anyDuplicated(h$id))

  out_csv <- file.path(out_dir, paste0("harmonized_", card_id, ".csv"))
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

  # ---- Checks ----
  cat("\n==", card_id, "==\nRows written:", nrow(h), "\n")
  cat("Non-excluded rows by position (treatment: 0 = beginning, 1 = end):\n")
  print(table(treatment = h$treatment[!h$exclude]))
  cat("Non-excluded rows by cue (0 = neutral, 1 = motivating):\n")
  print(table(cue = h$cue[!h$exclude]))
  cat("By survey_id:\n"); print(table(h$survey_id))
  cat("Answers the authors' STM drops (empty DTM row), by position:\n")
  print(table(treatment = h$treatment, authors_stm_empty = h$authors_stm_empty))
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE,
                na.strings = character(0))
  same <- identical(r$text, h$text)
  cat("Re-read text identical:", same, "\n")
  stopifnot(same)
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n")
  stopifnot(n_esc == 0)
  invisible(h)
}

# Item keys (raw/Question_keys_data.csv):
#   47 "Was hat {teacher} besonders gut gemacht?"   -> positive feedback
#   48 "Was koennte {teacher} in Zukunft besser machen?" -> negative feedback
build(47, "hahn2024_oeq_pos", character(0))
build(48, "hahn2024_oeq_neg", c("vielleicht", "bisschen", "bißchen"))
