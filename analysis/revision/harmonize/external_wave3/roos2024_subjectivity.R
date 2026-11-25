# ==============================================================================
# roos2024_subjectivity.R -- harmonized file for card roos2024_subjectivity
# ==============================================================================
# Roos, C. A., van der Linden, L., & Krahmer, E. (2024). Mitigating perceived
# polarization by acknowledging subjectivity: An experimental study of the
# impact of differently phrasing comments in online news discussions.
# PsyArXiv preprint, doi:10.31234/osf.io/f62sg.
# Data: DataverseNL doi:10.34894/8HCKQU (CC BY-NC 4.0). Preregistration:
# https://aspredicted.org/1RL_QY1.
# Within-subject online experiment (Qualtrics, the Netherlands, 3-16 May 2023),
# N = 175 native Dutch speakers; each read one subjectively and one
# non-subjectively phrased NU.nl discussion section (two tasks per person).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/roos2024_subjectivity.R
#
# Inputs (deposited originals; no Dataverse ingest exists for these files):
#   raw/RestructuredDataset.sav  (350 rows = participant x task; the authors'
#     analysis file, R_code.R line 7)
#   raw/CodingFirstRound.xlsx    (354 rows; Participant, Observation, Comment,
#     Coder 1, Coder 2; first-round codes of the two coders)
# Output: data/external/roos2024_subjectivity/harmonized_roos2024_subjectivity.csv
#
# Text item (Comment_Text; Questionnaire.pdf; label in the .sav): "Stel dat je
# mee zou doen aan deze online discussie, hoe zou jouw reactie eruitzien? Typ
# hieronder jouw reactie. Onderstaand kun je de discussie teruglezen ter
# inspiratie voor je antwoord." Asked after the perception scales for each
# discussion; the discussion is shown again below the box.
# Not carded here: Comment_Why ("Waarom zou jij wel of niet meedoen aan deze
# online discussie?"), the next item.
#
# Blindness: no text value is printed; only counts. The Comment column of the
# coder file is never read into R output.
# ==============================================================================

suppressPackageStartupMessages({ library(haven); library(readxl) })

raw_dir <- "data/external/roos2024_subjectivity/raw"
out_csv <- "data/external/roos2024_subjectivity/harmonized_roos2024_subjectivity.csv"
card_id <- "roos2024_subjectivity"

d <- read_sav(file.path(raw_dir, "RestructuredDataset.sav"))
stopifnot(nrow(d) == 350)
num <- function(x) as.numeric(zap_labels(x))

# ---- Design -------------------------------------------------------------------
# SPSS_Syntax.sps (RECODE Index1 ... INTO Condition / Time / Order / Topic):
#   Condition 0 = objective (non-subjective), 1 = subjective (Codebook.pdf)
#   Time      1 = first, 2 = second discussion shown
#   Order     1 non-polarized objective -> polarized subjective
#             2 polarized subjective    -> non-polarized objective
#             3 non-polarized subjective -> polarized objective
#             4 polarized objective     -> non-polarized subjective
#   Topic     1 Zwarte Piet (Black Pete), 2 ChatGPT, 3 Japan, 4 smokers'
#             health-care premium; PolarizedTopic 2 = yes (topics 1, 4)
# Each participant saw one polarized and one non-polarized topic, one in each
# phrasing; Qualtrics allocated the 32 blocks at random (Questionnaire.pdf:
# "[Random allocation to one of the eight stimuli discussions.]"; prereg Q4:
# 16 cells of polarized/non-polarized x objective/subjective x order).
cond  <- num(d$Condition); time <- num(d$Time); ord <- num(d$Order)
topic <- num(d$Topic); pol <- num(d$PolarizedTopic)
stopifnot(all(cond %in% 0:1), all(time %in% 1:2), all(ord %in% 1:4), all(topic %in% 1:4))
pair <- tapply(cond, d$ID1, function(z) paste(sort(z), collapse = ""))
stopifnot(length(pair) == 175, all(pair == "01"))       # one task per phrasing
stopifnot(all(tapply(pol, d$ID1, function(z) paste(sort(z), collapse = "")) == "12"))
topic_lab <- c("zwarte_piet", "chatgpt", "japan", "smokers_premium")[topic]

# ---- Text and encoding --------------------------------------------------------
txt <- trimws(as.character(d$Comment_Text))
stopifnot(all(validUTF8(txt)))
enc_count <- function(x) c(
  non_ascii   = sum(grepl("[^\\x01-\\x7F]", x, perl = TRUE)),
  diacritics  = sum(grepl("[À-ſ]", x)),   # e.g. e-acute, e-diaeresis
  replacement = sum(grepl("�", x, fixed = TRUE)))
enc_before <- enc_count(txt)

# ---- Human codes --------------------------------------------------------------
# CodingComments (restructured file; Codebook.pdf): 0 = objective, 1 = reply
# contains subjectivity indicators, NA = no substantive reply (meta-comment
# or will not join; preprint, Measures). Final codes after the coders
# resolved their first-round disagreements.
# CodingFirstRound.xlsx: legend in the file reads "0 = subjectief, 1 =
# objectief, NA = geen inhoudelijk comment", i.e. the REVERSE of
# CodingComments; recoded below to 1 = subjective. Observation = Time (checked:
# on all 301 rows where both coders agree, CodingComments == 1 - Coder 1 when
# matched on Time, versus 233 when matched on Condition). Participants 39 and
# 80 are in the coder file but not in the analysis file (removed by the
# authors: not native Dutch, or failed both attention checks; preprint,
# Participants).
cf <- as.data.frame(suppressMessages(read_excel(file.path(raw_dir, "CodingFirstRound.xlsx"))))
cf <- cf[, c("Participant", "Observation", "Coder 1", "Coder 2")]   # Comment column dropped
rc <- function(v) { v <- suppressWarnings(as.numeric(v)); stopifnot(all(v %in% c(0, 1, NA))); 1 - v }
cf$c1 <- rc(cf[["Coder 1"]]); cf$c2 <- rc(cf[["Coder 2"]])
k_d <- paste(num(d$ID1), time); k_c <- paste(cf$Participant, cf$Observation)
stopifnot(!anyDuplicated(k_c), all(k_d %in% k_c))
m <- match(k_d, k_c)
cc <- num(d$CodingComments)
agree <- !is.na(cf$c1[m]) & !is.na(cf$c2[m]) & cf$c1[m] == cf$c2[m]
stopifnot(all(cc[agree] == cf$c1[m][agree]))

# ---- Exclusion ----------------------------------------------------------------
# The authors removed non-native speakers and participants failing both
# attention checks before deposit (preprint, Participants); all 350 rows are in
# their analyses (R_code.R). For the reply-behaviour test (H3; R_code.R lines
# 135-138, glmer on CodingComments) rows coded NA (no serious reply: meta-
# comment or "would not join") drop out: preprint, Measures, "it was coded as
# missing". Those rows are flagged exclude = TRUE here. This exclusion is
# defined from the text by the human coders (post-treatment); see card notes.
excl <- is.na(cc)

h <- data.frame(
  id              = paste0(num(d$ID1), "_t", time),   # authors' ID1 + Time
  dataset         = card_id,
  country         = "NL",
  lang            = "nl",
  text            = txt,
  exclude         = excl,
  treatment       = as.integer(cond),                 # 1 = subjective discussion
  respondent_id   = num(d$ID1),
  topic           = topic_lab,
  polarized_topic = as.integer(pol == 2),
  order           = ord,                              # authors' Order code 1-4
  time            = time,                             # 1 first, 2 second task
  comment_chance  = num(d$Comment_Chance),            # closed: 1 very small - 5 very big
  human_subjective = cc,                              # final (resolved) code
  human_coder1    = cf$c1[m],                         # first round, 1 = subjective
  human_coder2    = cf$c2[m],
  stringsAsFactors = FALSE
)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = subjective):\n")
print(table(h$treatment[!h$exclude]))
cat("All rows, arm x topic:\n"); print(table(h$treatment, h$topic))
cat("Human final code by arm (non-excluded):\n")
print(table(arm = h$treatment[!h$exclude], subjective = h$human_subjective[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
cat("Encoding counts (rows) before write:", paste(names(enc_before), enc_before, collapse = ", "), "\n")
enc_after <- enc_count(r$text)
cat("Encoding counts (rows) after re-read:", paste(names(enc_after), enc_after, collapse = ", "), "\n")
stopifnot(identical(enc_before, enc_after), enc_after[["replacement"]] == 0)
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
