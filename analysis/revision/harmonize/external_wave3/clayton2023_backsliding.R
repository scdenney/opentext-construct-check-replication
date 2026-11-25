# ==============================================================================
# clayton2023_backsliding.R -- harmonized file for card clayton2023_backsliding
# ==============================================================================
# Clayton, K. (2023). Interventions to Help Citizens Identify Incremental
# Democratic Backsliding. TESS study clayton1617 (TESS 134), NORC AmeriSpeak,
# fielded 11/03/2023-12/07/2023, N = 5,717. OSF https://osf.io/m5adk/.
# Design: 6 frames (control + harm, sanctity, authority, fairness, loyalty
# "specific threat" paragraphs) x 2 parties of the actor = 12 cells.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/clayton2023_backsliding.R
#
# Input:  data/external/clayton2023_backsliding/raw/9089_TESS_134_Clayton_client_file_20231218.sav
# Output: data/external/clayton2023_backsliding/harmonized_clayton2023_backsliding.csv
#
# Text item: CONSIDER_WHY, "In a few words, please tell us why you
# [support/oppose] what the [Republicans/Democrats] did in this scenario?"
# (questionnaire raw/9089.134_TESS_Clayton_2023_vFINALsimple.docx, after
# THREAT, THREAT_STRENGTH, CONSIDER, CONSIDER_STRENGTH; shown if CONSIDER = 1,2).
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/clayton2023_backsliding/raw/9089_TESS_134_Clayton_client_file_20231218.sav"
out_csv  <- "data/external/clayton2023_backsliding/harmonized_clayton2023_backsliding.csv"
card_id  <- "clayton2023_backsliding"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 5717, !anyDuplicated(d$CaseId))
num <- function(v) as.integer(zap_labels(v))
cond  <- num(d$DOV_CONDITION)
frame <- num(d$DOV_FRAME_ASSIGN)
party <- num(d$DOV_PARTY_ASSIGN)

# ---- Cell -> factor mapping (questionnaire, "RANDOMIZATION DOVs") -------------
# DOV_PARTY_ASSIGN = 1 (Republican) for odd conditions, 2 (Democrat) for even.
# DOV_FRAME_ASSIGN: 1,2 -> 1 Control; 3,4 -> 2 Harm; 5,6 -> 3 Sanctity;
# 7,8 -> 4 Authority; 9,10 -> 5 Fairness; 11,12 -> 6 Loyalty. Verified here.
stopifnot(all(party == ifelse(cond %% 2 == 1, 1L, 2L)),
          all(frame == (cond + 1L) %/% 2L))
frame_lab <- c("control", "harm", "sanctity", "authority", "fairness", "loyalty")[frame]

# ---- Text -----------------------------------------------------------------------
# The SPSS file stores the missing-value codes of the item (labels 77 DON'T
# KNOW, 98 SKIPPED ON WEB, 99 REFUSED) as strings in the text column: 1,431
# cells hold "98". These are non-answers and are set to empty (dropped below).
# Line breaks normalized: \r\n and lone \r become \n (read.csv round-trip).
txt <- trimws(gsub("\r\n?", "\n", as.character(zap_labels(d$CONSIDER_WHY))))
is_code <- txt %in% c("77", "98", "99")
cat("Text cells holding a missing-value code (77/98/99):", sum(is_code), "\n")
txt[is_code] <- NA

# ---- Exclusions -----------------------------------------------------------------
# The proposal (raw/Clayton1617.pdf) states no exclusion rule and no article
# was found; every fielded respondent has a randomized cell. exclude = FALSE.
stopifnot(all(!is.na(cond)))

h <- data.frame(
  id        = as.character(d$CaseId),
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  # 1 = any of the five critic paragraphs ("specific threat" + moral frame),
  #     pooled as in the proposal's candidate-choice analysis (Appendix Table 1 note)
  # 0 = groundwork + justification only (control)
  treatment = as.integer(frame != 1L),
  frame     = frame_lab,                            # DOV_FRAME_ASSIGN
  party_actor = ifelse(party == 1L, "republican", "democrat"),  # DOV_PARTY_ASSIGN, randomized
  condition = cond,                                 # DOV_CONDITION 1-12
  # Closed outcomes, as stored (codes 77/98/99 = DK/skipped/refused set to NA)
  threat    = ifelse(num(d$THREAT) %in% 1:2, num(d$THREAT), NA),       # 1 poses, 2 does not pose
  threat_strength = ifelse(num(d$THREAT_STRENGTH) %in% 1:3, num(d$THREAT_STRENGTH), NA),
  support   = ifelse(num(d$CONSIDER) %in% 1:2, num(d$CONSIDER), NA),   # 1 support, 2 oppose (sets the item wording)
  support_strength = ifelse(num(d$CONSIDER_STRENGTH) %in% 1:3, num(d$CONSIDER_STRENGTH), NA),
  party_id7 = num(d$PartyID7),
  weight    = as.numeric(d$WEIGHTS_MAIN),           # kept as a design column; not used
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text[!is.na(h$text)])))
cat("Rows with empty text dropped:", sum(is.na(h$text) | h$text == ""), "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -----------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = critic paragraph shown):\n")
print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows, frame x party of actor:\n")
print(table(h$frame[!h$exclude], h$party_actor[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
