# ==============================================================================
# balint_closeups_tom.R -- harmonized file for card balint_closeups_tom
# ==============================================================================
# Balint, K. E., Blessing, J. N., & Rooney, B. (2020). Shot scale matters: The
# effect of close-up frequency on mental state attribution in film viewers.
# Poetics 83, 101480. doi:10.1016/j.poetic.2020.101480 (CC BY; open copy at
# d-nb.info/1227841582/34).
# Data: DataverseNL doi:10.34894/XLA9VS (CC0).
# Online between-subjects experiment (Qualtrics, 2015, native German speakers,
# University of Augsburg recruitment), N = 495 after the authors' cleaning;
# 15 randomly assigned versions of the animated film "Father and Daughter"
# (0, 1, 2, 3, 4, 5 or 10 inserted close-ups of the daughter, at different
# narrative positions, with a neutral or sad face).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/balint_closeups_tom.R
#
# Input (deposited original, ?format=original):
#   raw/Closeup_and_theory_of_mind_dataset.sav (495 rows x 48 columns)
# Output: data/external/balint_closeups_tom/harmonized_balint_closeups_tom.csv
#
# Text item (Q1_text, "Q1 Free response"; article section 2.4, wording given
# there in English, the instrument was German): "Please describe the story of
# the film scene in as much detail as possible using at least 6-10
# sentences." Not carded: Q2_text, the prompted retelling from the daughter's
# perspective (asks for feelings, thoughts and intentions in every arm).
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/balint_closeups_tom/raw"
out_csv <- "data/external/balint_closeups_tom/harmonized_balint_closeups_tom.csv"
card_id <- "balint_closeups_tom"

d <- read_sav(file.path(raw_dir, "Closeup_and_theory_of_mind_dataset.sav"))
stopifnot(nrow(d) == 495, !anyDuplicated(d$ID))
num <- function(x) as.numeric(zap_labels(x))

# ---- Design -------------------------------------------------------------------
# NumberCU value labels (.sav): 0 Zero CU, 1 One CU, 2 Two CU, 3 Three CU,
# 4 Four CU, 5 Five CU, 6 = "10 CU". Article 2.1: participants "were randomly
# assigned to one of the video versions"; unequal cell sizes by design (one-
# close-up versions at five positions, two positions for 2-5 close-ups).
# CU_frequency_CU_position: 15 version codes (value labels in the .sav),
# nested in NumberCU (checked below). FACE_NEUTR_SAD: 1 neutral / 2 sad,
# missing for the zero-close-up film (article 2.1: no face to manipulate).
ncu_code <- num(d$NumberCU)
stopifnot(all(ncu_code %in% 0:6))
n_cu <- c(0, 1, 2, 3, 4, 5, 10)[ncu_code + 1]
version <- num(d$CU_frequency_CU_position)
stopifnot(all(version %in% 1:15),
          all(tapply(n_cu, version, function(z) length(unique(z))) == 1))
face_l <- as.character(as_factor(d$FACE_NEUTR_SAD))
face <- ifelse(is.na(face_l), "none", face_l)
stopifnot(all(face %in% c("neutral", "sad", "none")), all((face == "none") == (n_cu == 0)))
# Two data-collection waves (Datagroups: 2015 summer / 2015 winter); kept as a
# stratum because arm shares may differ by wave.
wave <- ifelse(num(d$Datagroups) == 1, "summer2015", "winter2015")

# ---- Text and encoding --------------------------------------------------------
txt <- trimws(as.character(d$Q1_text))
stopifnot(all(validUTF8(txt)))
enc_count <- function(x) c(
  non_ascii   = sum(grepl("[^\\x01-\\x7F]", x, perl = TRUE)),
  umlaut_sz   = sum(grepl("[äöüÄÖÜß]", x)),
  replacement = sum(grepl("�", x, fixed = TRUE)))
enc_before <- enc_count(txt)

# ---- Exclusion ----------------------------------------------------------------
# None. The deposit holds the authors' analysis sample: 535 responded; those
# with missing responses, free responses under 10 words, or familiarity with
# the film were removed before deposit (article 2.2), leaving 495; H1 is
# tested on all 495 (Table 2). Every row has a randomized version.
excl <- rep(FALSE, nrow(d))

# ---- Human codes --------------------------------------------------------------
# Manual counts of explicit mental-state references in Q1 (article 2.5.1):
# one trained coder blind to condition coded all answers; a second coder
# coded a random 10% (Krippendorff's alpha 0.775 for Q1). Categories:
# affective (wishes, desires, feelings), cognitive (memory, knowledge, other
# cognitive functions), intentions (aims, goals, incl. "to"/"so that" and
# modal verbs). Female = the daughter (analysed); male = the father (coded,
# not analysed). The article's outcome is this count with log word count as
# offset (2.7).
fem <- as.numeric(d$Q1_AFF_COGN_INT_FEMALE)
stopifnot(isTRUE(all.equal(fem, as.numeric(d$Q1_AFF_FEM + d$Q1_COGN_FEM + d$Q1_INT_FEM))))

h <- data.frame(
  id         = as.character(d$ID),                  # Qualtrics id ("ID as Qualtricks added")
  dataset    = card_id,
  country    = "DE",
  lang       = "de",
  text       = txt,
  exclude    = excl,
  treatment  = paste0("cu", n_cu),                  # cu0, cu1, cu2, cu3, cu4, cu5, cu10
  n_closeups = n_cu,
  version    = version,                             # 15 film versions (number x position)
  face       = face,                                # crossed factor: neutral / sad / none
  wave       = wave,
  word_count = as.numeric(d$WCQ1),                             # LIWC word count of Q1 (authors' offset)
  human_msa_female       = fem,                     # Q1_AFF_COGN_INT_FEMALE
  human_msa_female_aff   = as.numeric(d$Q1_AFF_FEM),
  human_msa_female_cogn  = as.numeric(d$Q1_COGN_FEM),
  human_msa_female_int   = as.numeric(d$Q1_INT_FEM),
  human_msa_male         = as.numeric(d$Q1_AFF_COGN_INT_MALE),  # 1 NA in the deposit
  human_msa_any_female   = as.integer(fem > 0),     # binary form of the card target
  stringsAsFactors = FALSE
)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n")
print(table(factor(h$treatment[!h$exclude], paste0("cu", c(0:5, 10)))))
cat("Pooled: cu3+ (3,4,5,10) =", sum(h$n_closeups >= 3), "| cu1-2 =", sum(h$n_closeups %in% 1:2),
    "| any close-up =", sum(h$n_closeups > 0), "| none =", sum(h$n_closeups == 0), "\n")
cat("Arm x wave:\n"); print(table(h$treatment, h$wave))
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
