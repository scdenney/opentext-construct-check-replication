# ==============================================================================
# sydnor2016_video.R -- harmonized files for cards sydnor2016_video (Q2) and
#                       sydnor2016_video_whynot (Q4b)
# ==============================================================================
# Sydnor, E. (2016). How Conflict Orientation Influences Emotional and Political
# Responses to Incivility. TESS study sydnor770 (TESS3 216), GfK KnowledgePanel,
# N = 3,101 qualified respondents. OSF https://osf.io/s43kb/ (component 7jfhr,
# Sydnor770.zip). Design: 2 (civil / uncivil) x 2 (political: C-SPAN Planned
# Parenthood hearing / entertainment: MasterChef) video clips.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/sydnor2016_video.R
#
# Inputs (both from Sydnor770.zip, deposited 2021-08-24):
#   raw/TESS_Sydnor_Client_5.sav  text columns (Q2, Q4b) and all closed items
#   raw/TESS_Sydnor_Client_5.dta  the assignment XTESS216, which the .sav lacks
#   The two files have the same rows in the same CaseID order and identical
#   values in every shared closed column (checked below). Their text columns
#   differ only in whitespace in blank cells and in 5 non-empty answers
#   (probably non-ASCII characters), so text is taken from the .sav.
# Outputs:
#   data/external/sydnor2016_video/harmonized_sydnor2016_video.csv         (Q2)
#   data/external/sydnor2016_video/harmonized_sydnor2016_video_whynot.csv  (Q4b)
#
# Items (questionnaire raw/310.209.01009.1_TESS216_Sydnor_Final.docx):
#   Q2  "Please watch the video below. After watching the clip, you are invited
#       to ask questions or offer your own comments in the box below if you
#       would like to do so." [box under the video]
#   Q3  "Why did you decide not to leave a comment?" (if Q2 refused; not carded)
#   Q4  share the story? 1 yes, 2 no; Q4a commentary for the post (if yes)
#   Q4b "Why wouldn't you share the story?" (if Q4 = 2)
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

dir     <- "data/external/sydnor2016_video"
sav     <- read_sav(file.path(dir, "raw/TESS_Sydnor_Client_5.sav"))
dta     <- read_dta(file.path(dir, "raw/TESS_Sydnor_Client_5.dta"))
stopifnot(nrow(sav) == 3101, !anyDuplicated(sav$CaseID),
          identical(as.numeric(sav$CaseID), as.numeric(dta$CaseID)))
num <- function(v) as.integer(zap_labels(v))

# Shared closed columns must agree, so that XTESS216 from the .dta belongs to
# the .sav row it is attached to.
closed <- setdiff(names(sav), c("tm_start", "tm_finish", "Q1_Order", "Q5_Order",
                                "Q6_Order", "Q2", "Q3", "Q4a", "Q4b"))
for (v in closed)
  stopifnot(isTRUE(all.equal(as.vector(zap_labels(sav[[v]])),
                             as.vector(zap_labels(dta[[v]])), check.attributes = FALSE)))

# ---- Assignment (questionnaire, section I: XTESS216 1 = masterchef_civil.mp4;
# 2 = pp_civil.mp4; 3 = masterchef_uncivil.mp4; 4 = pp_uncivil.mp4; the .dta
# value labels say the same) ---------------------------------------------------
x <- num(dta$XTESS216)
stopifnot(all(x %in% 1:4),
          identical(unname(attr(dta$XTESS216, "labels")), c(1, 2, 3, 4)),
          identical(names(attr(dta$XTESS216, "labels")),
                    c("masterchef_civil.mp4", "pp_civil.mp4",
                      "masterchef_uncivil.mp4", "pp_uncivil.mp4")))
uncivil <- as.integer(x %in% c(3, 4))
topic   <- ifelse(x %in% c(2, 4), "political", "entertainment")

# ---- Exclusions -----------------------------------------------------------------
# Respondents who could not see or hear the test video were terminated before
# the treatment (questionnaire, S01/S02); the file holds qualified respondents
# only (QFLAG = 1, S01 = S02 = 1 for all rows). The proposal summary states no
# other exclusion. exclude = FALSE.
stopifnot(all(num(sav$QFLAG) == 1), all(num(sav$S01) == 1), all(num(sav$S02) == 1))

clean <- function(v) trimws(gsub("\r\n?", "\n", as.character(zap_labels(v))))
na_neg <- function(v) { v <- num(v); ifelse(v < 0, NA, v) }   # -1 = refused

base <- data.frame(
  id        = as.character(sav$CaseID),
  dataset   = NA_character_,
  country   = "US",
  lang      = "en",
  text      = NA_character_,
  exclude   = FALSE,
  treatment = uncivil,                      # 1 = uncivil clip, 0 = civil clip
  topic     = topic,                        # randomized independently of civility
  video     = c("masterchef_civil", "pp_civil", "masterchef_uncivil", "pp_uncivil")[x],
  # Conflict orientation, pre-treatment (Q1; 1 strongly agree ... 5 strongly disagree)
  co_challenge = na_neg(sav$Q1_1), co_exciting = na_neg(sav$Q1_2),
  co_hate_arguments = na_neg(sav$Q1_3), co_upset = na_neg(sav$Q1_4),
  commented = as.integer(!is.na(sav$Q2) & trimws(sav$Q2) != ""),   # Q2 non-empty
  share     = na_neg(sav$Q4),               # 1 yes, 2 no
  # Post-video ratings (1 not at all ... 5 extremely)
  clip_informative = na_neg(sav$Q5_1), clip_civil = na_neg(sav$Q5_2), clip_biased = na_neg(sav$Q5_3),
  emo_angry = na_neg(sav$Q6_1), emo_disgusted = na_neg(sav$Q6_2), emo_anxious = na_neg(sav$Q6_3),
  emo_amused = na_neg(sav$Q6_4), emo_entertained = na_neg(sav$Q6_5), emo_enthusiastic = na_neg(sav$Q6_6),
  party7    = na_neg(sav$DOV_XPARTY7),
  weight    = as.numeric(sav$weight),       # kept as a design column; not used
  stringsAsFactors = FALSE
)

check <- function(h, out_csv) {
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n==", basename(out_csv), "==\n")
  cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
  cat("Non-excluded rows per arm (1 = uncivil):\n"); print(table(h$treatment[!h$exclude]))
  cat("Non-excluded rows, civility x topic:\n"); print(table(h$treatment[!h$exclude], h$topic[!h$exclude]))
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  stopifnot(identical(r$text, h$text))
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n")
  stopifnot(n_esc == 0)
}

for (item in list(list(card = "sydnor2016_video", col = "Q2"),
                  list(card = "sydnor2016_video_whynot", col = "Q4b"))) {
  h <- base
  h$dataset <- item$card
  h$text <- clean(sav[[item$col]])
  stopifnot(all(validUTF8(h$text[!is.na(h$text)])))
  cat("\n", item$col, ": rows with empty text dropped:", sum(is.na(h$text) | h$text == ""), "\n")
  h <- h[!is.na(h$text) & h$text != "", ]
  if (item$col == "Q4b") stopifnot(all(h$share %in% 2))   # asked only if Q4 = 2
  check(h, file.path(dir, paste0("harmonized_", item$card, ".csv")))
}
