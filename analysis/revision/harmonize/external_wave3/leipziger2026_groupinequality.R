# ==============================================================================
# leipziger2026_groupinequality.R -- harmonized file for card leipziger2026_groupinequality
# ==============================================================================
# Source: Leipziger, L. E., Aarslew, L. F., & Christensen, M. E. (2026). Do
# group-based inequalities feel more unjust? Experimental evidence from
# economically disadvantaged groups in India, South Africa, and the United States.
# Public Opinion Quarterly 90(SI), 1261-, doi:10.1093/poq/nfag029 (article not
# read: closed access). Preregistrations: OSF qyzcu (India), z3th5 (South Africa),
# tpw7u (US), "Inequality Between Whom", registered February 2024.
# Data: Harvard Dataverse doi:10.7910/DVN/14AEWL (CC0 1.0). YouGov online panels,
# 2024: Scheduled Caste/Tribe respondents in India, Black South Africans, African
# Americans.
#
# Item: q18, "We are now interested in hearing your reasons for why there are
# differences in economic resources in [India / South Africa / the United States]."
# (codebooks; the India preregistration questionnaire adds "More than one reason
# may come to mind. Please write your thoughts in the field below. Some sentences
# and/or keywords are enough."). Asked in every arm, in Block F with q15, q16 and
# q17 in randomized order (m_Block_F_rnd).
#
# Arms (BlockE; codebooks, 1_cleaning.do lines 39-42, 183-186, 328-332):
#   IN, ZA: 1 intergroup frame (IN upper caste vs SC/ST; ZA White vs Black),
#           2 interpersonal frame (richest vs poorest 10% of households), 3 control.
#   US:     1 intergroup (White vs Black), 2 interpersonal (class-based),
#           3 intragroup class frame among African Americans,
#           4 intragroup class frame among White Americans, 5 control.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/leipziger2026_groupinequality.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))
DS   <- "leipziger2026_groupinequality"
CARD <- "leipziger2026_groupinequality"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# Attention screen as in 1_cleaning.do lines 17-30 (IN), 162-175 (ZA), 307-320 (US):
# kept if qScreen1 == 1 ("I understand"; attention_1 = 0) and the respondent ticked
# red (option 3) and green (option 5) and nothing else (attention_2 = 0). Stata
# semantics: a missing qScreen2_k satisfies `qScreen2_k != 0`, so it screens the
# row out.
passes_screen <- function(d) {
  a1_fail <- !(d$qScreen1 %in% 1)
  other <- sapply(c(1, 2, 4, 6, 7, 8, 9), function(k) {
    v <- as.numeric(d[[paste0("qScreen2_", k)]]); is.na(v) | v != 0 })
  a2_fail <- rowSums(other) > 0 | !(d$qScreen2_3 %in% 1 & d$qScreen2_5 %in% 1)
  !a1_fail & !a2_fail
}

# Position of q18 within Block F (1-4). m_Block_F_rnd holds e.g.
# "randomize(13, [1,3,2,0])"; ASSUMPTION: the bracket lists the module's items in
# display order with q15 = 0, q16 = 1, q17 = 2, q18 = 3 (questionnaire order).
q18_pos <- function(z) {
  z <- as.character(z)
  inside <- sub(".*\\[([0-9, ]+)\\].*", "\\1", z)
  out <- rep(NA_integer_, length(z))
  ok <- grepl("\\[[0-9, ]+\\]", z)
  out[ok] <- vapply(strsplit(inside[ok], ","), function(v) match(3L, as.integer(v)), 1L)
  out
}

arm_map <- list(
  IN = c("1" = "intergroup", "2" = "interpersonal", "3" = "control"),
  ZA = c("1" = "intergroup", "2" = "interpersonal", "3" = "control"),
  US = c("1" = "intergroup", "2" = "interpersonal", "3" = "intragroup_black",
         "4" = "intragroup_white", "5" = "control"))

build <- function(iso) {
  d <- read_dta(file.path(RAW, sprintf("%s_survey.dta", iso)))
  x <- as.character(d$q18); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))
  arm <- unname(arm_map[[iso]][as.character(as.integer(d$BlockE))])
  q15 <- as.integer(d$q15); q15[q15 %in% 8] <- NA     # 8 = don't know (cleaning line 47)
  q16 <- as.integer(d$q16); q16[q16 %in% 77] <- NA    # 77 = prefer not (cleaning line 57)
  text <- trimws(gsub("[[:space:]]+", " ", x))
  data.frame(
    id = sprintf("%s_%s", tolower(iso), as.character(d$recordno)),  # authors' record number
    dataset = CARD, country = iso,
    # questionnaire in English in all three countries (preregistration
    # questionnaires); answers containing Devanagari are tagged hi (1 in India)
    lang = ifelse(grepl("[ऀ-ॿ]", text), "hi", "en"),
    text = text,
    # exclude: the authors drop respondents failing either attention screen
    # (1_cleaning.do lines 30, 175, 320) and rows have no arm if they never reached
    # Block E
    exclude = !passes_screen(d) | is.na(arm),
    treatment = arm,
    q18_position = q18_pos(d$m_Block_F_rnd),  # randomized position of q18 in Block F
    fairness = q15,      # 1 very unfair ... 7 very fair (H1-H3 outcome)
    effort_luck = q16,   # 0 only luck ... 10 only effort (H1-H3 outcome)
    stringsAsFactors = FALSE)
}

h <- do.call(rbind, lapply(c("IN", "ZA", "US"), build))
stopifnot(!anyDuplicated(h$id))
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Excluded rows with text:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm by country:\n")
print(with(h[!h$exclude, ], table(country, treatment)))
cat("lang:\n"); print(with(h, table(country, lang)))
cat("q18 position among non-excluded (NA = parse failure):\n")
print(with(h[!h$exclude, ], table(country, q18_position, useNA = "ifany")))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
