# ==============================================================================
# corstange2016_refugees.R -- harmonized file for card corstange2016_refugees
# ==============================================================================
# Corstange, D. (2016). American Responses to the Syrian Refugee Crisis. TESS
# study corstangeBR69 (TESS3 229, fielded with Shafranek's study on the same
# GfK KnowledgePanel sample), N = 2,073. OSF https://osf.io/t8xq7/.
# Design: 8 conditions = control, 3 single frames, 3 pairs, 1 triple.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/corstange2016_refugees.R
#
# Input:  data/external/corstange2016_refugees/raw/TESS_229_Corstange_Client.sav
# Output: data/external/corstange2016_refugees/harmonized_corstange2016_refugees.csv
#
# Text item: D01 "In your own words, how would you describe the conflict?"
# (questionnaire raw/310.209.0215.1_TESS3229_..._v2.00.docx, section D; the
# selected preamble C01_A-H is displayed above the question; D01/D02 order
# randomized, DOV_D01_D02_Order).
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/corstange2016_refugees/raw/TESS_229_Corstange_Client.sav"
out_csv  <- "data/external/corstange2016_refugees/harmonized_corstange2016_refugees.csv"
card_id  <- "corstange2016_refugees"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 2073, !anyDuplicated(d$CaseID))
num <- function(v) as.integer(zap_labels(v))
arm <- num(d$XTESSCORS)
stopifnot(all(arm %in% 1:8))

# ---- Condition mapping (questionnaire, section C, "Base: If XTESSCORS = k") ----
# 1 C01_A no frame; 2 C01_B democracy vs dictatorship; 3 C01_C moderates vs
# religious extremists; 4 C01_D Sunni majority vs religious minorities;
# 5 C01_E democracy + extremism; 6 C01_F democracy + sectarian;
# 7 C01_G extremism + sectarian; 8 C01_H all three. The value labels in the
# .sav are bare numbers. Check: the frame-order split DOV_C01E/F/G/H_Split is
# filled only for XTESSCORS = 5/6/7/8 respectively, as the questionnaire says.
labs <- c("control", "democracy", "extremism", "sectarian",
          "democracy_extremism", "democracy_sectarian", "extremism_sectarian", "all_three")
splits <- list(`5` = d$DOV_C01E_Split, `6` = d$DOV_C01F_Split,
               `7` = d$DOV_C01G_Split, `8` = d$DOV_C01H_Split)
for (k in names(splits)) stopifnot(identical(!is.na(splits[[k]]), arm == as.integer(k)))
order_split <- rep(NA_integer_, nrow(d))
for (k in names(splits)) order_split[arm == as.integer(k)] <- num(splits[[k]])[arm == as.integer(k)]

# ---- Text -----------------------------------------------------------------------
# Refusals are flagged in D01_Refused (-1, 332 rows) and leave D01 empty; no
# missing-value codes are stored in the text column. Empty rows dropped below.
txt <- trimws(gsub("\r\n?", "\n", as.character(zap_labels(d$D01))))
stopifnot(all(is.na(txt[num(d$D01_Refused) %in% -1]) | txt[num(d$D01_Refused) %in% -1] == ""))

# ---- Exclusions -----------------------------------------------------------------
# The proposal (raw/CorstangeB69.pdf) states no exclusion rule; no article
# found. Every respondent has an assigned condition. exclude = FALSE.

d02 <- num(d$D02)
h <- data.frame(
  id        = as.character(d$CaseID),
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  treatment = labs[arm],                            # XTESSCORS, see mapping above
  has_democracy = as.integer(arm %in% c(2, 5, 6, 8)),
  has_extremism = as.integer(arm %in% c(3, 5, 7, 8)),
  has_sectarian = as.integer(arm %in% c(4, 6, 7, 8)),
  frame_order   = order_split,                      # insert order within 5-8 (questionnaire)
  d01_first     = as.integer(num(d$DOV_D01_D02_Order) == 1L),  # 1 = open item before refugee item
  # D02 refugee admissions: 1 expand greatly ... 5 reduce greatly; -1 refused -> NA
  refugees  = ifelse(d02 %in% 1:5, d02, NA),
  party7    = num(d$XPARTY7_New),
  weight    = as.numeric(d$weight_CORS),            # kept as a design column; not used
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text[!is.na(h$text)])))
cat("Rows with empty text dropped:", sum(is.na(h$text) | h$text == ""), "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -----------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n")
print(table(factor(h$treatment[!h$exclude], levels = labs)))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
