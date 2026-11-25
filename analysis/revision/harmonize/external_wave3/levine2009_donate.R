# ==============================================================================
# levine2009_donate.R -- harmonized file for card levine2009_donate
# ==============================================================================
# TESS study levine707 (TESS2 005), Adam Seth Levine, "What Not to Say When
# Asking for Donations: How the Content of Requests Affects Participation".
# Fielded by Knowledge Networks, 5/22/2009-9/10/2009, 1,281 completes.
# Data: OSF https://osf.io/r5d6k/ (TESS archive). Study page
# https://tessexperiments.org/study/levine707. Related published work:
# Levine, A. S. (2015). American Insecurity: Why Our Economic Fears Lead to
# Political Inaction. Princeton University Press, doi:10.1515/9781400852130
# (its Appendix D concerns "the ACSCAN donation experiment"; not read).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/levine2009_donate.R
#
# Input: data/external/levine2009_donate/raw/tess2_005_Levine_final_data.sav
#   (one row per respondent; CaseID = Knowledge Networks serial number).
# Output: data/external/levine2009_donate/harmonized_levine2009_donate.csv
#
# Design (questionnaire raw/tess2_005_levine_final.doc, "Detailed Study
# Procedure for TESS PID 707" and Display2 / AMT / SIGN):
#   XTESS005 1-6 = Information factor x Participation factor
#     1 general + donation        4 general + volunteer sign-up
#     2 financial + donation      5 financial + volunteer sign-up
#     3 non-financial + donation  6 non-financial + volunteer sign-up
#   Information: general ACS CAN text (1 & 4); the same text preceded by a
#   paragraph about worrying how to pay bills and the cost of cancer treatment
#   (2 & 5); or preceded by a paragraph about worrying about getting ill and
#   cancer pain (3 & 6). Participation: donate any of $15 received (AMT, arms
#   1-3) or sign up for a volunteer listserv (SIGN, arms 4-6).
# Text item Q0 ("[very Large text box]" after AMT/SIGN, same wording in all
#   arms): "We are interested in thoughts that you had while making your
#   decision. Please list any thoughts below, whether they were about the
#   information you read or the decision you made; also whether they were
#   positive, neutral, and/or negative. ..."
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/levine2009_donate/raw/tess2_005_Levine_final_data.sav"
out_csv  <- "data/external/levine2009_donate/harmonized_levine2009_donate.csv"
card_id  <- "levine2009_donate"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 1281, !anyDuplicated(d$CaseID))
num <- function(x) as.numeric(zap_labels(x))

# ---- Treatment ----------------------------------------------------------------
# Value labels of xtess005 match the questionnaire table (see header).
# Verified with counts: AMT is answered only in arms 1-3 and SIGN only in
# arms 4-6; weight1 is defined only for arms 1-3 and weight2 only for 4-6.
g <- num(d$xtess005)
stopifnot(all(g %in% 1:6))
stopifnot(all(is.na(d$SIGN[g <= 3])), all(is.na(d$AMT[g >= 4])))
info  <- c("general", "financial", "nonfinancial")[(g - 1) %% 3 + 1]
part  <- ifelse(g <= 3, "donate", "volunteer")
treatment <- paste(info, part, sep = "_")

# ---- Exclusions ---------------------------------------------------------------
# TESS file = fielded sample. The study summary (OSF wiki / study page) states
# no exclusion. Levine (2015, Appendix D) discusses noncompliance in the donation
# experiment; not read, so no exclusion is applied (ITT on all assigned rows).
exclude <- rep(FALSE, nrow(d))

# ---- Text -----------------------------------------------------------------------
txt <- d$Q0
stopifnot(is.character(txt), all(validUTF8(txt[!is.na(txt)])))

h <- data.frame(
  id          = d$CaseID,
  dataset     = card_id,
  country     = "US",
  lang        = "en",                   # English-language web survey; no language variable
  text        = trimws(txt),
  exclude     = exclude,
  treatment   = treatment,              # six arm labels: <info>_<participation>
  arm_code    = g,                      # XTESS005 1-6
  info        = info,                   # focal factor: general / financial / nonfinancial
  participation = part,                 # independently randomized factor: donate / volunteer
  # DOLLAR: data-only, random order of the donation table (1 = $0 first,
  # 2 = $15 first); assigned in all arms, shown only in arms 1-3.
  dollar_order = num(d$DOLLAR),
  # Closed outcomes: AMT donated ($0-15, arms 1-3; -1 = refused, set to NA);
  # SIGN (arms 4-6): 1 = yes, 2 = no, 3 = already a member, -1 refused -> NA.
  amount_donated = ifelse(num(d$AMT) < 0, NA, num(d$AMT)),
  sign_up     = ifelse(num(d$SIGN) < 0, NA, num(d$SIGN)),
  # Q9: worry about present financial situation (1 very ... 4 not at all);
  # post-treatment, asked after the text item.
  q9_fin_worry = ifelse(num(d$Q9) < 0, NA, num(d$Q9)),
  # Poststratification weight (not used): weight1 for arms 1-3, weight2 for 4-6.
  weight      = ifelse(g <= 3, d$weight1, d$weight2),
  stringsAsFactors = FALSE
)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n")
print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows, info x participation:\n")
print(table(h$info[!h$exclude], h$participation[!h$exclude]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
