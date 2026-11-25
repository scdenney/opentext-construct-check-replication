# ==============================================================================
# schnabel2015_service.R -- harmonized file for card schnabel2015_service
# ==============================================================================
# TESS study schnabelBR47 (TESS3 184, GfK KnowledgePanel, fielded 02/21/2015-
# 03/23/2015), L. Schnabel, L. Apgar and B. Powell, "Americans' Views on
# Refusal of Services to Same-Sex Couples and Interracial Couples".
# Article: Powell, B., Schnabel, L., & Apgar, L. (2017). Denial of service to
# same-sex and interracial couples: Evidence from a national survey
# experiment. Science Advances 3(12): eaao5834, doi:10.1126/sciadv.aao5834.
# Data: OSF https://osf.io/ur99b/ (TESS archive, public).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/schnabel2015_service.R
#
# Input:  data/external/schnabel2015_service/raw/TESS3_184_Powell_Client.sav
# Output: data/external/schnabel2015_service/harmonized_schnabel2015_service.csv
#
# Reading the string. haven::read_sav cannot parse the file family these TESS3
# exports belong to; foreign::read.spss can, but returns SPSS very long string
# segments as separate columns. The dictionary (subtype 14 record) declares Q3
# as one 12,000-byte string, stored as 48 segments in file order: Q3, Q30-Q39,
# Q3A-Q3Z, Q310-Q319, Q31A (47 x 255 bytes + 156). The searcher's "second
# open item" Q30 (318 non-empty) is segment 2 of Q3, not a separate item: the
# fielded questionnaire has a single open item. Each full segment carries 255
# bytes (ReadStat convention; confirmed on the sibling TESS3 file
# ishizuka2015_parenting). Segments are pasted back as raw bytes.
#
# Design (proposal raw/SchnabelB47.pdf, Appendices A-B; questionnaire
# raw/TESS3_184_Powell_Questionnaire_final.doc): XTESS184 = vignette 1-8,
#   1 gay / self-employed / religious      5 interracial / self-employed / religious
#   2 gay / self-employed / non-religious  6 interracial / self-employed / non-religious
#   3 gay / corporate chain / religious    7 interracial / corporate chain / religious
#   4 gay / corporate chain / non-religious 8 interracial / corporate chain / non-religious
# Q1 (closed): should the photographer / corporate chain be allowed to refuse
# services to the couple, or be required to provide them (order of the two
# options randomized, DOV_Q1SPLIT_V*); Q2 strength; Q3 (open, asked of all):
# "Why do you think that the [photographer/corporate chain] should [stub
# selected in Q1]? In your own words, please write a few sentences explaining
# why you feel this way." Q4 support for gay and lesbian / interracial marriage.
#
# Exclusions: the article (Materials and Methods, Statistical analysis) drops
# 24 of 2,059 cases with missing data on the experimental items or the
# marriage item; exclude = TRUE where Q1 or Q4 is refused (-1) or missing.
# This reconstruction gives exactly 24 (checked below); adding Q2 (strength)
# would give 33, so Q2 is not part of the rule.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(foreign))

raw_file <- "data/external/schnabel2015_service/raw/TESS3_184_Powell_Client.sav"
out_csv  <- "data/external/schnabel2015_service/harmonized_schnabel2015_service.csv"
card_id  <- "schnabel2015_service"

d <- suppressWarnings(read.spss(raw_file, to.data.frame = FALSE, reencode = FALSE,
                                use.value.labels = FALSE))
stopifnot(length(d$CaseID) == 2059, !anyDuplicated(d$CaseID), all(d$XTESS184 %in% 1:8))

# ---- Join the 48 segments of Q3 ----------------------------------------------------
nm <- names(d)
segs <- nm[(which(nm == "Q3")):(which(nm == "Q4") - 1)]
stopifnot(length(segs) == 48, segs[2] == "Q30", segs[48] == "Q31A")
for (s in segs[-48]) stopifnot(all(nchar(d[[s]], type = "bytes") == 255))
txt <- do.call(paste0, lapply(segs, function(s) d[[s]]))
Encoding(txt) <- "UTF-8"
stopifnot(all(validUTF8(txt)))
txt <- trimws(txt)
nb <- nchar(txt, type = "bytes")
cat("Q3 answers longer than 255 bytes:", sum(nb > 255), "| longest (bytes):", max(nb),
    "| at the 12,000-byte limit:", sum(nb >= 12000), "\n")
stopifnot(all(nb < 12000))
cat("Answers with CR line breaks normalized to LF:", sum(grepl("\r", txt, fixed = TRUE)), "\n")
txt <- gsub("\r\n?", "\n", txt)

# ---- Design -----------------------------------------------------------------------
v <- as.integer(d$XTESS184)
couple   <- ifelse(v <= 4, "gay", "interracial")
business <- ifelse(v %in% c(1, 2, 5, 6), "self_employed", "corporate_chain")
reason   <- ifelse(v %% 2 == 1, "religious", "nonreligious")
# Check against the insert variables: DOV_INSERT_Q1_Q3 (1 Photographer/He,
# 2 Corporate Chain/It) and DOV_INSERT_Q4 (1 Gay and Lesbian, 2 Interracial).
stopifnot(all((business == "self_employed") == (d$DOV_INSERT_Q1_Q3 == 1)),
          all((couple == "gay") == (d$DOV_INSERT_Q4 == 1)))
# Q1: 1 allowed to refuse, 2 required to provide, -1 refused. DOV_INSERT_Q2_Q3
# is the Q1 stub inserted in Q3's wording.
q1 <- ifelse(d$Q1 %in% 1:2, d$Q1, NA)
q2 <- ifelse(d$Q2 %in% 1:2, d$Q2, NA)
q4 <- ifelse(d$Q4 %in% 1:4, d$Q4, NA)
split_vars <- paste0("DOV_Q1SPLIT_V", 1:8)
q1_order <- sapply(seq_along(v), function(i) d[[split_vars[v[i]]]][i])

exclude <- is.na(q1) | is.na(q4)
cat("Excluded (Q1 or Q4 missing/refused):", sum(exclude), "(article: 24)\n")
stopifnot(sum(exclude) == 24)

h <- data.frame(
  id        = d$CaseID,
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = exclude,
  # vignette cell as lower-case label: couple_business_reason
  treatment = paste(couple, business, reason, sep = "_"),
  couple    = couple,
  business  = business,
  reason    = reason,
  vignette  = v,
  # closed outcomes, asked before the text: 1 = allowed to refuse, 0 = required
  # to provide (its stub is inserted into the Q3 wording)
  allow_refuse = ifelse(is.na(q1), NA, as.integer(q1 == 1)),
  strongly  = ifelse(is.na(q2), NA, as.integer(q2 == 1)),
  q1_order  = q1_order,
  # marriage support, asked after the text: 1 strongly agree ... 4 strongly disagree
  marriage_support = q4,
  weight    = d$weight,
  stringsAsFactors = FALSE)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -----------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
k <- h[!h$exclude, ]
cat("Non-excluded by couple:\n"); print(table(k$couple))
cat("Non-excluded by business:\n"); print(table(k$business))
cat("Non-excluded by reason:\n"); print(table(k$reason))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
