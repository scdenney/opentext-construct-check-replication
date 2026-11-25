# ==============================================================================
# braman2016_legitimacy.R -- harmonized file for card braman2016_legitimacy
# ==============================================================================
# Braman, E. "Exploring the Role of Constitutional Considerations, Public
# Support and Personal Preferences in Citizen Assessments of Proposed
# Congressional Action on Immigration and Gun Control." TESS study braman751
# (TESS3 225), GfK KnowledgePanel, US adults, N = 801, fielded 31 Aug - 4 Sep
# 2016 (response-rate report). OSF https://osf.io/k8pwg/.
#
# Design (questionnaire raw/310.209.01087.1_TESS3225_..._v3.00.docx): a mock AP
# article on a proposed federal law, 12 cells (XTESS225):
#   issue:     1-6 gun control (B08_G), 7-12 immigration / in-state tuition (B08_I)
#   authority: 1,2,7,8 legal experts agree Congress HAS authority;
#              3,4,9,10 experts divided; 5,6,11,12 experts agree Congress does NOT
#   support:   1,3,5,7,9,11 "over 85%" of Americans support; even cells "under 15%"
# Then B09 expert credibility, B10 agree Congress should pass it (6-point),
# B11 open "briefly explain your answer" (to B10), B12 legitimacy (7-point,
# 1 = clearly legitimate), B13 open "briefly explain your answer" (to B12).
# This file holds B13 (legitimacy explanation), the item tied to H1
# ("information about compliance with rules should be significant in judgments
# of the legitimacy", proposal p. 4).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/braman2016_legitimacy.R
# Input:  data/external/braman2016_legitimacy/raw/TESS_225_Braman_Client.sav
# Output: data/external/braman2016_legitimacy/harmonized_braman2016_legitimacy.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_dir <- "data/external/braman2016_legitimacy/raw"
out_csv <- "data/external/braman2016_legitimacy/harmonized_braman2016_legitimacy.csv"
card_id <- "braman2016_legitimacy"

d <- read_sav(file.path(raw_dir, "TESS_225_Braman_Client.sav"))
stopifnot(nrow(d) == 801, !anyDuplicated(d$CaseID))

txt <- d$B13
Encoding(txt) <- "UTF-8"
txt <- trimws(ifelse(is.na(txt), "", txt))
txt <- gsub("\r\n?", "\n", txt)   # CRLF -> LF (none found; kept for safety)
stopifnot(all(validUTF8(txt)))

# ---- Cell -> factor mapping (questionnaire insert logic, see header) ---------
cell <- as.numeric(d$XTESS225)
stopifnot(all(cell %in% 1:12))
issue     <- ifelse(cell <= 6, "gun_control", "immigration")
authority <- c("clear_authority", "clear_authority", "unclear_authority", "unclear_authority",
               "no_authority", "no_authority")[(cell - 1) %% 6 + 1]
support   <- ifelse(cell %% 2 == 1, "85pct", "15pct")
# Verify the mapping against the XTESS225 value labels.
labs <- as.character(as_factor(d$XTESS225))
stopifnot(all(grepl("Gun", labs) == (issue == "gun_control")),
          all(grepl("85%", labs) == (support == "85pct")),
          all(grepl("Clearly No Authority", labs) == (authority == "no_authority")),
          all(grepl("Unclear Authority", labs) == (authority == "unclear_authority")))

# ---- Exclusions ----------------------------------------------------------------
# None. All 801 are qualified, weighted completes (response-rate report; weight
# non-missing for all) and the proposal states no exclusions for TESS.
exclude <- rep(FALSE, nrow(d))

num <- function(x, ok) { x <- as.numeric(x); x[!x %in% ok] <- NA; x }
h <- data.frame(
  id        = as.integer(d$CaseID),
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = exclude,
  treatment = authority,              # clear_authority / unclear_authority / no_authority
  issue     = issue,
  public_support = support,
  cell      = cell,
  legitimacy = num(d$B12, 1:7),       # 1 clearly legitimate ... 7 clearly not (closed outcome)
  agree_pass = num(d$B10, 1:6),       # 1 strongly disagree ... 6 strongly agree
  experts_credible = as.character(as_factor(d$B09)),
  gun_regulation_view = num(d$B05, 1:6),       # pre-treatment
  limit_services_view = num(d$B06, 1:6),       # pre-treatment
  federal_too_much_view = num(d$B07, 1:6),     # pre-treatment
  weight    = as.numeric(d$weight),   # not used
  stringsAsFactors = FALSE
)

cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
h <- h[h$text != "", ]
write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks --------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
k <- h[!h$exclude, ]
cat("Non-excluded rows per authority arm:\n"); print(table(k$treatment))
cat("Authority x public support:\n"); print(table(k$treatment, k$public_support))
cat("Authority x issue:\n"); print(table(k$treatment, k$issue))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
