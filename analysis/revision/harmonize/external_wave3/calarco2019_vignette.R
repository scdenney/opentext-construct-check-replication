# ==============================================================================
# calarco2019_vignette.R -- harmonized file for card calarco2019_vignette
# ==============================================================================
# TESS study calarcoS38 (TESS 036, AmeriSpeak, fielded 02/15/2019-05/10/2019,
# in a combined instrument with TESS 033 Hamilton and TESS 035 Krupnikov),
# J. Calarco, "Public Perceptions of Prenatal Alcohol Consumption". No
# published article located. Data: OSF https://osf.io/dh3nj (TESS archive).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/calarco2019_vignette.R
#
# Input:  data/external/calarco2019_vignette/raw/CalarcoS38__8041.036_CALARCO_TESS036_weighted.sav
# Output: data/external/calarco2019_vignette/harmonized_calarco2019_vignette.csv
#
# Design (proposal raw/S38 proposal.pdf, Appendix A; fielded questionnaire
# raw/CalarcoS38___8041.033.035.036__TESS_Combined_v10clean.docx, BLOCK3):
# CALARCO_VIGNETTE 1-12 = 2 status x 3 drinking x 2 warning, value labels:
#   1-6 high status (lawyer), 7-12 low status (fielded as "cashier at a
#   supermarket"; the proposal had "house cleaner"); within each status,
#   1-2 / 7-8 drinks only water, 3-4 / 9-10 sips one glass of wine,
#   5-6 / 11-12 finishes two glasses of wine; odd = no warning, even = "she
#   recalls her doctor saying that pregnant women should avoid alcohol".
# Q7 approve/disapprove, Q8 harm to the baby, then Q9 (open): "If Megan were
# your friend, what would you say to Megan about her decision to [drink only
# water / sip one glass of wine / finish two glasses of wine] at the bar? In
# your own words, please write a few sentences explaining what you would say
# to Megan about her decision and why this is what you would say." The
# Calarco block's position among the three studies' blocks was randomized
# (BLOCKORDER1-3).
#
# Exclusions: none stated in the proposal or project description; exclude =
# FALSE throughout. Weights kept, not used.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_file <- "data/external/calarco2019_vignette/raw/CalarcoS38__8041.036_CALARCO_TESS036_weighted.sav"
out_csv  <- "data/external/calarco2019_vignette/harmonized_calarco2019_vignette.csv"
card_id  <- "calarco2019_vignette"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 2005, !anyDuplicated(d$CaseId), all(d$CALARCO_VIGNETTE %in% 1:12))
# SURV_LANG: all 2,005 interviews in English (1); SURV_MODE: all web.
stopifnot(all(zap_labels(d$SURV_LANG) == 1))

# ---- Text -------------------------------------------------------------------------
# Value labels: 77 DON'T KNOW, 98 SKIPPED ON WEB, 99 REFUSED; whole-answer codes
# are skips and set to empty. CR LF line breaks become LF (read.csv drops CR).
txt <- trimws(as.character(zap_labels(d$Q9)))
stopifnot(all(validUTF8(txt)))
skip <- txt %in% c("77", "98", "99")
cat("Skip codes (77/98/99) set to empty:", sum(skip), "\n")
txt[skip] <- ""
cat("Answers with CR line breaks normalized to LF:", sum(grepl("\r", txt, fixed = TRUE)), "\n")
txt <- gsub("\r\n?", "\n", txt)

# ---- Design -----------------------------------------------------------------------
v <- as.integer(d$CALARCO_VIGNETTE)
w <- (v - 1) %% 6 + 1
drinking <- c("none", "none", "light", "light", "heavy", "heavy")[w]
status   <- ifelse(v <= 6, "high", "low")
warning_ <- as.integer(v %% 2 == 0)
num <- function(x) { x <- as.numeric(zap_labels(x)); x[x %in% c(77, 98, 99)] <- NA; x }
bo <- cbind(zap_labels(d$BLOCKORDER1), zap_labels(d$BLOCKORDER2), zap_labels(d$BLOCKORDER3))
calarco_pos <- apply(bo, 1, function(r) match(3, r))   # 3 = CALARCO
stopifnot(!anyNA(calarco_pos))

h <- data.frame(
  id        = d$CaseId,
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  # drinking in the vignette: none (water), light (one glass), heavy (two glasses)
  treatment = drinking,
  # independently randomized: Megan's occupation (high = lawyer, low = cashier)
  status    = status,
  # independently randomized: 1 = doctor's warning recalled in the vignette
  warning   = warning_,
  vignette  = v,
  # closed outcomes, asked before the text: 1 strongly approve ... 5 strongly
  # disapprove; 1 strongly agree ... 5 strongly disagree that it will harm the baby
  approve   = num(d$Q7),
  harm      = num(d$Q8),
  block_position = calarco_pos,
  weight    = as.numeric(d$WEIGHT),
  stringsAsFactors = FALSE)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -----------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
cat("Arm x status:\n"); print(table(h$treatment, h$status))
cat("Arm x warning:\n"); print(table(h$treatment, h$warning))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
