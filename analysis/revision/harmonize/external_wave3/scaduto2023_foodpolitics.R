# ==============================================================================
# scaduto2023_foodpolitics.R -- harmonized file for card scaduto2023_foodpolitics
# ==============================================================================
# Source: Scaduto, G. (2025). Inferenze politiche da elementi apolitici: il caso
# del cibo in Italia (2023). DASSI SN248, doi:10.71732/RMEP03, CC BY 4.0.
# Paper: Scaduto & Negri (2024), South European Society and Politics 29(1).
# Preregistration: osf.io/ju38c (EGAP, 2023-04-17).
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/scaduto2023_foodpolitics.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "scaduto2023_foodpolitics"
CARD <- "scaduto2023_foodpolitics"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# Stata file, not the SPSS file: both are archive originals (no Dataverse .tab
# ingest), but SN248.sav truncates the longest OPENPART answer (row 728: 799
# characters in .dta, 788 in .sav, the .sav value a prefix of the .dta value).
# All other OPENPART values and all numeric variables are identical in the two.
d <- read_dta(file.path(RAW, "SN248.dta"))
stopifnot(nrow(d) == 1102, !anyDuplicated(d$ID))

# Treatment: RANDOM, codebook (SN248_CB_ita.pdf) p. 73: 1 = control, base menu;
# 2 = vegan; 3 = meat; 4 = traditional; 5 = ethnic (225/227/212/216/222).
# The archive converted RANDOM from text to numeric (SN248_NM_ita.pdf, section 2).
# The LimeSurvey export attached to the preregistration (osf.io/ju38c, pp. 21-22)
# confirms randomnumber 4 = traditional and 5 = ethnic. (The preregistration's
# design text, q17, lists 4 = Ethnic, 5 = Traditional; the instrument and the
# codebook agree with each other, so they are followed here.)
arm_labels <- c("base", "vegan", "meat", "traditional", "ethnic")
arm <- arm_labels[as.numeric(d$RANDOM)]
stopifnot(!anyNA(arm))

# Closed-ended outcomes (design columns). EXPPARTY codebook p. 75: 1-7 parties,
# 8 = "Non saprei" (don't know) -> NA. EXPIDEOL p. 74: 1 = far left ... 7 = far
# right, 8 = "Non saprei" -> NA.
party_labels <- c("Alleanza Verdi-Sinistra", "Partito Democratico", "Movimento 5 Stelle",
                  "Azione-Italia Viva", "Forza Italia", "Lega", "Fratelli d'Italia")
pty <- as.numeric(d$EXPPARTY)
ide <- as.numeric(d$EXPIDEOL)
exp_party    <- ifelse(pty %in% 1:7, party_labels[pty], NA_character_)
exp_ideology <- ifelse(ide %in% 1:7, ide, NA_real_)

# Text: OPENPART (questionnaire C4, SN248_Q_ita.pdf section C), shown only if
# EXPPARTY != "Non saprei"; the item was optional (no force-response marker in
# the LimeSurvey export), so 36 respondents who named a party left it empty.
clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))       # checked: all valid UTF-8, no mojibake
  trimws(gsub("[\r\n\t]+", " ", x))
}
text <- clean_text(d$OPENPART)

# Answering rates per arm (counts only), before rows without text are dropped.
cat("Per arm: assigned, named a party (EXPPARTY 1-7), non-empty OPENPART\n")
print(data.frame(arm = arm_labels,
                 assigned    = as.vector(table(factor(arm, arm_labels))),
                 named_party = as.vector(tapply(pty %in% 1:7, factor(arm, arm_labels), sum)),
                 text        = as.vector(tapply(nzchar(text), factor(arm, arm_labels), sum))))
stopifnot(all(pty[nzchar(text)] %in% 1:7))   # text only where a party was named

# Exclusions: none. The deposit holds 1,102 completed interviews; neither the
# preregistration nor the archive notes describe exclusions (speeders, attention
# checks). The article was not read, so an exclusion rule in the article would
# not be reflected here. Every row has a randomized RANDOM value.
h <- data.frame(id = as.character(as.integer(d$ID)), dataset = CARD, country = "IT", lang = "it",
                text = text, exclude = FALSE, treatment = arm,
                exp_party = exp_party, exp_ideology = exp_ideology,
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)
# No human codes of OPENPART exist in the deposit.

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm:\n")
print(table(treatment = factor(h$treatment[!h$exclude], arm_labels)))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
