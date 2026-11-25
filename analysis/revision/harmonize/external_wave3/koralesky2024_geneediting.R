# ==============================================================================
# koralesky2024_geneediting.R -- harmonized file for card koralesky2024_geneediting
# ==============================================================================
# Koralesky, Sirovica, Hendricks, Mills, von Keyserlingk & Weary (2023), "Social
# acceptance of genetic engineering technology", PLoS ONE 18(8): e0290070
# (PMC10431645); data deposit for Koralesky, von Keyserlingk & Weary (2025),
# "Assessing construct reliability through open-ended survey response
# analysis", PLoS ONE 20(4): e0320570.
# Data: Borealis doi:10.5683/SP3/PEPATK (CC BY 4.0). US MTurk, summer 2021.
#
# Only Study 1, Survey 3 (six randomized GE applications) is harmonized. Survey 1
# (15 sources), Survey 2 (8 terms) and Study 2 (no treatment) fail the
# inclusion criteria (see the drafter's report).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/koralesky2024_geneediting.R
#
# Input:  data/external/koralesky2024_geneediting/raw/Study1_Survey3_dataset.xlsx
#         (1,128 rows; Readme #7). One row per respondent; id = anonymID.
# Output: data/external/koralesky2024_geneediting/harmonized_koralesky2024_geneediting.csv
#
# Text item: Qual_animalaccept, "In a few sentences, please describe how
# acceptable you think this technology is for the animals involved." Asked in
# the five animal arms after the risk, benefit, trust and acceptance items and
# the attention check (questionnaire Study1_Survey3_USA.pdf, pp. 4-9). The wheat
# arm got a different prompt ("... for the plants involved", Qual_plantaccept)
# and is not part of this file (its Qual_animalaccept is empty in all 184 rows).
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(readxl))

raw_file <- "data/external/koralesky2024_geneediting/raw/Study1_Survey3_dataset.xlsx"
out_csv  <- "data/external/koralesky2024_geneediting/harmonized_koralesky2024_geneediting.csv"
card_id  <- "koralesky2024_geneediting"

d <- as.data.frame(read_excel(raw_file, sheet = 1, col_types = "text",
                              .name_repair = "minimal"))
stopifnot(nrow(d) == 1128, !anyDuplicated(d$anonymID), all(!is.na(d$anonymID)))
ne <- function(v) !is.na(v) & trimws(v) != ""

# ---- Treatment ----------------------------------------------------------------
# Text labels as deposited (Readme #7, variable 28; SAS Study1_Survey3.sas lines
# 21-27 map them to numTreatment 1-6). The authors combined the two heat-stress
# wordings into one treatment after finding no difference (article, Quantitative
# data analysis; Table 2 footnote); the wording is kept in heat_version.
arm_map <- c("Cattle heat resistance v1" = "heat",
             "Cattle heat resistance v2" = "heat",
             "Farm profitability and increased muscle growth in cattle" = "muscle",
             "Food safety and allergen-free milk" = "milk",
             "Pig disease resistance" = "pig",
             "Wheat plant mildew resistance" = "wheat")
stopifnot(all(d$Treatment %in% names(arm_map)))
arm <- unname(arm_map[d$Treatment])
heat_version <- ifelse(d$Treatment == "Cattle heat resistance v1", "v1",
                ifelse(d$Treatment == "Cattle heat resistance v2", "v2", NA_character_))
# The animal item is filled exactly in the five animal arms.
stopifnot(all(ne(d$Qual_animalaccept) == (arm != "wheat")))

# ---- Exclusions -----------------------------------------------------------------
# The deposit is already screened: incomplete responses, failed attention checks
# (AttentCheck must be 3; all 1,128 rows are 3) and blank or nonsensical open
# answers were removed before deposit (2025 article, Data preparation and
# analysis). The 2023 article (Data preparation) then removed participants
# whose answers were flagged as copied from online sources (Turnitin > 50%, plus
# a read-through) before its qualitative analysis: exclude = Copied? == 1, the
# authors' own flag (Readme #7, variable 29). Note: the flag is post-treatment;
# copied shares are 23-25% in every arm.
stopifnot(all(d$AttentCheck == "3"), all(d[["Copied?"]] %in% c("0", "1")))
copied <- as.integer(d[["Copied?"]])
exclude <- copied == 1

# ---- Closed-ended items -------------------------------------------------------------
# Labels at the scale ends carry the number after a line break ("Very risky\r\n5");
# the number is the stored value (SAS lines 49-160). Social trust items are
# reverse scored as in the SAS code (Totally agree = 5) so that high = high trust.
num <- function(v) as.integer(sub(".*([1-5])\\s*$", "\\1", v))
sc <- function(cols, rev = FALSE) {
  m <- sapply(cols, function(v) num(d[[v]]))
  if (rev) m <- 6 - m
  rowMeans(m)
}
accept  <- sc(c("Accept_soc", "Accept_envir", "Risk_ben_soc", "Risk_ben_envir"))
benefit <- sc(c("Benefit_soc", "Benefit_you", "Benefit_envir"))
risk    <- sc(c("Risk_soc", "Risk_you", "Risk_envir"))
# Raw coding: 1 = Totally agree ... 5 = Totally disagree (questionnaire p. 7);
# SAS recodes Totally agree to 5.
trust   <- sc(c("Referent_decision", "Referent_RB", "Referent_problems",
                "Referent_concerned", "Referent_pressures"), rev = TRUE)

h <- data.frame(
  id        = d$anonymID,
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  # Line breaks normalized (\r\n and lone \r become \n); no other change.
  text      = trimws(gsub("\r\n?", "\n", d$Qual_animalaccept)),
  exclude   = exclude,
  # heat = cattle heat-stress resistance (v1 + v2, pooled by the authors);
  # muscle = farm profitability and increased muscle growth in cattle;
  # milk = food safety and allergen-free milk; pig = pig disease resistance.
  treatment = arm,
  heat_version = heat_version,
  copied    = copied,
  accept = round(accept, 4), benefit = round(benefit, 4),
  risk = round(risk, 4), trust = round(trust, 4),
  stringsAsFactors = FALSE
)

stopifnot(all(validUTF8(h$text[!is.na(h$text)])))
cat("Rows with empty text dropped (wheat arm, other prompt):", sum(!ne(h$text)), "\n")
h <- h[ne(h$text), ]
stopifnot(!any(h$treatment == "wheat"))

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded (copied):", sum(h$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
cat("All rows per arm:\n"); print(table(h$treatment))
cat("Primary contrast muscle vs heat, non-excluded:",
    sum(!h$exclude & h$treatment %in% c("muscle", "heat")), "\n")
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
