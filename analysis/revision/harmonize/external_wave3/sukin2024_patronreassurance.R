# ==============================================================================
# sukin2024_patronreassurance.R -- harmonized file for card sukin2024_patronreassurance
# ==============================================================================
# Source: Sukin, L., & Lanoszka, A. (2024). Credibility in Crises: How Patrons
# Reassure Their Allies. International Studies Quarterly 68(2), sqae062,
# doi:10.1093/isq/sqae062. Data: Harvard Dataverse doi:10.7910/DVN/NAN9KR (CC0 1.0).
# Lucid samples in Poland, Romania, Latvia, Lithuania, Estonia, March 2022.
#
# Item: WhyPolicy, "Why do you support or oppose Policy [A/B/C/D/E/F]?" (online
# appendix 7.2), asked after the randomly assigned policy and its ratings, in the
# language the respondent chose (Polish, Romanian, Lithuanian, Latvian, Estonian or
# English). The answers are in the original languages (counts of language-specific
# letters below); the archive holds no translation.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/sukin2024_patronreassurance.R
# ==============================================================================
DS   <- "sukin2024_patronreassurance"
CARD <- "sukin2024_patronreassurance"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# combined_data.csv is the deposited original of the Dataverse ingest
# combined_data.tab; it is the file Replication.R reads (line 17), read the same way.
d <- read.csv(file.path(RAW, "combined_data.csv"), encoding = "UTF-8",
              stringsAsFactors = FALSE, na.strings = c("", "NA"))
stopifnot(nrow(d) == 2103, !anyDuplicated(d$ResponseId))

# Assigned policy (Replication.R header: A Sanctions, B Bilateral Summit,
# C In-Theatre Deployment, D Deterrent Deployment, E Multilateral Summit,
# F Reiterate Guarantee). Short holds the letter; it agrees with the letter in
# Policy, which EEurope.R lines 43-48 parse with grepl("A:") etc.
arm_map <- c(A = "sanctions", B = "bilateral_summit", C = "in_theater_deployment",
             D = "deterrent_deployment", E = "multilateral_summit", F = "reiterate_guarantee")
short <- ifelse(is.na(d$Short), "", d$Short)
for (L in names(arm_map)) stopifnot(all((short == L) == grepl(paste0(L, ":"), ifelse(is.na(d$Policy), "", d$Policy), fixed = TRUE)))
arm <- unname(arm_map[short])          # NA for the 72 rows that never reached the block

# Exclusions: the authors' main analysis sample df_clean2 (Replication.R lines
# 22-31): Consent == 1, AttnCheck == 1, Vet not missing (reached the end),
# ManCheck == 1, Duration..in.seconds. > 358 (speeders), State != 99. Rows without
# an assigned policy are also excluded. Table 3 N = 1,382 (checked below).
keep <- d$Consent %in% 1 & d$AttnCheck %in% 1 & !is.na(d$Vet) & d$ManCheck %in% 1 &
        d$Duration..in.seconds. > 358 & d$State %in% 1:5
keep[is.na(keep)] <- FALSE
stopifnot(sum(keep & !is.na(arm)) == 1382)
exclude <- !(keep & !is.na(arm))

# Country from State (appendix 7.2 order; Replication.R line 85-86 treats 2-4 as
# the Baltic states and 1 as Poland, the base category): 1 PL, 2 LV, 3 LT, 4 EE,
# 5 RO; 99 (no state) and missing are NA and excluded above. Language = the
# survey language the respondent selected (the English sample lives in Poland
# and Romania); lang is the survey language, not a detected text language.
country <- c("PL", "LV", "LT", "EE", "RO")[match(d$State, 1:5)]
lang <- unname(c(Polish = "pl", Romanian = "ro", Lithuanian = "lt", Latvian = "lv",
                 Estonian = "et", English = "en")[d$Language])
stopifnot(!anyNA(lang))

# Text: WhyPolicy, whitespace runs collapsed and trimmed.
clean_text <- function(x) {
  x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))
  trimws(gsub("[[:space:]]+", " ", x))
}
text <- clean_text(d$WhyPolicy)

# Design columns: risk (the authors' High risk = B, C, D, E; Replication.R
# variable Direct); target_latvia (randomized target of the attack, EEurope.R
# lines 51-54 recode); support and effective (closed ratings of the assigned
# policy, 1-4); treat_level and treat_type (the earlier, separate
# randomization Treatment_DO, EEurope.R lines 40-41; not used by the article).
risk <- ifelse(is.na(arm), NA_character_,
               ifelse(arm %in% c("sanctions", "reiterate_guarantee"), "low", "high"))
tgt <- ifelse(is.na(d$Target), NA_integer_,
              as.integer(d$Target %in% c("Latvia", "Läti", "Latvija", "Łotwie", "Letonia")))
treat_level <- ifelse(is.na(d$Treatment_DO), NA_integer_, as.integer(grepl("High", d$Treatment_DO)))
treat_type  <- ifelse(is.na(d$Treatment_DO), NA_integer_, as.integer(grepl("Nuclear", d$Treatment_DO)))

# Encoding counts by survey language (rows with text): language-specific letters,
# replacement characters, typical UTF-8-read-as-Latin-1 sequences, invalid UTF-8.
enc_counts <- function(x, lg) {
  f <- function(p) tapply(grepl(p, x), lg, sum)
  data.frame(rows = as.vector(table(lg)),
             pl = f("[ąęłżĄĘŁŻ]"),
             ro = f("[ăâîșțşţĂÂÎȘȚŞŢ]"),
             lt = f("[ėįųūĖĮŲŪ]"),
             lv = f("[āēīūķļņĀĒĪŪĶĻŅ]"),
             et = f("[õäöüÕÄÖÜ]"),
             replacement = f("�"),
             mojibake = f("Ã[\u0080-¿]|Ä[\u0080-¿]|Å[\u0080-¿]|È[\u0098-\u009b]|â€"))
}

h <- data.frame(id = d$ResponseId, dataset = CARD, country = country, lang = lang,
                text = text, exclude = exclude, treatment = arm,
                risk = risk, target_latvia = tgt,
                support = as.integer(d$Support), effective = as.integer(d$Effective),
                treat_level = treat_level, treat_type = treat_type,
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

cat("Encoding counts before writing (rows with text, by survey language):\n")
print(enc_counts(h$text, h$lang))
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
lv <- unname(arm_map)
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm:\n")
print(table(treatment = factor(h$treatment[!h$exclude], lv)))
cat("Non-excluded rows by risk:\n"); print(table(risk = h$risk[!h$exclude]))
cat("Non-excluded rows by country x lang:\n"); print(table(h$country[!h$exclude], h$lang[!h$exclude], useNA = "ifany"))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
cat("Encoding counts after re-read:\n"); print(enc_counts(r$text, r$lang))
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
