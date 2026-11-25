# ==============================================================================
# sperber2024_zambia.R -- harmonized file for card sperber2024_zambia
# ==============================================================================
# Source: Sperber, E., McClendon, G., & Kaaba, O. (2024). Comparing religious and
# secular interventions to increase young adult political participation. AJPS,
# doi:10.1111/ajps.12896. Data: Harvard Dataverse doi:10.7910/DVN/UBU601 (no
# licence set; see MANIFEST). Preregistration: osf.io/8xf59 (PAP, 28 May 2021).
#
# Item: identity_open (homework HW9, after Lesson 2). Human codes:
# homeworkanswers_identity.dta (RA codes id_religion, id_age, id_national,
# id_ethnic, id_gender).
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/sperber2024_zambia.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "sperber2024_zambia"
CARD <- "sperber2024_zambia"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# clean_Twilio.dta is the deposited original (fetched with format=original);
# clean_Twilio.tab is the Dataverse ingest and is not used (brief: ingests can
# damage text). The .dta was written by foreign::write.dta in Twilio_cleaning.R
# (last line), which stores strings as Latin-1/bytes, so encoding is checked below.
d <- read_dta(file.path(RAW, "clean_Twilio.dta"))
stopifnot(nrow(d) == 1183, !anyDuplicated(d$caseid))

h_codes <- read_dta(file.path(RAW, "homeworkanswers_identity.dta"))
stopifnot(nrow(h_codes) == 1183, !anyDuplicated(h_codes$caseid),
          setequal(h_codes$caseid, d$caseid))

# Treatment: track. Twilio_cleaning.R (end of file) recodes A -> infoonlytr,
# B -> seculartr, C -> religioustr; Codebook_cleaned_Twilio.pdf: A information
# only, B non-religious motivational, C religious motivational. The do-file
# (lines 7-17) builds trackA/B/C from the same letters.
arm_map <- c(infoonlytr = "infoonly", seculartr = "secular", religioustr = "religious")
arm <- unname(arm_map[as.character(d$track)])
stopifnot(!anyNA(arm))

# Text: identity_open = hw_a2_4 / hw_b2_4 / hw_c2_4 (Twilio_cleaning.R rename
# blocks), the same HW9 wording in all tracks (PAP p. 19).
clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  if (!all(validUTF8(x))) x <- iconv(x, from = "latin1", to = "UTF-8")
  stopifnot(all(validUTF8(x)))
  trimws(gsub("[\r\n\t]+", " ", x))
}
text <- clean_text(d$identity_open)

# Design columns: recruitment wave (the authors' i.wave_id fixed effects;
# Twilio file column `wave`, 1-3); exposure to the Christian-identity slide in
# Lesson 2 (lesson_sawchristianid, religious track only; codebook: 1 = yes,
# missing = no). Randomization strata (OSF registration osf.io/8xf59, q7: gender
# and Christian denomination) come from pp_baseline_cleaned.dta: dem_gender
# (Codebook_baseline.pdf: 0 female, 1 male; do-file line 125 `gen male=dem_gender`)
# and church_type (codebook: 1 Catholic ... 8 None, with "other"; labels kept).
# Only these columns are read; the file's free-text "other" fields are not.
b <- read_dta(file.path(RAW, "pp_baseline_cleaned.dta"),
              col_select = c(caseid, wave_id, track, church_type, dem_gender))
mb <- match(d$caseid, b$caseid)
stopifnot(!anyNA(mb),
          # baseline track letters agree with the Twilio labels (checked: all 1,183)
          identical(unname(c(A = "infoonlytr", B = "seculartr", C = "religioustr")[b$track[mb]]),
                    as.character(d$track)),
          all(as.integer(d$wave) == b$wave_id[mb]))
male        <- as.integer(b$dem_gender[mb])
church_type <- tolower(as.character(as_factor(b$church_type[mb])))
church_type[grepl("^mainline", church_type)] <- "mainline protestant"
wave <- as.integer(d$wave)
saw_christian_id <- as.integer(!is.na(d$lesson_sawchristianid) & d$lesson_sawchristianid == 1)
saw_youth_id     <- as.integer(!is.na(d$lesson_sawyouthid) & d$lesson_sawyouthid == 1)

# Human codes (Codebook_homeworkanswers_identity.pdf; RA coding of identity_open,
# 0/1 for all 1,183 respondents, non-respondents coded 0).
m <- match(d$caseid, h_codes$caseid)
hc <- function(v) as.integer(h_codes[[v]][m])

cat("Per arm: assigned, non-empty identity_open\n")
lv <- c("infoonly", "secular", "religious")
print(data.frame(arm = lv, assigned = as.vector(table(factor(arm, lv))),
                 text = as.vector(tapply(nzchar(text), factor(arm, lv), sum))))

# Exclusions: none. The do-file has no sample restriction for Table 5 (the
# `drop if wave_id==.` line is commented out) and every row has a track.
h <- data.frame(id = as.character(d$caseid), dataset = CARD, country = "ZM", lang = "en",
                text = text, exclude = FALSE, treatment = arm,
                wave = wave, male = male, church_type = church_type, saw_christian_id = saw_christian_id, saw_youth_id = saw_youth_id,
                human_religion = hc("id_religion"), human_age = hc("id_age"),
                human_national = hc("id_national"), human_ethnic = hc("id_ethnic"),
                human_gender = hc("id_gender"),
                stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm:\n")
print(table(treatment = factor(h$treatment[!h$exclude], lv)))
# Human-code totals are printed pooled, never by arm (by-arm shares are the
# outcome the protocol keeps blind until registration).
cat("Human codes = 1 among rows with text (pooled):\n")
print(colSums(h[, grep("^human_", names(h))]))
cat("Human codes = 1 among rows WITHOUT text (should be 0):",
    sum(hc("id_religion")[!nzchar(text)] + hc("id_age")[!nzchar(text)] +
        hc("id_national")[!nzchar(text)] + hc("id_gender")[!nzchar(text)] +
        hc("id_ethnic")[!nzchar(text)]), "\n")
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
