# ==============================================================================
# spruill2022_legalreasonable.R -- harmonized files for cards
#   spruill2022_legalreasonable_s1 and spruill2022_legalreasonable_s2
# ==============================================================================
# Spruill, M., & Lewis, N. A., Jr. (2022). Legal descriptions of police officers
# affect how citizens judge them. Journal of Experimental Social Psychology,
# 101, 104306. doi:10.1016/j.jesp.2022.104306. Data and code: OSF g2d54.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/spruill2022_legalreasonable.R
#
# Inputs (data/external/spruill2022_legalreasonable/raw/):
#   PsychologyofLegalDecisions_Data_Study_One.csv  (Qualtrics export, 1,124 rows)
#   PsychologyofLegalDecisions_Data_Study_Two.csv  (Qualtrics export, 1,093 rows)
# Outputs:
#   data/external/spruill2022_legalreasonable/harmonized_spruill2022_legalreasonable_s1.csv
#   data/external/spruill2022_legalreasonable/harmonized_spruill2022_legalreasonable_s2.csv
#
# Text item (one per arm, shown first after consent and the attention pledge):
#   "Describe the actions and/or conduct that come to mind when you imagine
#    a police officer. / ... an average police officer. / ... an objectively
#    reasonable police officer."
# Each arm's version is a separate Qualtrics question. Column-to-arm mapping
# read from the question-text header row of each export (NOT the mapping in
# the first MANIFEST, which had Q112/Q113 swapped):
#   Study 1: Q111 = "a police officer" (Blank), Q112 = "an objectively
#            reasonable police officer" (OR), Q113 = "an average police
#            officer" (Average).
#   Study 2: Q112 = OR, Q113 = Average.
# The same mapping holds for the trait-rating blocks in the authors' scripts
# (S1_quantitative.Rmd l. 62-330: Q122 Blank, Q123 OR, Q124 Average).
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

raw_dir <- "data/external/spruill2022_legalreasonable/raw"
out_dir <- "data/external/spruill2022_legalreasonable"

read_q <- function(f) {
  d <- read.csv(file.path(raw_dir, f), check.names = FALSE, colClasses = "character",
                encoding = "UTF-8", na.strings = character(0))
  # The two Qualtrics header rows (question text; ImportId JSON) are sorted
  # into the body of both files. They are not respondents: drop them.
  hdr <- d$Condition %in% c("Condition", "{\"ImportId\":\"Condition\"}")
  stopifnot(sum(hdr) == 2)
  d[!hdr, ]
}
ne   <- function(x) !is.na(x) & nzchar(trimws(x))
norm <- function(x) trimws(gsub("\r\n?", "\n", x))  # CR -> \n (read.csv does not round-trip \r)
num  <- function(x) suppressWarnings(as.numeric(x))

# Authors' scale items (S1_quantitative.Rmd l. 589-593 and S2_quantitative.Rmd
# l. 599-603): warmth = Tolerant, Warm, GoodNatured, Sincere; competence =
# Competent, Confident, Independent, Competitive, Intelligent. Item positions
# within each 27-item block (S1_quantitative.Rmd l. 62-330): Competent 1,
# Warm 2, Tolerant 8, Confident 10, Intelligent 15, Sincere 17, Competitive 18,
# GoodNatured 19, Independent 22. Scored as the item mean (psych::scoreItems
# default; missing items are ignored here rather than median-imputed).
warm_items <- c(2, 8, 19, 17); comp_items <- c(1, 10, 22, 18, 15)
scale_mean <- function(d, block, items) {
  m <- sapply(items, function(i) num(d[[paste0(block, "_", i)]]))
  r <- rowMeans(m, na.rm = TRUE); r[is.nan(r)] <- NA; r
}
pick_block <- function(d, arm_blocks, items) {
  out <- rep(NA_real_, nrow(d))
  for (a in names(arm_blocks)) {
    k <- d$Condition == a
    out[k] <- scale_mean(d[k, ], arm_blocks[[a]], items)
  }
  out
}

finish <- function(h, card_id) {
  stopifnot(all(validUTF8(h$text)), !anyDuplicated(h$id))
  cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
  h <- h[h$text != "", ]
  out_csv <- file.path(out_dir, paste0("harmonized_", card_id, ".csv"))
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Rows written:", nrow(h), "\n")
  cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
  cat("Excluded rows per arm:\n"); print(table(h$treatment[h$exclude]))
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  stopifnot(identical(r$text, h$text))
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n")
  stopifnot(n_esc == 0)
  invisible(h)
}

# ==============================================================================
# Study 1: three arms (Blank / Average / OR), not preregistered
# ==============================================================================
cat("==== Study 1 ====\n")
d1 <- read_q("PsychologyofLegalDecisions_Data_Study_One.csv")
stopifnot(nrow(d1) == 1122, !anyDuplicated(d1$ResponseId))
stopifnot(identical(as.vector(table(d1$Condition)[c("Blank", "Average", "OR")]), c(374L, 374L, 374L)))

# Treatment: embedded-data field Condition set by Qualtrics random assignment
# (article, Study 1 Method: "Each participant was randomly assigned to one of
# three conditions"). Labels follow S1_qualitative.Rmd l. 46-50.
arm_col1 <- c(Blank = "Q111", OR = "Q112", Average = "Q113")
arm1 <- c(Blank = "blank", Average = "average", OR = "reasonable")[d1$Condition]

# Text from the column that belongs to the assigned arm. Check that no
# respondent has text in another arm's column.
own1 <- vapply(seq_len(nrow(d1)), function(i) d1[[arm_col1[[d1$Condition[i]]]]][i], "")
other1 <- rowSums(sapply(arm_col1, function(q) ne(d1[[q]]))) - ne(own1)
cat("Respondents with text in another arm's column:", sum(other1 > 0), "\n")
stopifnot(sum(other1 > 0) == 0)

# Exclusions: authors' inclusion criteria, S1_quantitative.Rmd l. 424-436
# (result: 968 participants; article, Study 1 Participants):
#   ICAnswer_1 == 1 & ICAnswer_2 == 1 (18+, consent), QC == 2 (commits to
#   best answers), Q131 == 1 (jury eligible: "Yes"; l. 360), Q110 == 1 (final
#   consent "Yes"; l. 380). The Age filter at l. 449 is not assigned and drops
#   nobody.
keep1 <- d1$ICAnswer_1 == "1" & d1$ICAnswer_2 == "1" & d1$QC == "2" &
         d1$Q131 == "1" & d1$Q110 == "1"
cat("Authors' analytic sample (expect 968):", sum(keep1), "\n")
stopifnot(sum(keep1) == 968)

race1 <- num(d1$ParticipantRace)   # 1 White, 2 Black, 3 AI/AN, 4 Asian, 5 NH/PI, 6 Latinx, 7 Multiracial, 8 Other (l. 375)
h1 <- data.frame(
  id        = d1$ResponseId,
  dataset   = "spruill2022_legalreasonable_s1",
  country   = "US",
  lang      = "en",
  text      = norm(own1),
  exclude   = !keep1,
  treatment = unname(arm1),          # "blank" / "average" / "reasonable"
  race      = race1,
  race_white = as.integer(race1 == 1),   # authors' White vs NonWhite moderator
  warmth     = pick_block(d1, c(Blank = "Q122", OR = "Q123", Average = "Q124"), warm_items),
  competence = pick_block(d1, c(Blank = "Q122", OR = "Q123", Average = "Q124"), comp_items),
  stringsAsFactors = FALSE
)
h1 <- finish(h1, "spruill2022_legalreasonable_s1")

# ==============================================================================
# Study 2: two arms (Average / OR), preregistered (OSF k9fh6)
# ==============================================================================
cat("==== Study 2 ====\n")
d2 <- read_q("PsychologyofLegalDecisions_Data_Study_Two.csv")
stopifnot(nrow(d2) == 1091, !anyDuplicated(d2$ResponseId))
stopifnot(identical(as.vector(table(d2$Condition)[c("Average", "OR")]), c(546L, 545L)))

# Treatment: Qualtrics block randomizer FL_49 shows block FL_47 (OR) or FL_48
# (Average); Condition agrees with the randomizer on every row.
stopifnot(all((d2$FL_49_DO_FL_47 == "1") == (d2$Condition == "OR")),
          all((d2$FL_49_DO_FL_48 == "1") == (d2$Condition == "Average")))
arm_col2 <- c(OR = "Q112", Average = "Q113")
own2 <- vapply(seq_len(nrow(d2)), function(i) d2[[arm_col2[[d2$Condition[i]]]]][i], "")
other2 <- rowSums(sapply(arm_col2, function(q) ne(d2[[q]]))) - ne(own2)
cat("Respondents with text in another arm's column:", sum(other2 > 0), "\n")
stopifnot(sum(other2 > 0) == 0)

# Exclusions: S2_quantitative.Rmd l. 400-416 (result: 917; article, Study 2
# Participants): ICAnswer_1/2 == 1, QC == 2, ParticipantRace White (1) or
# Black (2), JuryEligibility == 1 ("Yes"; l. 327), Q110 == 1 (final consent).
# The preregistered TaskMaster rule (exclude > 50% of time off task; OSF
# k9fh6, "outliers") is NOT applied in the authors' script; the share is kept
# as offtask_share for a sensitivity check.
keep2 <- d2$ICAnswer_1 == "1" & d2$ICAnswer_2 == "1" & d2$QC == "2" &
         d2$ParticipantRace %in% c("1", "2") & d2$JuryEligibility == "1" & d2$Q110 == "1"
cat("Authors' analytic sample (expect 917):", sum(keep2), "\n")
stopifnot(sum(keep2) == 917)

off <- num(d2$totalOffTask); on <- num(d2$totalOnTask)
offshare <- ifelse(off + on > 0, off / (off + on), NA_real_)
cat("Analytic rows with off-task share > 0.5 (prereg rule, not applied):",
    sum(keep2 & !is.na(offshare) & offshare > 0.5), "; missing share:", sum(keep2 & is.na(offshare)), "\n")

race2 <- num(d2$ParticipantRace)
h2 <- data.frame(
  id        = d2$ResponseId,
  dataset   = "spruill2022_legalreasonable_s2",
  country   = "US",
  lang      = "en",
  text      = norm(own2),
  exclude   = !keep2,
  treatment = as.integer(d2$Condition == "OR"),   # 1 = objectively reasonable, 0 = average
  race_black = ifelse(race2 %in% c(1, 2), as.integer(race2 == 2), NA_integer_),  # quota stratum and moderator
  offtask_share = round(offshare, 4),
  warmth     = pick_block(d2, c(OR = "Q123", Average = "Q124"), warm_items),
  competence = pick_block(d2, c(OR = "Q123", Average = "Q124"), comp_items),
  # Blindness incident (card provenance.contamination): the drafter saw the
  # first 160 characters of this one answer while printing the first data row.
  # Flagged, not dropped (dropping would remove a row from one arm only).
  seen_by_drafter = d2$ResponseId == "R_1q8vRnf0a5sKUCs",
  stringsAsFactors = FALSE
)
stopifnot(sum(h2$seen_by_drafter) == 1)
h2 <- finish(h2, "spruill2022_legalreasonable_s2")
cat("seen_by_drafter row: treatment =", h2$treatment[h2$seen_by_drafter],
    "; exclude =", h2$exclude[h2$seen_by_drafter], "\n")
cat("S2 non-excluded rows by arm x race_black:\n")
print(table(treatment = h2$treatment[!h2$exclude], race_black = h2$race_black[!h2$exclude]))
