# Harmonized files for cards murphy2026_memoryontrial_s1, _s2, _s3 (OSF 5kd8w, "Memory on Trial").
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/murphy2026_memoryontrial.R
# Reads only data/external/murphy2026_memoryontrial/raw/; writes
# data/external/murphy2026_memoryontrial/harmonized_murphy2026_memoryontrial_s<k>.csv.
# Prints counts only; never prints text values (blindness rule).

suppressMessages(library(haven))
raw_dir <- "data/external/murphy2026_memoryontrial/raw"
out_dir <- "data/external/murphy2026_memoryontrial"

# Arm coding. The archive has no codebook; the numeric codes differ across files. Resolved from
# the arm-specific comprehension check that only the respondents of one arm saw (Qualtrics PDFs,
# Study 1 pp. 13-25 and Study 2 pp. 12-27: the defense block ends with "What does this expert
# witness say about Mary's memory?", the prosecution block with "... about Sean's memory?", the
# court block with "... about Sean and Mary's memories?"). The builder re-checks this below.
#   Study 1 ExpertWitnessTestimonyCondition: 0 None, 1 "About Mary" = defense, 2 "About Sean" =
#     prosecution, 3 "About both" = court (value labels; ExpertMary answered only by code 1).
#   Study 2 Condition (no value labels): 1 = prosecution (Expert_Sean answered only by code 1;
#     indicator ExpertWitness_Sean1), 2 = defense (Expert_Mary only by code 2; ExpertWitness_Mary2).
#   Study 3 Condition value labels: 0 Control, 1 Prosecution, 2 Defence, 3 Court (matches checks).
# Note that codes 1 and 2 are swapped between Study 1 and Studies 2-3.
arm_maps <- list(
  s1 = c(`0` = "control", `1` = "defense", `2` = "prosecution", `3` = "court"),
  s2 = c(`0` = "control", `1` = "prosecution", `2` = "defense", `3` = "court"),
  s3 = c(`0` = "control", `1` = "prosecution", `2` = "defense", `3` = "court"))

spec <- list(
  s1 = list(file = "ExpertWitnessData_Study1.sav", cond = "ExpertWitnessTestimonyCondition",
            text = "AnythingElse", id = NULL,
            chk = c(defense = "ExpertMary", prosecution = "ExpertSean", court = "ExpertBoth"),
            verdict = "Verdict", consent = "LikelyConsensual", conf = "Confidence",
            sean_acc = "SeanMemoryAccuracy", mary_acc = "MaryMemoryAccuracy",
            impact = "WitnessEffectiveness",
            country = "IE"),   # inferred: Irish education categories (Junior/Leaving Certificate), no Prolific ID
  s2 = list(file = "ExpertWitnessData_Study2.sav", cond = "Condition",
            text = "AnythingAboutVerdict", id = NULL,
            chk = c(defense = "Expert_Mary", prosecution = "Expert_Sean", court = "Expert_Both"),
            verdict = "Verdict", consent = "LikelyConsensual", conf = "Confidence",
            sean_acc = "SeanMemoryAccuracy", mary_acc = "MaryMemoryAccuracy",
            impact = "ExpertWitnessEffect",
            country = NA_character_),   # Prolific sample, country not stated in the archive
  s3 = list(file = "ExpertWitness_Study3.sav", cond = "Condition",
            text = "Q7", id = "ResponseId",
            chk = c(defense = "Exp_Def_Check", prosecution = "Exp_Pros_Check", court = "Exp_Court_Check"),
            verdict = "Q2.0", consent = "Q1.0", conf = "Q3.0",
            sean_acc = "Q4", mary_acc = "Q5", impact = "Q47",
            country = NA_character_))   # no survey PDF; UserLanguage EN-GB only

num <- function(x) as.numeric(zap_labels(x))

for (k in names(spec)) {
  s <- spec[[k]]
  card <- paste0("murphy2026_memoryontrial_", k)
  d <- read_sav(file.path(raw_dir, s$file))
  code <- num(d[[s$cond]])
  stopifnot(all(code %in% 0:3))
  arm <- unname(arm_maps[[k]][as.character(code)])
  # Verify the arm mapping: each check item answered only, and always, in its own arm.
  for (a in names(s$chk)) stopifnot(identical(!is.na(d[[s$chk[[a]]]]), arm == a))
  if (k == "s3") stopifnot(identical(as_factor(d$Condition, levels = "labels") |> as.character() |> tolower(),
                                     sub("defense", "defence", arm)))

  # Exclusions: none. No exclusion rule is documented in the archive (no preregistration or
  # article found), and the OSF abstract's N = 1,379 equals Study 1 + Study 2 rows (884 + 495)
  # with nothing dropped. Unfinished Study 1 rows (146; they left during the later attitude scales)
  # are kept and flagged in `finished`. Every row has a randomized condition (checked above).
  d$exclude <- FALSE

  txt <- as.character(d[[s$text]]); txt[is.na(txt)] <- ""
  d$text <- trimws(txt)
  # id: Study 3 has the Qualtrics ResponseId; Studies 1 and 2 have none (Study 2 has a Prolific
  # ID, a personal identifier, which is not used), so row order in the deposited .sav is the id.
  d$id <- if (is.null(s$id)) sprintf("%s_r%04d", k, seq_len(nrow(d))) else as.character(d[[s$id]])
  stopifnot(!anyDuplicated(d$id))

  v <- num(d[[s$verdict]])   # 1 = guilty, 2 = not guilty (value labels)
  h <- data.frame(
    id = d$id, dataset = card, country = s$country, lang = "en",
    text = d$text, exclude = d$exclude, treatment = arm,
    condition_raw = code,
    verdict_guilty = ifelse(is.na(v), NA_integer_, as.integer(v == 1)),
    likely_consensual = num(d[[s$consent]]),       # 1 not at all likely ... 5 definitely likely
    confidence = num(d[[s$conf]]),                 # 1 ... 5
    sean_memory_accuracy = num(d[[s$sean_acc]]),   # 1 not accurately at all ... 5 extremely
    mary_memory_accuracy = num(d[[s$mary_acc]]),
    testimony_impact = num(d[[s$impact]]),         # 1 much more likely guilty ... 5 much more likely innocent; expert arms only
    finished = as.integer(num(d$Finished) == 1),
    stringsAsFactors = FALSE)
  # Keep rows with empty text out (brief).
  h <- h[nzchar(h$text), ]
  out <- file.path(out_dir, paste0("harmonized_", card, ".csv"))
  write.csv(h, out, row.names = FALSE, fileEncoding = "UTF-8")

  cat("\n==", card, "== rows in .sav:", nrow(d), " rows written (non-empty text):", nrow(h), "\n")
  cat("non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
  cat("unfinished rows written:", sum(h$finished == 0), "\n")
  back <- read.csv(out, encoding = "UTF-8", stringsAsFactors = FALSE)
  cat("re-read text identical:", identical(back$text, h$text), "\n")
  cat("'<U+' escapes in file:", sum(grepl("<U+", readLines(out, encoding = "UTF-8", warn = FALSE), fixed = TRUE)),
      "; U+FFFD:", sum(grepl("�", h$text, fixed = TRUE)), "\n")
}
