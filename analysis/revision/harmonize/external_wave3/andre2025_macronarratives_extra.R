# ==============================================================================
# andre2025_macronarratives_extra.R -- harmonized files for the wave-3 cards
#   andre2025_macronarratives_provision  (narrative provision, April 2022)
#   andre2025_macronarratives_mp         (monetary-policy narrative, June 2022)
#   andre2025_macronarratives_media      (media experiment, February 2022)
# ==============================================================================
# Andre, Haaland, Roth, Wiederholt & Wohlfart, "Narratives about the
# Macroeconomy", REStud (2026); replication package Zenodo 15311300
# (narratives_replication.zip, md5 e132c04abad62809984d6065b735f696).
# The wave-2 card andre2025_macronarratives (attention experiment) and its
# harmonized file are built by external_wave2/econ/harmonize.py and are NOT
# touched here.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/andre2025_macronarratives_extra.R
#
# Inputs (data/external/andre2025_macronarratives/raw/, unchanged copies from
# the package's raw/ and processed/ folders; see MANIFEST.md):
#   dag_wave1.dta, dag_wave2.dta          narrative provision, waves 1 and 2
#   mp_dag_wave1.dta, mp_dag_wave2.dta    monetary-policy provision, waves 1, 2
#   media_wave1/2/3.dta                   media experiment, waves 1-3
#   purpose_dagprovision.dta, purpose_dag_mp.dta  hand-coded purpose guesses
#   processed/narratives.dta              authors' hand-coded DAG codes for
#                                         every `story` answer, by ResponseId
#                                         (built by cleaning_R/clean_narratives.R)
# The authors' Stata cleaning files are kept in raw/cleaning/ and cited below
# as <file>.do, l. <line>.
#
# Text column in every experiment: `story` ("Why has the inflation rate
# increased? ... Which factors do you think caused the increase in the
# inflation rate? Please respond in full sentences."), asked in the follow-up
# wave, identical wording in all arms.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/andre2025_macronarratives/raw"
out_dir <- "data/external/andre2025_macronarratives"

rd <- function(f) {
  d <- read_dta(file.path(raw_dir, f))
  # status / finished are labelled numerics: 0 = "IP Address", 1 = True
  as.data.frame(d)
}
num <- function(x) as.numeric(zap_labels(x))

# Authors' common filters on a raw wave (e.g. clean_dagprovision_wave1.do,
# ll. 21-27 and 63-68): drop missing prolific_pid; drop EVERY row of a
# prolific_pid that occurs more than once ("drop if n>1"); keep status == 0
# (valid IP address, not preview); keep finished == 1.
std_filter <- function(d, finished = TRUE) {
  d <- d[!is.na(d$prolific_pid) & d$prolific_pid != "", ]
  dup <- d$prolific_pid %in% d$prolific_pid[duplicated(d$prolific_pid)]
  d <- d[!dup, ]
  d <- d[!is.na(num(d$status)) & num(d$status) == 0, ]
  if (finished) d <- d[!is.na(num(d$finished)) & num(d$finished) == 1, ]
  d
}

# Inflation forecast recode (clean_inflation_point.do): bottom 0, top 20.
infl <- function(x) pmin(pmax(num(x), 0), 20)

# Human codes (one row per ResponseId; clean_narratives.R derives dagcode_*,
# dagcat_* and dag_both_DS from the coders' DAG strings in raw/dag_data/).
nar <- as.data.frame(read_dta(file.path(raw_dir, "processed/narratives.dta")))
stopifnot(!anyDuplicated(nar$ResponseId))
code_cols <- c(grep("^dagcode_", names(nar), value = TRUE),
               "dagcat_demand", "dagcat_supply", "dagcat_misc", "dag_both_DS",
               "dag_size", "dag_exists", "auxcode_dk", "auxcode_junk")
add_codes <- function(h, rid) {
  m <- match(rid, nar$ResponseId)
  for (v in code_cols) h[[paste0("human_", v)]] <- num(nar[[v]])[m]
  h
}

write_checked <- function(h, card_id) {
  # Rows with empty text are dropped (shared brief).
  cat("\n=====", card_id, "\nRows with empty text dropped:", sum(h$text == ""), "\n")
  h <- h[h$text != "", ]
  stopifnot(!anyDuplicated(h$id), all(validUTF8(h$text)))
  out_csv <- file.path(out_dir, paste0("harmonized_", card_id, ".csv"))
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Rows written:", nrow(h), " excluded:", sum(h$exclude), "\n")
  cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
  cat("Human codes merged (non-excluded):", sum(!is.na(h$human_dag_exists[!h$exclude])), "\n")
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  stopifnot(identical(r$text, h$text))
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n")
  stopifnot(n_esc == 0)
  invisible(h)
}

# ---- Two-wave provision experiments (same structure) ---------------------------
# Wave 1: randomization (treatment), clean_dagprovision_wave1.do /
#   clean_dag_mp_wave1.do: std_filter, then "drop if missing(treatment)"
#   (l. 146 / l. 146).
# Wave 2: `story`, clean_dagprovision_wave2.do / clean_dag_mp_wave2.do:
#   std_filter, then merge 1:1 with narratives.dta on responseid, keep matched
#   (ll. 74-78 / ll. 74-78).
# Pooling: merge_dagprovision.do / merge_dag_mp.do, l. 15: wave 1 is master,
#   wave-2 rows without a wave-1 match are dropped ("drop if _merge==2").
# The authors' Table 4 / Table 5 regressions (analysis/table4.do, table5.do)
# use every pooled row with wave-2 data and robust SEs; no further exclusion.
# Here every wave-2 row with text is kept, and exclude = TRUE marks the rows
# that the chain above drops (including rows with no randomized assignment).
build_provision <- function(w1f, w2f, purpose_f, arms, card_id, prefix) {
  w1 <- std_filter(rd(w1f))
  w1 <- w1[!is.na(w1$treatment) & w1$treatment != "", ]
  stopifnot(all(w1$treatment %in% arms))
  w2_all <- rd(w2f)
  w2_ok <- std_filter(w2_all)
  w2_ok <- w2_ok[w2_ok$responseid %in% nar$ResponseId, ]
  d <- w2_all
  d$tr <- w1$treatment[match(d$prolific_pid, w1$prolific_pid)]
  d$exclude <- !(d$responseid %in% w2_ok$responseid) | is.na(d$tr)
  cat(card_id, ": wave-2 rows", nrow(d), "| in authors' pooled sample", sum(!d$exclude),
      "| no wave-1 assignment", sum(is.na(d$tr)), "\n")
  pur <- as.data.frame(read_dta(file.path(raw_dir, purpose_f)))
  stopifnot(!anyDuplicated(pur$prolific_pid))
  h <- data.frame(
    id        = paste0(prefix, d$responseid),   # wave-2 Qualtrics ResponseId (hashed by the authors)
    dataset   = card_id,
    country   = "US",
    lang      = "en",
    text      = trimws(ifelse(is.na(d$story), "", d$story)),
    exclude   = d$exclude,
    treatment = d$tr,
    # Closed-ended outcome in the same follow-up survey, after `story`:
    # 12-month inflation point forecast (inflation_12_w2 in table4.do / table5.do).
    inflation_12_w2 = infl(d$infl12),
    # Hand-coded purpose guess (merge_dagprovision.do, ll. 25-28): 1 = guessed
    # the hypothesis; the authors' robustness drops these respondents.
    purpose_correct = num(pur$purpose_correct)[match(d$prolific_pid, pur$prolific_pid)],
    stringsAsFactors = FALSE
  )
  h <- add_codes(h, d$responseid)
  h
}

# Narrative provision (paper Section 5.2.1): arms baseline (no narrative),
# pudem (pent-up demand narrative), energy (energy-crisis narrative).
# treatment kept as lower-case arm labels (three arms).
prov <- build_provision("dag_wave1.dta", "dag_wave2.dta", "purpose_dagprovision.dta",
                        c("baseline", "pudem", "energy"),
                        "andre2025_macronarratives_provision", "prov_")
write_checked(prov, "andre2025_macronarratives_provision")

# Monetary-policy narrative (paper Section 5.2.2): arms mp vs energy (active
# control). treatment 0/1: 1 = mp, 0 = energy.
mp <- build_provision("mp_dag_wave1.dta", "mp_dag_wave2.dta", "purpose_dag_mp.dta",
                      c("mp", "energy"), "andre2025_macronarratives_mp", "mp_")
mp$treatment <- ifelse(is.na(mp$treatment), NA_integer_, as.integer(mp$treatment == "mp"))
write_checked(mp, "andre2025_macronarratives_mp")

# ---- Media experiment (paper Appendix 1) -------------------------------------
# Wave 1 (clean_media_wave1.do): std_filter (ll. 26-30, 58-62) and
#   "drop if missing(vote_trump)" (l. 95; vote 3 "Other" / 4 "Did not vote"
#   are set to missing, l. 93).
# Wave 2 (clean_media_wave2.do): duplicates dropped BEFORE "keep if
#   finished==1" (ll. 33-40); status == 0 (l. 67); country == "United States"
#   (l. 71); treatment "inflation" -> Tr = 1, "miami" -> Tr = 0 (ll. 78-79);
#   rows without treatment dropped (l. 83).
# Wave 3 (clean_media_wave3.do): std_filter (ll. 15-19, 47-51), merge with
#   narratives.dta on ResponseId.
# Pooling (merge_media.do): wave 1 master; wave-2 using-only rows dropped
#   (l. 15); wave 3 merged without dropping (l. 19). The Table 7 regressions
#   (analysis/table7.do) need Tr and the wave-1 baseline outcome, i.e. a row in
#   cleaned waves 1 and 2. exclude = TRUE otherwise.
w1 <- std_filter(rd("media_wave1.dta"))
w1 <- w1[!is.na(num(w1$vote)) & num(w1$vote) %in% c(1, 2), ]
w2 <- rd("media_wave2.dta")
w2 <- w2[!is.na(w2$prolific_pid) & w2$prolific_pid != "", ]
w2 <- w2[!(w2$prolific_pid %in% w2$prolific_pid[duplicated(w2$prolific_pid)]), ]
w2 <- w2[num(w2$finished) %in% 1 & num(w2$status) %in% 0 & w2$country == "United States", ]
w2 <- w2[w2$treatment %in% c("inflation", "miami"), ]
w3_all <- rd("media_wave3.dta")
w3_ok <- std_filter(w3_all)
d <- w3_all
d$tr <- w2$treatment[match(d$prolific_pid, w2$prolific_pid)]
d$in_w1 <- d$prolific_pid %in% w1$prolific_pid
d$exclude <- !(d$responseid %in% w3_ok$responseid) | is.na(d$tr) | !d$in_w1
cat("media: wave-3 rows", nrow(d), "| in analysis sample", sum(!d$exclude),
    "| no wave-2 assignment", sum(is.na(d$tr)), "| not in cleaned wave 1", sum(!d$in_w1), "\n")
# Wave-1 (pre-treatment) story: its hand codes are the authors' baseline
# control in table7.do; carried as design columns, prefixed base_. In
# narratives.dta the wave-1 story codes are stored under the WAVE-2 ResponseId
# (clean_media_wave2.do merges narratives.dta on the wave-2 ResponseId; no
# wave-1 ResponseId occurs in narratives.dta, checked 2026-11-17: 0 of 2,273).
w2r <- w2$responseid[match(d$prolific_pid, w2$prolific_pid)]
mb <- match(w2r, nar$ResponseId)
media <- data.frame(
  id        = paste0("media_", d$responseid),  # wave-3 ResponseId
  dataset   = "andre2025_macronarratives_media",
  country   = "US",
  lang      = "en",
  text      = trimws(ifelse(is.na(d$story), "", d$story)),
  exclude   = d$exclude,
  treatment = ifelse(is.na(d$tr), NA_integer_, as.integer(d$tr == "inflation")),
  # Closed-ended: articles read about the latest inflation numbers (wave 3;
  # inflation_news in merge_media.do, l. 28) and 12-month forecast.
  inflation_news  = num(d$inflation_news_),
  inflation_12_w3 = infl(d$infl12),
  vote_trump      = as.integer(num(w1$vote)[match(d$prolific_pid, w1$prolific_pid)] == 1),
  base_dagcat_demand = num(nar$dagcat_demand)[mb],
  base_dagcat_supply = num(nar$dagcat_supply)[mb],
  base_dagcat_misc   = num(nar$dagcat_misc)[mb],
  base_dag_both_DS   = num(nar$dag_both_DS)[mb],
  base_dag_size      = num(nar$dag_size)[mb],
  stringsAsFactors = FALSE
)
media <- add_codes(media, d$responseid)
write_checked(media, "andre2025_macronarratives_media")

# ---- Respondent overlap across experiments (hashed prolific_pid) ------------
pids <- list(attention = rd("attention_experiment.dta")$prolific_pid,
             provision = rd("dag_wave2.dta")$prolific_pid,
             mp = rd("mp_dag_wave2.dta")$prolific_pid,
             media = rd("media_wave3.dta")$prolific_pid)
cat("\nOverlap of prolific_pid between experiment text files:\n")
for (i in 1:3) for (j in (i + 1):4)
  cat(names(pids)[i], "x", names(pids)[j], ":",
      length(intersect(na.omit(pids[[i]]), na.omit(pids[[j]]))), "\n")
