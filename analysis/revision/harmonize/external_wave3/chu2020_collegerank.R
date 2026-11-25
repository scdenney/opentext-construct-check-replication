# ==============================================================================
# chu2020_collegerank.R -- harmonized file for card chu2020_collegerank
# ==============================================================================
# Chu, J. "Educational Rankings and Adolescent Perceptions of College Cost."
# TESS study chu1236 (TESS 068), NORC AmeriSpeak Teen Panel, US teens 13-17,
# web, English only, N = 800, fielded Nov 2020 - Feb 2021. OSF https://osf.io/yc2qb/.
#
# Design (questionnaire raw/8041.0068_TESS_Teen_1_v7clean_-_FINAL.docx;
# proposal raw/Chu1236.pdf; description raw/TESS-1236.R1_description.docx):
# two paired college profiles (task 1: College A vs B; task 2: College C vs D),
# six attributes per profile. Ranking (P_RANK68*) is drawn uniformly from five
# levels; the five "control" attributes (ownership, size, graduation rate,
# location, student-faculty ratio) from the real-world joint distribution
# (description, Programming Note), independently of ranking. On each pair:
# T68_1 size check (its stem states, for task 1 only, how A's ranking compares
# with B's), tuition and aid sliders for each college, preference T68_6A/6C
# (6-point, response order reversed when RND_01 = 1), then the open item
# "Please explain how you came to this choice in 1-2 sentences."
# This file holds the task-1 item T68_7A (the item the proposal specifies,
# p. 5 n. 6 and Appendix A item 8: "will only appear once, in first set of
# profiles"); the fielded survey also asked it after task 2 (T68_7C, not used).
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/chu2020_collegerank.R
# Input:  data/external/chu2020_collegerank/raw/8041.068_TESS_068_CHU.sav
# Output: data/external/chu2020_collegerank/harmonized_chu2020_collegerank.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_dir <- "data/external/chu2020_collegerank/raw"
out_csv <- "data/external/chu2020_collegerank/harmonized_chu2020_collegerank.csv"
card_id <- "chu2020_collegerank"

d <- read_sav(file.path(raw_dir, "8041.068_TESS_068_CHU.sav"))
stopifnot(nrow(d) == 800, !anyDuplicated(d$CaseId))
lab <- function(x) as.character(as_factor(x, levels = "labels"))

# Text. The column carries value labels 77/98/99 (DON'T KNOW / SKIPPED ON WEB /
# REFUSED, questionnaire "Please code refusals in CAWI: 98"); an answer equal to
# one of these codes is a skip, not text, and is set to empty.
txt <- as.character(zap_labels(d$T68_7A))
Encoding(txt) <- "UTF-8"
txt <- trimws(ifelse(is.na(txt), "", txt))
# Line breaks typed as CRLF (13 answers) are normalized to LF; read.csv drops
# the CR, so the file would otherwise not re-read identically.
txt <- gsub("\r\n?", "\n", txt)
stopifnot(all(validUTF8(txt)))
is_code <- txt %in% c("77", "98", "99")
cat("Text equal to a missing-value code (set empty):", sum(is_code), "\n")
txt[is_code] <- ""

# English only (project report p. 2; SURV_LANG all English).
stopifnot(all(as.numeric(d$SURV_LANG) == as.numeric(d$SURV_LANG[1])))

# ---- Task-1 profiles: chosen vs other -----------------------------------------
attrs <- c(rank = "RANK", ownership = "OWN", size = "SIZE", grad_rate = "GRAD",
           location = "LOC", student_faculty = "TEACH")
ca <- sapply(attrs, function(a) lab(d[[paste0("P_", a, "68A")]]))
cb <- sapply(attrs, function(a) lab(d[[paste0("P_", a, "68B")]]))
stopifnot(!anyNA(ca), !anyNA(cb))
# T68_6A: 1-3 prefer College A (much / prefer / slightly), 4-6 prefer College B
# (slightly / prefer / much); 77/98/99 missing (value labels).
p <- as.numeric(d$T68_6A)
choice <- ifelse(p %in% 1:3, "A", ifelse(p %in% 4:6, "B", NA))
strength <- c(3, 2, 1, 1, 2, 3)[ifelse(p %in% 1:6, p, NA)]   # 3 = much prefer
chosen <- ca; other <- cb
sw <- which(choice == "B")
chosen[sw, ] <- cb[sw, ]; other[sw, ] <- ca[sw, ]
chosen[is.na(choice), ] <- NA; other[is.na(choice), ] <- NA
colnames(chosen) <- paste0("chosen_", names(attrs))
colnames(other)  <- paste0("other_",  names(attrs))

# Slider predictions in USD (value labels "$0 Free" ... "$100,000 and more"),
# T68_2A/T68_4A tuition for A/B, T68_3A/T68_5A aid for A/B; 777777 DON'T KNOW,
# 999998 SKIPPED ON WEB, 999999 REFUSED set missing (value labels).
num <- function(x) { x <- as.numeric(x); x[x > 100000] <- NA; x }
tA <- num(d$T68_2A); tB <- num(d$T68_4A); aA <- num(d$T68_3A); aB <- num(d$T68_5A)
pick <- function(xa, xb, ch, own) ifelse(is.na(ch), NA, ifelse((ch == "A") == own, xa, xb))

# ---- Exclusions ----------------------------------------------------------------
# TESS file is the fielded sample (800 interviews, project report p. 2); no
# exclusions are stated in the proposal. Only rows whose preference T68_6A was
# skipped are excluded: "this choice" has no referent and chosen/other are
# undefined (11 rows). Ranking assignment is randomized for them.
exclude <- is.na(choice)
cat("Excluded (preference skipped):", sum(exclude), "\n")

rank_differs <- as.integer(ca[, "rank"] != cb[, "rank"])

h <- data.frame(
  id        = as.integer(d$CaseId),      # AmeriSpeak CaseId
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = exclude,
  treatment = rank_differs,              # 1 = A and B ranking levels differ (primary contrast)
  task      = 1L,
  chosen_college = choice,               # A (left) or B (right)
  preference_strength = strength,
  chosen, other,
  tuition_chosen = pick(tA, tB, choice, TRUE), tuition_other = pick(tA, tB, choice, FALSE),
  aid_chosen     = pick(aA, aB, choice, TRUE), aid_other     = pick(aA, aB, choice, FALSE),
  response_order_reversed = as.numeric(d$RND_01),
  parent_educ = lab(d$PEDU),             # subgroup factor in the proposal (first-gen)
  hh_income   = lab(d$INCOME),           # subgroup factor in the proposal (low income)
  age         = as.numeric(d$AGE),
  weight      = as.numeric(d$WEIGHT),    # not used
  stringsAsFactors = FALSE, check.names = FALSE
)

cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
h <- h[h$text != "", ]
write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks --------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
k <- h[!h$exclude, ]
cat("Non-excluded rows, ranking differs (1) vs same (0):\n"); print(table(k$treatment))
stopifnot(identical(k$treatment, as.integer(k$chosen_rank != k$other_rank)))
t25 <- as.integer(k$chosen_rank == "Top 25" | k$other_rank == "Top 25")
cat("Non-excluded rows, a Top 25 college in the pair (1) or not (0):\n"); print(table(t25))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
