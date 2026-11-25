# ==============================================================================
# pogacar2023_inactioninertia.R -- harmonized file for card
# pogacar2023_inactioninertia
# ==============================================================================
# Pogacar, Brigden, Plant, Kardes & Kellaris (2023), "The reference dependence
# roots of inaction inertia: A query theory account", PLOS ONE 18(3), e0282876
# (CC BY 4.0). Data: OSF https://osf.io/rjm58/ (the article's data
# availability statement points there).
# 831 students (public Midwestern university, course credit; article section 5)
# randomized to a big missed deal ($40), a small missed deal ($80) or no missed
# deal (control), each then offered the special pass for $90 (regular $100),
# and asked to list, one at a time (up to five), their thoughts about getting
# the pass versus not getting it (materials.pdf, "Pass Thought Listing").
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/pogacar2023_inactioninertia.R
#
# Inputs (data/external/pogacar2023_inactioninertia/raw/, unchanged OSF files):
#   data.xlsx  sheets FocusOrder (text, Cond, Writing, Choice, self-codes,
#              Focus, Order), GainLoss (same respondents, different row order;
#              coders' reference-point and gain/loss codes, Lang), Notes (Cond key)
#
# Unit: one row per respondent. The file already holds one row per respondent
# with up to five thought cells (PassThought1_TEXT..PassThought5_TEXT); the
# non-empty cells are joined in listing order with " | ", so that the unit of
# randomization and of analysis coincide. Per-thought human codes are kept as
# " | "-joined strings aligned with the joined thoughts, plus respondent-level
# summaries.
#
# Output: data/external/pogacar2023_inactioninertia/harmonized_pogacar2023_inactioninertia.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(readxl))

raw_dir <- "data/external/pogacar2023_inactioninertia/raw"
card_id <- "pogacar2023_inactioninertia"
out_csv <- file.path("data/external/pogacar2023_inactioninertia",
                     paste0("harmonized_", card_id, ".csv"))
f <- file.path(raw_dir, "data.xlsx")

# All columns read as text so that nothing is coerced; numeric recodes below.
rd <- function(sheet) as.data.frame(suppressWarnings(
  read_excel(f, sheet = sheet, col_types = "text")))
fo <- rd("FocusOrder")
gl <- rd("GainLoss")
stopifnot(nrow(fo) == 831, nrow(gl) == 831,
          !anyDuplicated(fo$ResponseID), setequal(fo$ResponseID, gl$ResponseID))
gl <- gl[match(fo$ResponseID, gl$ResponseID), ]

tcols <- paste0("PassThought", 1:5, "_TEXT")
ccols <- paste0("PassCode", 1:5)

# The two sheets must agree on assignment and text (Diff in GainLoss = Cond).
stopifnot(identical(fo$Cond, gl$Diff), identical(fo$Writing, gl$Writing))
for (v in c(tcols, ccols)) stopifnot(identical(fo[[v]], gl[[v]]))

# ---- 1. Treatment ------------------------------------------------------------
# Cond key (data.xlsx, sheet Notes): -1 = Big missed deal, 0 = Control,
# 1 = Small missed deal. Randomized between subjects (article section 5.1).
arm <- c("-1" = "big_missed", "0" = "control", "1" = "small_missed")[fo$Cond]
stopifnot(!anyNA(arm))

# ---- 2. Text: join the non-empty thought cells per respondent ----------------
th <- as.matrix(fo[tcols])
th[is.na(th)] <- ""
th <- trimws(th)
Encoding(th) <- "UTF-8"
stopifnot(all(validUTF8(th)))
has <- th != ""
# Cells are filled from the left (a respondent stops by choosing "finished"):
# no empty cell is followed by a non-empty one.
stopifnot(all(apply(has, 1, function(z) all(diff(as.integer(z)) <= 0))))
cat("Thought cells containing '|' (would blur the separator):",
    sum(grepl("|", th, fixed = TRUE)), "\n")
text <- apply(th, 1, function(z) paste(z[z != ""], collapse = " | "))
n_thoughts <- rowSums(has)

# ---- 3. Human codes ------------------------------------------------------------
# (a) Respondent self-codes (PassCode1..5): after listing, each respondent coded
# each own thought as a reason to get the special pass (+1), not to get it (-1)
# or neither (0) (article section 5.2; materials.pdf "Pass Coding"). Not blind
# to condition (respondents coded their own thoughts). Authors' thought focus =
# mean of these codes (column Focus); thought order = SMRD (column Order).
sc <- as.matrix(fo[ccols])
cat("Self-code present where thought empty:", sum(!has & !is.na(sc)),
    "| self-code missing where thought present:", sum(has & is.na(sc)), "\n")
joincodes <- function(m) vapply(seq_len(nrow(m)), function(i)
  paste(ifelse(is.na(m[i, has[i, ]]), "NA", m[i, has[i, ]]), collapse = " | "),
  character(1))
scn <- suppressWarnings(matrix(as.numeric(sc), nrow(sc)))
scn[!has] <- NA

# (b) Coders' codes (GainLoss sheet; article sections 5.3-5.4): two independent
# coders blind to condition coded each thought for a numerical reference point
# ($40 / $80 / $100; only thoughts with a numeric value) and gain/loss frame
# (+1 gain / -1 loss; thoughts with no clear frame left blank). Disagreements
# were resolved by discussion; the GainLoss sheet holds one value per thought,
# taken here as the resolved code (the per-coder sheets are in coder.xlsx,
# sheet "Process Coding", not used here).
rp <- as.matrix(gl[paste("ref point", 1:5)])
gn <- as.matrix(gl[paste0("GainLoss", 1:5)])
cat("Coder codes present where thought empty: ref point", sum(!has & !is.na(rp)),
    "| gain/loss", sum(!has & !is.na(gn)), "\n")
gnn <- suppressWarnings(matrix(as.numeric(gn), nrow(gn)))
gnn[!has] <- NA

num <- function(x) suppressWarnings(as.numeric(x))

# ---- 4. Assemble ---------------------------------------------------------------
h <- data.frame(
  id        = fo$ResponseID,     # authors' Qualtrics ResponseID
  dataset   = card_id,
  country   = "US",              # public Midwestern university (University of
                                 # Cincinnati IRB, article section 5)
  lang      = "en",
  text      = trimws(text),
  # The authors analyze all 831 respondents (F(2, 828) in sections 6.1.1 and
  # 6.1.4); no exclusion.
  exclude   = FALSE,
  treatment = unname(arm),       # "big_missed" / "control" / "small_missed"
  # Writing (0/1): thought listing before vs after the likelihood judgment,
  # randomized independently (article section 5.2). The deposit does not say
  # which value is "before".
  writing   = as.integer(fo$Writing),
  # Closed-ended outcome: likelihood of buying the special pass, 1-11
  # (materials.pdf "Choice"; article section 6.1.1).
  choice    = as.integer(fo$Choice),
  n_thoughts = n_thoughts,
  # "Is English your native or primary language?" (materials.pdf); 0/1 as
  # deposited, coding not documented (presumably 1 = yes).
  lang_native = as.integer(gl$Lang),
  # Self-codes (respondent-coded, not blind).
  human_self_codes      = joincodes(sc),            # per thought, aligned with text
  human_self_n_against  = rowSums(scn == -1, na.rm = TRUE),
  human_self_n_for      = rowSums(scn == 1, na.rm = TRUE),
  human_self_any_against = as.integer(rowSums(scn == -1, na.rm = TRUE) > 0),
  human_self_focus      = num(fo$Focus),            # authors' H3a measure
  human_self_order      = num(fo$Order),            # authors' H3b measure
  # Coders' codes (blind to condition, resolved).
  human_coder_refpoint  = joincodes(rp),            # per thought; NA = not coded
  human_coder_refpoint_index = num(gl$IndexRefPoint),
  human_coder_gainloss  = joincodes(gn),            # per thought; NA = not coded
  human_coder_any_loss  = as.integer(rowSums(gnn == -1, na.rm = TRUE) > 0),
  human_coder_gainloss_index = num(gl$IndexGain),
  stringsAsFactors = FALSE
)
h <- h[h$text != "", ]  # keep rows with empty text out (none expected)

# ---- 5. Write and check --------------------------------------------------------
write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

cat("Rows:", nrow(h), "\n")
cat("Non-excluded rows with text per arm:\n")
print(table(h$treatment[!h$exclude]))
cat("Thoughts per respondent:\n"); print(table(h$n_thoughts))
r <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
raw_out <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(raw_out, gregexpr("<U+", raw_out, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
