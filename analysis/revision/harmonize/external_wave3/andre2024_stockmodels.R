# ==============================================================================
# andre2024_stockmodels.R -- harmonized files for the andre2024_stockmodels_* cards
# ==============================================================================
# Andre, Schirmer & Wohlfart (2026), "Mental Models of the Stock Market",
# QJE 141(4): 2925-; replication package Harvard Dataverse doi:10.7910/DVN/BNFSAO
# (package zip mmsm_replication_package_20260713_105310). One builder, six cards
# (the human-coded detection study, detection_raw.sav, is not carded: its
# targets, explicit mispricing or risk reasoning, are near zero in households):
#
#   card id                               raw file(s)                         paper
#   andre2024_stockmodels_pricecosts      prices_costs_raw.sav                V.B-V.C, App. D.3.3
#   andre2024_stockmodels_memory          memory_w1_c1-4 + memory_w2_c1-4     V.D.2.i, App. D.3.7
#   andre2024_stockmodels_arbitrage       explain_eq_1_raw.sav                V.D.2.ii, App. D.3.8
#   andre2024_stockmodels_attention       attention_raw.sav                   V.A, App. D.3.1
#   andre2024_stockmodels_context         vary_price_info_1-3_raw.sav         V.D.1.i, App. D.3.4
#   andre2024_stockmodels_arbitrage_w2    explain_eq_1 + explain_eq_2_raw     V.D.2.ii fn. 14, App. D.3.8
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/andre2024_stockmodels.R
#
# Inputs: data/external/andre2024_stockmodels/raw/ (package files, unchanged).
# The raw Qualtrics .sav exports are used, not the package's out/data_out
# wide.dta / wide.rds (derived files; the .dta is a Stata export). Every
# exclusion below copies the authors' cleaning script
# raw/code/cleaning/clean_wide.R (line numbers cited); their analysis scripts
# (raw/code/analysis/...) apply no further sample filter to these surveys.
#
# Outputs: data/external/andre2024_stockmodels/harmonized_<card id>.csv
# Blindness: no text value is printed; only counts. R warnings go to stderr,
# which the caller should send to a file.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw <- "data/external/andre2024_stockmodels/raw"
out_dir <- "data/external/andre2024_stockmodels"
exp_dir <- file.path(raw, "data", "experiments")

# ---- helpers -----------------------------------------------------------------

rd <- function(f, cols) {
  d <- read_sav(file.path(exp_dir, f), col_select = tidyselect::any_of(cols))
  d <- as.data.frame(zap_labels(d), stringsAsFactors = FALSE)
  for (v in names(d)) if (is.character(d[[v]])) Encoding(d[[v]]) <- "UTF-8"
  d
}
blank <- function(x) is.na(x) | x %in% c("", " ")
# Authors' first-submission rule: !duplicated(PROLIFIC_PID) on the rows that
# survived the earlier filters (clean_wide.R applies the filters in sequence).
first_of <- function(pid, keep) {
  out <- keep
  idx <- which(keep)
  out[idx] <- !duplicated(pid[idx])
  out
}
# Authors' recode of the directional forecast (clean_wide.R l. 1160-1165):
# return_1 is 1 = "higher in scenario 1" (neutral news), 2 = "similar",
# 3 = "higher in scenario 2" (the news scenario); news_congruent = 3 means a
# higher return after good news or a lower return after bad news.
congruent <- function(return_1, good) ifelse(is.na(return_1), NA_integer_,
  as.integer(good * return_1 + (1 - good) * (4 - return_1)))

finish <- function(d, card_id, country, design_cols, human_cols = character(0)) {
  d$text <- trimws(d$text)
  # Keep empty text out. An answer that is exactly the string "NA" is treated
  # as empty: it carries no content, and read.csv() would turn it into a
  # missing value, so the file could not re-read to identical text.
  n_na <- sum(d$text %in% "NA")
  d <- d[!is.na(d$text) & d$text != "" & d$text != "NA", , drop = FALSE]
  d$dataset <- card_id
  d$country <- country
  d$lang <- "en"
  d$exclude <- d$exclude | is.na(d$treatment)
  out <- d[, c("id", "dataset", "country", "lang", "text", "exclude",
               "treatment", design_cols, human_cols)]
  stopifnot(!anyDuplicated(out$id), all(validUTF8(out$text)))
  f <- file.path(out_dir, paste0("harmonized_", card_id, ".csv"))
  write.csv(out, f, row.names = FALSE, fileEncoding = "UTF-8")
  # Checks (counts only)
  cat("\n==", card_id, "== rows:", nrow(out), " excluded:", sum(out$exclude),
      " dropped literal 'NA' answers:", n_na, "\n")
  cat("non-excluded rows per arm:\n")
  print(table(out$treatment[!out$exclude], useNA = "ifany"))
  back <- read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
  cat("re-read text identical:", identical(back$text, out$text), "\n")
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  cat("'<U+' escapes in file:", sum(lengths(regmatches(lines, gregexpr("<U\\+", lines)))), "\n")
  stopifnot(identical(back$text, out$text))
  invisible(out)
}

std_cols <- c("ResponseId", "PROLIFIC_PID", "treatment", "condition",
              "explanation", "return_1", "guess_hypothesis", "risktaking")

# ---- 1. pricecosts: Interpreting Past Price Changes Study (UK, Jan 2026) -----
# Filters (clean_wide.R l. 857-861): non-empty PROLIFIC_PID; non-missing
# risktaking (respondent completed the survey); first submission per PID.
# Arms: `treatment` control vs baseline_w_price, crossed with `frame`
# (clean_wide.R l. 1107: framing = frame). The authors' contrasts
# (code/analysis/side_calculations_paper/price_info_side_calc.R l. 43, 101, 127):
# price info = baseline_w_price & standard vs control; purchase_sum and
# number_of_shares vs baseline_w_price & standard. Prolific UK sample.
d <- rd("prices_costs_raw.sav", c(std_cols, "frame"))
k <- !blank(d$PROLIFIC_PID)
k <- k & !is.na(d$risktaking)
k <- first_of(d$PROLIFIC_PID, k)
arm <- rep(NA_character_, nrow(d))
arm[d$treatment == "control"] <- "control"
arm[d$treatment == "baseline_w_price" & d$frame == "standard"] <- "price_info"
arm[d$treatment == "baseline_w_price" & d$frame == "number_of_shares"] <- "number_of_shares"
arm[d$treatment == "baseline_w_price" & d$frame == "purchase_sum"] <- "purchase_cost"
pc <- data.frame(id = d$ResponseId, text = d$explanation, exclude = !k,
                 treatment = arm,
                 news_congruent = congruent(d$return_1, 1), # all Nike good news
                 stringsAsFactors = FALSE)
pc_out <- finish(pc, "andre2024_stockmodels_pricecosts", "GB", "news_congruent")
pc_pid <- d$PROLIFIC_PID[match(pc_out$id[!pc_out$exclude], d$ResponseId)]

# ---- 2. memory: Memory Study, outcome wave (wave 2, US, Apr-May 2025) --------
# Wave 1 (treatment wave) filters, clean_wide.R l. 566-577: non-empty PID,
# non-empty treatment, non-missing risktaking and !duplicated(PID), all inside
# one filter() call, so duplicated() runs on the full bound file; then the
# release duplicate flag is dropped (drop_release_duplicates, l. 43-85).
w1 <- do.call(rbind, lapply(sprintf("memory/memory_w1_c%d.sav", 1:4), function(f)
  rd(f, c("ResponseId", "PROLIFIC_PID", "treatment", "risktaking",
          "release_duplicate_flag"))))
k1 <- !blank(w1$PROLIFIC_PID) & !blank(w1$treatment) & !is.na(w1$risktaking) &
  !duplicated(w1$PROLIFIC_PID)
k1 <- k1 & w1$release_duplicate_flag %in% 0
w1 <- w1[k1, ]
stopifnot(!anyDuplicated(w1$PROLIFIC_PID))
# Wave 2 filters, clean_wide.R l. 604-620 and l. 740: non-empty PID; non-empty
# explanation (untrimmed comparison, as in the authors' code); first submission
# per PID in bind order c1..c4; release duplicate flag; PID in cleaned wave 1.
w2 <- do.call(rbind, lapply(1:4, function(i) {
  x <- rd(sprintf("memory/memory_w2_c%d.sav", i),
          c("ResponseId", "PROLIFIC_PID", "treatment", "condition", "explanation",
            "return_1", "release_duplicate_flag"))
  x$w2_collection <- paste0("w2_c", i)
  x
}))
k <- !blank(w2$PROLIFIC_PID)
k <- k & !(is.na(w2$explanation) | w2$explanation == "")
k <- first_of(w2$PROLIFIC_PID, k)
k <- k & w2$release_duplicate_flag %in% 0
k <- k & w2$PROLIFIC_PID %in% w1$PROLIFIC_PID
t1 <- w1$treatment[match(w2$PROLIFIC_PID, w1$PROLIFIC_PID)]
mem <- data.frame(
  id = w2$ResponseId, text = w2$explanation, exclude = !k,
  # treatment: wave-1 memory arm (code/analysis/exhibits_paper/_helpers/
  # tabD3_models.R l. 108: over_events = treatment_w1 == "overreaction_events")
  treatment = ifelse(t1 == "overreaction_events", 1L,
                     ifelse(t1 == "control_events", 0L, NA_integer_)),
  # wave-2 context, cross-randomized (tabD3_models.R l. 109)
  context_w2 = ifelse(w2$treatment == "price_10_framing_mispricing", "price_info_overreaction",
                      ifelse(w2$treatment == "control", "no_price_info", NA_character_)),
  w2_collection = w2$w2_collection,
  news_congruent = congruent(w2$return_1, 0),  # all Nike bad news
  stringsAsFactors = FALSE)
mem_out <- finish(mem, "andre2024_stockmodels_memory", "US", c("context_w2", "w2_collection", "news_congruent"))
cat("memory: non-excluded rows by context_w2 x treatment:\n")
print(table(mem_out$context_w2[!mem_out$exclude], mem_out$treatment[!mem_out$exclude], useNA = "ifany"))
mem_pid <- w2$PROLIFIC_PID[match(mem_out$id[!mem_out$exclude], w2$ResponseId)]

# ---- shared filter for the 2023 studies --------------------------------------
# attention, explain_eq_1, detection (clean_wide.R l. 288-339): non-empty PID;
# first submission per PID; non-empty guess_hypothesis (the end-of-survey item,
# i.e. respondent completed the survey).
filt_2023 <- function(d) {
  k <- !d$PROLIFIC_PID %in% c("", NA)
  k <- first_of(d$PROLIFIC_PID, k)
  k & !d$guess_hypothesis %in% c("", NA)
}

# ---- 3. arbitrage: Knowledge of arbitrage, main study wave 1 (US, Jun 2023) --
# Arms: explain_eq (arbitrage explanation) vs explain_tides (placebo text);
# tabD3_models.R l. 102.
d <- rd("explain_eq_1_raw.sav", std_cols)
k <- filt_2023(d)
arb <- data.frame(id = d$ResponseId, text = d$explanation, exclude = !k,
                  treatment = ifelse(d$treatment == "explain_eq", 1L,
                                     ifelse(d$treatment == "explain_tides", 0L, NA_integer_)),
                  news_congruent = congruent(d$return_1, 1),  # Nike good news
                  stringsAsFactors = FALSE)
arb_out <- finish(arb, "andre2024_stockmodels_arbitrage", "US", "news_congruent")
arb_pid <- d$PROLIFIC_PID[match(arb_out$id[!arb_out$exclude], d$ResponseId)]

# ---- 4. attention: Inferring price responses, main study (US, Jun 2023) ------
# Arms (tabD9_panelA.R l. 25-26): control, attention_trader (traders' reaction
# question), attention_trader_prices (traders' and price reaction questions).
d <- rd("attention_raw.sav", std_cols)
k <- filt_2023(d)
lab <- c(control = "control", attention_trader = "trader_reaction",
         attention_trader_prices = "price_reaction")
att <- data.frame(id = d$ResponseId, text = d$explanation, exclude = !k,
                  treatment = unname(lab[d$treatment]),
                  news_congruent = congruent(d$return_1, 1),
                  stringsAsFactors = FALSE)
att_out <- finish(att, "andre2024_stockmodels_attention", "US", "news_congruent")
att_pid <- d$PROLIFIC_PID[match(att_out$id[!att_out$exclude], d$ResponseId)]

# ---- 5. context: Varying the price information (US, Mar-Apr 2025) -----------
# Three Qualtrics exports of one survey (clean_wide.R l. 536-547): bind;
# non-empty PID; non-empty treatment; non-missing risktaking; first submission
# per PID in bind order. good/bad news (`condition`) is cross-randomized.
d <- do.call(rbind, lapply(1:3, function(i) {
  x <- rd(sprintf("vary_price_info_%d_raw.sav", i), std_cols)
  x$collection <- paste0("c", i)
  x
}))
k <- !blank(d$PROLIFIC_PID)
k <- k & !blank(d$treatment)
k <- k & !is.na(d$risktaking)
k <- first_of(d$PROLIFIC_PID, k)
lab <- c(control = "control",
         price_nonnumeric_framing_neutral = "price_qualitative",
         price_10_framing_neutral = "price10_neutral",
         price_30_framing_neutral = "price30_neutral",
         price_10_framing_mispricing = "price10_overreaction",
         price_10_framing_efficient = "price10_efficient")
good <- ifelse(d$condition == "nike_good", 1L, ifelse(d$condition == "nike_bad", 0L, NA_integer_))
ctx <- data.frame(id = d$ResponseId, text = d$explanation, exclude = !k,
                  treatment = unname(lab[d$treatment]),
                  good_news = good, collection = d$collection,
                  news_congruent = congruent(d$return_1, good),
                  stringsAsFactors = FALSE)
ctx_out <- finish(ctx, "andre2024_stockmodels_context", "US", c("good_news", "collection", "news_congruent"))
cat("context: non-excluded rows by arm x good_news:\n")
print(table(ctx_out$treatment[!ctx_out$exclude], ctx_out$good_news[!ctx_out$exclude], useNA = "ifany"))
ctx_pid <- d$PROLIFIC_PID[match(ctx_out$id[!ctx_out$exclude], d$ResponseId)]

# ---- 6. arbitrage_w2: Knowledge of arbitrage, follow-up wave (US, Jun 2023) -
# Wave 2, one to three days after wave 1, Amazon good news scenario, no new
# treatment. Filters (clean_wide.R l. 304-312): the 2023 filter above, then
# keep only PIDs retained in the cleaned wave 1; wave-1 treatment merged by
# PID (l. 314-329).
w1 <- rd("explain_eq_1_raw.sav", std_cols)
w1 <- w1[filt_2023(w1), ]
d <- rd("explain_eq_2_raw.sav", std_cols)
k <- filt_2023(d) & d$PROLIFIC_PID %in% w1$PROLIFIC_PID
t1 <- w1$treatment[match(d$PROLIFIC_PID, w1$PROLIFIC_PID)]
ar2 <- data.frame(id = d$ResponseId, text = d$explanation, exclude = !k,
                  treatment = ifelse(t1 == "explain_eq", 1L,
                                     ifelse(t1 == "explain_tides", 0L, NA_integer_)),
                  news_congruent = congruent(d$return_1, 1),  # Amazon good news
                  stringsAsFactors = FALSE)
ar2_out <- finish(ar2, "andre2024_stockmodels_arbitrage_w2", "US", "news_congruent")
ar2_pid <- d$PROLIFIC_PID[match(ar2_out$id[!ar2_out$exclude], d$ResponseId)]

# ---- respondent overlap across cards (pseudonymized release PIDs) ------------
pids <- list(pricecosts = pc_pid, memory = mem_pid, arbitrage = arb_pid,
             attention = att_pid, context = ctx_pid, arbitrage_w2 = ar2_pid)
cat("\nrespondents shared between cards (non-excluded rows):\n")
ov <- outer(names(pids), names(pids), Vectorize(function(a, b)
  length(intersect(pids[[a]], pids[[b]]))))
dimnames(ov) <- list(names(pids), names(pids))
print(ov)
