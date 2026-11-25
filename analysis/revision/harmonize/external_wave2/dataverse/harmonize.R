# Blind harmonizer: never prints text values. Usage: LANG=en_US.UTF-8 Rscript harmonize.R <card_id>
suppressPackageStartupMessages({ library(haven); library(readr); library(dplyr) })
E <- "data/external"  # path changed on recovery (2026-11-17): inputs, relative to the repository root
OUT <- Sys.getenv("HARMONIZE_OUT", E)  # path changed on recovery (2026-11-17): output root (default: as originally)
fix_utf8 <- function(x) {
  x <- as.character(x); bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) { c1 <- iconv(x[bad], "latin1", "UTF-8"); moj <- grepl("\u00c3|\u00c2", c1)
    c1[moj] <- iconv(x[bad][moj], "UTF-8", "UTF-8", sub = ""); x[bad] <- c1 }
  # undo double encoding (UTF-8 bytes read as latin1), only where the round trip gives valid UTF-8
  m <- !is.na(x) & grepl("\u00c3|\u00c2", x)
  if (any(m)) { z <- iconv(x[m], "UTF-8", "latin1"); ok <- !is.na(z) & validUTF8(z); Encoding(z) <- "UTF-8"; x[m][ok] <- z[ok]
    cat(sprintf("  [repaired double-encoded cells: %d of %d flagged]\n", sum(ok), sum(m))) }
  x }
clean_text <- function(x) { x <- fix_utf8(x); x <- gsub("[\r\n\t]+", " ", x); x <- trimws(x); x[is.na(x)] <- ""; x }
finish <- function(df, ds, card) {
  df$text <- clean_text(df$text)
  df <- df[nzchar(df$text), ]
  stopifnot(!anyDuplicated(df$id))
  df$dataset <- card
  core <- c("id", "dataset", "country", "lang", "text", "treatment", "exclude")
  df <- df[, c(core, sort(grep("^human_", names(df), value = TRUE)))]
  out <- file.path(OUT, ds, paste0("harmonized_", card, ".csv"))  # path changed on recovery (2026-11-17)
  write_csv(df, out, na = "")
  n_esc <- sum(grepl("<U+", readLines(out, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
  cat(sprintf("%s: rows=%d cols=%s exclude=%d <U+ lines=%d\n", basename(out), nrow(df), paste(names(df), collapse = ","), sum(df$exclude), n_esc))
  print(table(treatment = df$treatment, useNA = "ifany"))
}
card <- commandArgs(TRUE)[1]
if (card == "john2022_citizenvoice") {
  d <- read_dta(file.path(E, card, "raw/Deltapoll_PeterJohn220114-final2.dta"))
  df <- data.frame(id = as.character(d$id), country = "GB", lang = "en", text = d$PJ7_1,
                   treatment = as.integer(as.numeric(d$Scenario) %in% c(1, 2, 3, 6)), exclude = FALSE)
  finish(df, card, card)
}
if (card == "sambanis2025_externalthreat") {
  d <- read_dta(file.path(E, card, "raw/AJPS_Main_Study2.dta"))
  txt <- ifelse(as.numeric(d$threat) == 2, d$china_answer, d$control_answer)
  df <- data.frame(id = as.character(d$responseid), country = "US", lang = "en", text = txt,
                   treatment = as.integer(as.numeric(d$threat) == 2), exclude = FALSE)
  finish(df, card, card)
}
if (card == "davies2013_audiencecosts") {
  d <- read_sav(file.path(E, card, "raw/ISQ replication data.sav"))
  d <- d[!is.na(d$SPEC_W2_E1action), ]
  arm <- c("control", "threat", "escalated_threat", "action_no_casualties", "action_casualties")[as.numeric(d$SPEC_W2_E1action)]
  df <- data.frame(id = as.character(d$id), country = "GB", lang = "en", text = d$SPEC_W2_E1q2, treatment = arm, exclude = FALSE,
                   human_basic_code = as.character(as_factor(d$SPEC_W2_E1q2coded)),
                   human_detail_code = as.character(as_factor(d$SPEC_W2_E1morecodes)))
  finish(df, card, card)
}
if (card == "marx2025_genderlanguage") {
  d <- read_dta(file.path(E, card, "raw/online_survey_data.dta"))
  arm <- c("control", "passive_gnl", "active_gnl", "technical")[as.numeric(d$treatment)]
  df <- data.frame(id = as.character(d$lfdn), country = "DE", lang = "de", text = d$response, treatment = arm, exclude = FALSE)
  finish(df, card, card)
}
if (card %in% c("bursztyn2023_dissent_exp2", "bursztyn2023_dissent_exp4")) {
  f <- if (card == "bursztyn2023_dissent_exp2") "defund" else "deport"
  d <- read_dta(file.path(E, "bursztyn2023_dissent/raw/replication folder/data/raw", paste0(f, "-receiver-deidentified.dta")))
  cond <- as.character(d$condition)
  df <- data.frame(id = paste0(card, "_", seq_len(nrow(d))), country = "US", lang = "en", text = d$openended,
                   treatment = ifelse(cond == "excuse", 1L, ifelse(cond == "noexcuse", 0L, NA_integer_)),
                   exclude = !(cond %in% c("excuse", "noexcuse")))
  finish(df, "bursztyn2023_dissent", card)
}
if (card == "bursztyn2023_dissent_aux5") {
  d <- read_dta(file.path(E, "bursztyn2023_dissent/raw/replication folder/data/raw/defund-openended-deidentified.dta"))
  hc <- tolower(trimws(as.character(d$hand_coding)))
  hc <- gsub("persasion", "persuasion", hc); hc <- gsub("rationale,persuasion", "rationale, persuasion", hc, fixed = TRUE)
  df <- data.frame(id = paste0(card, "_", seq_len(nrow(d))), country = "US", lang = "en", text = d$open_ended,
                   treatment = as.integer(as.character(d$condition) == "excuse"), exclude = FALSE, human_hand_coding = hc)
  finish(df, "bursztyn2023_dissent", card)
}
if (card == "lupu2024_lawsofwar") {
  parts <- lapply(c(France = "FR", UK = "GB", USA = "US"), function(cc) NULL)
  out <- list()
  for (cn in c("France", "UK", "USA")) {
    d <- read_dta(file.path(E, card, "raw", paste0(cn, ".dta")))
    cc <- c(France = "FR", UK = "GB", USA = "US")[[cn]]
    out[[cn]] <- data.frame(id = paste0(cc, "_", as.character(d$ResponseID)), country = cc, lang = ifelse(cc == "FR", "fr", "en"),
                            text = clean_text(d$app_reb_open), treatment = as.integer(as.numeric(d$reblaw) == 1), exclude = FALSE)
  }
  finish(do.call(rbind, out), card, card)
}
if (card == "tripp2026_refugees") {
  d <- read_dta(file.path(E, card, "raw/Tripp Kam Bovitz open ends.dta"))
  d$text <- clean_text(d$text)
  d$mention <- ifelse(d$mention_1 == 1, 1, ifelse(d$mention_2 == 1, 2, 3))
  themes <- c("Definition","Cooperative","Uncooperative","Competent","Incompetent","Threatening","Improving","Push","Pull",
              "No_choice","Choice","In_need","Humanity","Politics","Personal_ties","Crime_ambiguous","Extreme","Sympathetic","Hostile")
  d <- d[order(d$respondent, d$mention), ]
  agg <- d %>% group_by(respondent) %>% summarise(
    text = paste(text[nzchar(text)], collapse = " | "), treatment = as.integer(first(refugeecondition)),
    across(all_of(paste0(themes, "_any")), ~ if (all(is.na(.x))) NA_integer_ else as.integer(max(.x, na.rm = TRUE))),
    human_uncodable_any = if (all(is.na(Uncodable_any_u))) NA_integer_ else as.integer(max(Uncodable_any_u, na.rm = TRUE)), .groups = "drop")
  names(agg) <- sub("^(.*)_any$", "human_\\1_any", names(agg)); names(agg) <- sub("^human_human_", "human_", names(agg))
  agg$id <- as.character(agg$respondent); agg$country <- "US"; agg$lang <- "en"; agg$exclude <- FALSE
  finish(as.data.frame(agg), card, card)
}
if (card == "marsh2025_lifestyleconjoint") {
  d <- read.csv(file.path(E, "marsh2025_wicked/raw/CES21_pilot1.csv"), check.names = FALSE, encoding = "UTF-8")
  stance <- function(s) grepl("pipeline", s, fixed = TRUE)
  out <- list()
  for (k in 1:5) {
    a <- d[[paste0("traits", k, "a")]]; b <- d[[paste0("traits", k, "b")]]
    out[[k]] <- data.frame(id = paste0(d$ResponseId, "_t", k), country = "US", lang = "en", text = d[[paste0("wevi", k, "_why")]],
                           treatment = as.integer(stance(a) | stance(b)), exclude = !(nzchar(a) & nzchar(b)))
  }
  finish(do.call(rbind, out), "marsh2025_wicked", card)
}
