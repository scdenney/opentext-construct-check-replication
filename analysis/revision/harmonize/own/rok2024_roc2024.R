# Harmonize ROK 24.1 election conjoint Q40 and Taiwan 2024 national-identity
# CBC #2 Q74. Blind: open text is copied, never printed; checks report counts.
# Run: LANG=en_US.UTF-8 Rscript rok2024_roc2024.R

source("analysis/revision/harmonize/own/common.R")  # path changed on recovery (2026-11-17)
suppressPackageStartupMessages(library(readr))

RES <- "/home/buddleman/Documents/github/research"
read_qualtrics <- function(path) {
  d <- read_csv(path, col_types = cols(.default = "c"), name_repair = "minimal",
                locale = locale(encoding = "UTF-8"), na = character(), trim_ws = FALSE, progress = FALSE)
  d <- as.data.frame(d[-c(1, 2), ], stringsAsFactors = FALSE)
  d
}
col1 <- function(d, nm) {               # a uniquely named column, by position
  i <- which(names(d) == nm)
  if (length(i) != 1) stop(nm, " occurs ", length(i), " times")
  d[[i]]
}
nz <- function(x) !is.na(x) & nchar(trimws(x)) > 0

# ---------------------------------------------------------------------------
# A. ROK 24.1 election conjoint, Q40 after task 6 of 10
# ---------------------------------------------------------------------------
pub_path <- file.path(RES, "replication/korean-election-conjoint/final_df_conjoint.csv")
raw_path <- file.path(RES, "projects/_archive/ROK-Experiments-2024/ROK 24.1 Experiments_March 16, 2024_12.56_choicetext.csv")

pub <- read.csv(pub_path, colClasses = "character", fileEncoding = "UTF-8", na.strings = character(0))
t6 <- pub[pub$question_profile %in% c("6.1", "6.2"), ]
p1 <- t6[t6$question_profile == "6.1", ]; p2 <- t6[t6$question_profile == "6.2", ]
p2 <- p2[match(p1$respondent_id, p2$respondent_id), ]
stopifnot(identical(p1$respondent_id, p2$respondent_id), nrow(p1) == 2005)

raw <- read_qualtrics(raw_path)
rw <- data.frame(rid = col1(raw, "ResponseId"), q39 = col1(raw, "Q39"),
                 q40 = col1(raw, "Q40"), rei = col1(raw, "RandomElectionIntro"),
                 stringsAsFactors = FALSE)
rw <- rw[match(p1$respondent_id, rw$rid), ]
stopifnot(!anyNA(rw$rid))

raw_choice <- c("후보 A" = "A", "후보 B" = "B")[rw$q39]
pub_choice <- ifelse(p2$chosen == "1", "B", ifelse(p2$chosen == "0", "A", NA))
recovered <- is.na(pub_choice)
text_pub <- p2$open_text_reason
cat("ROK: public matched =", sum(!recovered), "; recovered from raw =", sum(recovered), "\n")
cat("ROK: choice agrees (matched) =", sum(pub_choice[!recovered] == raw_choice[!recovered]),
    "; text identical (matched) =", sum(trimws(text_pub[!recovered]) == trimws(rw$q40[!recovered])), "\n")
stopifnot(all(pub_choice[!recovered] == raw_choice[!recovered]),
          all(trimws(text_pub[!recovered]) == trimws(rw$q40[!recovered])),
          !anyNA(raw_choice), all(rw$rei %in% c("1", "2")))

chosen_prof <- ifelse(recovered, raw_choice, pub_choice)
text <- rw$q40   # raw export text for all rows (matches public file up to outer whitespace)

attrs_rok <- c(age = "age_attribute", origin = "origin", gender = "gender_attribute",
               career = "career", scandal = "scandal", labor_policy = "labor_policy",
               housing_policy = "housing_policy", social_policy = "social_policy",
               foreign_policy = "foreign_policy", nuclear_policy = "nuclear_policy")
rok <- data.frame(id = p1$respondent_id, country = "KR", lang = "ko", text = text,
                  exclude = FALSE,
                  treatment = ifelse(rw$rei == "1", "general", "presidential"),
                  RandomElectionIntro = as.integer(rw$rei),
                  task = 6L, chosen_profile = chosen_prof,
                  stringsAsFactors = FALSE)
for (a in names(attrs_rok)) {
  v <- attrs_rok[[a]]
  rok[[paste0("chosen_", a)]] <- ifelse(chosen_prof == "A", p1[[v]], p2[[v]])
}
for (a in names(attrs_rok)) {
  v <- attrs_rok[[a]]
  rok[[paste0("other_", a)]] <- ifelse(chosen_prof == "A", p2[[v]], p1[[v]])
}
rok$recovered_from_raw <- recovered
stopifnot(!anyNA(rok[, grep("^(chosen|other)_", names(rok))]))
print(table(rok$treatment))

write_harmonized(rok, "rok2024_q40_election", manifest = list(
  source = paste0("replication/korean-election-conjoint/final_df_conjoint.csv (v2; task-6 rows 6.1/6.2: attributes, choice); ",
                  "Q40 text, RandomElectionIntro and the 22 unmatched task-6 choices from ",
                  "_archive/ROK-Experiments-2024/ROK 24.1 Experiments_March 16, 2024_12.56_choicetext.csv"),
  design = "Paired forced-choice candidate conjoint (10 tasks, 10 attributes) with respondent-level election-type intro (general vs presidential); open-end Q40 after task 6",
  lang = "ko",
  treatment_cols = "treatment / RandomElectionIntro (1 general, 2 presidential); task=6; chosen_profile; chosen_* and other_* (10 attributes, English labels)",
  human_codes = "none",
  routing = "API-eligible",
  card = "y",
  caveats = paste0("Public sample = 2,005 attention-check passers (Election-cbc.qmd: Q7 instructed response); exclude all FALSE. ",
                   "22 respondents with empty `chosen` in the public v2 file recovered from the March 16 export (recovered_from_raw = TRUE); ",
                   "for the other 1,983 the raw task-6 choice matches the public file exactly and the text matches up to outer whitespace; text is taken from the raw export for all rows. ",
                   "Paper text says the open-end followed task 2; QSF and export put Q40 after task 6. ",
                   "Paper's analytic sample is likely voters (not flagged here). Q40 is a single-line text box."),
  unverified = "none"))

# ---------------------------------------------------------------------------
# B. Taiwan 2024 national-identity CBC #2, Q74 on the task-3 page
# ---------------------------------------------------------------------------
tw_dir <- file.path(RES, "projects/roc-natid-cbc/data")
may <- read_qualtrics(file.path(tw_dir, "raw/ROC Experiments 2023_May 7, 2024_16.59.csv"))
mar <- read_qualtrics(file.path(tw_dir, "raw/ROC Experiments 2023_March 13, 2024_17.35.csv"))
clean <- read.csv(file.path(tw_dir, "clean/tw_merged_direct_conjoint.csv"),
                  colClasses = "character", fileEncoding = "UTF-8", na.strings = character(0))
clean <- clean[clean$question_profile %in% c("3.1", "3.2"),
               c("ResponseId", "question_profile", "person_choice", "Sentiment",
                 "Culture", "Ancestry", "Status", "Civicness", "Politics",
                 "Residence", "Origin", "Language")]

attr_en <- c("種族" = "Ancestry", "出生國" = "Origin", "國語能力" = "Language",
             "法律地位" = "Status", "在台灣居住的時間" = "Residence",
             "國家認同" = "Sentiment", "對政治制度的看法" = "Politics",
             "對文化和價值觀的看法" = "Culture", "公民參與" = "Civicness")
out_name <- c(Ancestry = "ancestry", Origin = "origin", Language = "language",
              Status = "legal_status", Residence = "residence", Sentiment = "sentiment",
              Politics = "politics", Culture = "culture", Civicness = "civicness")

# long task-3 attribute table from a raw export: rid, profile, attribute, level_zh
task3_long <- function(d) {
  rid <- col1(d, "ResponseId")
  do.call(rbind, lapply(1:9, function(k) {
    an <- col1(d, sprintf("C2-F-3-%d", k))
    rbind(data.frame(rid = rid, profile = "1", attr = attr_en[an],
                     zh = col1(d, sprintf("C2-F-3-1-%d", k)), stringsAsFactors = FALSE),
          data.frame(rid = rid, profile = "2", attr = attr_en[an],
                     zh = col1(d, sprintf("C2-F-3-2-%d", k)), stringsAsFactors = FALSE))
  }))
}
# Chinese -> English level dictionary from the March export (all 2,050 in the
# clean file), checked to be one-to-one
lm <- task3_long(mar)
stopifnot(!anyNA(lm$attr))
cl_long <- do.call(rbind, lapply(unname(attr_en), function(a)
  data.frame(rid = clean$ResponseId, profile = sub("^3\\.", "", clean$question_profile),
             attr = a, en = clean[[a]], stringsAsFactors = FALSE)))
lm <- merge(lm, cl_long, by = c("rid", "profile", "attr"))
dict <- unique(lm[, c("attr", "zh", "en")])
stopifnot(!anyDuplicated(dict[, c("attr", "zh")]), !anyDuplicated(dict[, c("attr", "en")]))
cat("TW: level dictionary entries =", nrow(dict), "\n")

ml <- task3_long(may)
stopifnot(!anyNA(ml$attr))
ml$en <- dict$en[match(paste(ml$attr, ml$zh), paste(dict$attr, dict$zh))]
stopifnot(!anyNA(ml$en))

rid <- col1(may, "ResponseId"); q66 <- col1(may, "Q66"); q74 <- col1(may, "Q74")
ch <- c("人物A" = "A", "人物B" = "B")[q66]
stopifnot(!anyNA(ch))
# consistency with the March export for the overlapping respondents (counts only)
mrid <- col1(mar, "ResponseId"); mq74 <- col1(mar, "Q74"); mq66 <- col1(mar, "Q66")
ov <- match(rid, mrid)
cat("TW: May n =", length(rid), "; also in March export =", sum(!is.na(ov)),
    "; Q74 identical on overlap =", sum(q74[!is.na(ov)] == mq74[ov[!is.na(ov)]]),
    "; Q66 identical on overlap =", sum(q66[!is.na(ov)] == mq66[ov[!is.na(ov)]]), "\n")
cat("TW: May respondents in roc-natid-cbc analysis file =", sum(rid %in% clean$ResponseId), "\n")

tw <- data.frame(id = rid, country = "TW", lang = "zh-Hant", text = q74,
                 exclude = FALSE, task = 3L, chosen_profile = ch,
                 stringsAsFactors = FALSE)
lev <- function(prof, a) {
  s <- ml[ml$profile == prof & ml$attr == a, ]
  s$en[match(rid, s$rid)]
}
for (pre in c("chosen", "other")) for (a in names(out_name)) {
  pa <- lev("1", a); pb <- lev("2", a)
  tw[[paste0(pre, "_", out_name[[a]])]] <-
    if (pre == "chosen") ifelse(ch == "A", pa, pb) else ifelse(ch == "A", pb, pa)
}
stopifnot(!anyNA(tw[, grep("^(chosen|other)_", names(tw))]))

write_harmonized(tw, "roc2024_q74_national", manifest = list(
  source = paste0("roc-natid-cbc/data/raw/ROC Experiments 2023_May 7, 2024_16.59.csv (Q74, Q66, C2-F-3-*); ",
                  "English level labels from roc-natid-cbc/data/clean/tw_merged_direct_conjoint.csv via the March 13 export"),
  design = "Paired forced-choice national-identity conjoint CBC #2 (6 tasks, 9 attributes); open-end Q74 on the task-3 page, between the choice and the 1-7 rating",
  lang = "zh-Hant",
  treatment_cols = "task=3; chosen_profile; chosen_* and other_* (9 attributes, English labels from the PB paper's analysis file); no respondent-level arm precedes Q74 (RandomTextFlag is set after this block)",
  human_codes = "none",
  routing = "API-eligible",
  card = "y",
  caveats = paste0("Delivered completes only (screen-outs and replaced cases are not in the export); exclude all FALSE. ",
                   "The May 7 and March 13 exports differ by 48 respondents each way; this file follows the May 7 export, ",
                   "which matches the sktw_naturalization_2024 Taiwan ids, while the roc-natid-cbc (Political Behavior) analysis file matches the March 13 export. ",
                   "Q74 was force-response. Q74 text was used by mistake as the Taiwan open text in this repo's 2024-25 STM/LLM pipeline before 00b_fix_tw_opentext.R."),
  unverified = "Why 48 respondents differ between the March 13 and May 7 exports (vendor replacement is likely but undocumented)"))
