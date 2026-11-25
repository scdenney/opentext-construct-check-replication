# ==============================================================================
# krishnarajan2022_rationalizing.R -- harmonized file for card krishnarajan2022_rationalizing
# ==============================================================================
# Krishnarajan, S. (2023). Rationalizing Democracy: The Perceptual Bias and
# (Un)Democratic Behavior. APSR 117(2), 474-496 (online 2022).
# Data: Harvard Dataverse doi:10.7910/DVN/WGPHFT (v1). Terms of use: "not to be
# distributed/posted outside of the Harvard Dataverse" -- raw/ and the output
# stay under data/ (git-ignored) and are never committed or shared.
#
# YouGov US survey, 29 Oct - 9 Nov 2020 (4,076 completes; 3,301 analyzed by the
# author). Each respondent reads four senator vignettes in two modules (two
# senator ACTIONS, two senator POLICY PROPOSALS; module order randomized), rates
# each, and explains the democracy rating ("You have stated that: '<rating>'.
# Please offer a brief explanation for your answer."). One output row = one
# vignette task (unit: task), four per respondent.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/krishnarajan2022_rationalizing.R
#
# Input (data/external/krishnarajan2022_rationalizing/raw/, unchanged):
#   data_core.dta  deposited Stata original of data_core.tab (queue batch
#                  krishnarajan_card_01); used instead of the .tab ingest
#   Data_setup.do  author's construction code (cited by line as "setup l. N")
# Output: data/external/krishnarajan2022_rationalizing/harmonized_krishnarajan2022_rationalizing.csv
# Blindness: no text value is printed; only counts. Warnings go to stderr.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/krishnarajan2022_rationalizing/raw"
out_csv <- "data/external/krishnarajan2022_rationalizing/harmonized_krishnarajan2022_rationalizing.csv"
card_id <- "krishnarajan2022_rationalizing"

d <- read_dta(file.path(raw_dir, "data_core.dta"))
stopifnot(nrow(d) == 4076, !anyDuplicated(d$caseid))
num <- function(x) { x <- as.numeric(x); x[x %in% 977] <- NA; x }  # 977 = "Don't know" (setup ll. 22, 379, 399, 430)

# ---- 1. Respondent-level variables ---------------------------------------------
# Issue positions recoded 0 (most left) to 4 (most right), setup ll. 53-90:
#   economy  view_eco = 5 - Q2_1 (agree "increase social spending" = left)
#   immig.   view_imm = Q2_2 - 1 (agree "reduce immigrants" = right)
#   health   view_hea = Q2_3 - 1 (agree "abolish Obamacare" = right)
# 977 (don't know) -> missing. left_right = sum, 0-12 (setup l. 94).
q2 <- function(x) { x <- as.numeric(x); x[!x %in% 1:5] <- NA; x }
view_eco <- 5 - q2(d$Q2_1); view_imm <- q2(d$Q2_2) - 1; view_hea <- q2(d$Q2_3) - 1
left_right <- view_eco + view_imm + view_hea
# Respondent side, setup ll. 586-588: left if left_right < 6, right if > 6;
# 6 (centre) and missing have no side.
resp_side <- ifelse(is.na(left_right), NA, ifelse(left_right < 6, "left", ifelse(left_right > 6, "right", "centre")))

# Author's exclusions (setup ll. 690-704; prereg "Data exclusion"):
#   keep if Q0 == 2 & Q26 == 4 & Tot_time > 120 (attention checks, > 2 minutes);
#   then drop respondents with dem missing or left_right missing in all four
#   rounds (mis_sum == 4). In Stata mis is 1 or missing, so mis_sum == 4 only
#   when every round is flagged: left_right missing, or all four democracy
#   ratings (Q6, Q10, Q14, Q18) missing/977.
att <- as.numeric(d$Q0) %in% 2 & as.numeric(d$Q26) %in% 4 & !is.na(d$Tot_time) & as.numeric(d$Tot_time) > 120
dem_all_missing <- is.na(num(d$Q6)) & is.na(num(d$Q10)) & is.na(num(d$Q14)) & is.na(num(d$Q18))
resp_exclude <- !att | is.na(left_right) | dem_all_missing
cat("Respondents:", nrow(d), "| author-kept:", sum(!resp_exclude), "(article: 3,301)\n")

module <- as.numeric(d$m_module_experiments_rnd_recoded)   # 1 = actions first, 2 = proposals first (DDI value labels)
stopifnot(all(module %in% 1:2))

# ---- 2. Task-level rows ----------------------------------------------------------
# Which vignette each item refers to. The questionnaire numbers items by module,
# not by display order: Q5-Q8 follow actions vignette 1, Q9-Q12 actions
# vignette 2, Q13-Q16 proposal 1, Q17-Q20 proposal 2 (DDI labels; Appendix B2).
# Checked with counts in this session: in BOTH module orders the democracy
# rating Q6 (Q10) differs by regular/undemocratic of Senators_actions_vignette_1
# (_2) by about 0.9-1.1 points and not at all by the proposal vignette, and
# Q14/Q18 track the proposal vignettes. NOTE: setup ll. 125-330 pair Q5-Q8 with
# the PROPOSAL vignette when module == 2 (treat_vignette_1 from
# Senators_policy_proposals_vignet, then agree_1 = Q5, jus_1 = Q7); that
# pairing is not used here.
slots <- list(
  list(item = "Q7",  rate = "Q6",  appr = "Q5",  list = "Q8",  vig = "Senators_actions_vignette_1",      iss = "Senators_actions_political_issue", mod = "action",   k = 1),
  list(item = "Q11", rate = "Q10", appr = "Q9",  list = "Q12", vig = "Senators_actions_vignette_2",      iss = "Senators_actions_political_issu0", mod = "action",   k = 2),
  list(item = "Q15", rate = "Q14", appr = "Q13", list = "Q16", vig = "Senators_policy_proposals_vignet", iss = "Senators_policy_proposals_issue_", mod = "proposal", k = 1),
  list(item = "Q19", rate = "Q18", appr = "Q17", list = "Q20", vig = "Senators_policy_proposals_vigne0", iss = "Senators_policy_proposals_issue0", mod = "proposal", k = 2))

# Raw vignette codes 1-12 within a module: 1-4 health care, 5-8 immigration,
# 9-12 social spending (DDI issue labels; cross-tab checked: each code sits in
# one issue). Within an issue, raw code order is regular-left, undemocratic-
# left, regular-right, undemocratic-right: setup ll. 129-157 map raw 1->1
# (Regular left), 3->2 (Regular right), 2->3 (Undemocratic left), 4->4
# (Undemocratic right), and likewise 5-8, 9-12 (treat_vignette_lab, ll. 112-117).
# DDI value labels agree: e.g. proposal 3 "abolish Obamacare." vs 4 "abolish
# Obamacare ... and prohibit senato[rs]"; 5 "welcome more immigrants." vs 6
# "... and prohibit"; action 5-6 pro-immigration group, 7-8 anti-immigration.
issue_lab <- c("health_care", "immigration", "social_spending")
out <- vector("list", 4)
for (s in slots) {
  raw <- as.numeric(d[[s$vig]])
  stopifnot(all(raw %in% 1:12))
  iss <- issue_lab[(raw - 1) %/% 4 + 1]
  stopifnot(identical(iss, issue_lab[as.numeric(d[[s$iss]])]))
  pos <- (raw - 1) %% 4 + 1                       # 1 reg-left, 2 undem-left, 3 reg-right, 4 undem-right
  behavior  <- ifelse(pos %in% c(1, 3), "regular", "undemocratic")
  direction <- ifelse(pos %in% c(1, 2), "left", "right")
  author_code <- c(1, 3, 2, 4)[pos] + 4 * ((raw - 1) %/% 4) + ifelse(s$mod == "proposal", 12, 0)  # treat_vignette 1-24
  # Display round: the first module shown occupies rounds 1-2 (prereg
  # "Randomization"; setup ll. 129-330).
  first <- ifelse(module == 1, "action", "proposal")
  round <- ifelse(first == s$mod, s$k, s$k + 2)
  # Alignment of the senator's side with the respondent's side (author's
  # left_right split; setup ll. 586-588; pos_rat, l. 603).
  align <- ifelse(is.na(resp_side) | resp_side == "centre", "neutral", ifelse(resp_side == direction, "agree", "disagree"))
  # Issue-specific stance on the shown issue (0-4, 0 = left), for sensitivity
  # analyses (Appendix C4b-d uses issue-specific left-right placement).
  stance <- ifelse(iss == "health_care", view_hea, ifelse(iss == "immigration", view_imm, view_eco))
  align_issue <- ifelse(is.na(stance) | stance == 2, "neutral",
                        ifelse((stance < 2) == (direction == "left"), "agree", "disagree"))
  txt <- as.character(d[[s$item]])
  out[[s$item]] <- data.frame(
    id = paste0(d$caseid, "_", s$item), text = txt, exclude = resp_exclude,
    treatment = paste(behavior, align, sep = "_"),
    respondent_id = as.character(d$caseid), item = s$item, round = round, module = s$mod,
    module_first = first, issue = iss, behavior = behavior, direction = direction,
    vignette_raw = raw, vignette_author = author_code,
    resp_side = ifelse(is.na(resp_side), "missing", resp_side), left_right = left_right,
    alignment = align, issue_stance = stance, alignment_issue = align_issue,
    approval = num(d[[s$appr]]), dem_rating = num(d[[s$rate]]), dem_list_rating = num(d[[s$list]]),
    stringsAsFactors = FALSE)
}
df <- do.call(rbind, out); rownames(df) <- NULL

# ---- 3. Text ------------------------------------------------------------------
# The answers are stored as Windows-1252 bytes, not UTF-8 (checked by byte
# counts: no cell holds valid multi-byte UTF-8; 811 cells hold bytes such as
# 0x92/0x93/0x94 = curly quotes and 0xE9/0xF3 = accented letters, all of which
# convert cleanly from cp1252). Convert every cell; ASCII is unchanged.
n_hi <- sum(grepl("[\x80-\xff]", df$text, useBytes = TRUE))
df$text <- iconv(df$text, "windows-1252", "UTF-8")
cat("Cells converted from Windows-1252 (non-ASCII bytes):", n_hi, "\n")
stopifnot(all(validUTF8(df$text[!is.na(df$text)])))
df$text <- trimws(gsub("[\r\n\t]+", " ", df$text))
# The author blanks five placeholder answers before the text analysis
# (setup ll. 807-811: four strings of the letter x of length 40, 33, 30, 27,
# and one survey URL). Same here; those rows then drop as empty.
junk <- c(strrep("x", 40), strrep("x", 33), strrep("x", 30), strrep("x", 27),
          "https://isurvey-us.yougov.com/a/vxrSJmxX0P8xWD")
cat("Author-blanked placeholder answers:", sum(df$text %in% junk), "\n")
df$text[df$text %in% junk] <- ""
df <- df[!is.na(df$text) & nzchar(df$text), ]
stopifnot(!anyDuplicated(df$id))

# Language: all respondents are YouGov US panelists answering an English
# questionnaire. Count answers containing letters outside Basic Latin/Latin-1.
nonlatin <- grepl("[^\\x01-\\x{024F}\\x{2000}-\\x{206F}\\x{20AC}\\x{2122}\\x{FE0F}\\x{1F000}-\\x{1FAFF}]", df$text, perl = TRUE)
cat("Answers with characters outside Latin/punctuation/emoji ranges:", sum(nonlatin), "\n")

df$dataset <- card_id; df$country <- "US"; df$lang <- "en"
core <- c("id", "dataset", "country", "lang", "text", "exclude", "treatment")
df <- df[, c(core, setdiff(names(df), core))]

# ---- 4. Write and check ---------------------------------------------------------
write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat("Rows:", nrow(df), "| respondents:", length(unique(df$respondent_id)),
    "| excluded rows:", sum(df$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(df$treatment[!df$exclude]))
cat("Non-excluded rows by item:\n"); print(table(df$item[!df$exclude]))
cat("Non-excluded by behavior x direction x issue:\n")
print(with(df[!df$exclude, ], table(paste(behavior, direction), issue)))
back <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read identical text:", identical(back$text, df$text), "\n")
n_esc <- sum(grepl("<U+", readLines(out_csv, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(back$text, df$text), n_esc == 0)
