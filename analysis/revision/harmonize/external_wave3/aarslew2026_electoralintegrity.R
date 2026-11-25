# ==============================================================================
# aarslew2026_electoralintegrity.R -- harmonized file for card aarslew2026_electoralintegrity
# ==============================================================================
# Aarslew, L. F. (2026). How violations of electoral integrity undermine
# partisan attachments. British Journal of Political Science 56.
# doi:10.1017/S0007123426101343. Data: Harvard Dataverse doi:10.7910/DVN/YWMRHS
# (v1), CC0 1.0.
#
# Study 1 (Lucid, 12 Dec 2021 - 6 Jan 2022; online appendix A). Respondents
# read about a future congressional election in their state in which the
# Republican or the Democratic party (vignette_party) either won fairly
# (control) or won after engaging in absentee/mail-in ballot fraud or in
# misinformation (condition 0/1/2; 6 cells of about 845). They then answered
# "Remember the text about a state election that you just read. If a new
# election for congressional seat were held after two years with the same
# candidates running, who would you vote for?" (vote_choice; appendix A.1).
# Respondents who chose a candidate got the matching open item: Q18 "In your
# own words, please explain why you would vote for the Democratic
# candidate...", Q19 Republican, Q20 Independent (variable labels; "Don't
# know" voters got no item). One row per respondent; at most one item each.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/aarslew2026_electoralintegrity.R
#
# Input (data/external/aarslew2026_electoralintegrity/raw/, unchanged):
#   data_S1.dta  deposited Stata original (not the .tab ingest)
#   S1_SI.do     author's appendix code (cited "SI.do l. N")
# Output: data/external/aarslew2026_electoralintegrity/harmonized_aarslew2026_electoralintegrity.csv
# Blindness: no text value is printed; only counts. Warnings go to stderr.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw_dir <- "data/external/aarslew2026_electoralintegrity/raw"
out_csv <- "data/external/aarslew2026_electoralintegrity/harmonized_aarslew2026_electoralintegrity.csv"
card_id <- "aarslew2026_electoralintegrity"

d <- read_dta(file.path(raw_dir, "data_S1.dta"))
stopifnot(nrow(d) == 5067)
# No respondent id is deposited: id = row order in data_S1.dta ("r0001" ...).
id <- sprintf("r%04d", seq_len(nrow(d)))

# ---- 1. Design --------------------------------------------------------------
# condition value labels: 0 control, 1 absentee, 2 misinformation. D = 1 for
# both violation arms (the article collapses them, fn. 10; SI.do uses i.D).
cond <- as.numeric(d$condition)
stopifnot(all(cond %in% 0:2), identical(as.numeric(d$D), as.numeric(cond > 0)))
treatment <- c("control", "absentee", "misinformation")[cond + 1]
vparty <- ifelse(as.numeric(d$vignette_party) == 1, "democrat", "republican")   # value labels 0 rep, 1 dem
# copartisan_D: 1 = the vignette party is the respondent's party (in-party
# condition), 0 = out-party; missing for pure independents and missing pid.
copartisan <- as.numeric(d$copartisan_D)
pid <- as.numeric(d$pid)                                                      # -3 strong dem ... 3 strong rep

# Exclusions: every Study 1 analysis in the article and SI.do splits by
# copartisan_D (in-party vs out-party malpractice, e.g. SI.do ll. 21-37) and so
# drops respondents without a partisan leaning (809 pure independents, 97
# missing pid; appendix A.1: 4,258 partisans). Excluded here too. The
# main-text code S1_Fig1.do (fetched to data/dvq/outbox/w3card_aarslew_01/)
# applies no other filter (ll. 25-52: `if copartisan_D==1/0` only). The
# appendix figure notes' N = 4,117 matches no filter in either do-file.
exclude <- is.na(copartisan)

# Closed vote choice (value labels): 1 Republican, 2 Democratic, 3
# independent/another, 4 Don't know.
vc <- as.numeric(d$vote_choice)
vote_choice <- c("republican", "democrat", "independent", "dont_know")[vc]
# Vote relative to the respondent's party (SI.do vote_inparty/outparty/independent).
vote_rel <- ifelse(is.na(vc) | vc == 4 | is.na(pid) | pid == 0, NA,
            ifelse(vc == 3, "independent",
            ifelse((vc == 2 & pid < 0) | (vc == 1 & pid > 0), "inparty", "outparty")))

# ---- 2. Text ------------------------------------------------------------------
# Merge the three routed items into one column; the item column records which
# was shown (Q18 Democratic, Q19 Republican, Q20 Independent).
q <- cbind(Q18 = as.character(d$Q18), Q19 = as.character(d$Q19), Q20 = as.character(d$Q20))
q[is.na(q)] <- ""
q[] <- trimws(gsub("[\r\n\t]+", " ", q))
has <- matrix(nzchar(q), nrow = nrow(q), dimnames = dimnames(q))
n_items <- rowSums(has)
stopifnot(all(n_items <= 1))
item <- ifelse(n_items == 0, NA, colnames(q)[max.col(has, ties.method = "first")])
text <- ifelse(n_items == 0, "", q[cbind(seq_len(nrow(q)), match(item, colnames(q)))])
stopifnot(all(validUTF8(text)))
# Routing check: each item matches the stated vote choice.
stopifnot(all(is.na(item) | (item == "Q18" & vc %in% 2) | (item == "Q19" & vc %in% 1) | (item == "Q20" & vc %in% 3)))

df <- data.frame(
  id = id, dataset = card_id, country = "US", lang = "en", text = text,
  exclude = exclude, treatment = treatment,
  vignette_party = vparty, copartisan = ifelse(is.na(copartisan), NA, ifelse(copartisan == 1, "inparty", "outparty")),
  pid = pid, vote_choice = vote_choice, vote_relation = vote_rel, item = item,
  pei = ifelse(as.numeric(d$pei) %in% 1:5, as.numeric(d$pei), NA),   # 6 = Don't know
  mc_pass = as.numeric(d$mc_pass), attention_3 = as.numeric(d$attention_3),
  stringsAsFactors = FALSE)

# Answering and choice by arm, all assigned rows (before dropping empty text).
cat("All rows:", nrow(df), "| partisans (non-excluded):", sum(!df$exclude), "\n")
cat("Answered (non-empty text) / assigned, partisans, by arm:\n")
pa <- df[!df$exclude, ]
print(rbind(assigned = table(pa$treatment), answered = table(pa$treatment[nzchar(pa$text)])))
cat("Vote choice by arm, partisans (counts; dont_know gets no open item):\n")
print(table(vote_choice = pa$vote_choice, treatment = pa$treatment, useNA = "ifany"))
cat("Vote relative to own party by arm x in/out-party condition, partisans:\n")
print(ftable(table(copartisan = pa$copartisan, treatment = pa$treatment, vote = pa$vote_relation, useNA = "ifany")))

# Answers consisting of exactly "NA" (5, found by count when the re-read
# check failed) would be read back as missing by read.csv(); they carry no
# content and are dropped like empty answers.
cat("Answers exactly 'NA' dropped:", sum(df$text == "NA"), "\n")
df <- df[nzchar(df$text) & df$text != "NA", ]
rownames(df) <- NULL

# ---- 3. Write and check ---------------------------------------------------------
write.csv(df, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
ok <- !df$exclude
cat("Rows written:", nrow(df), "| excluded:", sum(df$exclude), "\n")
cat("Non-excluded rows per arm:\n"); print(table(df$treatment[ok]))
cat("Non-excluded rows per arm x item:\n"); print(table(item = df$item[ok], treatment = df$treatment[ok]))
cat("Non-excluded rows per arm x vignette party x copartisan:\n")
print(ftable(table(treatment = df$treatment[ok], vignette_party = df$vignette_party[ok], copartisan = df$copartisan[ok])))
back <- read.csv(out_csv, encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Re-read identical text:", identical(back$text, df$text), "\n")
n_esc <- sum(grepl("<U+", readLines(out_csv, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(back$text, df$text), n_esc == 0)
