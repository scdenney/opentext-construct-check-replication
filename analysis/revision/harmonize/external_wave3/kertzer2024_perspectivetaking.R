# ==============================================================================
# kertzer2024_perspectivetaking.R -- harmonized file for card kertzer2024_perspectivetaking
# ==============================================================================
# Source: Kertzer, J. D., Brutger, R., & Quek, K. (2024). Perspective taking and
# security dilemma thinking: Experimental evidence from China and the United
# States. World Politics 76(2), 334-378, doi:10.1353/wp.2024.a924509.
# Data: Harvard Dataverse doi:10.7910/DVN/ACI6QO (CC0 1.0). Parallel online
# surveys fielded in spring 2016: US (English, 1,820 rows) and China (Chinese,
# 1,485 rows).
#
# Item: policyExpl, "Please briefly explain your response." / "请简要解释您的回答："
# asked of every respondent after the policy choice (choice/Incr1-3 in the US file,
# IncrAB/Incr1-3 in the China file), i.e. after both randomized factors
# (questionnaire in the article's appendix, "Perspective-Taking Experiment";
# SCS_Codebook.pdf). attribution1 (Writing task I, part of the escalation
# manipulation) and attribution2 (Writing task II, the perspective-taking
# treatment itself, PT arm only) are not used.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/kertzer2024_perspectivetaking.R
# ==============================================================================
DS   <- "kertzer2024_perspectivetaking"
CARD <- "kertzer2024_perspectivetaking"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

load_one <- function(f) { e <- new.env(); nm <- load(file.path(RAW, f), envir = e); get(nm[1], e) }
us <- load_one("SCS_US_Data.RData")      # object usa1 in SCS_Replication1.R line 23
cn <- load_one("SCS_China_Data.RData")   # object china1, line 22
stopifnot(nrow(us) == 1820, nrow(cn) == 1485)

# Policy choice on the 1-7 scale (1 decrease a lot ... 7 increase a lot), as built
# in SCS_Replication1.R line 67 (US) and line 139 (China: sum of the non-missing
# Incr1-Incr3 branches, helper sum2 at line 61).
sum2 <- function(x) { z <- x[!is.na(x)]; if (length(z)) sum(z) else NA }
us_pc <- ifelse(us$choice == 1, ifelse(us$Incr1 == 7, 7, ifelse(us$Incr1 == 6, 6, NA)),
         ifelse(us$choice == 2, ifelse(us$Incr2 == 1, 1, ifelse(us$Incr2 == 2, 2, NA)),
         ifelse(us$choice == 3, ifelse(us$Incr3 == 3, 3, ifelse(us$Incr3 == 4, 4,
                ifelse(us$Incr3 == 5, 5, NA))), NA)))
cn_pc <- apply(cbind(cn$Incr1, cn$Incr2, cn$Incr3), 1, sum2)

clean_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stopifnot(all(validUTF8(x)))
  trimws(gsub("[[:space:]]+", " ", x))
}

# No respondent id is in either file; id is the row order within each file,
# prefixed by country (us_0001 ..., cn_0001 ...).
mk <- function(d, iso, lang, esc, pc) data.frame(
  id = sprintf("%s_%04d", tolower(iso), seq_len(nrow(d))), dataset = CARD,
  country = iso, lang = lang, text = clean_text(d$policyExpl),
  # exclude: the deposited files already omit the respondents the authors drop
  # (US: VPS users and unvalidated locations, article fn. 16; the US file has the
  # 1,820 rows reported in the article). Rows with a missing assignment are
  # excluded; there are none (checked below).
  exclude = is.na(d$PT) | is.na(d[[esc]]),
  treatment = as.integer(d$PT),                 # 1 = perspective-taking writing task
  other_escalates = as.integer(d[[esc]]),       # ch_incr (US) / am_incr (China): 1 = the
                                                # other country increases naval deployments
  policy_choice = as.integer(pc), stringsAsFactors = FALSE)

h <- rbind(mk(us, "US", "en", "ch_incr", us_pc),
           mk(cn, "CN", "zh-Hans", "am_incr", cn_pc))   # questionnaire in Simplified Chinese
stopifnot(!any(h$exclude), all(h$treatment %in% 0:1), all(h$other_escalates %in% 0:1))
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)

enc_counts <- function(x) c(
  rows = length(x), non_ascii = sum(grepl("[^\x01-\x7f]", x, useBytes = TRUE)),
  replacement_char = sum(grepl("�", x, fixed = TRUE)),
  mojibake = sum(grepl("Ã|â€|Â", x)))
cat("Encoding counts before writing (US, CN):\n")
print(rbind(US = enc_counts(h$text[h$country == "US"]), CN = enc_counts(h$text[h$country == "CN"])))
write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (treatment = PT) by country and escalation factor:\n")
print(with(h[!h$exclude, ], table(country, other_escalates, treatment)))
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
