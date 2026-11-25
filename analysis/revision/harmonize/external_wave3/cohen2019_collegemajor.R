# ==============================================================================
# cohen2019_collegemajor.R -- harmonized file for card cohen2019_collegemajor
# ==============================================================================
# Cohen, E. D. "Social Class, College Debt, and the Purpose of College." TESS
# study cohen1099 (TESS 044), NORC AmeriSpeak, web only, English only, 1,610
# interviews. OSF https://osf.io/5b6tf/. Publication on the majors part of the
# study: Cohen, E. D., Quadlin, N. Y., & Rouhani, S. (2026). Who Can Afford to
# Be an English Major? Sociology of Education 99(3): 195-215 (abstract read;
# full text not accessed; analytic N = 1,121).
#
# Design (fielded questionnaire raw/8041.044_TESS_044_Cohen_v10clean.docx;
# proposal raw/Cohen1099.pdf, Appendices A and C): one preload VIGNOE 1-24
# assigns vignette and split ballot jointly ("1=Saw vignette 1 and Q9, 2=Saw
# vignette 1 and Q19, ..."): vignette = ceiling(VIGNOE / 2); odd VIGNOE answer
# open item Q9 (majors), even VIGNOE answer Q19 (college experiences; this
# card). DOV_OEASSIGNED records the same split. Fielded vignette text
# (questionnaire, VIGNETTE screen): vignettes 1-6 Michael, 7-12 Emily; within
# each gender, in order: lawyers / no financing information, factory workers /
# no information, lawyers / $30,000 loans, factory workers / $30,000 loans,
# lawyers / university funding so no debt, factory workers / no debt.
# Everyone then rates 8 majors (Block A) and ranks 9 college experiences
# (Block B; Q10-18), block order randomized (RND_00 / DOV_ORDER_BLOCKS); Q19
# follows the ranking in Block B. Q20-22 are manipulation checks.
#
# Exclusions: none stated in the proposal; exclude = FALSE throughout. The
# 2026 article's N of 1,121 is close to the 1,119 respondents who pass both
# manipulation checks (Q21 parents' occupation, Q22 financing); that article
# analyzes the majors item, and its exclusion rule could not be read, so it is
# not applied here. Both checks are kept as design columns.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/cohen2019_collegemajor.R
# Input:  data/external/cohen2019_collegemajor/raw/TESS_044_Cohen_Final_Data19Sep30.sav
# Output: data/external/cohen2019_collegemajor/harmonized_cohen2019_collegemajor.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_file <- "data/external/cohen2019_collegemajor/raw/TESS_044_Cohen_Final_Data19Sep30.sav"
out_csv  <- "data/external/cohen2019_collegemajor/harmonized_cohen2019_collegemajor.csv"
card_id  <- "cohen2019_collegemajor"

d <- read_sav(raw_file)
stopifnot(nrow(d) == 1610, !anyDuplicated(d$CaseId))

v <- as.numeric(d$VIGNOE)
stopifnot(all(v %in% 1:24))
q19_ballot <- v %% 2 == 0
# DOV_OEASSIGNED (1 = QUESTION 9, 2 = QUESTION 19) must agree with VIGNOE parity.
stopifnot(identical(as.numeric(d$DOV_OEASSIGNED) == 2, q19_ballot))
cat("Split ballot: Q9", sum(!q19_ballot), "/ Q19", sum(q19_ballot),
    "; binomial p =", signif(binom.test(sum(q19_ballot), length(v))$p.value, 3), "\n")

vig   <- ceiling(v / 2)                       # 1-12, questionnaire numbering
w     <- (vig - 1) %% 6 + 1                   # position within gender
gender <- ifelse(vig <= 6, "michael", "emily")
parents <- ifelse(w %% 2 == 1, "lawyers", "factory_workers")
financing <- c("no_info", "no_info", "debt_30k", "debt_30k", "no_debt", "no_debt")[w]

# Manipulation checks (Q21: 1 both lawyers, 2 both factory workers; Q22: 3 has
# to take out loans, 4 graduate without debt because of university aid, 5 story
# didn't say). Used to verify the mapping; kept as design columns.
q21 <- as.numeric(d$Q21); q22 <- as.numeric(d$Q22)
passed_class_check <- q21 == ifelse(parents == "lawyers", 1, 2)
passed_financing_check <- q22 == c(no_info = 5, debt_30k = 3, no_debt = 4)[financing]
cat("Mapping check, parents x Q21 (1 lawyers, 2 factory):\n"); print(table(parents, q21))
cat("Mapping check, financing x Q22 (3 loans, 4 no debt, 5 not said):\n"); print(table(financing, q22))

# Text. Value labels 77 DON'T KNOW / 98 SKIPPED ON WEB / 99 REFUSED; a cell
# equal to a skip code ("98", "99", "-1", "77") is a skip and set empty.
txt <- as.character(zap_labels(d$Q19))
Encoding(txt) <- "UTF-8"
txt <- trimws(ifelse(is.na(txt), "", txt))
txt <- gsub("\r\n?", "\n", txt)
stopifnot(all(validUTF8(txt)))
is_code <- txt %in% c("98", "99", "-1", "77")
cat("Text equal to a skip code (set empty):", sum(is_code), "\n")
txt[is_code] <- ""
stopifnot(all(txt[!q19_ballot] == ""))       # Q19 asked only on the even ballot

stopifnot(all(as.numeric(d$SURV_LANG) == as.numeric(d$SURV_LANG[1])))  # English only

rank_cols <- c(rank_friends = "Q10", rank_arts = "Q11", rank_campus_activities = "Q12",
               rank_career = "Q13", rank_fun = "Q14", rank_grades = "Q15",
               rank_diversity = "Q16", rank_dating = "Q17", rank_professors = "Q18")
ranks <- as.data.frame(lapply(rank_cols, function(q) { x <- as.numeric(d[[q]]); x[!x %in% 1:9] <- NA; x }))

h <- data.frame(
  id        = as.integer(d$CaseId),          # AmeriSpeak CaseId
  dataset   = card_id,
  country   = "US",
  lang      = "en",
  text      = txt,
  exclude   = FALSE,
  treatment = as.integer(parents == "lawyers"),   # 1 = lawyer parents, 0 = factory-worker parents
  parents, financing, gender,
  vignette  = vig,
  # DOV_ORDER_BLOCKS labels "BLOCK ORDER: A, B" / "BLOCK ORDER: B, A"
  block_order = ifelse(grepl("B, A", as.character(as_factor(d$DOV_ORDER_BLOCKS))), "B_first", "A_first"),
  ranks,
  passed_class_check, passed_financing_check,
  weight    = as.numeric(d$WEIGHT),          # not used
  stringsAsFactors = FALSE
)

cat("Rows with empty text dropped:", sum(h$text == ""), "\n")
h <- h[h$text != "", ]
write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks --------------------------------------------------------------------
cat("Rows written:", nrow(h), "\n")
k <- h[!h$exclude, ]
cat("Non-excluded rows, parents:\n"); print(table(k$parents))
cat("Non-excluded rows, financing:\n"); print(table(k$financing))
cat("Non-excluded rows, gender:\n"); print(table(k$gender))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
