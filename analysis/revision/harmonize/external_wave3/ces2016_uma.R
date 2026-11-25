# ==============================================================================
# ces2016_uma.R -- harmonized files for cards ces2016_uma_mexicans,
#                  ces2016_uma_blacks, ces2016_uma_millennials
# ==============================================================================
# CCES 2016, Team Module of the University of Massachusetts (UMA), YouGov,
# pre-election wave. Harvard Dataverse doi:10.7910/DVN/YLYV32 (CC0 1.0).
# Paper: Schaffner, B. F. "Follow the Racist? The Consequences of Trump's
# Expressions of Prejudice for Mass Rhetoric." Working paper (Tufts), section
# "Design" (pp. 6-8): the 2016 CCES experiment on 1,186 non-Latino white adults,
# 28 Sep - 6 Nov 2016. Also in Schaffner (2020), Cambridge Elements,
# doi:10.1017/9781108924153 (not openly readable; not used).
#
# Design (pre questionnaire raw/CCES16_UMA_pre.txt, lines 144-157 and 873-912):
#   UMA340_UMA363rand == 1 for two thirds of race == 1 (white) respondents
#   (random(1,3) in {1,2}); only they get UMA341_GROUP = random(1,4).
#   UMA341 grid "which candidate made these statements": every group sees the
#   neutral quotes b, d, e; UMA341a ("thugs ... Baltimore") if GROUP in [2,4];
#   UMA341c (Mexico "rapists") if GROUP in [3,4]. Row order randomized.
#   Next page UMA342: "In a few words, please let us know what comes to mind
#   when you think of the following groups?" six open boxes in random order:
#   ax Blacks, bx Mexicans, cx Whites, dx Politicians, ex The Middle Class,
#   fx Millennials. The paper analyzes Blacks, Mexicans and Millennials (p. 7-8),
#   so one file is written per analyzed item.
# Arm labels: 1 neutral, 2 thugs, 3 mexico, 4 both. Mapping checked below
# against item non-missingness of UMA341a / UMA341c.
# The paper's contrasts (Figure 1 caption, p. 13): respondents receiving a
# quotation vs those not receiving it, i.e. mexico_quote = arms {3,4} vs {1,2}
# and thugs_quote = arms {2,4} vs {1,3}; both 0/1 columns are written.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/ces2016_uma.R
# Input:  data/external/ces2016_uma/raw/CCES16_UMA_OUTPUT_Feb2017.sav
#         (Dataverse original-format SPSS file, not the .tab ingest)
# Output: data/external/ces2016_uma/harmonized_ces2016_uma_<group>.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))
raw_dir <- "data/external/ces2016_uma/raw"
out_dir <- "data/external/ces2016_uma"

d <- read_sav(file.path(raw_dir, "CCES16_UMA_OUTPUT_Feb2017.sav"))
stopifnot(nrow(d) == 2500, !anyDuplicated(d$V101))

grp <- as.numeric(d$UMA341_GROUP)          # NA = not asked (codebook: 1,309)
# Assignment exists exactly for the UMA340-343 branch (questionnaire l. 155-156).
stopifnot(identical(!is.na(grp), as.numeric(d$UMA340_UMA363rand) %in% 1),
          all(grp %in% c(1:4, NA)))
# Arm mapping check: the thugs item (341a) is answered only in groups 2/4 and
# the Mexico item (341c) only in groups 3/4 (questionnaire l. 882-887).
stopifnot(all(is.na(d$UMA341a[!grp %in% c(2, 4)])),
          all(is.na(d$UMA341c[!grp %in% c(3, 4)])))
arm <- c("neutral", "thugs", "mexico", "both")[grp]

# Empty-text rule: "__NA__" is the YouGov not-asked placeholder (exactly the
# 1,309 unassigned rows); "" is a skipped box; "." and the skip codes
# -1/98/99/-99 are treated as empty (only "." occurs, once per item).
empty_codes <- c("", "__NA__", ".", "-1", "98", "99", "-99")

items <- c(mexicans = "UMA342bx", blacks = "UMA342ax", millennials = "UMA342fx")

for (g in names(items)) {
  card_id <- paste0("ces2016_uma_", g)
  out_csv <- file.path(out_dir, paste0("harmonized_", card_id, ".csv"))
  txt <- as.character(d[[items[[g]]]])
  Encoding(txt) <- "UTF-8"
  txt <- trimws(ifelse(is.na(txt), "", txt))
  txt <- gsub("\r\n?", "\n", txt)
  stopifnot(all(validUTF8(txt)))
  txt[txt %in% empty_codes] <- ""

  h <- data.frame(
    id        = as.integer(d$V101),       # YouGov case id (V101)
    dataset   = card_id,
    country   = "US",
    lang      = "en",
    text      = txt,
    # No exclusions: the paper states none beyond the sample (non-Latino white
    # adults, N = 1,186, p. 6). The module assigned 1,191 race == 1 respondents;
    # 27 of them answer yes to the Hispanic question, but 1,191 - 27 does not
    # give 1,186 either, so no Hispanic exclusion is applied (column kept for
    # a sensitivity check). Unassigned rows carry the placeholder and drop out.
    exclude   = is.na(grp),
    treatment = arm,                      # neutral / thugs / mexico / both
    mexico_quote = as.integer(grp %in% c(3, 4)),
    thugs_quote  = as.integer(grp %in% c(2, 4)),
    hispanic  = as.integer(as.numeric(d$hispanic) %in% 1),   # 1 = Yes (value labels)
    stringsAsFactors = FALSE
  )
  h <- h[!is.na(grp), ]
  cat("\n==", card_id, "(", items[[g]], ")\n")
  cat("Assigned rows with empty text dropped:", sum(h$text == ""), "\n")
  h <- h[h$text != "", ]
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

  # ---- Checks ------------------------------------------------------------------
  cat("Rows written:", nrow(h), "\n")
  k <- h[!h$exclude, ]
  cat("Non-excluded rows per arm:\n"); print(table(factor(k$treatment, c("neutral", "thugs", "mexico", "both"))))
  cat("mexico_quote 1/0:", sum(k$mexico_quote == 1), sum(k$mexico_quote == 0),
      " thugs_quote 1/0:", sum(k$thugs_quote == 1), sum(k$thugs_quote == 0), "\n")
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  stopifnot(identical(r$text, h$text))
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n")
  stopifnot(n_esc == 0)
}
