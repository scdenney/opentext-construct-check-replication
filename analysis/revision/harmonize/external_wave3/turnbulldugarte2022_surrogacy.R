# ==============================================================================
# turnbulldugarte2022_surrogacy.R -- harmonized file for card turnbulldugarte2022_surrogacy
# ==============================================================================
# Source: Turnbull-Dugarte, S. J. (2023). Fine for Adam & Eve but not Adam & Steve?
# Homonegativity bias, parasocial contact, and public support for surrogacy.
# Journal of European Public Policy 31(2); online 2022, doi:10.1080/13501763.2022.2154823.
# Preprint read: OSF doi:10.31219/osf.io/t4exs (April 2022).
# Data: Harvard Dataverse doi:10.7910/DVN/PDOXJD (CC0 1.0). UK Qualtrics online
# panel, March 2021, five arms randomized at the respondent level.
#
# Item: Q21, "Thinking about the previous question, what mattered most to you when
# considering your answer?", asked of every respondent immediately after the
# closed support item (preprint section 6, p. 25). Text object: `simple` in
# QTAdatafile.Rdata (1,813 rows: Q21, treatment, homo, prestige, support), the
# file the authors' QTA.R uses (line 1). It carries no respondent id and no key
# to surrogacyDATA.tab (1,979 rows), so `id` is the row order of `simple`.
#
# Arms (mainanalysis.R lines 71-72 labels; preprint Table 2): 0 control (no
# couple), 1 celebrity gay couple, 2 celebrity straight couple, 3 ordinary gay
# couple, 4 ordinary straight couple. In the authors' file `homo` and `prestige`
# are 0 for the control arm; here they are NA for control so that 0 means the
# straight / ordinary couple only (QTA.R line 33 regresses on homo with control
# pooled into homo = 0; the card's primary contrast drops control instead).
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/turnbulldugarte2022_surrogacy.R
# ==============================================================================
DS   <- "turnbulldugarte2022_surrogacy"
CARD <- "turnbulldugarte2022_surrogacy"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

e <- new.env()
stopifnot(identical(load(file.path(RAW, "QTAdatafile.Rdata"), envir = e), "simple"))
s <- as.data.frame(e$simple)
stopifnot(nrow(s) == 1813)

# Encoding: 1,800 answers are pure ASCII; 13 are not valid UTF-8 and contain only
# the single bytes 0xD5 and 0xE2 (counted, not shown). In Mac OS Roman these are
# the right single quotation mark and the single low-9 quotation mark, both
# punctuation; in Windows-1252 they would be the letters O-tilde and a-circumflex,
# implausible inside English words. Those 13 are converted from Mac Roman.
x <- as.character(s$Q21); x[is.na(x)] <- ""
bad <- !validUTF8(x)
x[bad] <- iconv(x[bad], from = "macintosh", to = "UTF-8")
stopifnot(all(validUTF8(x)), !anyNA(x))
cat("Answers converted from Mac Roman:", sum(bad), "\n")

arm_lab <- c("0" = "control", "1" = "celeb_gay", "2" = "celeb_straight",
             "3" = "ordinary_gay", "4" = "ordinary_straight")
tr <- as.character(s$treatment)
stopifnot(all(tr %in% names(arm_lab)))
# consistency of the authors' factor columns with the arm (checked, not assumed)
stopifnot(all((as.character(s$homo) == "1") == (tr %in% c("1", "3"))),
          all((as.character(s$prestige) == "1") == (tr %in% c("1", "2"))))

h <- data.frame(
  id = seq_len(nrow(s)),                       # row order of `simple` (no id in deposit)
  dataset = CARD, country = "GB", lang = "en",
  text = trimws(gsub("[[:space:]]+", " ", x)),
  exclude = FALSE,                             # the authors drop no rows from `simple`
                                               # (QTA.R only removes stop words and tokens);
                                               # every row has an arm (checked above)
  treatment = unname(arm_lab[tr]),
  homo = ifelse(tr == "0", NA_integer_, as.integer(tr %in% c("1", "3"))),      # 1 = same-sex couple
  prestige = ifelse(tr == "0", NA_integer_, as.integer(tr %in% c("1", "2"))),  # 1 = celebrity couple
  support = as.integer(s$support),             # authors' dichotomized support (1 = agree /
                                               # strongly agree; preprint section 4), 16 NA
  stringsAsFactors = FALSE)
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief); none here

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm:\n")
print(table(h$treatment[!h$exclude]))
cat("Primary contrast (gay vs straight couple, control out):",
    sum(!h$exclude & h$treatment != "control"), "\n")
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
