# ==============================================================================
# lupu2024_lawsofwar_gov.R -- harmonized file for card lupu2024_lawsofwar_gov
# ==============================================================================
# Source: Lupu, Y., & Wallace, G. P. R. (2024). The laws of war and public
# support for foreign combatants. International Organization 78(4), 823-852,
# doi:10.1017/S0020818324000274. Data doi:10.7910/DVN/WYYEWK (CC0 1.0).
#
# Second open-ended item of the same respondents as card lupu2024_lawsofwar:
# app_gov_open (explanation of the government approval rating), treatment govlaw.
# Conventions follow the wave-2 builder for lupu2024_lawsofwar
# (analysis/revision/harmonize/external_wave2/dataverse/harmonize.R, lines 74-84):
# id = <ISO2>_<ResponseID>, country FR/GB/US, lang fr/en, no exclusions, same
# UTF-8 repair. The ids therefore match harmonized_lupu2024_lawsofwar.csv.
#
# Blind builder: never prints open-text values, only counts.
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/lupu2024_lawsofwar_gov.R
# ==============================================================================
suppressPackageStartupMessages(library(haven))

DS   <- "lupu2024_lawsofwar"
CARD <- "lupu2024_lawsofwar_gov"
RAW  <- file.path("data/external", DS, "raw")
OUT  <- file.path("data/external", DS, paste0("harmonized_", CARD, ".csv"))

# Same repair as the wave-2 builder (fix_utf8, harmonize.R lines 5-13): the
# France strings are double-encoded UTF-8 (UTF-8 bytes read as latin1). Reverse
# the latin1 round trip only where the result is valid UTF-8.
fix_utf8 <- function(x) {
  x <- as.character(x); bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) { c1 <- iconv(x[bad], "latin1", "UTF-8"); moj <- grepl("Ã|Â", c1)
    c1[moj] <- iconv(x[bad][moj], "UTF-8", "UTF-8", sub = ""); x[bad] <- c1 }
  m <- !is.na(x) & grepl("Ã|Â", x)
  if (any(m)) { z <- iconv(x[m], "UTF-8", "latin1"); ok <- !is.na(z) & validUTF8(z); Encoding(z) <- "UTF-8"; x[m][ok] <- z[ok]
    cat(sprintf("  [repaired double-encoded cells: %d of %d flagged]\n", sum(ok), sum(m))) }
  x }
clean_text <- function(x) { x <- fix_utf8(x); x <- gsub("[\r\n\t]+", " ", x); x <- trimws(x); x[is.na(x)] <- ""; x }

# Factor codes (value labels in the .dta, identical in all three files):
# govlaw/reblaw 0 = Comply, 1 = Violate; govtype 0 = Autocracy, 1 = Democracy;
# rebaims 0 = Overthrow, 1 = Secession; actor_order 'Govt First'/'Rebel First'.
# The replication code ("Lupu Wallace IO Replication Code-1.txt", line 93):
# "in govlaw and reblaw, 1=violation".
lab <- function(x) as.character(as_factor(x))
out <- list()
for (cn in c("France", "UK", "USA")) {
  d  <- read_dta(file.path(RAW, paste0(cn, ".dta")))
  stopifnot(!anyDuplicated(d$ResponseID), all(as.numeric(d$govlaw) %in% 0:1))
  cc <- c(France = "FR", UK = "GB", USA = "US")[[cn]]
  cat(cn, "\n")
  # app_gov_open is the merged column of the order-specific copies
  # App_Gov_Open_RG / App_Gov_Open_GR (checked: every non-empty answer is in
  # exactly one copy); the merged column is used, as for app_reb_open in wave 2.
  out[[cn]] <- data.frame(
    id = paste0(cc, "_", as.character(d$ResponseID)), dataset = CARD, country = cc,
    lang = ifelse(cc == "FR", "fr", "en"), text = clean_text(d$app_gov_open),
    # Exclusions: none. The replication code drops no respondents for the main
    # analyses (subsets only drop missing outcome codes in the mediation models),
    # and every row has a randomized govlaw value.
    exclude = FALSE,
    treatment = as.integer(as.numeric(d$govlaw) == 1),      # 1 = government violates
    reblaw = lab(d$reblaw), govtype = lab(d$govtype), rebaims = lab(d$rebaims),
    actor_order = lab(d$actor_order),
    app_gov5 = as.numeric(d$app_gov5),                       # 1-5 government approval (closed-ended)
    stringsAsFactors = FALSE)
}
h <- do.call(rbind, out)
cat("Rows before dropping empty text:", nrow(h), "\n")
h <- h[nzchar(h$text), ]   # rows with empty text are kept out (brief)
stopifnot(!anyDuplicated(h$id))
# No human codes: the article does not analyse the open-ended items.

write.csv(h, OUT, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Checks
cat(sprintf("\n%s: rows = %d, columns = %s\n", OUT, nrow(h), paste(names(h), collapse = ",")))
cat("Non-excluded rows per arm (0 = government complies, 1 = violates), by country:\n")
print(table(country = h$country[!h$exclude], treatment = h$treatment[!h$exclude]))
# na.strings = "": a few answers are the literal string "NA", which read.csv
# turns into a missing value by default (rows with empty text were dropped above,
# so "" never stands for an answer).
r <- read.csv(OUT, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"), na.strings = "")
cat("Answers read back as NA under read.csv defaults (literal \"NA\"):", sum(h$text == "NA"), "\n")
cat("Re-read text identical:", identical(r$text, h$text), "\n")
n_esc <- sum(grepl("<U+", readLines(OUT, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
cat("Lines with <U+ escapes:", n_esc, "\n")
stopifnot(identical(r$text, h$text), n_esc == 0)
# Shared respondents with card lupu2024_lawsofwar (same id construction)
reb <- file.path("data/external", DS, "harmonized_lupu2024_lawsofwar.csv")
if (file.exists(reb)) {
  ids <- read.csv(reb, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = c(id = "character"))$id
  cat("Ids shared with harmonized_lupu2024_lawsofwar.csv:", sum(h$id %in% ids), "of", nrow(h), "\n")
}
