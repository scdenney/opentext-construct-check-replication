# ==============================================================================
# rv_18_study_catalog.R -- one table row per study for the SI, from the study cards
# ==============================================================================
# Reads the registered study cards, analysis/output/revision/corpus_table.csv and
# revision/study_metadata.csv. The metadata file holds seven fields per study
# (countries, year fielded, sample, design, role of the open-text item, the
# question in English, shortened, and contrast_short, the primary contrast in at
# most 90 characters). The first six were extracted from the cards by
# language-model agents on 18 Nov 2026 under an instruction to write NA where a
# card does not state a field; years and countries were then checked against the
# card texts by script. The cards of the author's own studies do not name the
# sample provider; for those 18 studies the sample field was filled by hand from
# the studies' own documents: the working paper of the SK/TW 2024 study, the
# AKS 2021 article (International Migration Review, 2025), the IFES 2023 report,
# the NWO 2026 working papers, the Taiwan 2024 working paper, and the 2024
# election article (Party Politics). contrast_short is merged in by
# rv_18a_merge_contrast_short.R. Everything else in a row is copied from the card.
# No open-text answer is read.
# Writes paper/revision/si_tables/studies_rows.tex (rows of a six-column
# longtable) and catalog_summary_rows.tex.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_18_study_catalog.R [metadata.csv] [output dir]
# ==============================================================================

source("analysis/revision/rv_config.R")
args <- commandArgs(trailingOnly = TRUE)
MD_FILE <- if (length(args) >= 1) args[1] else "revision/study_metadata.csv"
TEX <- if (length(args) >= 2) args[2] else "paper/revision/si_tables"
cards <- registered_cards()
ct <- read.csv(file.path(OUT, "corpus_table.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
md <- read.csv(MD_FILE, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "NA")
cites <- read.csv("revision/dataset_citations.csv", stringsAsFactors = FALSE)
stopifnot(setequal(md$dataset, names(cards)), setequal(ct$dataset[ct$dataset %in% names(cards)], names(cards)))
if (!"contrast_short" %in% names(md))
  stop(MD_FILE, " has no column contrast_short; run analysis/revision/rv_18a_merge_contrast_short.R first", call. = FALSE)
cs_bad <- md$dataset[is.na(md$contrast_short) | trimws(md$contrast_short) == ""]
if (length(cs_bad)) stop("contrast_short is empty for: ", paste(cs_bad, collapse = ", "), call. = FALSE)
cs_long <- md$dataset[nchar(md$contrast_short) > 90]
if (length(cs_long)) stop("contrast_short is longer than 90 characters for: ", paste(cs_long, collapse = ", "), call. = FALSE)
is_own <- function(x) grepl("^(sktw|aks21|ifes2023|nwo26|prc|roc|rok)", x)

flat <- function(x) trimws(gsub("\\s+", " ", paste(unlist(x), collapse = " ")))
# at most n characters, cut at a word boundary; the ellipsis counts as one
cut_at <- function(x, n) {
  if (nchar(x) <= n) return(x)
  y <- sub("[[:space:][:punct:]]+$", "", sub("\\s+\\S*$", "", substr(x, 1, n)))
  paste0(y, "\u2026")
}
esc <- function(x) {
  x <- gsub("\\\\", "/", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = FALSE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("→", " to ", x); x <- gsub("≥", "$\\\\geq$", x); x <- gsub("≤", "$\\\\leq$", x)
  x <- gsub("×", "$\\\\times$", x); x <- gsub("≠", "$\\\\neq$", x); x <- gsub("≈", "$\\\\approx$", x)
  x <- gsub("−", "$-$", x); x <- gsub("\u2026", "\\\\ldots{}", x)
  x <- gsub("<", "$<$", x, fixed = TRUE); x <- gsub(">", "$>$", x, fixed = TRUE)
  x <- gsub("\"([^\"]*)\"", "``\\1''", x)                      # straight double quotes
  x <- gsub("(^|[ (\\[])'", "\\1`", x)                           # opening single quote
  # Korean, Chinese and Japanese runs in a font that has the glyphs
  gsub("([ᄀ-ᇿ⺀-鿿가-힯豈-﫿＀-￯]+)", "{\\\\cjkfont \\1}", x, perl = TRUE)
}
has_human <- function(id) any(grepl("^human_", names(read.csv(harmonized_path(id), nrows = 1, encoding = "UTF-8"))))
dir_sym <- function(d) {
  d <- tolower(flat(d))
  if (grepl("^increase", d)) "+" else if (grepl("^decrease", d)) "$-$" else if (grepl("^none", d)) "0" else "$\\pm$"
}

ids <- names(cards)[order(!is_own(names(cards)), names(cards), method = "radix")]
# a: an answer or example had been seen before the card was written (all of
#    the author's studies); h: hand codes exist; s: shares respondents with
#    another card
shared <- instrument_of(ids) %in% names(which(table(instrument_of(names(cards))) > 1))
# 24 Nov 2026: flag a from the written rule (coder A, revision/coding/coded_cards.csv): own study or example answers read
cc <- read.csv("revision/coding/coded_cards.csv", colClasses = "character")
flags <- cbind(a = cc$examples_read_A[match(ids, cc$dataset)] != "0" | is_own(ids),
               h = vapply(ids, has_human, NA) | ids == "aks21_c2_nkbusiness",  # the author's study with two coders
               s = shared)
rownames(flags) <- ids
nl <- "\\newline "
cells <- t(vapply(ids, function(id) {
  c <- oc_study_read(cards[[id]]); m <- md[md$dataset == id, ]; r <- ct[ct$dataset == id, ]
  key <- cites$bibkey[match(id, cites$dataset)]
  if (!is_own(id) && is.na(key)) warning("No citation for ", id)
  place <- paste(c(if (!is.na(m$countries)) m$countries, if (!is.na(m$year_fielded)) gsub("-", "--", m$year_fielded)), collapse = ", ")
  lang <- strsplit(r$languages, "/")[[1]]
  fl <- flags[id, ]
  c(study = paste0(sprintf("\\texttt{%s}", gsub("_", "\\_\\allowbreak{}", id, fixed = TRUE)), nl,
                   if (!is.na(key)) sprintf("\\citep{%s}", key) else "author's study"),
    setting = paste0(if (nchar(place)) esc(place) else "not stated", nl, esc(m$design)),
    item = esc(cut_at(flat(m$item_english), 120)),
    contrast = esc(flat(m$contrast_short)),
    target = paste(vapply(c$constructs$target, function(t) sprintf("%s (%s)", esc(gsub("_", " ", t$name)), dir_sym(t$expected_direction)), ""),
                   collapse = nl),
    answers = paste0(format(r$n_text, big.mark = ","), nl, format(r$n_E, big.mark = ","), nl,
                     esc(if (length(lang) > 2) sprintf("%s +%d", lang[1], length(lang) - 1) else paste(lang, collapse = ", ")),
                     if (any(fl)) paste0(nl, paste(names(fl)[fl], collapse = " ")) else ""))
}, character(6)))
row_tex <- paste0(apply(cells, 1, paste, collapse = " & "), " \\\\")
heading <- function(lab) c(sprintf("\\multicolumn{6}{l}{\\textit{%s}} \\\\", lab), "\\addlinespace[2pt]")
own <- is_own(ids)
writeLines(enc2utf8(c(heading("Author's studies"), rbind(row_tex[own], "\\addlinespace[2pt]"),
                      heading("Public studies"), rbind(row_tex[!own], "\\addlinespace[2pt]"))),
           file.path(TEX, "studies_rows.tex"), useBytes = TRUE)
# approximate printed length of each column: LaTeX markup removed, a citation
# counted as 20 characters
visible <- function(x) {
  x <- gsub("\\\\citep\\{[^}]*\\}", strrep("x", 20), x)
  x <- gsub("\\\\newline ", " ", x); x <- gsub("\\\\allowbreak\\{\\}", "", x)
  x <- gsub("\\$-\\$", "-", x); x <- gsub("\\\\ldots\\{\\}", ".", x)
  x <- gsub("\\\\[a-zA-Z]+\\s?", "", x); nchar(gsub("[{}$]", "", x))
}
cat(sprintf("studies_rows.tex: %d rows (%d author's, %d public); flags a %d, h %d, s %d; items cut at 120 characters %d\n",
            nrow(cells), sum(own), sum(!own), sum(flags[, "a"]), sum(flags[, "h"]), sum(flags[, "s"]),
            sum(nchar(md$item_english) > 120, na.rm = TRUE)))
cat("mean printed characters per column:\n"); print(round(colMeans(apply(cells, 2, visible)), 1))
cat("mean characters of LaTeX source per column:\n"); print(round(colMeans(apply(cells, 2, nchar)), 1))

# Summary of the corpus by the extracted fields
tab <- function(x, lab) { t <- sort(table(x), decreasing = TRUE); c(sprintf("\\textit{%s} & \\\\", lab), sprintf("\\quad %s & %d \\\\", names(t), t)) }
us <- ifelse(is.na(md$countries), "Not stated", ifelse(grepl(";|countries|democracies", md$countries), "Several countries",
       ifelse(md$countries == "United States", "United States", "Another single country")))
yr <- suppressWarnings(as.integer(substr(md$year_fielded, 1, 4)))
period <- ifelse(is.na(yr), "Not stated in the card", ifelse(yr <= 2015, "2015 or earlier", ifelse(yr <= 2019, "2016--2019",
          ifelse(yr <= 2022, "2020--2022", "2023--2026"))))
period <- factor(period, levels = c("2015 or earlier", "2016--2019", "2020--2022", "2023--2026", "Not stated in the card"))
pt <- table(period)
# every target construct of every card, by the direction the original authors expected
dirs <- unlist(lapply(names(cards), function(id) vapply(oc_study_read(cards[[id]])$constructs$target, function(t) dir_sym(t$expected_direction), "")))
dt <- table(factor(c("+" = "the treatment raises it", "$-$" = "the treatment lowers it", "$\\pm$" = "the direction depends on a subgroup",
                     "0" = "no effect expected")[dirs],
                   levels = c("the treatment raises it", "the treatment lowers it", "the direction depends on a subgroup", "no effect expected")))
dt <- dt[dt > 0]
writeLines(enc2utf8(c(tab(md$design, "Design"), "\\addlinespace", tab(md$item_role, "What the open-text item asks for"), "\\addlinespace",
                      tab(us, "Where respondents live"), "\\addlinespace",
                      "\\textit{Year fielded (first year)} & \\\\", sprintf("\\quad %s & %d \\\\", names(pt), pt),
                      "\\addlinespace", "\\textit{Target constructs, by the direction the authors expected} & \\\\",
                      sprintf("\\quad %s & %d \\\\", names(dt), dt))),
           file.path(TEX, "catalog_summary_rows.tex"), useBytes = TRUE)
print(dt); cat("targets:", length(dirs), "\n")
print(table(md$design)); print(table(us)); print(pt)
cat("answers per study: median", median(ct$n_text[ct$dataset %in% names(cards)]), "range", range(ct$n_text[ct$dataset %in% names(cards)]), "\n")
