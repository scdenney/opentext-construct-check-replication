# Shared helpers for harmonizing Steven's own open-text studies.
# BLINDNESS: nothing here prints, samples or summarizes open-text values;
# write_harmonized() reports counts only.
# Run with: LANG=en_US.UTF-8 Rscript <script>.R

OH_REPO <- "."  # path changed on recovery (2026-11-17): run from the repository root
OH_OUT <- Sys.getenv("HARMONIZE_OUT", file.path(OH_REPO, "data/harmonized"))  # path changed on recovery (2026-11-17)
OH_MANIFEST <- Sys.getenv("HARMONIZE_MANIFEST", "analysis/output/revision/harmonize_manifest")  # path changed on recovery (2026-11-17)
dir.create(OH_OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(OH_MANIFEST, showWarnings = FALSE, recursive = TRUE)

# Column names that must never reach an output file (identifiers, PII,
# timestamps, other free text). Anchored on "_" or string ends.
OH_PII_RE <- paste0("(^|_)(ip|ipaddress|email|e_mail|name|firstname|lastname|",
                    "recipient\\w*|pid|uid|psid|rid|prolific\\w*|panel_?id|",
                    "lat|lon|latitude|longitude|locationlatitude|locationlongitude|locationaccuracy|startdate|",
                    "enddate|recordeddate|date|timestamp|externalreference|",
                    "distributionchannel|useragent)($|_)")

# Count non-empty texts without exposing them.
oh_n_text <- function(x) sum(!is.na(x) & nchar(trimws(as.character(x))) > 0)

# df must hold id, country, lang, text, exclude; dataset is set from `id`.
# Extra columns: treatment / original treatment variable / task / chosen_* /
# other_* / human codes. Returns the counts invisibly and writes a manifest row.
write_harmonized <- function(df, id, manifest = list()) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  need <- c("id", "country", "lang", "text", "exclude")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("missing columns: ", paste(miss, collapse = ", "))
  df$dataset <- id
  df$id <- as.character(df$id)
  df$text <- ifelse(is.na(df$text), "", as.character(df$text))
  df$exclude <- as.logical(df$exclude)
  if (anyNA(df$exclude)) stop("exclude has NA")
  if (anyNA(df$id) || any(df$id == "")) stop("id has NA/empty")
  if (anyDuplicated(df$id)) stop("duplicate ids in ", id)
  pii <- grep(OH_PII_RE, tolower(names(df)), value = TRUE, perl = TRUE)
  if (length(pii)) stop("PII-like columns present: ", paste(pii, collapse = ", "))
  front <- c("id", "dataset", "country", "lang", "text", "exclude")
  df <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
  # factors -> character so labels, not codes, are written
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  path <- file.path(OH_OUT, paste0(id, ".csv"))
  if (!l10n_info()[["UTF-8"]]) stop("R is not in a UTF-8 locale; run with LANG=en_US.UTF-8")
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  # Encoding checks (counts only): no "<U+XXXX>" escapes, valid UTF-8, and the
  # text and id columns survive a round trip unchanged.
  n_esc <- as.integer(suppressWarnings(system2("grep", c("-c", shQuote("<U+"), shQuote(path)),
                              stdout = TRUE)))
  utf8_ok <- system2("iconv", c("-f", "UTF-8", "-t", "UTF-8", shQuote(path)),
                     stdout = FALSE, stderr = FALSE) == 0
  rb <- read.csv(path, colClasses = "character", na.strings = character(0),
                 encoding = "UTF-8", fileEncoding = "UTF-8")
  rt_ok <- identical(enc2utf8(rb$text), enc2utf8(df$text)) &&
    identical(rb$id, df$id) && nrow(rb) == nrow(df)
  if (n_esc != 0 || !utf8_ok || !rt_ok) {
    stop(sprintf("encoding check failed for %s: <U+ lines=%d, utf8=%s, roundtrip=%s",
                 id, n_esc, utf8_ok, rt_ok))
  }
  counts <- list(n_rows = nrow(df), n_text = oh_n_text(df$text),
                 grep_U_escapes = n_esc, utf8_valid = utf8_ok, roundtrip_identical = rt_ok,
                 n_excluded = sum(df$exclude),
                 n_text_included = oh_n_text(df$text[!df$exclude]))
  cat(sprintf("[%s] rows=%d nonempty_text=%d excluded=%d nonempty_text_included=%d\n",
              id, counts$n_rows, counts$n_text, counts$n_excluded,
              counts$n_text_included))
  cat(sprintf("  encoding: <U+ lines=%d utf8_valid=%s roundtrip_identical=%s\n",
              n_esc, utf8_ok, rt_ok))
  cat("  columns:", paste(names(df), collapse = ", "), "\n")
  m <- c(list(id = id, output = path, columns = names(df)), counts, manifest)
  jsonlite::write_json(m, file.path(OH_MANIFEST, paste0(id, ".json")),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(counts)
}

# Safe structural peek: names, classes, and for NON-text columns only, the
# number of distinct values. Never call on the open-text column's values.
oh_peek <- function(df, text_cols = character()) {
  for (v in names(df)) {
    x <- df[[v]]
    cat(sprintf("%-40s %-10s", v, class(x)[1]))
    if (!(v %in% text_cols)) cat(sprintf(" n_distinct=%d", length(unique(x))))
    else cat(sprintf(" [TEXT: nonempty=%d]", oh_n_text(x)))
    cat("\n")
  }
}
