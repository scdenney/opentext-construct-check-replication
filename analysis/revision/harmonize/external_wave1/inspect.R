# Structural inspection only: names, classes, non-empty counts. Never prints text values.
options(warn = -1)
suppressWarnings(suppressMessages({library(data.table)}))
# Guarded table: prints only if levels are few and short (no free text can leak).
safe_table <- function(x, max_levels = 25, max_chars = 40) {
  u <- unique(as.character(x))
  if (length(u) > max_levels || any(nchar(u[!is.na(u)], type = "chars") > max_chars, na.rm = TRUE)) {
    cat("  [safe_table refused: ", length(u), " levels, max nchar ",
        max(nchar(u[!is.na(u)]), 0), "]\n", sep = "")
    return(invisible(NULL))
  }
  print(table(x, useNA = "ifany"))
}
describe <- function(d, label) {
  cat("=====", label, "rows:", nrow(d), "cols:", ncol(d), "\n")
  for (nm in names(d)) {
    x <- d[[nm]]
    cl <- class(x)[1]
    if (is.character(x)) {
      ne <- sum(!is.na(x) & nchar(trimws(x)) > 0)
      cat(sprintf("  %-40s chr  nonempty=%d unique=%d maxnchar=%d\n", nm, ne, length(unique(x)), suppressWarnings(max(nchar(x, allowNA=TRUE, type="bytes"), na.rm=TRUE))))
    } else {
      cat(sprintf("  %-40s %s  NA=%d unique=%d\n", nm, cl, sum(is.na(x)), length(unique(x))))
    }
  }
}
rd_tab <- function(f) as.data.frame(suppressWarnings(fread(f, encoding = "UTF-8", showProgress = FALSE, na.strings = NULL)), stringsAsFactors = FALSE)
# Decode invalid-UTF-8 strings: CP1252 (superset of Latin-1 for 0x80-0x9F), Latin-1 fallback.
fix_enc <- function(x) {
  x <- as.character(x); bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) { y <- iconv(x[bad], "CP1252", "UTF-8"); nay <- is.na(y); y[nay] <- iconv(x[bad][nay], "latin1", "UTF-8"); x[bad] <- y }
  Encoding(x[!is.na(x)]) <- "UTF-8"; x
}
nonempty <- function(x) { x <- as.character(x); !is.na(x) & nchar(trimws(x)) > 0 }
