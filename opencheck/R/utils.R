`%||%` <- function(x, y) if (is.null(x)) y else x

# Sort named lists recursively so the hash does not depend on key order.
canonicalize <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x)) && all(names(x) != "")) {
      x <- x[order(names(x), method = "radix")]
    }
    x <- lapply(x, canonicalize)
  } else if (is.character(x)) {
    x <- enc2utf8(x)
  }
  x
}

canonical_json <- function(x) {
  jsonlite::toJSON(canonicalize(x), auto_unbox = TRUE, digits = NA,
                   null = "null", na = "null")
}

hash_object <- function(x) {
  digest::digest(as.character(canonical_json(x)), algo = "sha256",
                 serialize = FALSE)
}

hash_file <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  digest::digest(file = path, algo = "sha256")
}

timestamp_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Run `expr` with a fixed seed and restore the caller's RNG state afterwards.
with_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  expr
}

# Read YAML as UTF-8 bytes so CJK text parses even in a C locale.
read_yaml_utf8 <- function(path) {
  txt <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(txt) <- "UTF-8"
  yaml::yaml.load(txt)
}

write_json_file <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_utf8(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, digits = NA,
                              null = "null", na = "null"), path)
  invisible(path)
}

# Write text as UTF-8 bytes; writeLines() and cat() re-encode to the native
# locale, which turns CJK text into <U+....> escapes in a C locale.
write_utf8 <- function(lines, path, append = FALSE) {
  con <- file(path, open = if (append) "ab" else "wb")
  on.exit(close(con))
  writeBin(charToRaw(paste0(enc2utf8(paste(lines, collapse = "\n")), "\n")),
           con)
}

read_lines_utf8 <- function(path) {
  txt <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(txt) <- "UTF-8"
  x <- sub("\r$", "", strsplit(txt, "\n", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

read_json_file <- function(path) {
  jsonlite::fromJSON(paste(read_lines_utf8(path), collapse = "\n"),
                     simplifyVector = TRUE, simplifyDataFrame = TRUE)
}

append_log <- function(log, entry) {
  if (is.null(log)) return(invisible(NULL))
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  write_utf8(jsonlite::toJSON(c(list(time = timestamp_utc()), entry),
                              auto_unbox = TRUE, digits = NA, null = "null",
                              na = "null"), log, append = TRUE)
  invisible(NULL)
}

# Text written from R in a C locale turns CJK characters into "<U+D574>"
# escapes. Refuse such text before it reaches a model.
check_text_encoding <- function(texts) {
  bad <- grepl("<U\\+[0-9A-Fa-f]{4,6}>", texts)
  if (any(bad)) {
    stop(sum(bad), " text(s) contain <U+XXXX> escapes: the data were written in ",
         "a non-UTF-8 locale. Rebuild them with LANG=en_US.UTF-8.", call. = FALSE)
  }
  invisible(TRUE)
}

# Parse a JSON-lines file in one call (much faster than line-by-line fromJSON).
read_jsonl <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- read_lines_utf8(path)
  if (!length(lines)) return(list())
  jsonlite::fromJSON(paste0("[", paste(lines, collapse = ","), "]"),
                     simplifyVector = FALSE)
}

top_df <- function(top) {
  if (!length(top)) return(NULL)
  data.frame(token = vapply(top, function(t) as.character(t$token %||% ""), ""),
             logprob = vapply(top, function(t) as.numeric(t$logprob %||% NA), 0),
             stringsAsFactors = FALSE)
}
