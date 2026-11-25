#' Draw discovery/estimation splits
#'
#' Assigns every unit to a discovery fold (`"D"`) or an estimation fold
#' (`"E"`), stratified by treatment and any other columns given in `strata`.
#' Do this once, before any text is read. Within each stratum, units are
#' sorted by id before sampling, so the assignment does not depend on row
#' order. Split `k` uses seed `seed + k - 1`.
#'
#' @param data A data frame with one row per unit (respondent or task).
#' @param id Name of the unique id column.
#' @param strata Character vector of columns to stratify on (e.g. treatment,
#'   language). `NULL` for no stratification.
#' @param p_discovery Share of each stratum assigned to the discovery fold
#'   (`scheme = "holdout"`).
#' @param seed Integer seed for split 1.
#' @param n_splits Number of independent splits (for the K-split sensitivity
#'   analysis). Split 1 is the primary split.
#' @param scheme `"holdout"` (folds D and E) or `"twofold"` (folds A and B,
#'   each serving once as discovery and once as estimation).
#' @return An `oc_split` data frame with columns `id`, `split`, `fold`.
oc_split <- function(data, id, strata = NULL, p_discovery = 0.3, seed,
                     n_splits = 1L, scheme = c("holdout", "twofold")) {
  scheme <- match.arg(scheme)
  if (missing(seed)) stop("`seed` is required: a split must be reproducible.",
                          call. = FALSE)
  if (!id %in% names(data)) stop("Id column not found: ", id, call. = FALSE)
  missing_strata <- setdiff(strata, names(data))
  if (length(missing_strata)) {
    stop("Strata columns not found: ", paste(missing_strata, collapse = ", "),
         call. = FALSE)
  }
  if (p_discovery <= 0 || p_discovery >= 1) {
    stop("`p_discovery` must be strictly between 0 and 1.", call. = FALSE)
  }
  ids <- as.character(data[[id]])
  if (anyNA(ids) || any(ids == "")) stop("Ids must be non-missing.", call. = FALSE)
  if (anyDuplicated(ids)) stop("Ids must be unique.", call. = FALSE)

  key <- stratum_key(data, strata)
  out <- vector("list", n_splits)
  for (k in seq_len(n_splits)) {
    fold <- character(length(ids))
    with_seed(seed + k - 1L, {
      for (s in sort(unique(key), method = "radix")) {
        idx <- which(key == s)
        idx <- idx[order(ids[idx], method = "radix")]
        n <- length(idx)
        if (scheme == "holdout") {
          take <- idx[sample.int(n, round(p_discovery * n))]
          fold[idx] <- "E"
          fold[take] <- "D"
        } else {
          shuffled <- idx[sample.int(n, n)]
          n_a <- round(n / 2)
          fold[shuffled[seq_len(n_a)]] <- "A"
          fold[setdiff(shuffled, shuffled[seq_len(n_a)])] <- "B"
        }
      }
    })
    out[[k]] <- data.frame(id = ids, split = k, fold = fold,
                           stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, out)
  attr(res, "meta") <- list(
    scheme = scheme,
    p_discovery = if (scheme == "holdout") p_discovery else 0.5,
    seed = seed,
    n_splits = as.integer(n_splits),
    strata = strata %||% character(0),
    id_col = id,
    n = length(ids),
    data_hash = hash_object(list(ids = sort(ids, method = "radix"),
                                 key = key[order(ids, method = "radix")])),
    created = timestamp_utc(),
    opencheck_version = as.character(utils::packageVersion("opencheck"))
  )
  class(res) <- c("oc_split", "data.frame")
  res
}

stratum_key <- function(data, strata) {
  if (is.null(strata) || !length(strata)) return(rep("all", nrow(data)))
  parts <- lapply(strata, function(s) {
    v <- as.character(data[[s]])
    v[is.na(v)] <- "<NA>"
    v
  })
  do.call(paste, c(parts, sep = "\u001f"))
}

#' Hash of a split's assignments and settings
#'
#' The creation time is excluded, so the same split drawn twice has the same
#' hash.
#' @param split An `oc_split` object.
oc_split_hash <- function(split) {
  meta <- attr(split, "meta")
  meta$created <- NULL
  meta$opencheck_version <- NULL
  a <- as.data.frame(split)[, c("id", "split", "fold")]
  a <- a[order(a$split, a$id, method = "radix"), ]
  hash_object(list(meta = meta, id = a$id, split = a$split, fold = a$fold))
}

#' Write a split manifest once
#'
#' Splits are drawn once. If `path` already holds an identical split, nothing
#' is written; if it holds a different split, this is an error.
#' @param split An `oc_split` object.
#' @param path File path (JSON).
oc_split_write <- function(split, path) {
  if (!inherits(split, "oc_split")) stop("`split` must be an oc_split.", call. = FALSE)
  h <- oc_split_hash(split)
  if (file.exists(path)) {
    old <- oc_split_read(path)
    if (identical(oc_split_hash(old), h)) {
      message("Identical split manifest already at ", path, "; not rewritten.")
      return(invisible(path))
    }
    stop("A different split manifest already exists at ", path,
         ". Splits are drawn once; do not redraw after seeing results.",
         call. = FALSE)
  }
  a <- as.data.frame(split)[, c("id", "split", "fold")]
  write_json_file(list(meta = attr(split, "meta"), split_hash = h,
                       assignments = a), path)
}

#' Read a split manifest and check it has not been edited
#' @param path File path written by [oc_split_write()].
oc_split_read <- function(path) {
  x <- read_json_file(path)
  res <- as.data.frame(x$assignments, stringsAsFactors = FALSE)
  res$id <- as.character(res$id)
  res$split <- as.integer(res$split)
  meta <- x$meta
  meta$strata <- as.character(unlist(meta$strata %||% character(0)))
  attr(res, "meta") <- meta
  class(res) <- c("oc_split", "data.frame")
  if (!identical(oc_split_hash(res), x$split_hash)) {
    stop("Split manifest at ", path, " does not match its stored hash; ",
         "it was edited after it was written.", call. = FALSE)
  }
  res
}

#' @export
print.oc_split <- function(x, ...) {
  meta <- attr(x, "meta")
  cat("<oc_split> scheme=", meta$scheme, " n=", meta$n, " splits=",
      meta$n_splits, " seed=", meta$seed, "\n", sep = "")
  if (length(meta$strata)) cat("strata: ", paste(meta$strata, collapse = ", "),
                               "\n", sep = "")
  print(table(split = x$split, fold = x$fold))
  invisible(x)
}
