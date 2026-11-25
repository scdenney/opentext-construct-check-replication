#' Data for discovery and codebook work (discovery fold only)
#'
#' Returns only the rows assigned to the discovery fold of split `k`. This is
#' the only way discovery and codebook functions in this package see text, so
#' estimation-fold responses never shape the coding scheme.
#'
#' @param data A data frame containing the split's id column.
#' @param split An `oc_split` object (or a path to a manifest).
#' @param k Which split (1 = primary).
#' @param fold Discovery fold: `"D"` for holdout splits (the default), or
#'   `"A"`/`"B"` for two-fold splits (must be given).
#' @param log Optional path to a JSON-lines access log.
#' @param purpose Optional note recorded in the log.
oc_discovery_data <- function(data, split, k = 1L, fold = NULL, log = NULL,
                              purpose = NULL) {
  split <- as_split(split)
  fold <- discovery_fold(split, fold)
  rows <- fold_rows(data, split, k, fold)
  append_log(log, list(access = "discovery", split_hash = oc_split_hash(split),
                       k = k, fold = fold, n = length(rows),
                       purpose = purpose %||% ""))
  data[rows, , drop = FALSE]
}

#' Stop if any id is outside the discovery fold
#'
#' Use this for anything chosen by hand during discovery, such as few-shot
#' examples or rows quoted in a codebook.
#' @inheritParams oc_discovery_data
#' @param ids Ids to check.
oc_assert_discovery <- function(ids, split, k = 1L, fold = NULL) {
  split <- as_split(split)
  fold <- discovery_fold(split, fold)
  a <- split[split$split == k, ]
  ok <- as.character(ids) %in% a$id[a$fold == fold]
  if (!all(ok)) {
    bad <- as.character(ids)[!ok]
    stop(length(bad), " id(s) are not in discovery fold ", fold, " of split ",
         k, " (e.g. ", paste(utils::head(bad, 3), collapse = ", "), "). ",
         "Examples and codebook material must come from the discovery fold.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Data for estimation (estimation fold only, after the measurement is frozen)
#'
#' Requires a lockfile from [oc_freeze()] that was frozen against this split
#' and whose component files are unchanged. Returns only the estimation-fold
#' rows of the split the lock names.
#'
#' @inheritParams oc_discovery_data
#' @param lock A lock object or the path to a lockfile.
oc_estimation_data <- function(data, split, lock, log = NULL, purpose = NULL) {
  split <- as_split(split)
  if (missing(lock) || is.null(lock)) {
    stop("Estimation data need a lockfile: freeze the codebook, prompts and ",
         "models with oc_freeze() first.", call. = FALSE)
  }
  if (is.character(lock)) lock <- oc_lock_read(lock)
  oc_lock_verify(lock)
  if (!identical(lock$split_hash, oc_split_hash(split))) {
    stop("The lock was frozen against a different split manifest.",
         call. = FALSE)
  }
  rows <- fold_rows(data, split, lock$split_k, lock$estimation_fold)
  append_log(log, list(access = "estimation", split_hash = lock$split_hash,
                       lock_hash = lock$lock_hash, k = lock$split_k,
                       fold = lock$estimation_fold, n = length(rows),
                       purpose = purpose %||% ""))
  data[rows, , drop = FALSE]
}

as_split <- function(split) {
  if (is.character(split)) split <- oc_split_read(split)
  if (!inherits(split, "oc_split")) stop("`split` must be an oc_split or a ",
                                         "manifest path.", call. = FALSE)
  split
}

discovery_fold <- function(split, fold) {
  scheme <- attr(split, "meta")$scheme
  if (scheme == "holdout") {
    if (!is.null(fold) && fold != "D") {
      stop("In a holdout split the discovery fold is \"D\".", call. = FALSE)
    }
    return("D")
  }
  if (is.null(fold) || !fold %in% c("A", "B")) {
    stop("In a two-fold split, name the discovery fold: \"A\" or \"B\".",
         call. = FALSE)
  }
  fold
}

other_fold <- function(scheme, fold) {
  if (scheme == "holdout") return("E")
  if (fold == "A") "B" else "A"
}

fold_rows <- function(data, split, k, fold) {
  id_col <- attr(split, "meta")$id_col
  if (!id_col %in% names(data)) stop("Data lack the split's id column: ",
                                     id_col, call. = FALSE)
  if (!k %in% split$split) stop("Split ", k, " is not in the manifest.",
                                call. = FALSE)
  ids <- as.character(data[[id_col]])
  a <- split[split$split == k, ]
  unknown <- setdiff(ids, a$id)
  if (length(unknown)) {
    stop(length(unknown), " row(s) have ids that are not in the split ",
         "manifest; the data changed after the split was drawn.", call. = FALSE)
  }
  which(ids %in% a$id[a$fold == fold])
}
