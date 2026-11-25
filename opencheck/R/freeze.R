#' Freeze the measurement before the estimation fold is read
#'
#' Hashes every file that defines the measurement (codebook YAML, prompt
#' templates, pairwise question, and so on), records the models and decoding
#' settings, and checks that every hand-picked example comes from the
#' discovery fold. The lockfile is written once: freezing different content
#' to the same path is an error, so changing the measurement after the fact
#' needs a new, separately named lock.
#'
#' @param path Where to write the lockfile (JSON).
#' @param split The `oc_split` (or manifest path) the measurement was built on.
#' @param components Named character vector of file paths to hash.
#' @param k The split the measurement was built on (1 = primary).
#' @param discovery_fold `"D"` for holdout splits; `"A"` or `"B"` for
#'   two-fold splits.
#' @param example_ids Ids of any responses used as few-shot or codebook
#'   examples. All must be in the discovery fold.
#' @param models Model identifiers with exact revisions (list or data frame).
#' @param decoding Decoding settings (temperature, seed, max_tokens, ...).
#' @param prompts Optional list of `oc_prompt` objects (or their hashes) whose
#'   exact text is frozen. When the lock lists prompts, classification and
#'   export with this lock accept only those prompts.
#' @param notes Free-text notes stored in the lock.
oc_freeze <- function(path, split, components, k = 1L, discovery_fold = NULL,
                      example_ids = NULL, models = NULL, decoding = NULL,
                      prompts = NULL, notes = NULL) {
  split <- as_split(split)
  if (missing(components) || !length(components) || is.null(names(components)) ||
      any(names(components) == "")) {
    stop("`components` must be a named vector of file paths.", call. = FALSE)
  }
  scheme <- attr(split, "meta")$scheme
  dfold <- discovery_fold(split, discovery_fold)
  if (!k %in% split$split) stop("Split ", k, " is not in the manifest.",
                                call. = FALSE)
  if (length(example_ids)) oc_assert_discovery(example_ids, split, k, dfold)

  lock_dir <- normalizePath(dirname(path), mustWork = FALSE)
  comp <- data.frame(
    name = names(components),
    path = vapply(components, rel_to, character(1), base = lock_dir),
    sha256 = vapply(components, hash_file, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  comp <- comp[order(comp$name, method = "radix"), ]
  lock <- list(
    lock_version = 1L,
    split_hash = oc_split_hash(split),
    split_k = as.integer(k),
    scheme = scheme,
    discovery_fold = dfold,
    estimation_fold = other_fold(scheme, dfold),
    components = comp,
    n_example_ids = length(example_ids),
    example_ids_hash = if (length(example_ids)) {
      hash_object(sort(as.character(example_ids), method = "radix"))
    } else NULL,
    models = models,
    decoding = decoding,
    prompt_hashes = prompt_hashes(prompts),
    notes = notes
  )
  lock$lock_hash <- lock_hash(lock)
  lock$created <- timestamp_utc()
  lock$opencheck_version <- as.character(utils::packageVersion("opencheck"))

  if (file.exists(path)) {
    old <- oc_lock_read(path)
    if (identical(old$lock_hash, lock$lock_hash)) {
      message("Identical lock already at ", path, "; not rewritten.")
      return(invisible(old))
    }
    stop("A different lock already exists at ", path, ". A frozen measurement ",
         "is not edited; write a new lock under a new name and report the ",
         "change.", call. = FALSE)
  }
  write_json_file(lock, path)
  oc_lock_read(path)
}

#' Read a lockfile and check it has not been edited
#' @param path Lockfile path.
oc_lock_read <- function(path) {
  lock <- read_json_file(path)
  lock$components <- as.data.frame(lock$components, stringsAsFactors = FALSE)
  if (!identical(lock_hash(lock), lock$lock_hash)) {
    stop("Lockfile ", path, " does not match its stored hash; it was edited ",
         "after freezing.", call. = FALSE)
  }
  attr(lock, "dir") <- normalizePath(dirname(path))
  lock
}

#' Check that every frozen file is unchanged
#' @param lock A lock from [oc_lock_read()] or [oc_freeze()].
oc_lock_verify <- function(lock) {
  if (is.character(lock)) lock <- oc_lock_read(lock)
  base <- attr(lock, "dir") %||% getwd()
  comp <- lock$components
  now <- vapply(seq_len(nrow(comp)), function(i) {
    p <- resolve_from(comp$path[i], base)
    if (!file.exists(p)) return(NA_character_)
    hash_file(p)
  }, character(1))
  bad <- is.na(now) | now != comp$sha256
  if (any(bad)) {
    stop("Frozen component(s) changed or missing since the lock was made: ",
         paste(comp$name[bad], collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

lock_hash <- function(lock) {
  lock$lock_hash <- NULL
  lock$created <- NULL
  lock$opencheck_version <- NULL
  comp <- lock$components
  lock$components <- list(name = comp$name, path = comp$path,
                          sha256 = comp$sha256)
  hash_object(lock)
}

rel_to <- function(path, base) {
  p <- normalizePath(path, mustWork = TRUE)
  prefix <- paste0(base, .Platform$file.sep)
  if (startsWith(p, prefix)) substring(p, nchar(prefix) + 1L) else p
}

resolve_from <- function(path, base) {
  p <- if (grepl("^(/|[A-Za-z]:)", path)) path else file.path(base, path)
  if (!file.exists(p)) {
    alt <- file.path(dirname(base), basename(dirname(path)), basename(path))
    if (file.exists(alt)) return(alt)
  }
  p
}

prompt_hashes <- function(prompts) {
  if (is.null(prompts) || !length(prompts)) return(NULL)
  if (inherits(prompts, "oc_prompt")) prompts <- list(prompts)
  h <- vapply(prompts, function(p) if (inherits(p, "oc_prompt")) p$hash else as.character(p), "")
  sort(unique(h), method = "radix")
}
