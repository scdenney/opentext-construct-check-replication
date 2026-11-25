#' A 0-9 rating prompt for one construct
#'
#' The single-response rating baseline. The answer is one digit, so the
#' expected rating is read from the first token's probabilities over 0-9.
#' @inheritParams oc_prompt_presence
oc_prompt_rating <- function(construct, context = "", source = NULL) {
  check_construct(construct)
  system <- paste(c(
    if (nzchar(context)) trimws(context),
    paste0("Rate how strongly the survey response expresses this consideration.\n",
           construct$name, ": ", trimws(construct$definition)),
    "Use a scale from 0 (not at all) to 9 (it is the main point of the response). Write only the digit."
  ), collapse = "\n\n")
  make_prompt(system = system,
              user = "Response: {text}\n\nRating (0-9):",
              codes = as.character(0:9), kind = "rating",
              sources = source_hash(source), construct = construct$name)
}

#' Expected rating from a classification result of a rating prompt
#' @param res Output of [oc_classify()] run with an [oc_prompt_rating()].
oc_rating_score <- function(res) {
  p <- as.matrix(res[, paste0("p_", 0:9)])
  as.vector(p %*% (0:9))
}

#' A pairwise comparison prompt for one construct
#'
#' The model sees two responses and answers 1 or 2: which expresses more of
#' the construct (DiGiuseppe & Flynn 2026). Use it with [oc_pairs()],
#' [oc_export_pairs()] and [oc_import_pairs()], then [oc_bt()].
#' @inheritParams oc_prompt_presence
oc_prompt_pairwise <- function(construct, context = "", source = NULL) {
  check_construct(construct)
  system <- paste(c(
    if (nzchar(context)) trimws(context),
    paste0("Compare two survey responses on this consideration.\n",
           construct$name, ": ", trimws(construct$definition)),
    "Judge only how much each response expresses the consideration, not its length, quality or tone. Write only 1 or 2."
  ), collapse = "\n\n")
  make_prompt(system = system,
              user = "Response 1: {text1}\n\nResponse 2: {text2}\n\nWhich response expresses the consideration more? Answer 1 or 2:",
              codes = c("1", "2"), kind = "pairwise",
              sources = source_hash(source), construct = construct$name)
}

#' Export pairwise requests for a batch run
#'
#' Every pair is asked in its sampled order; held-out pairs are also asked
#' reversed so position bias can be measured. Request ids are
#' `<pair>:fwd` and `<pair>:rev`.
#' @param pairs Output of [oc_pairs()].
#' @param data Data frame holding the texts (estimation fold).
#' @param prompt An [oc_prompt_pairwise()].
#' @param path Output JSON-lines path.
#' @param id,text Column names in `data`.
#' @param lock Optional lock; see [oc_classify()].
#' @param reverse Which pairs to also ask reversed: `"holdout"`, `"all"` or
#'   `"none"`.
#' @param id_prefix Optional prefix for request ids (e.g. the dataset id), so
#'   pair ids stay unique when two datasets share an identical prompt.
oc_export_pairs <- function(pairs, data, prompt, path, id = "id", text = "text",
                            lock = NULL, reverse = c("holdout", "all", "none"),
                            id_prefix = NULL) {
  reverse <- match.arg(reverse)
  if (!identical(prompt$kind, "pairwise")) stop("Use an oc_prompt_pairwise().",
                                                call. = FALSE)
  if (!is.null(lock)) check_prompt_frozen(prompt, lock)
  tx <- stats::setNames(as.character(data[[text]]), as.character(data[[id]]))
  check_text_encoding(tx)
  if (!all(c(pairs$id1, pairs$id2) %in% names(tx))) {
    stop("Some paired ids are not in `data`.", call. = FALSE)
  }
  rev_ok <- switch(reverse, holdout = pairs$holdout, all = rep(TRUE, nrow(pairs)),
                   none = rep(FALSE, nrow(pairs)))
  one <- function(pid, a, b, ord) {
    as.character(jsonlite::toJSON(list(
      id = paste0(if (!is.null(id_prefix)) paste0(id_prefix, ":") else "", pid, ":", ord),
      prompt_hash = prompt$hash,
      system = prompt$system, user = render_pair(prompt, tx[[a]], tx[[b]]),
      codes = I(prompt$codes)), auto_unbox = TRUE))
  }
  lines <- c(
    vapply(seq_len(nrow(pairs)), function(i) {
      one(pairs$pair[i], pairs$id1[i], pairs$id2[i], "fwd")
    }, ""),
    vapply(which(rev_ok), function(i) {
      one(pairs$pair[i], pairs$id2[i], pairs$id1[i], "rev")
    }, "")
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_utf8(lines, path)
  invisible(path)
}

#' Read pairwise results into Bradley-Terry outcomes
#'
#' For each pair, `y` is the probability that `id1` expresses more of the
#' construct: P("1") / (P("1") + P("2")) from the first token, flipped for
#' reversed requests and averaged over the orders asked. `order_gap` is the
#' fwd-minus-rev difference in that probability (position bias), available
#' for pairs asked both ways.
#' @param results Results JSON-lines file written by the runner.
#' @param pairs The pairs that were exported.
#' @param prompt The pairwise prompt.
#' @param id_prefix The prefix used at export, if any (records without it are
#'   ignored).
oc_import_pairs <- function(results, pairs, prompt, id_prefix = NULL) {
  recs <- read_jsonl(results)
  recs <- Filter(function(r) identical(r$prompt_hash, prompt$hash) &&
                   !isTRUE(r$skipped), recs)
  if (!is.null(id_prefix)) {
    pre <- paste0(id_prefix, ":")
    recs <- Filter(function(r) startsWith(as.character(r$id), pre), recs)
    recs <- lapply(recs, function(r) { r$id <- substring(r$id, nchar(pre) + 1L); r })
  }
  p1 <- vapply(recs, function(r) {
    lp <- top_df(r$top)
    s <- score_output(list(content = r$raw %||% "", logprobs = lp), c("1", "2"))
    as.numeric(s$probs[["1"]] %||% NA)
  }, 0)
  key <- vapply(recs, function(r) as.character(r$id), "")
  pair <- sub(":(fwd|rev)$", "", key)
  ord <- sub("^.*:", "", key)
  fwd <- stats::setNames(p1[ord == "fwd"], pair[ord == "fwd"])
  rev <- stats::setNames(1 - p1[ord == "rev"], pair[ord == "rev"])
  out <- pairs
  out$id1 <- as.character(out$id1)
  out$id2 <- as.character(out$id2)
  f <- fwd[as.character(out$pair)]
  r <- rev[as.character(out$pair)]
  out$y <- ifelse(is.na(f), r, ifelse(is.na(r), f, (f + r) / 2))
  out$order_gap <- f - r
  out
}

check_construct <- function(construct) {
  if (is.null(construct$name) || is.null(construct$definition)) {
    stop("A construct needs a name and a definition.", call. = FALSE)
  }
}

source_hash <- function(source) {
  if (is.null(source)) character(0) else c(source = hash_file(source))
}

render_pair <- function(prompt, a, b) {
  u <- enc2utf8(prompt$user)
  u <- paste(strsplit(u, "{text1}", fixed = TRUE)[[1]], collapse = enc2utf8(a))
  u <- paste(strsplit(u, "{text2}", fixed = TRUE)[[1]], collapse = enc2utf8(b))
  Encoding(u) <- "UTF-8"
  u
}
