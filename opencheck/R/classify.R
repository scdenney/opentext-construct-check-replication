#' Classify responses with a frozen prompt, resumably
#'
#' Each result is appended to a JSON-lines cache as soon as it returns, keyed
#' by response id, prompt hash and backend settings, so an interrupted run
#' resumes where it stopped and a changed prompt never reuses old answers.
#'
#' Code probabilities come from the first generated token: the top
#' alternatives are mapped to the codes whose label they begin, summed per
#' code and renormalised. `p_valid` is the share of first-token probability
#' that falls on any valid code.
#'
#' @param data Data frame of responses (from [oc_discovery_data()] or
#'   [oc_estimation_data()]).
#' @param prompt An `oc_prompt` from [oc_prompt()] or [oc_prompt_presence()].
#' @param backend An `oc_backend`.
#' @param cache Path to the JSON-lines cache.
#' @param id,text Column names.
#' @param lock Optional lock (object or path). When given, the lock is
#'   verified and every source file behind the prompt must be one of its
#'   frozen components. Use it for every estimation-fold run.
#' @param progress Print progress every 100 responses.
#' @return A data frame: `id`, `model`, `prompt_hash`, `label`, `p_label`,
#'   `p_valid`, `valid`, plus one `p_<code>` column per code.
oc_classify <- function(data, prompt, backend, cache, id = "id", text = "text",
                        lock = NULL, progress = TRUE) {
  if (!inherits(prompt, "oc_prompt")) stop("`prompt` must be an oc_prompt.",
                                           call. = FALSE)
  if (!inherits(backend, "oc_backend")) stop("`backend` must be an oc_backend.",
                                             call. = FALSE)
  if (!is.null(lock)) check_prompt_frozen(prompt, lock)
  ids <- as.character(data[[id]])
  texts <- as.character(data[[text]])
  check_text_encoding(texts)
  done <- read_cache(cache, prompt$hash, backend)
  todo <- which(!ids %in% names(done))
  for (j in seq_along(todo)) {
    i <- todo[j]
    msgs <- list(list(role = "system", content = prompt$system),
                 list(role = "user", content = render_user(prompt, texts[i])))
    out <- backend$call(msgs)
    rec <- score_output(out, prompt$codes)
    entry <- c(list(id = ids[i], model = backend$id, prompt_hash = prompt$hash,
                    settings_hash = backend$settings_hash,
                    raw = out$content), rec,
               list(top = out$logprobs))
    append_log(cache, entry)
    done[[ids[i]]] <- entry
    if (progress && j %% 100 == 0) message(j, "/", length(todo), " classified")
  }
  rows <- lapply(ids, function(x) done[[x]])
  if (!length(ids)) {
    res <- data.frame(id = character(0), model = character(0), prompt_hash = character(0),
                      label = character(0), p_label = numeric(0), p_valid = numeric(0),
                      valid = logical(0), stringsAsFactors = FALSE)
    for (cd in prompt$codes) res[[paste0("p_", cd)]] <- numeric(0)
    return(res)
  }
  res <- data.frame(
    id = ids,
    model = backend$id,
    prompt_hash = prompt$hash,
    label = vapply(rows, function(r) r$label %||% NA_character_, ""),
    p_label = vapply(rows, function(r) as.numeric(r$p_label %||% NA), 0),
    p_valid = vapply(rows, function(r) as.numeric(r$p_valid %||% NA), 0),
    valid = vapply(rows, function(r) isTRUE(r$valid), logical(1)),
    stringsAsFactors = FALSE
  )
  for (cd in prompt$codes) {
    res[[paste0("p_", cd)]] <- vapply(rows, function(r) {
      as.numeric(r$probs[[cd]] %||% NA)
    }, 0)
  }
  if (!is.null(prompt$code_labels)) {
    lab <- prompt$code_labels
    res$label <- unname(lab[res$label])
    names(res)[match(paste0("p_", names(lab)), names(res))] <- paste0("p_", lab)
  }
  res
}

check_prompt_frozen <- function(prompt, lock) {
  if (is.character(lock)) lock <- oc_lock_read(lock)
  oc_lock_verify(lock)
  missing <- setdiff(unname(prompt$sources), lock$components$sha256)
  if (!length(prompt$sources) || length(missing)) {
    stop("The prompt's source files are not frozen in this lock. Freeze the ",
         "codebook or study card before classifying the estimation fold.",
         call. = FALSE)
  }
  frozen <- unlist(lock$prompt_hashes)
  if (length(frozen) && !prompt$hash %in% frozen) {
    stop("This prompt's text differs from the prompts frozen in the lock.",
         call. = FALSE)
  }
  invisible(TRUE)
}

read_cache <- function(cache, prompt_hash, backend) {
  recs <- read_jsonl(cache)
  keep <- vapply(recs, function(e) identical(e$prompt_hash, prompt_hash) &&
                   identical(e$settings_hash, backend$settings_hash), logical(1))
  recs <- recs[keep]
  if (!length(recs)) return(list())
  stats::setNames(recs, vapply(recs, function(e) as.character(e$id), ""))
}

normalize_token <- function(x) {
  tolower(gsub("^[[:space:]\"'`*]+|[[:space:]\"'`*.:,]+$", "", x))
}

score_output <- function(out, codes) {
  txt <- normalize_token(out$content)
  label <- if (txt %in% codes) txt else {
    hit <- codes[startsWith(txt, codes)]
    # A one-token answer is a prefix of the code it begins (max_tokens = 1).
    pre <- if (nzchar(txt)) codes[startsWith(codes, txt)] else character(0)
    if (length(hit) == 1) hit else if (length(pre) == 1) pre else NA_character_
  }
  probs <- stats::setNames(rep(NA_real_, length(codes)), codes)
  p_valid <- NA_real_
  p_shared <- NA_real_
  lp <- out$logprobs
  if (!is.null(lp) && nrow(lp)) {
    lp <- lp[order(-lp$logprob), , drop = FALSE]
    tok <- normalize_token(lp$token)
    p <- exp(lp$logprob)
    begins <- lapply(tok, function(t) if (nzchar(t)) codes[startsWith(codes, t)] else character(0))
    # A token that begins several codes (e.g. "c" for civic_* and cultural_*)
    # cannot be assigned to one; its mass is reported apart.
    shared <- lengths(begins) > 1
    mass <- vapply(codes, function(cd) {
      sum(p[!shared & vapply(begins, function(b) cd %in% b, logical(1))])
    }, 0)
    p_shared <- sum(p[shared])
    p_valid <- sum(mass) + p_shared
    if (!shared[1] && sum(mass) > 0) probs <- mass / sum(mass)
  }
  if (is.na(label) && !all(is.na(probs))) label <- names(probs)[which.max(probs)]
  list(label = label, valid = !is.na(label),
       p_label = if (is.na(label)) NA_real_ else unname(probs[label]),
       p_valid = p_valid, p_shared = p_shared, probs = as.list(probs))
}
