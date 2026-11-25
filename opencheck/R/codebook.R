#' Read a codebook
#'
#' Codes follow Halterman & Keith (2026): label, definition, clarification,
#' negative_clarification, examples. Everything that reaches the model lives
#' in the YAML: the study context, special rules and few-shot examples. Few-shot
#' examples must say where they came from: `id` for a response from the data
#' (checked against the discovery fold when the lock is frozen), or
#' `constructed: true` for a sentence written by the researcher.
#'
#' The older project format (a top-level `codes:` map only) is also read.
#'
#' @param path Path to the YAML codebook.
oc_codebook_read <- function(path) {
  y <- read_yaml_utf8(path)
  if (is.null(y$codes)) stop("Codebook has no `codes`.", call. = FALSE)
  meta <- y$codebook %||% list()
  codes <- lapply(names(y$codes), function(nm) {
    cd <- y$codes[[nm]]
    cd$label <- cd$label %||% nm
    cd
  })
  names(codes) <- vapply(codes, `[[`, "", "label")
  cb <- list(
    name = meta$name %||% sub("\\.ya?ml$", "", basename(path)),
    version = as.character(meta$version %||% NA),
    context = trimws(meta$context %||% ""),
    instructions = as.character(unlist(meta$instructions %||% character(0))),
    residual_code = meta$residual_code %||% NULL,
    codes = codes,
    fewshot = y$fewshot %||% list(),
    source = normalizePath(path),
    source_hash = hash_file(path)
  )
  class(cb) <- "oc_codebook"
  oc_codebook_validate(cb)
  cb
}

#' Check a codebook
#' @param cb An `oc_codebook`.
oc_codebook_validate <- function(cb) {
  labels <- names(cb$codes)
  if (anyDuplicated(labels)) stop("Duplicate code labels.", call. = FALSE)
  if (any(!grepl("^[a-z][a-z0-9_]*$", labels))) {
    stop("Code labels must be lower-case identifiers (a-z, 0-9, _): ",
         paste(labels[!grepl("^[a-z][a-z0-9_]*$", labels)], collapse = ", "),
         call. = FALSE)
  }
  no_def <- labels[vapply(cb$codes, function(cd) is.null(cd$definition),
                          logical(1))]
  if (length(no_def)) stop("Codes without a definition: ",
                           paste(no_def, collapse = ", "), call. = FALSE)
  if (!is.null(cb$residual_code) && !cb$residual_code %in% labels) {
    stop("residual_code is not one of the codes.", call. = FALSE)
  }
  for (ex in cb$fewshot) {
    if (!isTRUE(ex$code %in% labels)) {
      stop("Few-shot example has an unknown code: ", ex$code %||% "<none>",
           call. = FALSE)
    }
    if (is.null(ex$id) && !isTRUE(ex$constructed)) {
      stop("Each few-shot example needs an `id` (drawn from the data) or ",
           "`constructed: true` (written by the researcher).", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Ids of responses used as examples in a codebook
#'
#' Pass these to [oc_freeze()] so it can check that they all come from the
#' discovery fold.
#' @param cb An `oc_codebook`.
oc_codebook_example_ids <- function(cb) {
  ids <- vapply(cb$fewshot, function(ex) as.character(ex$id %||% NA), "")
  in_codes <- unlist(lapply(cb$codes, function(cd) {
    ex <- unlist(cd$examples, recursive = FALSE)
    vapply(ex, function(e) if (is.list(e)) as.character(e$id %||% NA) else NA_character_, "")
  }))
  ids <- c(ids, in_codes)
  unique(ids[!is.na(ids)])
}

#' Build a single-label classification prompt from a codebook
#'
#' The model is asked to answer with one code label and nothing else, so the
#' first generated token carries the label's probability.
#' @param cb An `oc_codebook` or a path to one.
#' @param answer `"label"`: the model writes the code name. `"number"`: codes
#'   are numbered 1-9 and the model writes the number, a single token, so code
#'   names that share a first token (economic_need, economic_gain) stay
#'   separable. Results are mapped back to code names.
oc_prompt <- function(cb, answer = c("label", "number")) {
  answer <- match.arg(answer)
  if (is.character(cb)) cb <- oc_codebook_read(cb)
  labels <- names(cb$codes)
  order <- c(setdiff(labels, cb$residual_code), cb$residual_code)
  if (answer == "number") return(numbered_prompt(cb, order))
  code_text <- vapply(order, function(l) {
    cd <- cb$codes[[l]]
    paste0("### ", l, "\nDefinition: ", trimws(cd$definition),
           if (!is.null(cd$clarification)) paste0("\nIncludes: ", trimws(cd$clarification)) else "",
           if (!is.null(cd$negative_clarification)) paste0("\nExcludes: ", trimws(cd$negative_clarification)) else "")
  }, "")
  shots <- vapply(cb$fewshot, function(ex) {
    paste0("Response: ", ex$text, "\nCode: ", ex$code)
  }, "")
  system <- paste(c(
    if (nzchar(cb$context)) cb$context,
    "## Codes", code_text,
    if (length(cb$instructions)) c("## Rules", paste0("- ", cb$instructions)),
    if (length(shots)) c("## Examples", shots),
    paste0("Answer with exactly one code from: ", paste(order, collapse = ", "),
           ". Write only the code.")
  ), collapse = "\n\n")
  make_prompt(system = system,
              user = paste0("Response: {text}\n\nValid codes: ",
                            paste(order, collapse = ", "), "\nCode:"),
              codes = order, kind = "classify",
              sources = c(codebook = cb$source_hash))
}

#' Build a yes/no presence prompt for one construct
#'
#' Use one presence prompt per target and placebo construct. Placebos coded
#' this way are separate judgments, so a rise in the target cannot lower them
#' mechanically.
#' @param construct A list with `name` and `definition` (as in a study card).
#' @param context Optional study context shown to the model.
#' @param source Optional path of the file the construct came from (a study
#'   card or codebook); its hash is checked against the lock.
oc_prompt_presence <- function(construct, context = "", source = NULL) {
  if (is.null(construct$name) || is.null(construct$definition)) {
    stop("A construct needs a name and a definition.", call. = FALSE)
  }
  system <- paste(c(
    if (nzchar(context)) trimws(context),
    paste0("Decide whether the survey response expresses this consideration.\n",
           construct$name, ": ", trimws(construct$definition)),
    "Answer yes or no. Write only yes or no."
  ), collapse = "\n\n")
  make_prompt(system = system,
              user = "Response: {text}\n\nDoes the response express this consideration? Answer yes or no:",
              codes = c("yes", "no"), kind = "presence",
              sources = if (!is.null(source)) c(source = hash_file(source)) else character(0),
              construct = construct$name)
}

make_prompt <- function(system, user, codes, kind, sources, construct = NULL) {
  p <- list(system = system, user = user, codes = codes, kind = kind,
            sources = sources, construct = construct)
  p$hash <- hash_object(list(system = system, user = user, codes = codes))
  class(p) <- "oc_prompt"
  p
}

render_user <- function(prompt, text) {
  parts <- strsplit(enc2utf8(prompt$user), "{text}", fixed = TRUE)[[1]]
  out <- paste0(parts[1], enc2utf8(text), if (length(parts) > 1) parts[2] else "")
  Encoding(out) <- "UTF-8"
  out
}

numbered_prompt <- function(cb, order) {
  # Digits for up to 9 codes, letters beyond that: both are single tokens.
  if (length(order) > 26) stop("Numbered answers support at most 26 codes.", call. = FALSE)
  num <- if (length(order) <= 9) as.character(seq_along(order)) else LETTERS[seq_along(order)]
  code_text <- vapply(seq_along(order), function(i) {
    cd <- cb$codes[[order[i]]]
    paste0("### ", num[i], ". ", order[i], "\nDefinition: ", trimws(cd$definition),
           if (!is.null(cd$clarification)) paste0("\nIncludes: ", trimws(cd$clarification)) else "",
           if (!is.null(cd$negative_clarification)) paste0("\nExcludes: ", trimws(cd$negative_clarification)) else "")
  }, "")
  shots <- vapply(cb$fewshot, function(ex) {
    paste0("Response: ", ex$text, "\nCode: ", num[match(ex$code, order)])
  }, "")
  menu <- paste0(num, " = ", order, collapse = ", ")
  system <- paste(c(
    if (nzchar(cb$context)) cb$context,
    "## Codes", code_text,
    if (length(cb$instructions)) c("## Rules", paste0("- ", cb$instructions)),
    if (length(shots)) c("## Examples", shots),
    paste0("Answer with the ", if (length(order) <= 9) "number" else "letter",
           " of exactly one code (", menu, "). Write only the ", if (length(order) <= 9) "number" else "letter", ".")
  ), collapse = "\n\n")
  p <- make_prompt(system = system,
                   user = paste0("Response: {text}\n\nCodes: ", menu, "\n",
                                 if (length(order) <= 9) "Number:" else "Letter:"),
                   codes = num, kind = "classify",
                   sources = c(codebook = cb$source_hash))
  p$code_labels <- stats::setNames(order, num)
  p
}
