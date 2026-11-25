#' Request an LLM-induced codebook from discovery-fold responses
#'
#' Builds one generation request: the study context, the card's target
#' construct, and a random sample of discovery-fold responses. The model
#' drafts a codebook in opencheck's YAML schema. One code must carry the
#' target construct's name. Run the request with the runner in generation mode
#' (`--max-tokens 2000`), then read the draft with [oc_induce_import()].
#'
#' @param card A study card (list) or its path.
#' @param data Discovery-fold data (from [oc_discovery_data()]).
#' @param path Output JSON-lines path.
#' @param seed Seed for sampling responses.
#' @param n_max Maximum responses shown.
#' @param max_chars Character budget for the responses shown.
#' @param n_codes Range of substantive codes to ask for.
#' @param id,text Column names.
#' @param context Study context shown to the model.
oc_induce_request <- function(card, data, path, seed, n_max = 400L,
                              max_chars = 40000L, n_codes = c(4L, 8L),
                              id = "id", text = "text", context = "") {
  if (is.character(card)) card <- oc_study_read(card)
  ids <- sort(as.character(data[[id]]), method = "radix")
  tx <- stats::setNames(as.character(data[[text]]), as.character(data[[id]]))
  order <- with_seed(seed, sample(ids))
  shown <- character(0)
  used <- 0L
  for (i in order) {
    t <- gsub("[[:space:]]+", " ", substr(tx[[i]], 1, 400))
    if (!nzchar(trimws(t))) next
    if (used + nchar(t) > max_chars || length(shown) >= n_max) break
    shown <- c(shown, t)
    used <- used + nchar(t)
  }
  target <- card$constructs$target[[1]]
  system <- paste(c(
    "You are designing a codebook for classifying open-ended survey responses.",
    if (nzchar(context)) trimws(context),
    paste0("Open-ended question: ", card$text$prompt),
    paste0("The researchers' construct of interest is ", target$name, ": ",
           trimws(target$definition)),
    paste0("Read the sample of responses and write a codebook of ", n_codes[1],
           " to ", n_codes[2], " mutually exclusive codes that together cover ",
           "the main considerations respondents give, plus a residual code ",
           "none_of_above for uninformative answers. One code must be named ",
           target$name, " and capture the construct of interest as it appears ",
           "in these responses. Code names are lower-case identifiers. Each code ",
           "has a definition, a clarification (what it includes) and a ",
           "negative_clarification (what it excludes and which code to use ",
           "instead). Do not quote the responses."),
    paste0("Output only YAML in this form:\ncodebook:\n  name: ", card$id,
           "_induced\n  residual_code: none_of_above\ncodes:\n  code_name:\n",
           "    definition: ...\n    clarification: ...\n",
           "    negative_clarification: ...")
  ), collapse = "\n\n")
  user <- paste0("Sample of ", length(shown), " responses:\n\n",
                 paste0("- ", shown, collapse = "\n"))
  line <- as.character(jsonlite::toJSON(list(
    id = paste0("induce:", card$id, ":", seed),
    prompt_hash = hash_object(list(system = system, user = user)),
    system = system, user = user, codes = I("yaml")), auto_unbox = TRUE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_utf8(line, path)
  invisible(list(path = path, n_shown = length(shown), chars = used))
}

#' Turn an induced codebook draft into a codebook file
#'
#' Extracts the YAML from the model's answer, checks that the target code and
#' a residual code exist, records the provenance, and writes the codebook.
#' The file is validated with [oc_codebook_read()].
#' @param results Runner output for the induction request.
#' @param card The study card (list or path).
#' @param out Path for the codebook YAML.
#' @param context Study context to store in the codebook (shown to the
#'   classifier).
oc_induce_import <- function(results, card, out, context = "") {
  if (is.character(card)) card <- oc_study_read(card)
  recs <- lapply(read_lines_utf8(results), jsonlite::fromJSON,
                 simplifyVector = TRUE)
  rec <- recs[[length(recs)]]
  raw <- rec$raw %||% ""
  y <- sub("^[^`]*```(ya?ml)?[[:space:]]*", "", raw)
  y <- sub("```.*$", "", y)
  cb <- tryCatch(yaml::yaml.load(y), error = function(e) NULL)
  if (is.null(cb) || is.null(cb$codes)) {
    stop("The model's answer did not parse as a codebook.", call. = FALSE)
  }
  target <- card$constructs$target[[1]]$name
  if (!target %in% names(cb$codes)) {
    stop("The induced codebook lacks the target code ", target, ".",
         call. = FALSE)
  }
  if (is.null(cb$codes$none_of_above)) {
    cb$codes$none_of_above <- list(definition = "Uninformative, off-topic or empty.")
  }
  cb$codebook <- utils::modifyList(cb$codebook %||% list(),
                                   list(residual_code = "none_of_above",
                                        context = if (nzchar(context)) context else NULL,
                                        induced_by = rec$model %||% NA,
                                        induced_from = rec$id))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  write_utf8(yaml::as.yaml(cb), out)
  oc_codebook_read(out)
  invisible(out)
}
