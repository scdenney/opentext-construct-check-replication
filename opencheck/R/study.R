#' Read a study card
#'
#' A study card (YAML) states, before any text is read: the treatment, the
#' construct it is meant to activate, placebo constructs it should not move,
#' the open-text item, the unit, and the estimand. See
#' `system.file("templates", "study_card.yaml", package = "opencheck")`.
#' @param path Path to the YAML card.
oc_study_read <- function(path) {
  card <- read_yaml_utf8(path)
  oc_study_validate(card)
  card
}

study_required <- list(
  top = c("id", "title", "source", "text", "unit", "treatment", "constructs",
          "estimand", "languages", "provenance"),
  text = c("column", "prompt", "position"),
  treatment = c("variable", "contrast"),
  provenance = c("written_before_reading_text", "contamination")
)

#' Check a study card for required fields
#' @param card A list read from YAML.
oc_study_validate <- function(card) {
  miss <- setdiff(study_required$top, names(card))
  for (f in c("text", "treatment", "provenance")) {
    absent <- setdiff(study_required[[f]], names(card[[f]]))
    if (f %in% names(card) && length(absent)) {
      miss <- c(miss, paste0(f, ".", absent))
    }
  }
  target <- card$constructs$target
  if (!length(target) || !all(vapply(target, function(t) {
    all(c("name", "definition") %in% names(t))
  }, logical(1)))) {
    miss <- c(miss, "constructs.target (each with name and definition)")
  }
  if (length(miss)) {
    stop("Study card is missing: ", paste(miss, collapse = "; "), call. = FALSE)
  }
  if (!length(card$constructs$placebo)) {
    warning("Study card ", card$id, " lists no placebo constructs; the ",
            "false-positive check will be unavailable.", call. = FALSE)
  }
  if (!is.logical(card$provenance$written_before_reading_text)) {
    stop("provenance.written_before_reading_text must be true or false.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Hash of a study card's content (independent of key order)
#' @param card A list or a path to a YAML card.
oc_study_hash <- function(card) {
  if (is.character(card)) card <- oc_study_read(card)
  hash_object(card)
}

#' Register a study card before analysis
#'
#' Copies the card into `registry` under its id and hash and appends a line to
#' `registry/registry.jsonl`. Registering a changed card under an existing id
#' needs `amend` with a reason; the amendment is logged, not hidden.
#' @param path Path to the YAML card.
#' @param registry Registry directory.
#' @param amend `NULL`, or a short reason for changing an already registered
#'   card.
oc_study_register <- function(path, registry, amend = NULL) {
  card <- oc_study_read(path)
  h <- oc_study_hash(card)
  log <- file.path(registry, "registry.jsonl")
  prior <- if (file.exists(log)) {
    lapply(read_lines_utf8(log), jsonlite::fromJSON)
  } else list()
  same_id <- Filter(function(e) identical(e$id, card$id), prior)
  if (length(same_id)) {
    if (any(vapply(same_id, function(e) identical(e$hash, h), logical(1)))) {
      message("Card ", card$id, " is already registered with this content.")
      return(invisible(h))
    }
    if (is.null(amend)) {
      stop("Card ", card$id, " is already registered with different content. ",
           "Pass `amend = \"reason\"` to log an amendment.", call. = FALSE)
    }
  }
  dir.create(registry, recursive = TRUE, showWarnings = FALSE)
  file.copy(path, file.path(registry, paste0(card$id, ".", substr(h, 1, 12),
                                             ".yaml")), overwrite = FALSE)
  append_log(log, list(id = card$id, hash = h,
                       written_before_reading_text =
                         card$provenance$written_before_reading_text,
                       amendment = amend %||% ""))
  invisible(h)
}
