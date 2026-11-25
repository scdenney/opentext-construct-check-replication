#' Install the opencheck skill for a coding agent
#'
#' The package ships a guided workflow (a "skill") that takes a researcher
#' through the steps of the check in order: the study card, the split, the
#' topic model on the discovery fold, the yes/no questions, the lock, the
#' estimates and the report. This copies the skill folder (`SKILL.md` and
#' `reference/`) to the folder from which the agent reads skills. The same
#' folder works in Claude Code and Codex.
#'
#' @param agent `"claude"` installs to `~/.claude/skills/opencheck`,
#'   `"codex"` to `~/.codex/skills/opencheck`. Ignored when `path` is given.
#' @param path Optional destination folder for the skill itself (for example
#'   a project's `.claude/skills/opencheck`). Overrides `agent`.
#' @param overwrite Replace an existing folder at the destination. Without it,
#'   an existing folder is an error.
#' @return The destination folder, invisibly.
#' @export
oc_skill_install <- function(agent = c("claude", "codex"), path = NULL,
                             overwrite = FALSE) {
  agent <- match.arg(agent)
  src <- system.file("skills", "opencheck", package = "opencheck")
  if (!nzchar(src) || !file.exists(file.path(src, "SKILL.md"))) {
    stop("The skill is missing from the installed package.", call. = FALSE)
  }
  dest <- if (is.null(path)) {
    file.path(path.expand("~"), paste0(".", agent), "skills", "opencheck")
  } else {
    path.expand(path)
  }
  if (file.exists(dest)) {
    if (!isTRUE(overwrite)) {
      stop("A folder already exists at ", dest, ". Pass `overwrite = TRUE` ",
           "to replace it.", call. = FALSE)
    }
    unlink(dest, recursive = TRUE)
  }
  files <- list.files(src, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  for (d in unique(dirname(file.path(dest, files)))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  ok <- file.copy(file.path(src, files), file.path(dest, files))
  if (!all(ok)) {
    stop("Could not copy: ", paste(files[!ok], collapse = ", "), call. = FALSE)
  }
  message("Installed the opencheck skill to ", dest)
  invisible(dest)
}
