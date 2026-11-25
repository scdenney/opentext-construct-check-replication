# master.R -- entry point of the replication package for
# "What Were They Thinking? Open-Text Answers as a Construct Check in Survey Experiments".
#
# Run from the package root:
#   Rscript master.R               reporting: every table and figure, from the stored estimates
#   Rscript master.R estimation    re-estimates the treatment contrasts of every study whose answers are in
#                                  data/ and compares them with the stored estimates
#   Rscript master.R audit         the permutation audit and its checks (CPU, hours)
#
# The measurement stage (language-model labels, topic-model fits) is not rerun here: it needs a GPU
# cluster or a paid API. Its outputs are in data/results/a1/ and analysis/stm_fits/.

stage <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(stage)) stage <- "reporting"
stopifnot(stage %in% c("reporting", "estimation", "audit"), file.exists("analysis/revision/rv_config.R"))
Sys.setenv(LANG = "en_US.UTF-8", OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
started <- Sys.time()
out <- "analysis/output/revision"

has_answers <- function(i) {
  file.exists(file.path("data/harmonized", paste0(i, ".csv"))) ||
    length(Sys.glob(file.path("data/external", "*", paste0("harmonized_", i, ".csv")))) > 0
}
studies <- read.csv(file.path(out, "corpus_table.csv"), stringsAsFactors = FALSE)$dataset
present <- vapply(studies, has_answers, logical(1))

if (stage == "audit") {
  if (!all(present)) stop("The audit reads the answers of every study; ", sum(!present), " are not in data/ (docs/data_sources.csv).", call. = FALSE)
  if (system2("bash", c("analysis/revision/run_all.sh", "audit")) != 0) stop("run_all.sh audit failed", call. = FALSE)
}

if (stage == "reporting") {
  if (all(present)) {
    # every study's answers are here: the full reporting stage, as the article ran it
    if (system2("bash", c("analysis/revision/run_all.sh", "reporting")) != 0) stop("run_all.sh failed; see the logs in ", out, call. = FALSE)
  } else {
    # Six reporting scripts read the answers of studies that are linked, not deposited. They are skipped,
    # and the tables they write stay as stored: rv_31 (respondent flow), rv_33 (benchmark checks),
    # rv_15 (numbers quoted in the text), rv_27 (topic-model refits), rv_28 (prompt as treatment),
    # rv_18 (study catalog). The others run in the order of analysis/revision/run_all.sh.
    run <- c("rv_09_summary.R", "rv_32_registered_targets.R", "rv_34_displayed_content.R", "rv_16_robustness.R",
             "rv_24_note_checks.R", "rv_23_stm_tables.R", "rv_11_si_tables.R", "rv_10_note_figures.R",
             "rv_25_human_figure.R", "rv_12_si_figures.R")
    # rv_32 reads the answers of the two items of the 2026 four-country survey
    if (!all(present[c("nwo26_exp2", "nwo26_exp3_comprehension")])) run <- setdiff(run, "rv_32_registered_targets.R")
    cat(sum(present), "of", length(studies), "studies have their answers in data/.",
        "Running", length(run), "of 16 reporting scripts; the others need answers that are not here (see README.md).\n")
    for (s in run) {
      cat("==", s, "\n")
      log <- file.path(out, paste0("run_all_", sub("\\.R$", "", s), ".log"))
      if (system2("Rscript", file.path("analysis/revision", s), stdout = log, stderr = log) != 0) stop(s, " failed; see ", log, call. = FALSE)
    }
  }
}

if (stage == "estimation") {
  ids <- list.dirs("data/results/a1", recursive = FALSE, full.names = FALSE)
  has_data <- vapply(ids, has_answers, logical(1))
  cat(sum(has_data), "of", length(ids), "studies have their answers in data/;",
      "the others are rebuilt from the original deposits (docs/data_sources.csv).\n")
  Sys.setenv(OUT_NAME = "effects_a1_recomputed.csv")
  status <- system2("Rscript", c("analysis/revision/rv_06_effects.R", ids[has_data]))
  if (status != 0) stop("rv_06_effects.R failed", call. = FALSE)
  key <- c("dataset", "model", "arm", "construct", "contrast")
  new <- read.csv(file.path(out, "effects_a1_recomputed.csv"), stringsAsFactors = FALSE)
  old <- read.csv(file.path(out, "effects_a1.csv"), stringsAsFactors = FALSE)
  cov <- read.csv(file.path(out, "effects_stmcov.csv"), stringsAsFactors = FALSE)  # the topic model with the treatment covariate
  cols <- intersect(names(old), names(cov))
  old <- rbind(old[cols], cov[cols])
  m <- merge(new, old, by = key, suffixes = c(".new", ".stored"))
  d_est <- max(abs(m$estimate.new - m$estimate.stored), na.rm = TRUE)
  d_p <- max(abs(m$p_ri.new - m$p_ri.stored), na.rm = TRUE)
  cat(sprintf("%d recomputed rows; %d match a stored row; largest difference: estimate %.2e, p-value %.2e\n",
              nrow(new), nrow(m), d_est, d_p))
  writeLines(sprintf("%s  rows recomputed %d, matched %d, max |estimate diff| %.3e, max |p diff| %.3e",
                     format(Sys.time(), "%Y-%m-%d %H:%M"), nrow(new), nrow(m), d_est, d_p),
             file.path(out, "estimation_check.log"))
}

writeLines(capture.output(sessionInfo()), file.path(out, "session_info_master.log"))
cat(sprintf("%s stage finished in %s.\n", stage, format(round(Sys.time() - started, 1))))
