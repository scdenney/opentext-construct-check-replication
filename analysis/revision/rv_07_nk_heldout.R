# ==============================================================================
# rv_07_nk_heldout.R -- does codebook iteration beat model size, held out?
# ==============================================================================
# The published result (kappa 0.40 -> 0.67 over codebook v1 -> v3) was measured
# on the same 2,005 human-coded items the codebook was revised against. Here:
#   - 10 random dev/test halves of the human-coded items (stratified by code);
#   - start from a theory-only codebook (revision/codebooks_nk/v0.yaml);
#   - two automated revision rounds: an LLM reads DEV disagreements only and
#     rewrites the definitions (no quoting of responses);
#   - every version is scored on the TEST half only; the published v3 is
#     scored too, as an in-sample reference; a model-size ladder is scored at
#     fixed codebooks.
# Steps: prepare | revise <r> | classify <r> | evaluate
# Usage: Rscript analysis/revision/rv_07_nk_heldout.R <step> [r]
# ==============================================================================

source("analysis/revision/rv_config.R")
args <- commandArgs(trailingOnly = TRUE)
step <- args[1]
r <- if (length(args) > 1) as.integer(args[2]) else NA_integer_
NK_DIR <- file.path(JOBS, "nk")
CB <- file.path(ROOT, "revision/codebooks_nk")
dir.create(NK_DIR, recursive = TRUE, showWarnings = FALSE)
SEEDS <- 10L
CODE_MAP <- c("1" = "economic_rationale", "2" = "normative_rationale",
              "3" = "other_rationale", "4" = "uninformative")
REV_MODEL <- "qwen36_35b"

load_nk <- function() {
  x <- read.csv(file.path(ROOT, "data/nk_entrepreneurship/whycheck.csv"),
                encoding = "UTF-8", stringsAsFactors = FALSE)
  txt <- ifelse(nzchar(trimws(x$eng.new %||% "")), x$eng.new, x$eng.original)
  d <- data.frame(id = as.character(x$respid), text = txt,
                  human = unname(CODE_MAP[as.character(x$valuenew)]),
                  stringsAsFactors = FALSE)
  d <- d[!is.na(d$text) & nzchar(trimws(d$text)) & !is.na(d$human), ]
  opencheck:::check_text_encoding(d$text)
  d
}

split_path <- file.path(SPLITS, "nk_heldout.holdout.json")
version_path <- function(k, rr) file.path(CB, sprintf("k%02d_rev%d.yaml", k, rr))

codebook_for <- function(k, rr) {
  if (rr == 0) return(file.path(CB, "v0.yaml"))
  p <- version_path(k, rr)
  if (file.exists(p)) p else codebook_for(k, rr - 1)
}

if (step == "prepare") {
  d <- load_nk()
  if (!file.exists(split_path)) {
    oc_split_write(oc_split(d, "id", strata = "human", p_discovery = 0.5,
                            seed = 7000L, n_splits = SEEDS), split_path)
  }
  lines <- character(0)
  for (v in c("v0.yaml", "v3_published.yaml")) {
    f <- file.path(NK_DIR, sub(".yaml$", ".jsonl", v))
    oc_export_requests(d, oc_prompt(file.path(CB, v)), f)
    lines <- c(lines, opencheck:::read_lines_utf8(f))
  }
  opencheck:::write_utf8(lines, file.path(NK_DIR, "round0.jsonl"))
  cat(nrow(d), "items;", length(lines), "round-0 requests\n")
}

classify_results <- function(d, cb_path, mk, run) {
  pr <- oc_prompt(cb_path)
  be <- oc_backend_offline(MODELS[[mk]])
  cache <- file.path(CACHE, "nk", mk, paste0(substr(pr$hash, 1, 16), ".jsonl"))
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  res <- alice_fetch(run)
  if (file.exists(res)) oc_import_results(res, pr, be, cache)
  have <- if (file.exists(cache)) unique(vapply(opencheck:::read_jsonl(cache), function(e) as.character(e$id), "")) else character(0)
  if (!length(have)) return(NULL)
  oc_classify(d[d$id %in% have, ], pr, be, cache, progress = FALSE)
}

if (step == "revise") {
  d <- load_nk()
  s <- oc_split_read(split_path)
  lines <- character(0)
  for (k in seq_len(SEEDS)) {
    cb_path <- codebook_for(k, r - 1)
    run <- if (r == 1) paste0("nk_round0_", REV_MODEL) else paste0("nk_round", r - 1, "_", REV_MODEL)
    res <- classify_results(d, cb_path, REV_MODEL, run)
    dev <- oc_discovery_data(d, s, k = k, log = ACCESS_LOG, purpose = paste("NK revision round", r))
    m <- merge(dev, res[, c("id", "label")], by = "id")
    wrong <- m[is.na(m$label) | m$label != m$human, ]
    shown <- opencheck:::with_seed(100L * r + k, wrong[sample(nrow(wrong), min(80, nrow(wrong))), ])
    cb_txt <- paste(opencheck:::read_lines_utf8(cb_path), collapse = "\n")
    system <- paste(
      "You revise a codebook used by a language model to classify open-ended survey answers.",
      "You see the current codebook and a sample of answers the model classified differently",
      "from trained human coders, with the human code and the model's code. Rewrite the",
      "definition, clarification and negative_clarification of the codes so that the model",
      "would agree with the human coders, fixing systematic misunderstandings rather than",
      "single cases. Keep the same four code names. Do not quote or paraphrase individual",
      "answers and do not add examples. Output only the full revised codebook as YAML in the",
      "same structure.", sep = " ")
    user <- paste0("Current codebook:\n\n", cb_txt, "\n\nDisagreements (", nrow(shown), " of ",
                   nrow(wrong), " on ", nrow(m), " development items):\n\n",
                   paste0("- Answer: ", gsub("[[:space:]]+", " ", substr(shown$text, 1, 300)),
                          " | human: ", shown$human, " | model: ", shown$label, collapse = "\n"))
    lines <- c(lines, as.character(jsonlite::toJSON(list(
      id = sprintf("nkrev:r%d:k%02d", r, k),
      prompt_hash = opencheck:::hash_object(list(system = system, user = user)),
      system = system, user = user, codes = I("yaml")), auto_unbox = TRUE)))
    message("seed ", k, ": ", nrow(wrong), "/", nrow(m), " dev disagreements")
  }
  opencheck:::write_utf8(lines, file.path(NK_DIR, sprintf("revise_r%d.jsonl", r)))
}

if (step == "classify") {
  d <- load_nk()
  res <- alice_fetch(sprintf("nk_revise%d_%s", r, REV_MODEL))
  recs <- lapply(opencheck:::read_lines_utf8(res), jsonlite::fromJSON)
  lines <- character(0)
  for (k in seq_len(SEEDS)) {
    hit <- Filter(function(x) identical(x$id, sprintf("nkrev:r%d:k%02d", r, k)), recs)
    out <- version_path(k, r)
    ok <- FALSE
    if (length(hit) && !file.exists(out)) {
      raw <- hit[[1]]$raw %||% ""
      y <- sub("```.*$", "", sub("^[^`]*```(ya?ml)?[[:space:]]*", "", raw))
      cb <- tryCatch(yaml::yaml.load(y), error = function(e) NULL)
      if (!is.null(cb$codes) && setequal(names(cb$codes), CODE_MAP)) {
        cb$codebook <- utils::modifyList(cb$codebook %||% list(), list(revised_by = MODELS[[REV_MODEL]],
                          revision_round = r, seed = k))
        cb$codebook$residual_code <- "uninformative"
        opencheck:::write_utf8(yaml::as.yaml(cb), out)
        ok <- tryCatch({ oc_codebook_read(out); TRUE }, error = function(e) { file.remove(out); FALSE })
      }
    }
    if (file.exists(out)) ok <- TRUE
    message("seed ", k, " round ", r, ": ", if (ok) "revised codebook" else "parse failed, previous version kept")
    f <- file.path(NK_DIR, sprintf("round%d_k%02d.jsonl", r, k))
    oc_export_requests(d, oc_prompt(codebook_for(k, r)), f)
    lines <- c(lines, opencheck:::read_lines_utf8(f))
  }
  lines <- unique(lines)
  opencheck:::write_utf8(lines, file.path(NK_DIR, sprintf("round%d.jsonl", r)))
  cat(length(lines), "round", r, "requests\n")
}

if (step == "evaluate") {
  d <- load_nk()
  s <- oc_split_read(split_path)
  rows <- list()
  runs <- list.files(file.path(JOBS, "submitted"), pattern = "^nk_round")
  for (run in runs) {
    rr <- as.integer(sub("^nk_round([0-9]+)_.*$", "\\1", run))
    mk <- sub("^nk_round[0-9]+_", "", run)
    versions <- if (rr == 0) c(v0 = file.path(CB, "v0.yaml"), v3_published = file.path(CB, "v3_published.yaml")) else NULL
    for (k in seq_len(SEEDS)) {
      vv <- if (rr == 0) versions else stats::setNames(codebook_for(k, rr), paste0("rev", rr))
      res_all <- lapply(vv, function(v) classify_results(d, v, mk, run))
      for (vn in names(vv)) {
        res <- res_all[[vn]]
        if (is.null(res)) next
        a <- s[s$split == k, ]
        for (fold in c("D", "E")) {
          ids <- a$id[a$fold == fold]
          m <- merge(d[d$id %in% ids, c("id", "human")], res[, c("id", "label")], by = "id")
          kp <- oc_kappa(m$label, m$human)
          rows[[length(rows) + 1]] <- data.frame(model = mk, version = vn, round = rr, seed = k,
            half = if (fold == "D") "dev" else "test", kappa = kp[["kappa"]],
            accuracy = kp[["agreement"]], n = kp[["n"]], stringsAsFactors = FALSE)
        }
      }
    }
  }
  # Size ladder: other models scored on v0, the published v3 and each seed's
  # final automated codebook (round 2, or round 1 where round 2 failed to parse).
  for (run in list.files(file.path(JOBS, "submitted"), pattern = "^nk_ladder_")) {
    mk <- sub("^nk_ladder_", "", run)
    for (k in seq_len(SEEDS)) {
      vv <- c(v0 = file.path(CB, "v0.yaml"), v3_published = file.path(CB, "v3_published.yaml"),
              rev_final = codebook_for(k, 2))
      for (vn in names(vv)) {
        res <- classify_results(d, vv[[vn]], mk, run)
        if (is.null(res)) next
        a <- s[s$split == k, ]
        for (fold in c("D", "E")) {
          ids <- a$id[a$fold == fold]
          m <- merge(d[d$id %in% ids, c("id", "human")], res[, c("id", "label")], by = "id")
          kp <- oc_kappa(m$label, m$human)
          rows[[length(rows) + 1]] <- data.frame(model = mk, version = vn, round = NA, seed = k,
            half = if (fold == "D") "dev" else "test", kappa = kp[["kappa"]],
            accuracy = kp[["agreement"]], n = kp[["n"]], stringsAsFactors = FALSE)
        }
      }
    }
  }
  ev <- unique(do.call(rbind, rows))
  write.csv(ev, file.path(OUT, "nk_heldout_eval.csv"), row.names = FALSE)
  summ <- aggregate(cbind(kappa, accuracy) ~ model + version + half, data = ev,
                    FUN = function(x) c(mean = mean(x), sd = sd(x)))
  print(summ)
}
