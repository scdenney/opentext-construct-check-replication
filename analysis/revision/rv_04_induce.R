# ==============================================================================
# rv_04_induce.R -- A2: LLM-induced codebook on D, frozen, applied to E
# ==============================================================================
# Steps (run in order; each is resumable):
#   request   one induction request per dataset x split (D of that split only)
#   codebooks parse the drafts into codebook YAMLs, freeze a lock per split
#             (codebook file + exact prompt), export E-classification requests
#   import    score E classifications into caches -> measures per split
# Splits 1..N_SPLITS of the holdout manifest; split 1 is the primary split.
# Usage: Rscript analysis/revision/rv_04_induce.R <step> [dataset ...]
# ==============================================================================

source("analysis/revision/rv_config.R")
args <- commandArgs(trailingOnly = TRUE)
step <- args[1]
cards <- registered_cards()
ids <- if (length(args) > 1) args[-1] else names(cards)
IND_DIR <- file.path(JOBS, "induce")
CB_DIR <- file.path(ROOT, "revision/codebooks_induced")
dir.create(IND_DIR, recursive = TRUE, showWarnings = FALSE)
INDUCE_MODEL <- "qwen36_35b"
# Splits to induce/classify (env K_MAX, default all). External datasets were
# added late in the run and use split 1 only.
K_MAX <- as.integer(Sys.getenv("K_MAX", N_SPLITS))
OUT_TAG <- Sys.getenv("OUT_TAG", "")

prep <- function(id) {
  card_path <- cards[[id]]
  list(card_path = card_path, card = oc_study_read(card_path),
       s = oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json"))),
       d = { x <- read_harmonized(id); x[nchar(trimws(x$text)) > 0, ] })
}

if (step == "request") {
  lines <- character(0)
  for (id in ids) {
    p <- prep(id)
    for (k in seq_len(K_MAX)) {
      dk <- oc_discovery_data(p$d, p$s, k = k, log = ACCESS_LOG, purpose = paste("A2 induction split", k))
      f <- file.path(IND_DIR, id, sprintf("k%02d.jsonl", k))
      cjk <- mean(dk$lang %in% c("ko", "zh", "zh-Hans", "zh-Hant", "ja")) > 0.5
      oc_induce_request(p$card, dk, f, seed = 1000L + k, context = card_context(p$card),
                        max_chars = if (cjk) 10000L else 36000L)
      lines <- c(lines, opencheck:::read_lines_utf8(f))
    }
    message(id, ": ", N_SPLITS, " induction requests")
  }
  opencheck:::write_utf8(lines, file.path(IND_DIR, paste0("requests_all", OUT_TAG, ".jsonl")))
  cat(length(lines), "induction requests\n")
}

if (step == "codebooks") {
  res <- alice_fetch(paste0("induce", OUT_TAG, "_", INDUCE_MODEL))
  recs <- lapply(opencheck:::read_lines_utf8(res), jsonlite::fromJSON)
  status <- list()
  bundle <- character(0)
  for (id in ids) {
    p <- prep(id)
    for (k in seq_len(K_MAX)) {
      rid <- paste0("induce:", p$card$id, ":", 1000L + k)
      hit <- Filter(function(r) identical(r$id, rid), recs)
      cb_path <- file.path(CB_DIR, id, sprintf("k%02d.yaml", k))
      ok <- FALSE
      msg <- "no result"
      if (length(hit)) {
        tmp <- tempfile(fileext = ".jsonl")
        opencheck:::write_utf8(as.character(jsonlite::toJSON(hit[[1]], auto_unbox = TRUE)), tmp)
        msg <- tryCatch({
          if (!file.exists(cb_path)) oc_induce_import(tmp, p$card, cb_path, context = card_context(p$card))
          ok <- TRUE
          "ok"
        }, error = function(e) conditionMessage(e))
      }
      status[[paste(id, k)]] <- data.frame(dataset = id, k = k, ok = ok, msg = msg)
      if (!ok) next
      cb <- oc_codebook_read(cb_path)
      pr <- oc_prompt(cb, answer = "number")
      lock <- oc_freeze(file.path(LOCKS, sprintf("%s.a2.k%02d.json", id, k)), p$s,
                        components = c(codebook = cb_path), k = k, prompts = list(pr),
                        models = as.list(MODELS), decoding = list(temperature = 0, seed = 42,
                        max_tokens = 1, top_logprobs = 20, thinking = FALSE),
                        notes = paste("A2 induced codebook, split", k, "; induced by", MODELS[[INDUCE_MODEL]]))
      ek <- oc_estimation_data(p$d, p$s, lock, log = ACCESS_LOG, purpose = paste("A2 classify split", k))
      f <- file.path(IND_DIR, id, sprintf("classify_k%02d.jsonl", k))
      oc_export_requests(ek, pr, f, lock = lock)
      bundle <- c(bundle, opencheck:::read_lines_utf8(f))
    }
  }
  st <- do.call(rbind, status)
  write.csv(st, file.path(OUT, paste0("a2_induction_status", OUT_TAG, ".csv")), row.names = FALSE)
  print(aggregate(ok ~ dataset, data = st, FUN = sum))
  opencheck:::write_utf8(bundle, file.path(IND_DIR, paste0("classify_all", OUT_TAG, ".jsonl")))
  cat(length(bundle), "A2 classification requests\n")
}

if (step == "import") {
  run_models <- sub(paste0("^a2", OUT_TAG, "_"), "", list.files(file.path(JOBS, "submitted"), pattern = paste0("^a2", OUT_TAG, "_")))
  for (mk in intersect(run_models, names(MODELS))) {
    res <- alice_fetch(paste0("a2", OUT_TAG, "_", mk))
    system2("python3", c("-I", "analysis/revision/rv_split_results.py", res))
    by_prompt <- file.path(dirname(res), "by_prompt")
    be <- oc_backend_offline(MODELS[[mk]])
    for (id in ids) {
      p <- prep(id)
      for (k in seq_len(K_MAX)) {
        lp <- file.path(LOCKS, sprintf("%s.a2.k%02d.json", id, k))
        if (!file.exists(lp)) next
        lock <- oc_lock_read(lp)
        cb <- oc_codebook_read(file.path(CB_DIR, id, sprintf("k%02d.yaml", k)))
        pr <- oc_prompt(cb, answer = "number")
        ek <- oc_estimation_data(p$d, p$s, lock, log = ACCESS_LOG, purpose = paste("A2 import split", k))
        cache <- file.path(CACHE, "a2", id, mk, sprintf("k%02d.jsonl", k))
        dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
        rf <- file.path(by_prompt, paste0(pr$hash, ".jsonl"))
        if (!file.exists(rf)) next
        oc_import_results(rf, pr, be, cache)
        done <- if (!file.exists(cache)) character(0) else unique(vapply(opencheck:::read_jsonl(cache), function(e) as.character(e$id), ""))
        if (!length(done)) next
        r <- oc_classify(ek[ek$id %in% done, ], pr, be, cache, lock = lock, progress = FALSE)
        out <- file.path(RESULTS, "a2", id, mk)
        dir.create(out, recursive = TRUE, showWarnings = FALSE)
        write.csv(r, file.path(out, sprintf("k%02d.csv", k)), row.names = FALSE)
      }
      message(id, " / ", mk, " imported")
    }
  }
}
