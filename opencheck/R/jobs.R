#' Export a request bundle for a batch run
#'
#' Writes one JSON line per response (id, prompt hash, system and user
#' messages, valid codes) for the offline vLLM runner shipped in
#' `system.file("python", "oc_vllm_runner.py", package = "opencheck")`.
#' Only ids and text leave the machine.
#'
#' @inheritParams oc_classify
#' @param path Output JSON-lines path.
oc_export_requests <- function(data, prompt, path, id = "id", text = "text",
                               lock = NULL) {
  if (!inherits(prompt, "oc_prompt")) stop("`prompt` must be an oc_prompt.",
                                           call. = FALSE)
  if (!is.null(lock)) check_prompt_frozen(prompt, lock)
  ids <- as.character(data[[id]])
  texts <- as.character(data[[text]])
  check_text_encoding(texts)
  lines <- vapply(seq_along(ids), function(i) {
    as.character(jsonlite::toJSON(list(
      id = ids[i], prompt_hash = prompt$hash, system = prompt$system,
      user = render_user(prompt, texts[i]), codes = I(prompt$codes)
    ), auto_unbox = TRUE))
  }, "")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_utf8(lines, path)
  invisible(path)
}

#' A backend for results produced by a batch job
#'
#' Records the model and decoding settings the runner used. It never calls a
#' model: [oc_classify()] with this backend reads results already imported
#' with [oc_import_results()], and stops if any are missing.
#' @param model Model id passed to the runner.
#' @param temperature,seed,max_tokens,top_logprobs Settings the runner used.
#' @param engine Label for the inference engine.
oc_backend_offline <- function(model, temperature = 0, seed = 42L,
                               max_tokens = 1L, top_logprobs = 20L,
                               engine = "vllm-offline") {
  settings <- list(model = model, temperature = temperature, seed = seed,
                   max_tokens = max_tokens, top_logprobs = top_logprobs,
                   engine = engine)
  structure(list(id = model, settings = settings,
                 settings_hash = hash_object(settings),
                 call = function(messages) {
                   stop("Response not in the cache: run the batch job and ",
                        "oc_import_results() first.", call. = FALSE)
                 },
                 base_url = engine),
            class = "oc_backend")
}

#' Import batch results into a classification cache
#'
#' Scores each result line for `prompt` exactly as [oc_classify()] does and
#' appends it to `cache`. Lines for other prompts are ignored.
#' @param results Results JSON-lines file written by the runner.
#' @inheritParams oc_classify
#' @return Number of new entries.
oc_import_results <- function(results, prompt, backend, cache) {
  done <- read_cache(cache, prompt$hash, backend)
  new <- list()
  for (r in read_jsonl(results)) {
    if (!identical(r$prompt_hash, prompt$hash) || isTRUE(r$skipped)) next
    id <- as.character(r$id)
    if (!is.null(done[[id]]) || !is.null(new[[id]])) next
    lp <- top_df(r$top)
    out <- list(content = r$raw %||% "", logprobs = lp)
    new[[id]] <- c(list(id = id, model = backend$id, prompt_hash = prompt$hash,
                        settings_hash = backend$settings_hash, raw = out$content),
                   score_output(out, prompt$codes), list(top = lp))
  }
  if (length(new)) {
    lines <- vapply(new, function(e) as.character(jsonlite::toJSON(
      c(list(time = timestamp_utc()), e), auto_unbox = TRUE, digits = NA,
      null = "null", na = "null")), "")
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    write_utf8(lines, cache, append = TRUE)
  }
  length(new)
}

#' Write a SLURM script for the offline vLLM runner on ALICE
#'
#' Defaults follow the ALICE house conventions: the private LUCDH A40
#' partition, the shared vLLM virtual environment, an offline Hugging Face
#' cache, and per-job compile caches under `$TMPDIR`.
#'
#' @param path Where to write the script locally.
#' @param root Remote working directory holding the runner and the bundle.
#' @param requests,results File names (relative to `root`, and to
#'   `root/results` for `results`).
#' @param model Model id (must be in the Hugging Face cache).
#' @param job_name SLURM job name.
#' @param pilot Number of requests for a pilot run (0 = all).
#' @param partition,account,gres,cpus,mem,time SLURM resources.
#' @param max_len,top,seed,max_tokens Runner settings (`max_tokens > 1` for
#'   generation, e.g. codebook induction).
#' @param quantization Optional vLLM quantization, e.g. `"fp8"` for
#'   weight-only FP8 of a bf16 model on an A40.
#' @param enforce_eager Run vLLM without torch.compile (for architectures
#'   whose compile path fails).
#' @param tensor_parallel Number of GPUs to split the model across (request
#'   the same number in `gres`).
#' @param venv,hf_home,email Cluster paths and notification address.
oc_slurm_job <- function(path, root, requests, results, model, job_name,
                         pilot = 0L, partition = "gpu_lucdh",
                         account = "gpu_lucdh", gres = "gpu:a40:1", cpus = 8L,
                         mem = "48G", time = "08:00:00", max_len = 4096L,
                         top = 20L, seed = 42L, max_tokens = 1L,
                         quantization = "", enforce_eager = FALSE,
                         tensor_parallel = 1L,
                         venv = "/zfsstore/user/denneysc/gei_textbooks/ocr_pipeline/.venv_vllm",
                         hf_home = "/zfsstore/user/denneysc/.hf_cache",
                         email = "s.c.denney@hum.leidenuniv.nl") {
  tpl <- read_lines_utf8(system.file("templates", "vllm_job.slurm",
                                     package = "opencheck"))
  vals <- list(ROOT = root, REQUESTS = requests, RESULTS = results,
               MODEL = model, JOB_NAME = job_name, PILOT = pilot,
               PARTITION = partition, ACCOUNT = account, GRES = gres,
               CPUS = cpus, MEM = mem, TIME = time, MAX_LEN = max_len,
               TOP = top, SEED = seed, MAX_TOKENS = max_tokens,
               QUANT_ARG = if (nzchar(quantization)) paste("--quantization", quantization) else "",
               EAGER_ARG = if (isTRUE(enforce_eager)) "--enforce-eager" else "",
               TP_ARG = if (tensor_parallel > 1) paste("--tensor-parallel", tensor_parallel) else "",
               VENV = venv, HF_HOME = hf_home,
               EMAIL = email)
  for (k in names(vals)) {
    tpl <- gsub(paste0("{{", k, "}}"), as.character(vals[[k]]), tpl,
                fixed = TRUE)
  }
  if (any(grepl("{{", tpl, fixed = TRUE))) stop("Unfilled template field.",
                                                call. = FALSE)
  write_utf8(tpl, path)
  invisible(path)
}
