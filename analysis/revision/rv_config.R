# ==============================================================================
# rv_config.R -- shared settings for the revision analysis (split-first protocol)
# ==============================================================================
# Every script sources this file. Run from the project root with
#   LANG=en_US.UTF-8 Rscript analysis/revision/<script>.R
# ==============================================================================

suppressMessages(pkgload::load_all("~/Documents/github/resources/opencheck", quiet = TRUE))

ROOT       <- normalizePath(".")
HARMONIZED <- file.path(ROOT, "data/harmonized")
EXTERNAL   <- file.path(ROOT, "data/external")
CARDS      <- file.path(ROOT, "revision/study_cards")
REGISTRY   <- file.path(ROOT, "revision/registry")
SPLITS     <- file.path(ROOT, "revision/splits")
LOCKS      <- file.path(ROOT, "revision/locks")
JOBS       <- file.path(ROOT, "data/jobs")
RESULTS    <- file.path(ROOT, "data/results")
CACHE      <- file.path(ROOT, "data/cache")
OUT        <- file.path(ROOT, "analysis/output/revision")
ACCESS_LOG <- file.path(ROOT, "revision/access_log.jsonl")
ALICE_ROOT <- "/zfsstore/user/denneysc/opencheck"

SPLIT_SEED   <- 42L     # holdout: split 1 primary, 2-11 sensitivity
TWOFOLD_SEED <- 4242L
N_SPLITS     <- 11L
P_DISCOVERY  <- 0.3
PAIR_K       <- 8L
PAIR_SEED    <- 20261029L

# Planned roster, recorded in every lock (adding a model later needs a new,
# separately named lock). Open-weight models run on ALICE (vLLM, first-token
# logprobs); API models only within the budget (see revision/model_roster.md).
MODELS <- c(
  qwen36_35b = "Qwen/Qwen3.6-35B-A3B-FP8",
  qwen36_27b = "Qwen/Qwen3.6-27B-FP8",
  qwen38_27b = "Qwen/Qwen3.8-27B-FP8",
  gemma4_e4b = "google/gemma-4-E4B-it",
  gemma4_12b = "google/gemma-4-12B-it",
  gemma4_31b = "google/gemma-4-31B-it",
  exaone45_33b = "LGAI-EXAONE/EXAONE-4.5-33B-FP8",
  # jev = "typesafe/jev-1.13",   # set aside on 18 Nov 2026 (analysis/output/revision/set_aside/README.md)
  gpt6_luna = "openai/gpt-6-luna",
  claude_sonnet = "anthropic/claude-sonnet-5-5"
)
MODELS_PRIMARY <- c("qwen36_35b", "qwen36_27b", "gemma4_31b")

# Where ALICE loads a model from, when the repo id alone does not resolve
# offline (snapshot without refs/, or a separate cache). Identity stays the
# repo id in MODELS, which is what locks record.
MODEL_PATHS <- c(
  qwen36_27b = "/zfsstore/user/denneysc/.hf_cache/hub/models--Qwen--Qwen3.6-27B-FP8/snapshots/e89b16ebf1988b3d6befa7de50abc2d76f26eb09",
  qwen38_27b = "/zfsstore/user/denneysc/.hf_cache_oc/models--Qwen--Qwen3.8-27B-FP8/snapshots/017b9c7af6b5689d5dd426a76e0bc077eb5ca20a",
  gemma4_e4b = "/zfsstore/user/denneysc/.hf_cache_oc/models--google--gemma-4-E4B-it/snapshots/ee0ef6023621cff504d758262d4e04895a5af4a2",
  exaone45_33b = "/zfsstore/user/denneysc/.hf_cache_oc/models--LGAI-EXAONE--EXAONE-4.5-33B-FP8/snapshots/69570ad5d9a5159816ef267f2f4d37e36447f1be"
)

for (d in c(LOCKS, JOBS, RESULTS, CACHE, OUT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Registered cards: the latest registered version of each id.
registered_cards <- function() {
  log <- file.path(REGISTRY, "registry.jsonl")
  if (!file.exists(log)) return(list())
  e <- lapply(opencheck:::read_lines_utf8(log), jsonlite::fromJSON)
  ids <- unique(vapply(e, `[[`, "", "id"))
  out <- lapply(ids, function(i) {
    last <- Filter(function(x) x$id == i, e)
    h <- last[[length(last)]]$hash
    file.path(REGISTRY, paste0(i, ".", substr(h, 1, 12), ".yaml"))
  })
  names(out) <- ids
  out
}

# Context sentence shown to every model for a dataset (from the card).
card_context <- function(card) {
  ctx <- card$context %||% NULL
  if (!is.null(ctx)) return(ctx)
  paste0("These are open-ended answers from a survey experiment (", card$title,
         "). Respondents answered: \"", card$text$prompt,
         "\". Read each answer in its original language.")
}

harmonized_path <- function(id) {
  f <- file.path(HARMONIZED, paste0(id, ".csv"))
  if (file.exists(f)) return(f)
  hit <- Sys.glob(file.path(EXTERNAL, "*", paste0("harmonized_", id, ".csv")))
  if (length(hit)) return(hit[1])
  NA_character_
}

read_harmonized <- function(id) {
  f <- harmonized_path(id)
  if (is.na(f)) stop("No harmonized data for ", id, call. = FALSE)
  d <- read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
  if ("exclude" %in% names(d)) d <- d[!d$exclude %in% c(TRUE, "TRUE"), ]
  d$text[is.na(d$text)] <- ""
  d <- d[nchar(trimws(d$text)) > 0, ]   # the unit of analysis is a non-empty answer
  opencheck:::check_text_encoding(d$text)
  d$id <- as.character(d$id)
  d
}

# Theory-only prompts from a card: A1 presence and C rating for target and
# placebos, B pairwise for the (first) target.
a1_prompts <- function(card, card_path) {
  ctx <- card_context(card)
  cons <- c(card$constructs$target, card$constructs$placebo)
  role <- c(rep("target", length(card$constructs$target)),
            rep("placebo", length(card$constructs$placebo)))
  pr <- list()
  for (k in seq_along(cons)) {
    nm <- cons[[k]]$name
    pr[[paste0("presence__", nm)]] <- oc_prompt_presence(cons[[k]], ctx, card_path)
    pr[[paste0("rating__", nm)]] <- oc_prompt_rating(cons[[k]], ctx, card_path)
  }
  t1 <- card$constructs$target[[1]]
  pr[[paste0("pairwise__", t1$name)]] <- oc_prompt_pairwise(t1, ctx, card_path)
  attr(pr, "role") <- stats::setNames(c(rep(role, each = 2), "target"), names(pr))
  pr
}


# ALICE hardware per model (see revision/model_roster.md and the alice skill):
# A40 (gpu_lucdh) for FP8 checkpoints up to ~35B; A100-80G for bf16 Gemma 4 31B.
ALICE_HW <- list(
  default    = list(partition = "gpu_lucdh", account = "gpu_lucdh", gres = "gpu:a40:1", quant = ""),
  a100       = list(partition = "gpu-a100-80g", account = "humanities", gres = "gpu:a100:1", quant = ""),
  # Two A40s with tensor parallelism: bf16 models of 48-90 GB (e.g. Gemma 4 31B)
  a40x2      = list(partition = "gpu_lucdh", account = "gpu_lucdh", gres = "gpu:a40:2", quant = "",
                    venv = "/zfsstore/user/denneysc/venvs/vllm-terms", eager = TRUE, tp = 2L),
  gemma4_31b = list(partition = "gpu_lucdh", account = "gpu_lucdh", gres = "gpu:a40:1", quant = "fp8",
                    venv = "/zfsstore/user/denneysc/venvs/vllm-terms", eager = TRUE)
)
# Gemma 4, Qwen3.8 and EXAONE 4.5 need the newer stack (vLLM 0.30, transformers 5.17).
NEW_VENV <- "/zfsstore/user/denneysc/venvs/vllm-terms"
for (mk in c("gemma4_e4b", "gemma4_12b", "qwen38_27b", "exaone45_33b")) {
  ALICE_HW[[mk]] <- list(partition = "gpu_lucdh", account = "gpu_lucdh", gres = "gpu:a40:1",
                         quant = "", venv = NEW_VENV, eager = TRUE)
}

# Write a SLURM job for one model and one request bundle, copy runner + bundle
# to ALICE, submit, and return the job id. One rsync and one ssh per job.
alice_submit <- function(requests, run_name, model_key, time = "04:00:00",
                         max_len = 4096L, max_tokens = 1L, mem = "48G", hw = NULL) {
  hw <- hw %||% ALICE_HW[[model_key]] %||% ALICE_HW$default
  local_dir <- file.path(JOBS, "submitted", run_name)
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(requests, file.path(local_dir, "requests.jsonl"), overwrite = TRUE)
  file.copy(system.file("python", "oc_vllm_runner.py", package = "opencheck"),
            file.path(local_dir, "oc_vllm_runner.py"), overwrite = TRUE)
  remote <- file.path(ALICE_ROOT, run_name)
  oc_slurm_job(file.path(local_dir, "job.slurm"), root = remote,
               requests = "requests.jsonl", results = "results.jsonl",
               model = if (model_key %in% names(MODEL_PATHS)) MODEL_PATHS[[model_key]] else MODELS[[model_key]],
               job_name = paste0("oc-", substr(run_name, 1, 24)),
               partition = hw$partition, account = hw$account, gres = hw$gres,
               quantization = hw$quant, time = time, mem = mem,
               venv = hw$venv %||% "/zfsstore/user/denneysc/gei_textbooks/ocr_pipeline/.venv_vllm",
               enforce_eager = isTRUE(hw$eager), tensor_parallel = hw$tp %||% 1L,
               max_len = max_len, max_tokens = max_tokens)
  rs <- system2("rsync", c("-a", paste0("--rsync-path=", shQuote(paste0("mkdir -p ", remote, "/logs ", remote, "/results && rsync"))),
                           file.path(local_dir, c("requests.jsonl", "oc_vllm_runner.py", "job.slurm")),
                           paste0("alice3:", remote, "/")), stdout = TRUE, stderr = TRUE)
  out <- system2("ssh", c("alice3", shQuote(paste0("sbatch ", remote, "/job.slurm"))),
                 stdout = TRUE, stderr = TRUE)
  jid <- sub(".*Submitted batch job ([0-9]+).*", "\\1", grep("Submitted batch job", out, value = TRUE))
  if (!length(jid)) stop("sbatch failed: ", paste(c(rs, out), collapse = "\n"))
  cat(sprintf("%s\t%s\t%s\t%s\n", timestamp_now(), jid, run_name, model_key),
      file = file.path(JOBS, "submitted", "jobs.tsv"), append = TRUE)
  message("Submitted ", jid, " (", run_name, ", ", model_key, ")")
  jid
}

# Copy a run's results back from ALICE (API runs, e.g. *_jev, live locally).
alice_fetch <- function(run_name) {
  local_dir <- file.path(RESULTS, run_name)
  if (grepl("_(jev|gpt6_luna|claude_sonnet)$", run_name)) return(file.path(local_dir, "results.jsonl"))
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  system2("rsync", c("-a", paste0("alice3:", file.path(ALICE_ROOT, run_name, "results"), "/"),
                     paste0(local_dir, "/")), stdout = TRUE, stderr = TRUE)
  file.path(local_dir, "results.jsonl")
}

timestamp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

# Dataset pairs with identical prompts (same card constructs and context):
# their pairwise request ids carry a dataset prefix (re-run in batch b4), and
# unprefixed pairwise results for them are ignored as ambiguous.
PAIR_PREFIX <- c("busby2019_study1", "busby2019_study2", "egami2022_prison_exp2", "egami2022_prison_exp3")

# Strata for the blocked effect estimator and for permutation: the country
# when a study spans countries, plus any design column listed in EXTRA_STRATA
# because the assignment probability of the primary factor differs across it.
EXTRA_STRATA <- list(
  # the developer label was assigned with different probabilities in the
  # generic and the detailed policy condition (52.5% vs 48.6%; cells 1:2)
  broockman2026_housing = "project_char",
  # the low-cost and high-cost versions were separate samples
  miller2016_humanfinancialcosts = "level"
)
effect_strata <- function(id, e) {
  cols <- character(0)
  if ("country" %in% names(e) && length(unique(e$country)) > 1) cols <- "country"
  cols <- c(cols, intersect(EXTRA_STRATA[[id]] %||% character(0), names(e)))
  if (!length(cols)) return(NULL)
  do.call(paste, c(unname(as.list(e[cols])), sep = "|"))
}

# Documents for stm::fitNewDocuments() built from a dfm whose columns are the
# fitted model's vocabulary, in the model's order (dfm_match(x, vocab)).
# quanteda's convert(to = "stm") must not be used for this: it drops every word
# that does not occur in the answers being converted and renumbers the rest, so
# its word numbers point into a shorter list than the model's vocabulary and
# every word after the first absent one is read as a different word. Until
# 19 Nov 2026 the projections used convert(); see revision/deviations.md.
stm_documents <- function(m) {
  mm <- methods::as(m, "TsparseMatrix")
  i <- mm@i + 1L; j <- mm@j + 1L; x <- as.integer(mm@x)
  unname(lapply(split(seq_along(i), factor(i, levels = seq_len(nrow(mm)))), function(k) {
    o <- order(j[k]); rbind(j[k][o], x[k][o])
  }))
}

# Survey sample (instrument) of each study card. Several of the author's
# surveys carry more than one open-text item, answered by the same
# respondents (overlap checked on harmonized ids, 29 Oct); external studies
# are their own samples. Used to report headline rates per sample.
# 24 Nov 2026: rok2024_q40_election joins the SKTW 2024 group. It shares 1,966 of the
# 1,999 South Korean respondents of sktw_naturalization_2024 (found in the pre-submission
# review; the 29 Oct check had missed the pair). Groups: 117 -> 116.
instrument_of <- function(dataset) {
  dplyr::case_when(
    dataset %in% c("sktw_naturalization_2024", "roc2024_q74_national", "rok2024_q40_election") ~ "SKTW 2024",
    grepl("^aks21_", dataset) ~ "AKS21",
    grepl("^ifes2023_de_", dataset) ~ "IFES 2023 Germany",
    grepl("^ifes2023_sk_", dataset) ~ "IFES 2023 South Korea",
    grepl("^ifes2023_nk_", dataset) ~ "IFES 2023 NK migrants",
    grepl("^nwo26_", dataset) ~ "NWO 2026",
    grepl("^prc2026_", dataset) ~ "PRC 2025-26",
    # wave 3: public studies with two open items answered by the same respondents
    grepl("^hahn2024_oeq_", dataset) ~ "Hahn et al. 2024",
    grepl("^lupu2024_lawsofwar", dataset) ~ "Lupu & Wallace 2024",
    grepl("^andre2024_stockmodels_arbitrage", dataset) ~ "Andre et al. 2026, arbitrage",
    grepl("^nyhan2024_targetedcorrections_", dataset) ~ "Carey et al. 2025, Study 1",
    grepl("^sydnor2016_video", dataset) ~ "Sydnor 2016 (TESS)",
    grepl("^ishizuka2015_parenting", dataset) ~ "Ishizuka 2015 (TESS)",
    grepl("^pepin2016_couples", dataset) ~ "Pepin 2016 (TESS)",
    grepl("^bonilla2024_sourcecues_", dataset) ~ "Bonilla et al. 2022, Studies 3-4",
    grepl("^ces2016_uma_", dataset) ~ "Schaffner 2020 (CCES 2016 module)",
    TRUE ~ dataset)
}

# The one rounding rule for numbers printed in the SI tables: half away from
# zero on the decimal value, so 0.665 prints as 0.67 and 0.625 as 0.63
# (sprintf() and formatC() round the binary value and print 0.62 for 0.625).
# The 1e-9 absorbs binary error such as 0.665 * 100 = 66.4999... NA prints "--".
fmt_num <- function(x, d = 2) {
  r <- sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d
  ifelse(is.na(x), "--", formatC(r, format = "f", digits = d))
}

# Plain names for the SI tables
MODEL_LABEL <- c(qwen36_35b = "Qwen3.6-35B (reference)", gemma4_e4b = "Gemma 4 E4B", gemma4_31b = "Gemma 4 31B",
                 gpt6_luna = "GPT-6 Luna", majority = "Majority vote", stm = "Topic model",
                 gemma4_12b = "Gemma 4 12B", qwen36_27b = "Qwen3.6-27B")
ARM_LABEL <- c(A1_presence = "Yes/no question", A1_presence_soft = "Yes/no, probability", C_rating = "Rating",
               B_pairwise = "Pairwise scaling", A2 = "Induced codebook", D_topic = "Topic model")
plain_id <- function(x) gsub("_", " ", x, fixed = TRUE)
