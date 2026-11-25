# ==============================================================================
# rv_02_bundle_a1.R -- theory-only arms on the estimation fold (A1, B, C)
# ==============================================================================
# The prompts come from the registered study card alone (no data):
#   A1  yes/no presence for the target and each placebo construct
#   C   0-9 rating for the target and each placebo construct
#   B   pairwise comparison on the target construct (Bradley-Terry)
# They are frozen into a lock (card file + exact prompt hashes) before the
# estimation fold is opened. Writes one request bundle shared by all models,
# plus an index of prompt hashes for import.
# Usage: Rscript analysis/revision/rv_02_bundle_a1.R [dataset ...]
# ==============================================================================

source("analysis/revision/rv_config.R")

args <- commandArgs(trailingOnly = TRUE)
cards <- registered_cards()
ids <- if (length(args)) args else names(cards)
bundle_dir <- file.path(JOBS, "a1")
dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
index <- list()
for (id in ids) {
  card_path <- cards[[id]]
  card <- oc_study_read(card_path)
  f1 <- file.path(SPLITS, paste0(id, ".holdout.json"))
  if (!file.exists(f1)) { message("No split for ", id); next }
  s <- oc_split_read(f1)
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  pr <- a1_prompts(card, card_path)
  lock_path <- file.path(LOCKS, paste0(id, ".a1.k1.json"))
  lock <- oc_freeze(lock_path, s, components = c(card = card_path), k = 1L,
                    prompts = unname(pr), models = as.list(MODELS),
                    decoding = list(temperature = 0, seed = 42, max_tokens = 1,
                                    top_logprobs = 20, thinking = FALSE),
                    notes = "Theory-only arms (A1 presence, C rating, B pairwise) from the registered card")
  e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "A1/B/C bundle")
  out_dir <- file.path(bundle_dir, id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(pr)) {
    p <- pr[[nm]]
    f <- file.path(out_dir, paste0(nm, ".jsonl"))
    if (p$kind == "pairwise") {
      pairs <- oc_pairs(e$id, k = PAIR_K, holdout = 0.1, seed = PAIR_SEED)
      write.csv(pairs, file.path(out_dir, "pairs.csv"), row.names = FALSE)
      oc_export_pairs(pairs, e, p, f, lock = lock)
    } else {
      oc_export_requests(e, p, f, lock = lock)
    }
    index[[paste(id, nm)]] <- data.frame(
      dataset = id, prompt = nm, kind = p$kind, construct = p$construct,
      role = attr(pr, "role")[[nm]], prompt_hash = p$hash,
      n = length(opencheck:::read_lines_utf8(f)), file = f, stringsAsFactors = FALSE)
  }
  message(id, ": E = ", nrow(e), ", lock ", substr(lock$lock_hash, 1, 12))
}
idx <- do.call(rbind, index)
old <- file.path(bundle_dir, "index.csv")
if (file.exists(old)) {
  prev <- read.csv(old, stringsAsFactors = FALSE)
  idx <- rbind(prev[!prev$dataset %in% idx$dataset, ], idx)
}
write.csv(idx, old, row.names = FALSE)
# Concatenate file by file: the full bundle is larger than one R string can
# hold (2^31 - 1 bytes) once the corpus passes about 100 studies.
all_f <- file.path(bundle_dir, "requests_all.jsonl")
unlink(all_f); file.create(all_f)
for (f in idx$file) file.append(all_f, f)
print(aggregate(n ~ dataset + kind, data = idx, FUN = sum))
cat("total requests:", sum(idx$n), "\n")
