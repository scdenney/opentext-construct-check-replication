# ==============================================================================
# rv_13b_stm_diagnostic.R -- does the chosen topic track the construct?
# ==============================================================================
# Descriptive check on the estimation fold, after the STM arm is frozen and
# estimated (nothing here feeds back into any measure). For each target:
#   r_chosen  response-level correlation between the chosen topic's proportion
#             and the reference model's yes/no label for the target
#   r_best    the largest such correlation over all topics (hindsight)
#   best_is_chosen  whether the labeler picked that topic
# Separates "the labeler picked the wrong topic" from "no topic tracks the
# construct". Writes analysis/output/revision/tables/stm_topic_diagnostic.csv.
# ==============================================================================

source("analysis/revision/rv_config.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
source_lines <- readLines("analysis/revision/rv_13_stm_arm.R")
# reuse the tokenizer definitions from the STM arm
eval(parse(text = source_lines[grep("^SW_KO <-", source_lines):(grep("^choose_K <-", source_lines) - 1)]))
STM_DIR <- file.path(JOBS, "stm")
choice <- read.csv(file.path(STM_DIR, "topic_choice.csv"))
choice <- choice[!grepl("@", choice$unit), ]

rows <- mclapply(unique(choice$dataset), function(id) {
  ch <- choice[choice$dataset == id, ]
  # a study without reference-model labels yet has nothing to compare with
  if (!file.exists(file.path(RESULTS, "a1", id, "qwen36_35b", "measures.csv"))) return(NULL)
  fit <- readRDS(file.path(STM_DIR, paste0(id, ".stm.rds")))
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  e <- oc_estimation_data(d, s, file.path(LOCKS, paste0(id, ".stm.k1.json")),
                          log = ACCESS_LOG, purpose = "stm diagnostic")
  lang <- if ("lang" %in% names(e)) e$lang else rep("en", nrow(e))
  m <- dfm_match(dfm(tokens_for(e$text, lang, fit$sw_ko)), fit$vocab)
  ok <- rowSums(m) > 0
  theta <- fitNewDocuments(model = fit$mod, documents = stm_documents(m[ok, ]), newData = NULL,
                           origData = NULL, prevalence = NULL, verbose = FALSE)$theta
  yn <- read.csv(file.path(RESULTS, "a1", id, "qwen36_35b", "measures.csv"))
  do.call(rbind, lapply(seq_len(nrow(ch)), function(i) {
    col <- paste0("yes__", ch$construct[i])
    if (!col %in% names(yn)) return(NULL)
    y <- yn[[col]][match(e$id[ok], yn$id)]
    r <- suppressWarnings(apply(theta, 2, function(t) cor(t, y, use = "complete.obs")))
    data.frame(dataset = id, construct = ch$construct[i], K = fit$K, chosen = ch$topic[i],
               r_chosen = if (ch$topic[i] > 0) r[ch$topic[i]] else NA_real_,
               r_best = max(r, na.rm = TRUE), best = which.max(r),
               best_is_chosen = ch$topic[i] == which.max(r), share_yes = mean(y, na.rm = TRUE))
  }))
}, mc.cores = 8)
bad <- vapply(rows, inherits, NA, "try-error")
if (any(bad)) message("Failed: ", paste(unique(choice$dataset)[bad], collapse = ", "))
out <- do.call(rbind, rows[!bad])
write.csv(out, file.path(OUT, "tables", "stm_topic_diagnostic.csv"), row.names = FALSE)
cat(sprintf("targets %d; labeler picked a topic for %d; picked the best-correlated topic in %d (%.0f%%)\n",
            nrow(out), sum(out$chosen > 0), sum(out$best_is_chosen, na.rm = TRUE),
            100 * mean(out$best_is_chosen[out$chosen > 0], na.rm = TRUE)))
cat(sprintf("median r with the yes/no label: chosen topic %.2f, best topic in hindsight %.2f\n",
            median(out$r_chosen, na.rm = TRUE), median(out$r_best, na.rm = TRUE)))
