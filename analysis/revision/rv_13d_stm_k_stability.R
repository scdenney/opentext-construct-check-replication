# ==============================================================================
# rv_13d_stm_k_stability.R -- is the number of topics chosen on D reproducible?
# ==============================================================================
# rv_13 chooses K with searchK() on the discovery fold. searchK() holds out
# words from a random tenth of the documents before it fits each model, and
# rv_13 did not fix that draw (heldout.seed), so the coherence and exclusivity
# it compares differ from run to run. A test run on the South Korea and Taiwan
# study chose K = 6; the full run chose K = 12.
# This script repeats the choice with S fixed held-out seeds per study and
# reports how often each K wins. Discovery fold only; no measure changes.
# Usage: Rscript analysis/revision/rv_13d_stm_k_stability.R [S] [dataset ...]
# Writes analysis/output/revision/tables/stm_k_stability.csv.
# ==============================================================================

source("analysis/revision/rv_config.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
src13 <- readLines("analysis/revision/rv_13_stm_arm.R")
eval(parse(text = src13[grep("^SW_KO <-", src13):(grep("^choose_K <-", src13) - 1)]))   # tokenizer
args <- commandArgs(trailingOnly = TRUE)
S <- if (length(args)) as.integer(args[1]) else 20L
cards <- registered_cards()
ids <- if (length(args) > 1) args[-1] else names(cards)
K_GRID <- c(4L, 6L, 8L, 10L, 12L)
STM_DIR <- file.path(JOBS, "stm")

rows <- mclapply(ids, function(id) {
  f <- file.path(STM_DIR, paste0(id, ".stm.rds"))
  if (!file.exists(f)) return(NULL)
  used <- readRDS(f)$K
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id)
  d <- d[nchar(trimws(d$text)) > 0, ]
  dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = "stm arm: K stability on D")
  lang <- if ("lang" %in% names(dd)) dd$lang else rep("en", nrow(dd))
  m <- dfm_trim(dfm(tokens_for(dd$text, lang)), min_docfreq = 3)
  st <- convert(m[rowSums(m) > 0, ], to = "stm")
  pick <- vapply(seq_len(S), function(sd) {
    sk <- searchK(st$documents, st$vocab, K = K_GRID, init.type = "Spectral", heldout.seed = sd,
                  max.em.its = 75, verbose = FALSE, cores = 1, seed = 42)
    sc <- as.numeric(scale(unlist(sk$results$semcoh))) + as.numeric(scale(unlist(sk$results$exclus)))
    K_GRID[which.max(sc)]
  }, 0L)
  tab <- table(factor(pick, levels = K_GRID))
  data.frame(dataset = id, n_D = length(st$documents), K_used = used, seeds = S,
             K_modal = K_GRID[which.max(tab)], share_modal = max(tab) / S,
             share_K_used = mean(pick == used), n_distinct_K = sum(tab > 0),
             t(setNames(as.integer(tab), paste0("n_K", K_GRID))))
}, mc.cores = 12)
bad <- vapply(rows, inherits, NA, "try-error")
if (any(bad)) message("Failed: ", paste(ids[bad], collapse = ", "), "\n", paste(unlist(rows[bad]), collapse = "\n"))
out <- do.call(rbind, rows[!bad])
of <- file.path(OUT, "tables", if (length(args) > 1) "stm_k_stability_subset.csv" else "stm_k_stability.csv")
write.csv(out, of, row.names = FALSE)
print(out, row.names = FALSE, digits = 2)
cat(sprintf("\nstudies %d; the K used wins in a median %.0f%% of %d held-out draws; one K wins every draw in %d studies; median number of distinct K chosen %g\n",
            nrow(out), 100 * median(out$share_K_used), S, sum(out$share_modal == 1), median(out$n_distinct_K)))
