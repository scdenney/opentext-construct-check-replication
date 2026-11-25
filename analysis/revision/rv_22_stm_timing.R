# Time STM's K selection (searchK over K = 4..12) on all studies.
commandArgs <- function(trailingOnly = TRUE) "none"
source("analysis/revision/rv_13_stm_arm.R")
f <- Sys.glob("data/jobs/stm/*.stmcov.rds")
n <- sapply(f, function(x) readRDS(x)$n_docs)
pick <- sub("\\.stmcov\\.rds$", "", basename(f))
res <- do.call(rbind, lapply(pick, function(id) {
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id); d <- d[nchar(trimws(d$text)) > 0, ]
  dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = "timing: STM K selection (searchK)")
  lang <- if ("lang" %in% names(dd)) dd$lang else rep("en", nrow(dd))
  t0 <- proc.time()[["elapsed"]]
  m <- dfm_trim(dfm(tokens_for(dd$text, lang)), min_docfreq = 3); ok <- rowSums(m) > 0
  st <- convert(m[ok, ], to = "stm"); t1 <- proc.time()[["elapsed"]]
  ck <- choose_K(st); t2 <- proc.time()[["elapsed"]]
  data.frame(id = id, docs = sum(ok), prep_s = t1 - t0, searchK_s = t2 - t1, K = ck$K)
}))
print(res); cat("searchK seconds: median", median(res$searchK_s), "max", max(res$searchK_s), "\n")
write.csv(res, "analysis/output/revision/stm_searchK_timing_all.csv", row.names = FALSE)
