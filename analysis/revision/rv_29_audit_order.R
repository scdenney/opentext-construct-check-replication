# rv_29_audit_order.R -- does the permutation audit (rv_08) depend on the order of
# documents and vocabulary? Plan: revision/deviations.md (24 Nov 2026).
# Run: LANG=en_US.UTF-8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript analysis/revision/rv_29_audit_order.R [dataset ...]
source("analysis/revision/rv_config.R"); source("analysis/revision/rv_treatments.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
K <- 8L; B <- 200L; CORES <- 11L
out_dir <- file.path(OUT, "doubledip"); stored <- sub("_summary.csv$", "", basename(Sys.glob(file.path(out_dir, "*_summary.csv"))))
stored <- sort(stored[vapply(stored, function(i) nrow(read.csv(file.path(out_dir, paste0(i, "_perm.csv")))) == B, NA)])
set.seed(20261124); ids <- sample(stored, 20)
if (length(commandArgs(TRUE))) ids <- commandArgs(TRUE)
src <- readLines("analysis/revision/rv_08_doubledip.R"); at <- function(p) grep(p, src)[1]
setup <- parse(text = src[at("^d <- read_harmonized"):(at("^fit_stm <-") - 1)])
eval(parse(text = src[at("^pick_topic <-"):(at("^# SPLIT_ONLY") - 1)]))
fit_stm <- function(m, treat) { meta <- data.frame(treat = treat)
  list(mod = stm(stm_documents(m), colnames(m), K = K, prevalence = ~treat, data = meta, init.type = "Spectral",
                 max.em.its = 75, verbose = FALSE, seed = 42), meta = meta) }
dir.create(file.path(OUT, "doubledip_order"), showWarnings = FALSE)
for (id in ids) {
  f <- file.path(OUT, "doubledip_order", paste0(id, ".csv")); if (file.exists(f)) next
  eval(setup); full <- full[, sort(colnames(full), method = "radix")]; dD <- dD[, sort(colnames(dD), method = "radix")]
  perms <- opencheck:::with_seed(20261029, lapply(seq_len(B), function(b) opencheck:::permute_within(d$treatment, strata)))
  res <- do.call(rbind, lapply(0:1, function(r) {
    o <- function(m, s) { if (r == 0) list(seq_len(nrow(m)), seq_len(ncol(m))) else { set.seed(20261124 + s); list(sample(nrow(m)), sample(ncol(m))) } }
    of <- o(full, 1); od <- o(dD, 2)
    z <- mclapply(seq_len(B), function(b) tryCatch({ tr <- perms[[b]]
      a <- fit_stm(full[of[[1]], of[[2]]], tr[of[[1]]]); set.seed(1); pa <- pick_topic(a)
      g <- fit_stm(dD[od[[1]], od[[2]]], tr[iD][od[[1]]]); set.seed(1); pg <- pick_topic(g)
      th <- fitNewDocuments(g$mod, documents = stm_documents(dfm_match(dE, g$mod$vocab)), prevalencePrior = "None", verbose = FALSE)$theta
      c(p_old = unname(pa$p), p_split = unname(e_test(th, pg$k, tr[iE])["p"])) }, error = function(e) c(p_old = NA, p_split = NA)), mc.cores = CORES)
    z <- as.data.frame(do.call(rbind, z)); data.frame(dataset = id, order = r, B = sum(!is.na(z$p_old)),
      fpr_old = mean(z$p_old < .05, na.rm = TRUE), fpr_split = mean(z$p_split < .05, na.rm = TRUE)) }))
  s <- read.csv(file.path(out_dir, paste0(id, "_summary.csv")))
  write.csv(rbind(res, data.frame(dataset = id, order = "stored", B = s$B, fpr_old = s$fpr_old, fpr_split = s$fpr_split)), f, row.names = FALSE)
  message(id, " done ", format(Sys.time(), "%H:%M"))
}
x <- do.call(rbind, lapply(file.path(OUT, "doubledip_order", paste0(ids, ".csv")), read.csv))
write.csv(x, file.path(OUT, "tables", "audit_order.csv"), row.names = FALSE)
print(aggregate(cbind(fpr_old, fpr_split) ~ order, x, mean), digits = 4)
w <- reshape(x[, c("dataset", "order", "fpr_old", "fpr_split")], idvar = "dataset", timevar = "order", direction = "wide"); print(w, digits = 2, row.names = FALSE)
