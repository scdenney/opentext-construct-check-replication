# rv_30_audit_arms.R -- what produces the full-sample false-positive rate when no topic is selected?
# Post hoc (24 Nov 2026, after the pre-submission review); plan in revision/deviations.md.
# On the 20 studies of the row-order check (rv_29), under the same 200 permuted labels, the full-sample
# fit is crossed three ways:
#   covariate: prevalence ~ treatment (refit per permutation) | no covariate (one fit; labels never enter it)
#   topic:     selected (largest absolute statistic) | fixed (topic 1) | Bonferroni over the eight
#   test:      stm::estimateEffect(uncertainty = "Global") | unblocked difference in mean topic share (rv_08 e_test)
# Run: LANG=en_US.UTF-8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript analysis/revision/rv_30_audit_arms.R [dataset ...]
source("analysis/revision/rv_config.R"); source("analysis/revision/rv_treatments.R")
suppressMessages({ library(stm); library(quanteda); library(parallel) })
K <- 8L; B <- 200L; CORES <- 12L
NB <- as.integer(Sys.getenv("ARMS_NB", B))   # smoke test: first NB permutations, written to a separate folder
dd_dir <- file.path(OUT, "doubledip")
stored <- sub("_summary.csv$", "", basename(Sys.glob(file.path(dd_dir, "*_summary.csv"))))
stored <- sort(stored[vapply(stored, function(i) nrow(read.csv(file.path(dd_dir, paste0(i, "_perm.csv")))) == B, NA)])
set.seed(20261124); ids <- sample(stored, 20)   # the same 20 studies as rv_29
if (length(commandArgs(TRUE))) ids <- commandArgs(TRUE)
src <- readLines("analysis/revision/rv_08_doubledip.R"); at <- function(p) grep(p, src)[1]
setup <- parse(text = src[at("^d <- read_harmonized"):(at("^fit_stm <-") - 1)])
out_dir <- file.path(OUT, if (NB == B) "doubledip_arms" else "doubledip_arms_test"); dir.create(out_dir, showWarnings = FALSE)

ee_p <- function(mod, treat) {   # estimateEffect p and t for every topic
  set.seed(1)
  tt <- summary(estimateEffect(1:K ~ treat, mod, metadata = data.frame(treat = treat), uncertainty = "Global"))$tables
  list(p = vapply(tt, function(x) x["treat", "Pr(>|t|)"], 0), t = vapply(tt, function(x) x["treat", "t value"], 0))
}
dm_p <- function(th, tr) {       # rv_08 e_test for every topic
  a <- th[tr == 1, , drop = FALSE]; b <- th[tr == 0, , drop = FALSE]
  z <- (colMeans(a) - colMeans(b)) / sqrt(apply(a, 2, var) / nrow(a) + apply(b, 2, var) / nrow(b))
  list(p = 2 * pnorm(-abs(z)), t = z)
}
row <- function(x) { k <- which.max(abs(x$t)); c(sel = x$p[[k]], fix = x$p[[1]], bonf = min(1, K * min(x$p))) }

for (id in ids) {
  f <- file.path(out_dir, paste0(id, ".csv")); if (file.exists(f)) next
  eval(setup); full <- full[, sort(colnames(full), method = "radix")]
  docs <- stm_documents(full); vocab <- colnames(full)
  perms <- opencheck:::with_seed(20261029, lapply(seq_len(B), function(b) opencheck:::permute_within(d$treatment, strata)))
  m0 <- stm(docs, vocab, K = K, init.type = "Spectral", max.em.its = 75, verbose = FALSE, seed = 42)
  z <- mclapply(seq_len(NB), function(b) tryCatch({ tr <- perms[[b]]
    m1 <- stm(docs, vocab, K = K, prevalence = ~treat, data = data.frame(treat = tr), init.type = "Spectral",
              max.em.its = 75, verbose = FALSE, seed = 42)
    c(cov_ee = row(ee_p(m1, tr)), cov_dm = row(dm_p(m1$theta, tr)),
      nocov_ee = row(ee_p(m0, tr)), nocov_dm = row(dm_p(m0$theta, tr))) }, error = function(e) rep(NA_real_, 12)), mc.cores = CORES)
  z <- as.data.frame(do.call(rbind, z)); z$dataset <- id; z$b <- seq_len(NB)
  write.csv(z, f, row.names = FALSE); message(id, " done ", format(Sys.time(), "%H:%M"))
}
if (NB != B) { print(colMeans(read.csv(f)[1:12] < .05), digits = 2); quit(save = "no") }
x <- do.call(rbind, lapply(file.path(out_dir, paste0(ids, ".csv")), read.csv))
cols <- setdiff(names(x), c("dataset", "b"))
by_study <- aggregate(x[cols] < .05, list(dataset = x$dataset), mean, na.rm = TRUE)
write.csv(by_study, file.path(OUT, "tables", "audit_arms_by_study.csv"), row.names = FALSE)
s <- data.frame(cell = cols, mean = colMeans(by_study[cols]), min = apply(by_study[cols], 2, min), max = apply(by_study[cols], 2, max))
p <- do.call(rbind, strsplit(s$cell, "[_.]")); s$covariate <- p[, 1]; s$test <- p[, 2]; s$topic <- p[, 3]
st <- do.call(rbind, lapply(ids, function(i) read.csv(file.path(dd_dir, paste0(i, "_summary.csv")))))
s$stored <- NA; s$stored[s$cell == "cov_ee.sel"] <- mean(st$fpr_old); s$stored[s$cell == "cov_ee.fix"] <- mean(st$fpr_old_fixed)
write.csv(s[, c("covariate", "topic", "test", "mean", "min", "max", "stored")], file.path(OUT, "tables", "audit_arms.csv"), row.names = FALSE)
print(s[, c("covariate", "topic", "test", "mean", "min", "max", "stored")], digits = 3, row.names = FALSE)
cat("permutations with a failed fit:", sum(!complete.cases(x[cols])), "of", nrow(x), "\n")

# SI rows (percent): fit and topic tested by test
g <- function(cv, tp, ts) fmt_num(100 * s$mean[s$covariate == cv & s$topic == tp & s$test == ts], 1)
lab <- c(sel = "Selected (largest absolute statistic)", fix = "Fixed in advance (first topic)", bonf = "Selected, Bonferroni for eight topics")
rows <- unlist(lapply(c("cov", "nocov"), function(cv) c(
  sprintf("\\multicolumn{3}{@{}l}{\\textit{%s}} \\\\", if (cv == "cov") "Fit with the treatment as a prevalence covariate (refit for each permutation)" else "Fit without the covariate (one fit; the labels never enter it)"),
  vapply(names(lab), function(tp) sprintf("\\quad %s & %s & %s \\\\", lab[[tp]], g(cv, tp, "ee"), g(cv, tp, "dm")), ""))))
writeLines(rows, "paper/revision/si_tables/audit_arms_rows.tex"); cat(rows, sep = "\n")
