# ==============================================================================
# rv_27_stm_refits_summary.R -- range of the topic-model results over refits
# ==============================================================================
# Reads analysis/output/revision/stm_refits_effects.csv and the measures saved by
# rv_26_stm_refits.R; computes, per arm and refit, every rate the note reports
# (reporting rule: revision/deviations.md, 23 Nov 2026). The frozen fits go
# through the same code (refit = -1) as a check against the published figures.
# Writes tables/stm_refits_by_fit.csv and tables/stm_refits_summary.csv.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_27_stm_refits_summary.R
# ==============================================================================
source("analysis/revision/rv_config.R")
suppressMessages(library(parallel))
src17 <- readLines("analysis/revision/rv_17_human_codes.R")
eval(parse(text = src17[grep("^source\\(\"analysis/revision/rv_treatments", src17)[1]:(grep("^cards <- registered_cards", src17)[1] - 1)]))
cards <- registered_cards()
TAB <- file.path(OUT, "tables")
key <- function(d) paste(d$dataset, d$construct, d$contrast)
a1 <- read.csv(file.path(OUT, "effects_a1.csv")); a1 <- a1[a1$role == "target" & a1$primary, ]
ref <- a1[a1$model == REF & a1$arm == "A1_presence", ]
U <- key(ref); small <- pmin(ref$n1, ref$n0) < 200; inst <- instrument_of(ref$dataset)

x <- read.csv(file.path(OUT, "stm_refits_effects.csv"))
froz <- function(d, m, arm) {
  d <- d[d$model == m & d$arm == "D_topic" & d$role == "target" & d$primary, ]
  data.frame(dataset = d$dataset, construct = d$construct, contrast = d$contrast, arm = arm, refit = -1L, matched = TRUE,
             estimate = d$estimate, est_sd = d$est_sd, p_ri = d$p_ri, expected_sign = d$expected_sign,
             detected = d$p_ri < .05 & d$estimate * d$expected_sign > 0, detected_best = NA, converged = NA)
}
x <- rbind(x, froz(a1, "stm", "none"), froz(read.csv(file.path(OUT, "effects_stmcov.csv")), "stmcov", "cov"))
x$k <- key(x); x <- x[x$k %in% U, ]
units <- unique(x[, c("arm", "refit")])

# human codes: topic share against the authors' codes
hc_ids <- unique(vapply(MAP, `[[`, "", "id"))
hc <- do.call(rbind, mclapply(hc_ids, function(id) {
  card <- oc_study_read(cards[[id]]); targets <- vapply(card$constructs$target, `[[`, "", "name")
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
  d <- read_harmonized(id); d <- d[nchar(trimws(d$text)) > 0, ]
  e <- oc_estimation_data(d, s, oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json"))), log = ACCESS_LOG, purpose = "stm refits: human-code comparison")
  yn <- read.csv(file.path(RESULTS, "a1", id, REF, "measures.csv"))
  meas <- readRDS(file.path(RESULTS, "stm_refit", paste0(id, ".measures.rds")))
  for (m in c("stm", "stmcov")) { f <- file.path(RESULTS, "a1", id, m, "measures.csv")
    meas[[if (m == "stm") "none_r-1" else "cov_r-1"]] <- if (file.exists(f)) read.csv(f, colClasses = c(id = "character")) else data.frame(id = e$id) }
  st <- effect_strata(id, e); cns <- contrasts_for(id, e, targets); out <- list()
  for (m in Filter(function(z) z$id == id, MAP)) {
    h <- m$h(e); y <- yn[[paste0("yes__", m$construct)]][match(e$id, yn$id)]; py <- yn[[paste0("pyes__", m$construct)]][match(e$id, yn$id)]
    ok <- !is.na(h) & !is.na(y)
    for (nm in names(meas)) {
      v <- meas[[nm]][[paste0("stm__", m$construct)]]; v <- if (is.null(v)) NULL else v[match(e$id, meas[[nm]]$id)]
      a <- if (is.null(v)) NA else { k <- ok & !is.na(py) & !is.na(v); auc(v[k], h[k]) }
      n <- det <- 0L
      for (cn in cns) if (m$construct %in% cn$primary) {
        keep <- ok & !is.na(cn$treat)
        if (sum(cn$treat[keep] == 1) < 20 || sum(cn$treat[keep] == 0) < 20) next
        n <- n + 1L
        if (is.null(v)) next
        sgn <- expected_sign(card, m$construct) * (if (grepl("_opposition$", cn$name)) -1L else 1L)
        ef <- oc_effect(v[keep], cn$treat[keep], strata = if (is.null(st)) NULL else st[keep], n_perm = 2000L, seed = 42L)
        det <- det + isTRUE(ef$p_ri < .05 & ef$estimate * sgn > 0)
      }
      out[[length(out) + 1]] <- data.frame(dataset = id, construct = m$construct, arm = sub("_r.*", "", nm),
                                           refit = as.integer(sub(".*_r", "", nm)), auc = a, n = n, det = det)
    }
  }
  do.call(rbind, out)
}, mc.cores = 10, mc.preschedule = FALSE))
write.csv(hc, file.path(TAB, "stm_refits_human.csv"), row.names = FALSE)

by_fit <- do.call(rbind, lapply(seq_len(nrow(units)), function(i) {
  d <- x[x$arm == units$arm[i] & x$refit == units$refit[i], ]
  m <- d[match(U, d$k), ]; det <- m$detected %in% TRUE; mt <- m$matched %in% TRUE
  g <- hc[hc$arm == units$arm[i] & hc$refit == units$refit[i], ]
  data.frame(units[i, ], matched = sum(mt), det_all = mean(det), det_matched = sum(det) / sum(mt),
             per_sample = mean(tapply(det[mt], inst[mt], mean)), sign_ok = mean((m$estimate * m$expected_sign > 0)[mt]),
             det_small = mean(det[small]), det_large = mean(det[!small]), hindsight = mean(m$detected_best %in% TRUE),
             auc = median(g$auc, na.rm = TRUE), n_auc = sum(!is.na(g$auc)), human_det = sum(g$det), human_n = sum(g$n))
}))
write.csv(by_fit, file.path(TAB, "stm_refits_by_fit.csv"), row.names = FALSE)
cat("frozen fits through the same code:\n"); print(by_fit[by_fit$refit == -1, ], digits = 3, row.names = FALSE)
r <- by_fit[by_fit$refit >= 0, ]
v <- setdiff(names(r), c("arm", "refit", "human_n"))
summ <- do.call(rbind, lapply(c("none", "cov"), function(a) do.call(rbind, lapply(v, function(q) {
  z <- r[r$arm == a, q]; f <- by_fit[by_fit$arm == a & by_fit$refit == -1, q]
  data.frame(arm = a, quantity = q, frozen = f, median = median(z), min = min(z), max = max(z), sd = sd(z), frozen_rank = sum(z <= f))
}))))
write.csv(summ, file.path(TAB, "stm_refits_summary.csv"), row.names = FALSE); print(summ, digits = 3, row.names = FALSE)
w <- function(a) r[r$arm == a, ][order(r$refit[r$arm == a]), ]
for (q in c("det_all", "det_matched", "sign_ok", "det_small", "human_det")) { g <- w("cov")[[q]] - w("none")[[q]]
  cat(sprintf("covariate gain, %-12s median %.3f [%.3f, %.3f]; positive in %d of %d refits\n", q, median(g), min(g), max(g), sum(g > 0), length(g))) }
cat("reference prompt, all 205:", mean(ref$p_ri < .05 & ref$estimate * ref$expected_sign > 0), "\n")
for (a in c("none", "cov")) {
  z <- x[x$arm == a & x$refit >= 0, ]; P <- sapply(split(z, z$refit), function(d) d$detected[match(U, d$k)] %in% TRUE)
  s <- rowSums(P); pr <- combn(ncol(P), 2)
  cat(sprintf("%s: contrasts always detected %d, never %d, sometimes %d of %d; same call between two refits %.3f (kappa %.2f)\n", a, sum(s == ncol(P)), sum(s == 0),
              sum(s > 0 & s < ncol(P)), length(s), mean(apply(pr, 2, function(p) mean(P[, p[1]] == P[, p[2]]))),
              mean(apply(pr, 2, function(p) { o <- mean(P[, p[1]] == P[, p[2]]); q1 <- mean(P[, p[1]]); q2 <- mean(P[, p[2]]); e <- q1 * q2 + (1 - q1) * (1 - q2); (o - e) / (1 - e) }))))
  E <- sapply(split(z, z$refit), function(d) d$estimate[match(U, d$k)] * d$expected_sign[match(U, d$k)])
  sw <- apply(E, 1, function(v) { v <- v[!is.na(v)]; length(v) > 1 && any(v > 0) && any(v < 0) })
  cat(sprintf("   estimate changes sign across refits in %d contrasts\n", sum(sw)))
}

# SI table rows
lab <- c(matched = "Contrasts with a matched topic", det_all = "Detected, all 205", det_matched = "Detected, covered", per_sample = "Per sample",
         sign_ok = "Theorized sign", det_small = "Detected, smaller arm under 200", hindsight = "Detected, hindsight topic", auc = "Median AUC against human codes",
         human_det = "Human-coded contrasts detected, of 46")
f2 <- function(v, q) if (q %in% c("matched", "human_det")) sub("\\.0$", "", sprintf("%.1f", v)) else sprintf("%.2f", v)
cell <- function(a, q) { z <- summ[summ$arm == a & summ$quantity == q, ]
  sprintf("%s & %s [%s, %s]", if (q == "hindsight") "--" else f2(z$frozen, q), f2(z$median, q), f2(z$min, q), f2(z$max, q)) }
writeLines(sprintf("%s & %s & %s \\\\", lab, vapply(names(lab), function(q) cell("none", q), ""), vapply(names(lab), function(q) cell("cov", q), "")),
           "paper/revision/si_tables/stm_refits_rows.tex")
