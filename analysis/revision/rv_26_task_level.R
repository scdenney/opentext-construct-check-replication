# rv_26_task_level.R -- respondent-clustered check for the four studies randomized at the task level
# (post hoc, 23 Nov 2026). The main analysis splits and permutes rows. Here: cluster-robust (CR1) test of the
# blocked contrast for every primary contrast and arm, and the same test on one random row per respondent.
source("analysis/revision/rv_config.R"); source("analysis/revision/rv_treatments.R")
src06 <- readLines("analysis/revision/rv_06_effects.R"); eval(parse(text = src06[grep("^contrasts_for <-", src06):(grep("^args <-", src06) - 1)]))
ids <- c("graeber2024_storiesmemory", "krishnarajan2022_rationalizing", "marsh2025_lifestyleconjoint", "roos2024_subjectivity"); out <- list()
crp <- function(y, tr, st, cl) { m <- if (length(unique(st)) == 1) lm(y ~ tr) else lm(y ~ tr + factor(st)); X <- model.matrix(m); u <- residuals(m)
  B <- solve(crossprod(X)); S <- rowsum(X * u, cl); G <- nrow(S); V <- B %*% crossprod(S) %*% B * G / (G - 1); c(coef(m)["tr"], 2 * pt(-abs(coef(m)["tr"] / sqrt(V["tr", "tr"])), G - 1)) }
for (id in ids) {
  card <- oc_study_read(registered_cards()[[id]]); tg <- vapply(card$constructs$target, `[[`, "", "name")
  s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json"))); s <- s[s$split == 1, ]
  d <- read_harmonized(id); d <- d[nchar(trimws(d$text)) > 0, ]; resp <- if ("respondent_id" %in% names(d)) d$respondent_id else sub("_[^_]*$", "", d$id)
  d$resp <- resp; f <- s$fold[match(d$id, s$id)]; both <- length(intersect(resp[f == "D"], resp[f == "E"]))
  e <- d[f == "E" & !is.na(f), ]; st <- effect_strata(id, e) %||% rep("a", nrow(e))
  cat(id, ": rows", nrow(d), "respondents", length(unique(resp)), "E rows", nrow(e), "E respondents", length(unique(e$resp)), "in both folds", both, "\n")
  for (cn in contrasts_for(id, e, tg)) for (cv in cn$primary) for (mk in c("qwen36_35b", "gemma4_e4b", "stm", "stmcov")) {
    mf <- file.path(RESULTS, "a1", id, mk, "measures.csv"); if (!file.exists(mf)) next
    m <- read.csv(mf); col <- paste0(if (grepl("^stm", mk)) "stm__" else "yes__", cv); if (!col %in% names(m)) next
    y <- m[[col]][match(e$id, m$id)]; k <- !is.na(y) & !is.na(cn$treat); sg <- expected_sign(card, cv)
    r <- crp(y[k], cn$treat[k], st[k], e$resp[k]); set.seed(42); o <- sample(which(k)); o <- o[!duplicated(e$resp[o])]; r1 <- crp(y[o], cn$treat[o], st[o], e$resp[o])
    out[[length(out) + 1]] <- data.frame(id, contrast = cn$name, construct = cv, model = mk, sign = sg, est = r[1], p_cluster = r[2], est_one = r1[1], p_one = r1[2],
      det_cluster = r[2] < .05 & sign(r[1]) == sg, det_one = r1[2] < .05 & sign(r1[1]) == sg)
  }
}
o <- do.call(rbind, out); e0 <- rbind(read.csv(file.path(OUT, "effects_a1.csv")), read.csv(file.path(OUT, "effects_stmcov.csv")))
arm <- ifelse(grepl("^stm", o$model), "D_topic", "A1_presence"); k <- match(paste(o$id, o$construct, o$contrast, o$model, arm), paste(e0$dataset, e0$construct, e0$contrast, e0$model, e0$arm))
o$det_main <- e0$p_ri[k] < .05 & e0$estimate[k] * e0$expected_sign[k] > 0
write.csv(o, file.path(OUT, "tables", "task_level_check.csv"), row.names = FALSE); print(o[, c("id", "construct", "model", "sign", "est", "p_cluster", "p_one", "det_main", "det_cluster", "det_one")], digits = 3, row.names = FALSE)
