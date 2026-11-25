# rv_24_note_checks.R -- intervals and checks for the shortened note (23 Nov 2026, post hoc).
# Cluster bootstrap over survey samples (instrument_of), 5,000 draws, seed 42: detection rates over all
# 205 primary contrasts (unmatched target = undetected) and their paired differences from the 35B prompt.
source("analysis/revision/rv_config.R")
eff <- rbind(read.csv(file.path(OUT, "effects_a1.csv")), read.csv(file.path(OUT, "effects_stmcov.csv")))
eff <- eff[eff$role == "target" & eff$primary, ]
eff$rec <- eff$p_ri < .05 & eff$estimate * eff$expected_sign > 0
k <- function(d) paste(d$dataset, d$construct, d$contrast)
ref <- eff[eff$model == "qwen36_35b" & eff$arm == "A1_presence", ]; ids <- k(ref); inst <- instrument_of(ref$dataset)
get <- function(m, a) { d <- eff[eff$model == m & eff$arm == a, ]; v <- d$rec[match(ids, k(d))]; ifelse(is.na(v), FALSE, v) }
V <- cbind(llm35 = ref$rec, g31 = get("gemma4_31b", "A1_presence"), e4b = get("gemma4_e4b", "A1_presence"), stm = get("stm", "D_topic"), stmcov = get("stmcov", "D_topic"))
set.seed(42); g <- split(seq_along(ids), inst)
B <- t(replicate(5000, colMeans(V[unlist(g[sample(length(g), replace = TRUE)]), , drop = FALSE])))
q <- function(x) sprintf("[%.3f, %.3f]", quantile(x, .025), quantile(x, .975))
out <- data.frame(quantity = c(colnames(V), "llm35-stm", "llm35-stmcov", "stmcov-stm", "llm35-e4b"),
  est = c(colMeans(V), mean(V[, 1] - V[, 4]), mean(V[, 1] - V[, 5]), mean(V[, 5] - V[, 4]), mean(V[, 1] - V[, 3])),
  ci = c(apply(B, 2, q), q(B[, 1] - B[, 4]), q(B[, 1] - B[, 5]), q(B[, 5] - B[, 4]), q(B[, 1] - B[, 3])))
write.csv(out, file.path(OUT, "tables", "note_intervals.csv"), row.names = FALSE); print(out, digits = 3); cat(length(g), "samples\n")
# by expected direction
cat("35B by direction:\n"); print(aggregate(ref$rec, list(sign = ref$expected_sign), function(x) c(n = length(x), det = sum(x))))
h <- read.csv(file.path(OUT, "tables", "human_codes_effects.csv")); print(table(human = h$rec_human, llm = h$rec_model)); print(names(h))
cat("non-significant (p>=.05):", sum(ref$p_ri >= .05), " sig wrong sign:", sum(ref$p_ri < .05 & ref$estimate * ref$expected_sign < 0), " undetected w/ theorized sign:", sum(!ref$rec & ref$estimate * ref$expected_sign > 0), "\n")
pl <- read.csv(file.path(OUT, "effects_a1.csv")); pl <- pl[pl$model == "qwen36_35b" & pl$arm == "A1_presence" & pl$role == "placebo", ]
cat("placebo n", nrow(pl), "up", mean(pl$p_ri < .05 & pl$estimate > 0), "down", mean(pl$p_ri < .05 & pl$estimate < 0), "\n")
c0 <- read.csv(file.path(OUT, "tables", "closed_outcomes.csv")); print(names(c0)); print(nrow(c0))
