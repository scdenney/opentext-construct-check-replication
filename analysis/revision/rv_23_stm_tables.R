# ==============================================================================
# rv_23_stm_tables.R -- SI rows for the STM covariate arm and the Bonferroni
# version of the full-sample permutation audit (23 Nov 2026, post hoc; see
# revision/deviations.md). Every small-arm rate uses the same denominator: all
# primary contrasts, an unmatched target counting as undetected.
# Writes si_tables/stm_arms_rows.tex, si_tables/doubledip_bonferroni_rows.tex and
# tables/stm_arms.csv. Run: Rscript analysis/revision/rv_23_stm_tables.R
# ==============================================================================
source("analysis/revision/rv_config.R")
TAB <- file.path(OUT, "tables"); TEX <- "paper/revision/si_tables"
eff <- rbind(read.csv(file.path(OUT, "effects_a1.csv")), read.csv(file.path(OUT, "effects_stmcov.csv")))
eff <- eff[eff$role == "target" & eff$primary, ]
eff$rec <- eff$p_ri < 0.05 & eff$estimate * eff$expected_sign > 0
eff$sign_ok <- eff$estimate * eff$expected_sign > 0
key <- function(d) paste(d$dataset, d$construct, d$contrast)
ref <- eff[eff$model == "qwen36_35b" & eff$arm == "A1_presence", ]
small <- key(ref)[pmin(ref$n1, ref$n0) < 200]
psw <- function(d, v) { i <- instrument_of(d$dataset); weighted.mean(v, 1 / ave(v, i, FUN = length)) }
hum <- function(f, col) sum(read.csv(file.path(TAB, f))[[col]], na.rm = TRUE)
auc <- function(f, col) median(read.csv(file.path(TAB, f))[[col]], na.rm = TRUE)
arms <- list(list("Zero-shot yes/no (35B)", "qwen36_35b", "A1_presence", "human_codes_effects.csv", "rec_model", "human_codes_agreement.csv", "auc"),
             list("Topic model, no covariate", "stm", "D_topic", "human_codes_effects.csv", "rec_stm", "human_codes_agreement.csv", "auc_stm"),
             list("Topic model, treatment covariate", "stmcov", "D_topic", "human_codes_effects_stmcov.csv", "rec_stm", "human_codes_agreement_stmcov.csv", "auc_stm"))
out <- do.call(rbind, lapply(arms, function(a) {
  d <- eff[eff$model == a[[2]] & eff$arm == a[[3]], ]
  data.frame(arm = a[[1]], n = nrow(d), studies = length(unique(d$dataset)), det_cov = mean(d$rec), det_all = sum(d$rec) / nrow(ref),
             per_sample = psw(d, d$rec), sign = mean(d$sign_ok), small_k = sum(d$rec[key(d) %in% small]), small_n = length(small),
             human = hum(a[[4]], a[[5]]), auc = auc(a[[6]], a[[7]]))
}))
write.csv(out, file.path(TAB, "stm_arms.csv"), row.names = FALSE)
writeLines(sprintf("%s & %d (%d) & %.2f & %.2f & %.2f & %.2f & %d of %d & %d of 46 & %.2f \\\\", out$arm, out$n, out$studies, out$det_cov,
                   out$det_all, out$per_sample, out$sign, out$small_k, out$small_n, out$human, out$auc), file.path(TEX, "stm_arms_rows.tex"))
f <- Sys.glob(file.path(OUT, "doubledip", "*_perm.csv")); f <- f[!grepl("nwo26_exp3_comprehension", f)]
b <- vapply(f, function(x) mean(read.csv(x)$p_old < 0.05 / 8, na.rm = TRUE), 0); id <- sub("_perm.csv$", "", basename(f))
writeLines(sprintf("Full sample, topic selected, Bonferroni (8 topics) & %.3f & %.3f & %.3f & %.3f & %d \\\\", mean(b), min(b), max(b),
                   mean(tapply(b, instrument_of(id), mean)), sum(b > 0.35)), file.path(TEX, "doubledip_bonferroni_rows.tex"))
print(out, digits = 3); cat(readLines(file.path(TEX, "doubledip_bonferroni_rows.tex")), "\n")

# Detection by prompt type (codes: prompt_type_handcoded.csv)
code <- read.csv(file.path(OUT, "prompt_type_handcoded.csv")); code <- setNames(code$code, code$dataset)
m <- list(c("\\texttt{Qwen3.6-35B-A3B}, yes/no", "qwen36_35b", "A1_presence"), c("\\texttt{Gemma 4 31B}, yes/no", "gemma4_31b", "A1_presence"),
          c("\\texttt{GPT-6 Luna}, yes/no", "gpt6_luna", "A1_presence"), c("\\texttt{Gemma 4 E4B}, yes/no", "gemma4_e4b", "A1_presence"),
          c("Topic model, no covariate", "stm", "D_topic"), c("Topic model, treatment covariate", "stmcov", "D_topic"))
cell <- function(d, n) sprintf("%.2f (%d of %d)", sum(d$rec) / n, sum(d$rec), n)
rows <- vapply(m, function(x) {
  d <- eff[eff$model == x[2] & eff$arm == x[3], ]
  f <- function(k) { dd <- d[code[d$dataset] == k, ]; n <- if (x[3] == "D_topic") sum(code[ref$dataset] == k) else nrow(dd); cell(dd, n) }
  g <- function(k) { big <- key(ref)[pmin(ref$n1, ref$n0) >= 200 & code[ref$dataset] == k]; dd <- d[key(d) %in% big, ]
                     cell(dd, if (x[3] == "D_topic") length(big) else nrow(dd)) }
  sprintf("%s & %s & %s & %s & %s \\\\", x[1], f("own_answer"), f("other"), g("own_answer"), g("other"))
}, "")
writeLines(rows, file.path(TEX, "prompt_type_rows.tex")); cat(rows, sep = "\n")
