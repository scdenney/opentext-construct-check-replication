# rv_28_prompt_as_treatment.R -- detection without the studies whose arms answered
# different writing prompts (24 Nov 2026, post hoc; see revision/deviations.md).
# A: the arms got different questions or writing tasks. B: A plus items whose
# wording itself carries the treatment, or that describe an imagined scene that
# differed by arm. Read from the cards' prompt and position fields, all 139.
source("analysis/revision/rv_config.R")
A <- c("ward2026_localcandidates", "gadarian2014_anxiety", "busby2019_study1", "busby2019_study2", "coles2026_gratitude",
       "ebersole2016_powerrecall", "gidron2025_polarization")
B <- c(A, "peyton2022_imaginedcontact", "tripp2026_refugees", "spruill2022_legalreasonable_s1", "spruill2022_legalreasonable_s2",
       "kwon2025_productjourney", "marx2025_genderlanguage")
eff <- rbind(read.csv(file.path(OUT, "effects_a1.csv")), read.csv(file.path(OUT, "effects_stmcov.csv")))
eff <- eff[eff$role == "target" & eff$primary, ]
eff$rec <- eff$p_ri < .05 & eff$estimate * eff$expected_sign > 0
k <- function(d) paste(d$dataset, d$construct, d$contrast)
ref <- eff[eff$model == "qwen36_35b" & eff$arm == "A1_presence", ]
arms <- list(qwen36_35b = "A1_presence", gemma4_31b = "A1_presence", gpt6_luna = "A1_presence", gemma4_e4b = "A1_presence", stm = "D_topic", stmcov = "D_topic")
cat("contrasts of A and B:", sum(ref$dataset %in% A), sum(ref$dataset %in% B), "of", nrow(ref), "\n")
print(ref[ref$dataset %in% B, c("dataset", "construct", "estimate", "p_ri", "rec")], row.names = FALSE, digits = 2)
out <- do.call(rbind, lapply(names(arms), function(m) { d <- eff[eff$model == m & eff$arm == arms[[m]], ]
  r <- function(drop) { u <- ref[!ref$dataset %in% drop, ]; if (m == "gpt6_luna") u <- u[k(u) %in% k(d), ]; v <- d$rec[match(k(u), k(d))] %in% TRUE; sprintf("%.1f (%d/%d)", 100 * mean(v), sum(v), length(v)) }
  data.frame(model = m, all = r(character()), without_A = r(A), without_B = r(B), without_ward = r("ward2026_localcandidates")) }))
print(out, row.names = FALSE); write.csv(out, file.path(OUT, "tables", "prompt_as_treatment.csv"), row.names = FALSE)
h <- read.csv(file.path(OUT, "tables", "human_codes_effects.csv")); print(table(h$dataset[h$dataset %in% B])); i <- !h$dataset %in% A
cat("human-coded without A: n", sum(i), "human", sum(h$rec_human[i]), "llm", sum(h$rec_model[i]), "stm", sum(h$rec_stm[i]), "\n")
n <- sapply(c(A, B[!B %in% A]), function(i) { d <- read_harmonized(i); nrow(d) }); print(n)
pt <- read.csv(file.path(OUT, "prompt_type.csv")); print(pt[pt$dataset %in% B, 1:2])
