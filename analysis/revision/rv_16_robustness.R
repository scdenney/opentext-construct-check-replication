# ==============================================================================
# rv_16_robustness.R -- recovery of the theorized construct under alternative
# specifications
# ==============================================================================
# Reads analysis/output/revision/effects_a1.csv and corpus_table.csv; runs no
# model and reads no text. A target counts as recovered when the estimate of
# its primary contrast is significant (randomization p < .05) AND has the
# theorized sign. Each row changes one thing about that baseline: the test, the
# threshold, the weighting, the part of the corpus, or the model.
# Writes tables/robustness.csv and paper/revision/si_tables/robustness_rows.tex.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_16_robustness.R
# ==============================================================================

source("analysis/revision/rv_config.R")
TEX <- "paper/revision/si_tables"
REF <- "qwen36_35b"
ARMS <- c(yesno = "A1_presence", rating = "C_rating", pairwise = "B_pairwise", topic = "D_topic")

a1 <- read.csv(file.path(OUT, "effects_a1.csv"), stringsAsFactors = FALSE)
ct <- read.csv(file.path(OUT, "corpus_table.csv"), stringsAsFactors = FALSE)
reg <- lapply(opencheck:::read_lines_utf8(file.path(REGISTRY, "registry.jsonl")), jsonlite::fromJSON)
first_time <- tapply(vapply(reg, `[[`, "", "time"), vapply(reg, `[[`, "", "id"), min)
early <- names(first_time)[as.Date(substr(first_time, 1, 10)) < as.Date("2026-11-01")]
is_own <- function(x) grepl("^(sktw|aks21|ifes2023|nwo26|prc|roc|rok)", x)

p <- a1[a1$primary & a1$role == "target", ]
p$inst <- instrument_of(p$dataset)
p$expected <- sign(p$estimate) == p$expected_sign
p$rec <- p$p_ri < 0.05 & p$expected
p$n_E <- ct$n_E[match(p$dataset, ct$dataset)]
# 24 Nov 2026: "blind" now means the card records no example answer read (coder A, revision/coding/coded_cards.csv)
cc <- read.csv("revision/coding/coded_cards.csv", colClasses = "character")
p$blind <- cc$examples_read_A[match(p$dataset, cc$dataset)] == "0"
p$english <- ct$languages[match(p$dataset, ct$dataset)] == "en"
p$own <- is_own(p$dataset)
p$small_arm <- pmin(p$n1, p$n0) < 200

# the topic-model arm is its own "model" in effects_a1.csv
sel <- function(model, arm) p[p$model == (if (arm == "D_topic") "stm" else model) & p$arm == arm, ]
share <- function(d, hit = d$rec, weighted = FALSE) {
  if (!nrow(d)) return(c(NA, 0))
  w <- if (weighted) 1 / ave(rep(1, nrow(d)), d$inst, FUN = sum) else rep(1, nrow(d))
  c(weighted.mean(hit, w), nrow(d))
}
# one row of the table: a label and a function from (data for one arm) to c(share, n)
spec <- function(label, f, model = REF, arms = ARMS, arm_map = identity) {
  v <- lapply(ARMS, function(a) if (a %in% arms) f(sel(model, arm_map(a))) else c(NA, 0))
  data.frame(specification = label, yesno = v$yesno[1], rating = v$rating[1], pairwise = v$pairwise[1], topic = v$topic[1],
             n_yesno = v$yesno[2], n_rating = v$rating[2], n_pairwise = v$pairwise[2], n_topic = v$topic[2])
}
sub <- function(keep) function(d) share(d[keep(d), ])
LLM <- ARMS[1:3]
# Median over every corpus that keeps one card per survey sample, from
# rv_09_summary.R; the number in parentheses is the number of survey samples.
ps <- read.csv(file.path(OUT, "tables", "a1_rates_per_sample.csv"), stringsAsFactors = FALSE)
one_card <- function(label) {
  v <- lapply(ARMS, function(a) {
    r <- ps[ps$model == (if (a == "D_topic") "stm" else REF) & ps$arm == a, ]
    if (nrow(r) == 1) c(r$one_med, r$n_samples) else c(NA, 0)
  })
  data.frame(specification = label, yesno = v$yesno[1], rating = v$rating[1], pairwise = v$pairwise[1], topic = v$topic[1],
             n_yesno = v$yesno[2], n_rating = v$rating[2], n_pairwise = v$pairwise[2], n_topic = v$topic[2])
}

rows <- rbind(
  spec("Baseline: significant ($p<.05$) in the theorized direction", share),
  spec("Any significant estimate, either direction", function(d) share(d, d$p_ri < 0.05)),
  spec("Each survey sample weighted once", function(d) share(d, weighted = TRUE)),
  one_card("One card per survey sample (median over all selections)"),
  spec("Normal-approximation test, $|t|>1.96$", function(d) share(d, abs(d$estimate / d$se) > qnorm(0.975) & d$expected)),
  spec("Randomization $p<.01$", function(d) share(d, d$p_ri < 0.01 & d$expected)),
  spec("Yes/no as a probability, not a label", share, arms = "A1_presence", arm_map = function(a) "A1_presence_soft"),
  spec("Author's studies (mainly Korean, Chinese and German answers)", sub(function(d) d$own)),
  spec("Public studies", sub(function(d) !d$own)),
  spec("Public studies, card records no example answer read", sub(function(d) !d$own & d$blind)),
  spec("Public studies, English answers only", sub(function(d) !d$own & d$english)),
  spec("Public studies, other or several languages", sub(function(d) !d$own & !d$english)),
  spec("Studies collected in October 2026 (53)", sub(function(d) d$dataset %in% early)),
  spec("Studies collected in November 2026 (86)", sub(function(d) !d$dataset %in% early)),
  spec("Smaller arm under 200 answers", sub(function(d) d$small_arm)),
  spec("Smaller arm of 200 answers or more", sub(function(d) !d$small_arm)),
  spec("Small model (Gemma 4 E4B)", share, model = "gemma4_e4b", arms = c("A1_presence", "C_rating")),
  spec("Majority vote of the models run on the study", share, model = "majority", arms = "A1_presence")
)
write.csv(rows, file.path(OUT, "tables", "robustness.csv"), row.names = FALSE)

cell <- function(x, n) ifelse(is.na(x), "--", sprintf("%s (%d)", fmt_num(x), n))
writeLines(enc2utf8(sprintf("%s & %s & %s & %s & %s \\\\", rows$specification, cell(rows$yesno, rows$n_yesno),
                            cell(rows$rating, rows$n_rating), cell(rows$pairwise, rows$n_pairwise), cell(rows$topic, rows$n_topic))),
           file.path(TEX, "robustness_rows.tex"), useBytes = TRUE)
print(rows[, 1:5], row.names = FALSE, digits = 3)
r <- unlist(rows[, c("yesno", "rating", "pairwise")]); r <- r[!is.na(r)]
cat(sprintf("range over the language-model cells: %.3f to %.3f; topic model: %.3f to %.3f\n", min(r), max(r),
            min(rows$topic, na.rm = TRUE), max(rows$topic, na.rm = TRUE)))

# Where recovery is lower: language, source and arm size together (yes/no, reference model)
q <- sel(REF, "A1_presence")
q$group <- ifelse(q$own, "author, non-English", ifelse(q$english, "public, English", "public, other languages"))
tab <- aggregate(cbind(recovered = rec, small_arm = small_arm) ~ group, q, function(x) sprintf("%d of %d", sum(x), length(x)))
print(tab, row.names = FALSE)
pn <- q[!q$own & !q$english, ]
cat(sprintf("public, other languages: %d contrasts in %d studies; recovered %d; with the smaller arm of 200 or more: %d of %d; without the two Danish studies: %d of %d\n",
            nrow(pn), length(unique(pn$dataset)), sum(pn$rec), sum(pn$rec & !pn$small_arm), sum(!pn$small_arm),
            sum(pn$rec & !grepl("^bisgaard", pn$dataset)), sum(!grepl("^bisgaard", pn$dataset))))
pe <- q[!q$own & q$english, ]
cat(sprintf("public, English: recovered %d of %d; with the smaller arm of 200 or more: %d of %d\n", sum(pe$rec), nrow(pe),
            sum(pe$rec & !pe$small_arm), sum(!pe$small_arm)))
for (m in c("gemma4_e4b", "gpt6_luna", "gemma4_31b")) {
  z <- sel(m, "A1_presence"); z <- z[!z$own & !z$english, ]
  k <- merge(z[, c("dataset", "construct", "contrast", "rec")], pn[, c("dataset", "construct", "contrast", "rec")],
             by = c("dataset", "construct", "contrast"), suffixes = c("", "_ref"))
  cat(sprintf("  %s on public, other languages: recovered %d of %d (reference model on the same contrasts: %d)\n", m, sum(k$rec), nrow(k), sum(k$rec_ref)))
}
fit <- glm(rec ~ I(log(pmin(n1, n0))) + english + own, data = q, family = binomial)
print(round(summary(fit)$coefficients, 3))

# The pairwise arm covers only the first target of each study. Compare the arms
# on those same contrasts, by round of collection.
key <- c("dataset", "construct", "contrast")
b <- sel(REF, "B_pairwise")
m <- merge(b[, c(key, "rec")], sel(REF, "A1_presence")[, c(key, "rec")], by = key, suffixes = c("_pairwise", "_yesno"))
m$round <- ifelse(m$dataset %in% early, "October", "November")
for (r in c("October", "November")) with(m[m$round == r, ], cat(sprintf(
  "pairwise contrasts, %s (%d): pairwise %.3f, yes/no %.3f; same recovery call %.3f\n",
  r, length(rec_pairwise), mean(rec_pairwise), mean(rec_yesno), mean(rec_pairwise == rec_yesno))))
bt <- read.csv(file.path(OUT, "bt_diagnostics.csv"))
bt <- bt[bt$model == REF, ]
cat(sprintf("reference model, held-out pairwise accuracy (median): October %.3f, November %.3f\n",
            median(bt$heldout_acc[bt$dataset %in% early], na.rm = TRUE), median(bt$heldout_acc[!bt$dataset %in% early], na.rm = TRUE)))

