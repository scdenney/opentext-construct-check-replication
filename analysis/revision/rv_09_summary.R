# ==============================================================================
# rv_09_summary.R -- cross-study tables and figures for the revision
# ==============================================================================
# Inputs (whichever exist): effects_a1.csv, effects_a2.csv, effects_a2_splits.csv,
# bt_diagnostics.csv, doubledip/*_summary.csv, nk_heldout_eval.csv, corpus_table.csv.
# Outputs: analysis/output/revision/{tables,figures}/ and summary_numbers.md
# (every number quoted in the results memo).
# ==============================================================================

source("analysis/revision/rv_config.R")
suppressMessages({library(ggplot2); library(dplyr)})
TAB <- file.path(OUT, "tables"); FIG <- file.path(OUT, "figures")
dir.create(TAB, showWarnings = FALSE); dir.create(FIG, showWarnings = FALSE)
rd <- function(f) { p <- file.path(OUT, f); if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL }
theme_oc <- theme_bw(base_size = 12) + theme(panel.background = element_rect(fill = "grey95"),
  panel.grid = element_line(colour = "white"), legend.position = "bottom")
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
num <- character(0)
note <- function(...) num <<- c(num, paste0(...))

# --- 1. Theory-only arms: target (primary) and placebo estimates ---------------
# Primary model: qwen36_35b (the only model run on every dataset; PRC and the
# North Korean-migrant samples stay on ALICE). The other models and the cross-model majority
# are robustness rows.
a1 <- rd("effects_a1.csv")
if (!is.null(a1)) {
  a1$sig <- a1$p_ri < 0.05
  a1$sign_ok <- ifelse(is.na(a1$expected_sign), NA, sign(a1$estimate) == a1$expected_sign)
  # Headline measure: a target is recovered when the estimate is significant AND
  # has the theorized sign. `sig` alone also counts significant reversals.
  a1$rec <- a1$sig & !is.na(a1$sign_ok) & a1$sign_ok
  prim <- a1 %>% filter(primary, role == "target")
  plac <- a1 %>% filter(role == "placebo")
  arm_rates <- bind_rows(
    prim %>% group_by(model, arm) %>% summarise(kind = "target (primary contrast)", n = n(),
      n_datasets = n_distinct(dataset), share_recovered = mean(rec), share_p05 = mean(sig),
      share_expected_sign = mean(sign_ok, na.rm = TRUE),
      share_p05_up = NA_real_, share_p05_down = NA_real_, .groups = "drop"),
    plac %>% group_by(model, arm) %>% summarise(kind = "placebo (all contrasts)", n = n(),
      n_datasets = n_distinct(dataset), share_p05 = mean(sig), share_expected_sign = NA_real_,
      share_p05_up = mean(sig & estimate > 0), share_p05_down = mean(sig & estimate < 0), .groups = "drop"))
  write.csv(arm_rates, file.path(TAB, "a1_detection_and_placebo_rates.csv"), row.names = FALSE)

  # Per survey sample: several cards share respondents (instrument_of() in
  # rv_config.R). (a) weight each sample once; (b) every corpus that keeps one
  # card per sample (min, median, max).
  per_sample <- function(d, value) {
    d$inst <- instrument_of(d$dataset)
    d$v <- value(d)
    w <- d %>% group_by(inst) %>% mutate(w = 1 / n()) %>% ungroup()
    cards <- split(unique(d$dataset), instrument_of(unique(d$dataset)))
    combos <- expand.grid(cards[lengths(cards) > 1], stringsAsFactors = FALSE)
    single <- unlist(cards[lengths(cards) == 1])
    one <- if (nrow(combos)) apply(combos, 1, function(k) mean(d$v[d$dataset %in% c(single, k)], na.rm = TRUE))
           else mean(d$v, na.rm = TRUE)
    data.frame(n_samples = length(cards), weighted = weighted.mean(w$v, w$w, na.rm = TRUE),
               one_min = min(one), one_med = median(one), one_max = max(one), n_corpora = length(one))
  }
  clus <- bind_rows(lapply(split(prim, list(prim$model, prim$arm), drop = TRUE), function(d) cbind(
    model = d$model[1], arm = d$arm[1], n = nrow(d), n_datasets = n_distinct(d$dataset),
    unweighted = mean(d$rec), per_sample(d, function(x) x$rec),
    sig_unweighted = mean(d$sig), sig_weighted = per_sample(d, function(x) x$sig)$weighted,
    sign_weighted = per_sample(d[!is.na(d$sign_ok), ], function(x) x$sign_ok)$weighted)))
  write.csv(clus, file.path(TAB, "a1_rates_per_sample.csv"), row.names = FALSE)
  for (i in seq_len(nrow(clus))) with(clus[i, ], note(sprintf(
    "%s | %s | per sample: %d cards from %d samples; recovered unweighted %.2f, sample-weighted %.2f, one card per sample %.2f (%.2f-%.2f over %d corpora); any p<.05 unweighted %.2f, weighted %.2f; expected sign weighted %.2f",
    model, arm, n_datasets, n_samples, unweighted, weighted, one_med, one_min, one_max, n_corpora, sig_unweighted, sig_weighted, sign_weighted)))
  for (i in seq_len(nrow(arm_rates))) with(arm_rates[i, ], note(sprintf(
    "%s | %s | %s: n=%d (%d datasets), p<.05 = %.2f%s", model, arm, kind, n, n_datasets, share_p05,
    if (kind == "placebo (all contrasts)") sprintf(" (up %.3f, down %.3f)", share_p05_up, share_p05_down)
    else sprintf(", recovered (p<.05 in the theorized direction) %.2f, expected sign %.2f", share_recovered, share_expected_sign))))
  fp <- prim %>% filter(model == "qwen36_35b", arm %in% c("A1_presence", "C_rating", "B_pairwise")) %>%
    group_by(dataset, construct) %>%
    mutate(label = if (n_distinct(contrast) > 1) paste0(dataset, " : ", construct, " (", contrast, ")")
                   else paste(dataset, construct, sep = " : ")) %>% ungroup()
  if (nrow(fp)) {
    ord <- fp %>% filter(arm == "A1_presence") %>% arrange(est_sd) %>% pull(label)
    fp$label <- factor(fp$label, levels = unique(c(ord, setdiff(fp$label, ord))))
    g <- ggplot(fp, aes(x = est_sd, y = label, colour = arm)) +
      geom_vline(xintercept = 0, linetype = 2) +
      geom_errorbarh(aes(xmin = est_sd - 1.96 * se_sd, xmax = est_sd + 1.96 * se_sd),
                     height = 0, position = position_dodge(width = 0.6)) +
      geom_point(position = position_dodge(width = 0.6)) +
      scale_colour_manual(values = okabe, labels = c(A1_presence = "A1 yes/no presence",
        B_pairwise = "B pairwise (Bradley-Terry)", C_rating = "C 0-9 rating")) +
      labs(x = "Effect of the primary contrast (SD of the measure, estimation fold)", y = NULL, colour = NULL) +
      theme_oc
    ggsave(file.path(FIG, "fig_target_effects_by_arm.pdf"), g, width = 9, height = max(4, 0.32 * length(levels(fp$label))), limitsize = FALSE)
  }
  pl <- plac %>% filter(model == "qwen36_35b")
  if (nrow(pl)) {
    g <- ggplot(pl, aes(x = est_sd)) + geom_histogram(bins = 30, fill = "grey40") +
      geom_vline(xintercept = 0, linetype = 2) + facet_wrap(~arm) +
      labs(x = "Placebo construct effect (SD units)", y = "Count") + theme_oc
    ggsave(file.path(FIG, "fig_placebo_effects.pdf"), g, width = 8, height = 3.8)
  }
}

# --- 2. Induced codebooks (A2): split sensitivity -------------------------------
a2s <- rd("effects_a2_splits.csv"); a2 <- rd("effects_a2.csv")
if (!is.null(a2s)) {
  sp <- a2s %>% filter(primary) %>% mutate(label = paste(dataset, contrast, sep = " : "))
  g <- ggplot(sp, aes(x = estimate, y = label)) + geom_vline(xintercept = 0, linetype = 2) +
    geom_point(aes(colour = k == 1), alpha = 0.7) + scale_colour_manual(values = c("grey50", "#D55E00"),
    labels = c("splits 2-11", "primary split"), name = NULL) +
    labs(x = "Effect on share coded as target (A2, per split)", y = NULL) + theme_oc
  ggsave(file.path(FIG, "fig_a2_split_sensitivity.pdf"), g, width = 9, height = max(4, 0.3 * length(unique(sp$label))), limitsize = FALSE)
  if (!is.null(a2)) {
    pr <- a2[a2$primary, ]
    note(sprintf("A2 primary contrasts: %d; median-aggregated p<.05 (|est/se|>1.96) in %d; mean share of splits significant %.2f",
                 nrow(pr), sum(abs(pr$estimate / pr$se) > 1.96), mean(pr$share_sig)))
  }
}

# --- 3. Pairwise BT diagnostics -----------------------------------------------
bt <- rd("bt_diagnostics.csv")
if (!is.null(bt)) {
  write.csv(bt, file.path(TAB, "bt_diagnostics.csv"), row.names = FALSE)
  note(sprintf("BT held-out accuracy: median %.3f (range %.3f-%.3f); ICI median %.3f; mean |order gap| %.3f",
               median(bt$heldout_acc), min(bt$heldout_acc), max(bt$heldout_acc), median(bt$ici), mean(bt$order_gap, na.rm = TRUE)))
}

# --- 4. Double-dipping --------------------------------------------------------
dd_files <- list.files(file.path(OUT, "doubledip"), pattern = "_summary.csv$", full.names = TRUE)
if (length(dd_files)) {
  dd <- do.call(rbind, lapply(dd_files, function(x) {
    y <- read.csv(x)
    for (c in c("fpr_old_fixed", "fpr_split_fixed")) if (!c %in% names(y)) y[[c]] <- NA
    y
  }))
  # Assignment probability varies by country in nwo26_exp3, which confounds the
  # pooled topic test (see revision/deviations.md).
  dd <- dd[dd$dataset != "nwo26_exp3_comprehension", ]
  write.csv(dd, file.path(TAB, "doubledip_fpr.csv"), row.names = FALSE)
  note(sprintf("Double-dipping (STM, permuted labels, B=200): mean FPR old = %.3f, split = %.3f; no selection (fixed topic): old = %.3f, split = %.3f; over %d datasets",
               mean(dd$fpr_old), mean(dd$fpr_split), mean(dd$fpr_old_fixed, na.rm = TRUE),
               mean(dd$fpr_split_fixed, na.rm = TRUE), nrow(dd)))
  ddi <- dd %>% group_by(inst = instrument_of(dataset)) %>%
    summarise(old = mean(fpr_old), split = mean(fpr_split), .groups = "drop")
  note(sprintf("Double-dipping per survey sample (each sample counts once): FPR old = %.3f, split = %.3f; %d samples",
               mean(ddi$old), mean(ddi$split), nrow(ddi)))
  long <-rbind(data.frame(dataset = dd$dataset, procedure = "full sample (old)", fpr = dd$fpr_old),
                data.frame(dataset = dd$dataset, procedure = "split (D -> E)", fpr = dd$fpr_split))
  g <- ggplot(long, aes(x = fpr, y = dataset, colour = procedure)) + geom_vline(xintercept = 0.05, linetype = 2) +
    geom_point(size = 2.5) + scale_colour_manual(values = c("#D55E00", "#0072B2"), name = NULL) +
    labs(x = "False-positive rate under permuted treatment (p < .05)", y = NULL) + theme_oc
  ggsave(file.path(FIG, "fig_doubledip_fpr.pdf"), g, width = 8, height = max(3.5, 0.35 * nrow(dd)), limitsize = FALSE)
}

# --- 5. NK held-out: codebook iteration vs model size --------------------------
nk <- rd("nk_heldout_eval.csv")
if (!is.null(nk)) {
  s <- nk %>% group_by(model, version, half) %>% summarise(kappa = mean(kappa), sd = sd(kappa),
        acc = mean(accuracy), n_seeds = n(), .groups = "drop")
  write.csv(s, file.path(TAB, "nk_heldout_summary.csv"), row.names = FALSE)
  for (i in seq_len(nrow(s))) with(s[i, ], note(sprintf("NK held-out | %s | %s | %s: kappa %.3f, acc %.3f (%d seeds)",
                                                        model, version, half, kappa, acc, n_seeds)))
  te <- nk %>% filter(half == "test")
  g <- ggplot(te, aes(x = version, y = kappa, colour = model)) + geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.5) + scale_colour_manual(values = okabe) +
    labs(x = "Codebook version", y = "Cohen's kappa vs human codes (test half)", colour = NULL) + theme_oc
  ggsave(file.path(FIG, "fig_nk_heldout.pdf"), g, width = 8, height = 4.5)
}

writeLines(c("# Numbers for the results memo (generated by rv_09_summary.R)", "", paste0("- ", num)),
           file.path(OUT, "summary_numbers.md"))
cat(paste0("- ", num), sep = "\n")
