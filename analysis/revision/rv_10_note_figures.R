# Compact figures for the research note (paper/revision/figures/note_*.pdf).
# The full per-study figures from rv_09_summary.R go to the SI.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_10_note_figures.R

suppressMessages({ library(dplyr); library(ggplot2) })
OUT <- "analysis/output/revision"
FIG <- "paper/revision/figures"
theme_oc <- theme_bw(base_size = 12) + theme(panel.background = element_rect(fill = "grey95"),
  panel.grid = element_line(colour = "white"), legend.position = "bottom")
own_prefix <- "^(sktw|aks21|ifes2023|nwo26|prc|roc|rok)"

# Figure 1: false-positive rate under permuted labels, studies sorted by the full-sample procedure
dd <- read.csv(file.path(OUT, "tables", "doubledip_fpr.csv"))
dd <- dd[order(dd$fpr_old), ]
dd$rank <- seq_len(nrow(dd))
proc <- c("Full sample, topic selected", "Split: discovery sample to estimation sample")
long <- rbind(data.frame(rank = dd$rank, procedure = proc[1], fpr = dd$fpr_old),
              data.frame(rank = dd$rank, procedure = proc[2], fpr = dd$fpr_split))
mns <- data.frame(procedure = proc, m = c(mean(dd$fpr_old), mean(dd$fpr_split)), x = c(1, nrow(dd)), y = c(0.83, 0.14), h = c(0, 1))
mns$lab <- sprintf("Mean, %s: %.1f percent", c("full sample", "split"), 100 * mns$m)
g1 <- ggplot(long, aes(rank, 100 * fpr, colour = procedure, shape = procedure)) +
  geom_hline(yintercept = 5, linetype = 2) + geom_point(size = 2) +
  geom_text(data = mns, aes(x, 100 * y, label = lab, hjust = h), size = 3.5, show.legend = FALSE) +
  scale_colour_manual(values = c("#D55E00", "#0072B2"), name = NULL) +
  scale_shape_manual(values = c(16, 17), name = NULL) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = sprintf("Study (%d), ordered by the full-sample rate", nrow(dd)),
       y = "False positives (percent, p < .05)") + theme_oc
ggsave(file.path(FIG, "note_doubledip.pdf"), g1, width = 6.5, height = 3.6)

# Figure 2: (a) z-statistics of primary target contrasts by measure, reference model and topic models;
# (b) detection rate by whether the target restates displayed content (rv_34, coder A; intervals from rv_24 and rv_34).
suppressMessages(library(patchwork))
a1 <- read.csv(file.path(OUT, "effects_a1.csv"))
# pairwise scaling is reported in the SI only (it suits presence measures poorly)
fam <- c("Language model", "Topic model"); fam_col <- c("#0072B2", "#D55E00"); fam_shape <- c(16, 17)
lab <- c("qwen36_35b|A1_presence" = "Yes/no question", "qwen36_35b|C_rating" = "Rating, 0 to 9",
         "stm|D_topic" = "Topic share", "stmcov|D_topic" = "Topic share,\ntreatment covariate")
z <- rbind(a1, read.csv(file.path(OUT, "effects_stmcov.csv"))) %>% filter(role == "target", primary) %>%
  mutate(k = paste(model, arm, sep = "|")) %>% filter(k %in% names(lab)) %>%
  mutate(sgn = ifelse(is.na(expected_sign), 1, expected_sign), z = estimate / se * sgn, det = p_ri < .05 & estimate * sgn > 0,
         measure = factor(lab[k], levels = rev(lab)), kind = factor(ifelse(grepl("^stm", model), fam[2], fam[1]), levels = fam))
rate <- z %>% group_by(measure, kind) %>% summarise(d = sum(det, na.rm = TRUE), .groups = "drop") %>%
  mutate(txt = sprintf("%d%% of 205", round(100 * d / 205)))
stopifnot(rate$d[order(rate$measure)] == c(79, 74, 140, 138))   # covariate topic, topic, rating, yes/no
theme_f2 <- theme_oc + theme(plot.title = element_text(size = 11, face = "bold"), plot.title.position = "plot")
g2a <- ggplot(z, aes(pmax(pmin(z, 20), -5), measure, colour = kind, shape = kind)) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = 2, linewidth = 0.4) + geom_vline(xintercept = 0, colour = "grey50") +
  geom_jitter(height = 0.2, width = 0, size = 1.3, alpha = 0.55) +
  geom_text(data = rate, aes(x = 21.9, label = txt), hjust = 0, size = 3.4, colour = "black") +
  scale_colour_manual(values = setNames(fam_col, fam), guide = "none") + scale_shape_manual(values = setNames(fam_shape, fam), guide = "none") +
  scale_x_continuous(breaks = seq(-5, 20, 5)) + coord_cartesian(xlim = c(-5, 20), clip = "off") +
  labs(x = "z-statistic in the theorized direction (truncated at -5 and 20)", y = NULL,
       title = "a. Test statistics of the 205 primary contrasts") + theme_f2 + theme(plot.margin = margin(5, 78, 5, 5))
num <- function(x) as.numeric(strsplit(gsub("\\[|\\]", "", x), ", ")[[1]])
iv <- read.csv(file.path(OUT, "tables", "note_intervals.csv")); dc <- read.csv(file.path(OUT, "tables", "displayed_content.csv"))
dc <- dc[dc$coding == "displayed_A" & dc$measure %in% c("llm35", "stm") & dc$group %in% c("yes", "no"), ]
grp <- c(all = "All contrasts (205)", yes = sprintf("Target restates\ndisplayed content (%d)", dc$n[dc$group == "yes"][1]),
         no = sprintf("Other targets (%d)", dc$n[dc$group == "no"][1]))
b <- rbind(data.frame(group = "all", measure = c("llm35", "stm"), rate = iv$est[match(c("llm35", "stm"), iv$quantity)],
                      ci = iv$ci[match(c("llm35", "stm"), iv$quantity)]), dc[, c("group", "measure", "rate", "ci")])
b$lo <- vapply(b$ci, function(x) num(x)[1], 0); b$hi <- vapply(b$ci, function(x) num(x)[2], 0)
b$group <- factor(grp[b$group], levels = rev(grp)); b$kind <- factor(ifelse(b$measure == "stm", "Topic share", "Yes/no question"), levels = c("Yes/no question", "Topic share"))
g2b <- ggplot(b, aes(100 * rate, group, colour = kind, shape = kind)) +
  geom_errorbarh(aes(xmin = 100 * lo, xmax = 100 * hi), height = 0, position = position_dodge(width = -0.5)) +
  geom_point(size = 2.4, position = position_dodge(width = -0.5)) +
  scale_colour_manual(values = setNames(fam_col, levels(b$kind)), name = NULL) + scale_shape_manual(values = setNames(fam_shape, levels(b$kind)), name = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = "Contrasts detected (percent, with 95 percent interval)", y = NULL, title = "b. Detection by whether the target restates displayed content") + theme_f2
g2 <- g2a / g2b + plot_layout(heights = c(1.25, 1))
ggsave(file.path(FIG, "note_recovery.pdf"), g2, width = 6.5, height = 5.4)
cat("wrote", file.path(FIG, c("note_doubledip.pdf", "note_recovery.pdf")), "\n")

# Figure: the worked example (SK/TW naturalization), all panels from existing outputs
id <- "sktw_naturalization_2024"
ex <- a1 %>% filter(dataset == id, contrast == "cue_shown")
model_lab <- c(qwen36_35b = "Qwen3.6-35B", qwen36_27b = "Qwen3.6-27B", gemma4_31b = "Gemma 4 31B",
               gemma4_e4b = "Gemma 4 E4B", gpt6_luna = "GPT-6 Luna", majority = "Majority")
con_lab <- c(civic_commitment = "Civic commitment\n(target)", family_ties = "Marriage or family ties\n(placebo)",
             ethnic_ancestry = "Ethnic ancestry\n(placebo)")
theme_ex <- theme_bw(base_size = 9.5) + theme(panel.background = element_rect(fill = "grey95"), axis.title.x = element_text(size = 8.5),
  panel.grid = element_line(colour = "white"), plot.title = element_text(size = 10, face = "bold"),
  plot.title.position = "plot")
pa <- ex %>% filter(arm == "A1_presence", model %in% names(model_lab)) %>%
  mutate(model = factor(model_lab[model], levels = rev(model_lab)),
         construct = factor(con_lab[construct], levels = con_lab))
ga <- ggplot(pa, aes(100 * estimate, model, colour = role)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_errorbarh(aes(xmin = 100 * ci_low, xmax = 100 * ci_high), height = 0) + geom_point(size = 2) +
  facet_wrap(~construct, ncol = 3) +
  scale_colour_manual(values = c(target = "#0072B2", placebo = "#999999"), guide = "none") +
  labs(x = "Effect of showing the attribute on the share of answers that mention the construct\n(percentage points)", y = NULL,
       title = "a. Frozen yes/no measures on the estimation sample") + theme_ex
sp <- read.csv(file.path(OUT, "effects_a2_splits.csv")) %>% filter(dataset == id, primary)
pub <- read.csv(file.path(OUT, "published_on_E.csv")) %>% filter(dataset == id)
names(pub)[1:5] <- c("dataset", "country", "sample", "measure", "estimate")
pe <- pub[pub$sample == "E", ]
wh <- c("Induced on the\ndiscovery sample\n(5 splits, countries pooled)", "Codebook developed\non the full sample")
# the pooled splits and the two country estimates get their own colour and shape, as in Figure 2
ser <- c("Countries pooled", "South Korea", "Taiwan"); ser_col <- c("#0072B2", "#D55E00", "#009E73"); ser_shape <- c(16, 17, 15)
pb <- rbind(data.frame(what = wh[1], est = sp$estimate, lo = sp$ci_low, hi = sp$ci_high, lab = "", ser = ser[1], g = seq_len(nrow(sp))),
            data.frame(what = wh[2], est = pe$estimate, lo = pe$ci_low, hi = pe$ci_high, lab = pe$country, ser = pe$country, g = seq_len(nrow(pe))))
stopifnot(pb$ser %in% ser, sort(pe$country) == ser[2:3])
pb$what <- factor(pb$what, levels = rev(wh)); pb$ser <- factor(pb$ser, levels = ser); dg <- position_dodge(width = 0.7)
gb <- ggplot(pb, aes(100 * est, what, group = g, colour = ser, shape = ser)) + geom_vline(xintercept = 0, colour = "grey50") +
  geom_errorbarh(aes(xmin = 100 * lo, xmax = 100 * hi), height = 0, alpha = 0.8, position = dg) +
  geom_point(size = 2.3, position = dg) +
  geom_text(aes(x = 100 * hi, label = lab), hjust = -0.15, size = 2.8, colour = "black", position = dg) +
  scale_colour_manual(values = setNames(ser_col, ser), guide = "none") + scale_shape_manual(values = setNames(ser_shape, ser), guide = "none") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  labs(x = "Effect on the civic code (percentage points)", y = NULL, title = "b. Codebooks") + theme_ex
# Panel c (topic selection, full sample vs split) was removed on 24 Nov 2026: the fits do not
# reproduce across row orders (rv_26, stm_refits_example.csv).
g3 <- ga / gb + plot_layout(heights = c(1, 0.75))
ggsave(file.path(FIG, "note_example.pdf"), g3, width = 6.5, height = 4.6)
print(pa %>% select(model, construct, estimate, ci_low, ci_high, p_ri) %>% arrange(construct, model), digits = 3)
print(round(100 * sort(sp$estimate), 1)); print(pub[, c("country", "sample", "estimate")])
print(ex %>% filter(role == "target", arm %in% c("C_rating", "B_pairwise")) %>% select(model, arm, est_sd), digits = 2)
