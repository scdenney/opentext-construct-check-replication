# Figures for the SI (paper/revision/figures/si_*.pdf), each at most 6.5 x 8.5 in
# and readable at print size. Per-study numbers are in the SI tables, so no
# figure lists every study.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_12_si_figures.R

suppressMessages({ library(dplyr); library(ggplot2); library(patchwork) })
OUT <- "analysis/output/revision"
FIG <- "paper/revision/figures"
theme_si <- theme_bw(base_size = 10) + theme(panel.background = element_rect(fill = "grey95"),
  panel.grid = element_line(colour = "white"), legend.position = "bottom",
  plot.title = element_text(size = 10, face = "bold"), plot.title.position = "plot")
model_lab <- c(qwen36_35b = "Qwen3.6-35B (reference)", gemma4_e4b = "Gemma 4 E4B",
               gpt6_luna = "GPT-6 Luna", gemma4_31b = "Gemma 4 31B")
own_prefix <- "^(sktw|aks21|ifes2023|nwo26|prc|roc|rok)"
a1 <- read.csv(file.path(OUT, "effects_a1.csv"))
prim <- a1 %>% filter(role == "target", primary)

# 1. Model agreement: study-level yes/no estimates against the reference model
ref <- prim %>% filter(model == "qwen36_35b", arm == "A1_presence") %>%
  select(dataset, construct, contrast, ref = est_sd)
others <- intersect(setdiff(names(model_lab), "qwen36_35b"), unique(prim$model))
ma <- prim %>% filter(model %in% others, arm == "A1_presence") %>%
  select(dataset, construct, contrast, model, est = est_sd) %>%
  inner_join(ref, by = c("dataset", "construct", "contrast")) %>%
  mutate(source = ifelse(grepl(own_prefix, dataset), "Author's studies", "Public archives"))
rs <- ma %>% group_by(model) %>% summarise(r = cor(est, ref), n = n(), .groups = "drop") %>%
  mutate(lab = sprintf("r = %.2f, n = %d", r, n))
ma$model <- factor(model_lab[ma$model], levels = model_lab)
rs$model <- factor(model_lab[rs$model], levels = model_lab)
lim <- range(c(ma$est, ma$ref))
g1 <- ggplot(ma, aes(ref, est)) + geom_abline(linetype = 2, colour = "grey40") +
  geom_hline(yintercept = 0, colour = "grey70") + geom_vline(xintercept = 0, colour = "grey70") +
  # 24 Nov 2026: source mapped to shape as well as colour (triangle = author's studies, circle = public archives)
  geom_point(aes(colour = source, shape = source), size = 1.6, alpha = 0.8) +
  geom_text(data = rs, aes(x = lim[1], y = lim[2], label = lab), hjust = 0, vjust = 1, size = 3.2) +
  facet_wrap(~model, nrow = 1) + coord_equal(xlim = lim, ylim = lim) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9"), name = NULL) + scale_shape_manual(values = c(17, 16), name = NULL) +
  labs(x = "Reference model (Qwen3.6-35B), SD units", y = "Other model, SD units") + theme_si
ggsave(file.path(FIG, "si_model_agreement.pdf"), g1, width = 6.5, height = 3.4)

# 2. Arm agreement within the reference model
aw <- prim %>% filter(model == "qwen36_35b", arm %in% c("A1_presence", "C_rating", "B_pairwise")) %>%
  select(dataset, construct, contrast, arm, est_sd) %>%
  tidyr::pivot_wider(names_from = arm, values_from = est_sd)
arm_panel <- function(y, ylab) {
  d <- aw[!is.na(aw[[y]]), ]
  ggplot(d, aes(A1_presence, .data[[y]])) + geom_abline(linetype = 2, colour = "grey40") +
    geom_point(size = 1.6, alpha = 0.8, colour = "#0072B2") +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.3, size = 3.2,
             label = sprintf("r = %.2f, n = %d", cor(d$A1_presence, d[[y]]), nrow(d))) +
    labs(x = "Yes/no, SD units", y = ylab) + theme_si
}
g2 <- arm_panel("C_rating", "Rating 0-9, SD units") | arm_panel("B_pairwise", "Pairwise (Bradley-Terry), SD units")
ggsave(file.path(FIG, "si_arm_agreement.pdf"), g2, width = 6.5, height = 3.2)

# 3. Induced codebooks across five splits (author's studies, where five were drawn)
sp <- read.csv(file.path(OUT, "effects_a2_splits.csv")) %>% filter(primary) %>%
  group_by(dataset, construct, contrast) %>% filter(n() >= 5) %>%
  mutate(med = median(estimate)) %>% ungroup() %>%
  mutate(label = paste0(dataset, ": ", gsub("_", " ", construct)))
sp$label <- factor(sp$label, levels = unique(sp$label[order(sp$med)]))
g3 <- ggplot(sp, aes(100 * estimate, label)) + geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(colour = "#0072B2", alpha = 0.6, size = 1.6) +
  geom_point(aes(x = 100 * med), shape = 124, size = 4) +
  labs(x = "Effect on the target code (percentage points)", y = NULL) + theme_si
ggsave(file.path(FIG, "si_a2_splits.pdf"), g3, width = 6.5, height = 0.22 * n_distinct(sp$label) + 1.2)

# 4. Placebos: share rising and falling at p < .05, by model and arm
arm_lab <- c(A1_presence = "Yes/no", A1_presence_soft = "Yes/no (probability)", C_rating = "Rating 0-9")
pl <- a1 %>% filter(role == "placebo", model %in% c(names(model_lab), "majority"), arm %in% names(arm_lab)) %>%
  group_by(model, arm) %>%
  summarise(n = n(), Rise = mean(p_ri < .05 & estimate > 0), Fall = mean(p_ri < .05 & estimate < 0),
            .groups = "drop") %>%
  tidyr::pivot_longer(c(Rise, Fall), names_to = "direction", values_to = "share") %>%
  mutate(se = sqrt(share * (1 - share) / n),
         model = factor(c(model_lab, majority = "Majority")[model], levels = rev(c(model_lab, "Majority"))),
         arm = factor(arm_lab[arm], levels = arm_lab))
g4 <- ggplot(pl, aes(100 * share, model, colour = direction)) +
  geom_vline(xintercept = 2.5, linetype = 2) +
  geom_errorbar(aes(xmin = 100 * pmax(share - 1.96 * se, 0), xmax = 100 * (share + 1.96 * se)),
                 width = 0, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  facet_wrap(~arm, nrow = 1) +
  scale_colour_manual(values = c(Rise = "#D55E00", Fall = "#0072B2"), name = NULL) +
  labs(x = "Placebo estimates significant at p < .05 (percent)", y = NULL) + theme_si
ggsave(file.path(FIG, "si_placebo.pdf"), g4, width = 6.5, height = 3.2)

# 5. NK held-out: test-half kappa by codebook version, mean +/- 1 SD over ten splits
nk <- read.csv(file.path(OUT, "nk_heldout_eval.csv"))
ver_lab <- c(v0 = "Theory only", rev1 = "LLM revision 1", rev2 = "LLM revision 2",
             rev_final = "LLM revision 2", v3_published = "Researchers' v3\n(in-sample)")
nk <- nk %>% filter(half == "test", version %in% names(ver_lab)) %>%
  mutate(version = ifelse(version == "rev2", "rev_final", version)) %>%
  filter(!(version == "rev1")) %>%
  group_by(model, version) %>% summarise(k = mean(kappa), sd = sd(kappa), .groups = "drop") %>%
  mutate(x = match(version, c("v0", "rev_final", "v3_published")),
         model = c(gemma4_e4b = "Gemma 4 E4B", gemma4_12b = "Gemma 4 12B",
                   qwen36_27b = "Qwen3.6-27B", qwen36_35b = "Qwen3.6-35B")[model])
pal <- c("Gemma 4 E4B" = "#009E73", "Gemma 4 12B" = "#E69F00", "Qwen3.6-27B" = "#56B4E9", "Qwen3.6-35B" = "#CC79A7")
g5 <- ggplot(nk, aes(x, k, colour = model)) +
  geom_line(data = filter(nk, x <= 2), linewidth = 0.8) +
  geom_line(data = filter(nk, x >= 2), linewidth = 0.8, linetype = 2) +
  geom_errorbar(aes(ymin = k - sd, ymax = k + sd), width = 0.06, position = position_dodge(width = 0.15)) +
  geom_point(size = 2, position = position_dodge(width = 0.15)) +
  geom_text(data = filter(nk, x == 1), aes(label = model), hjust = 1.15, size = 3.2, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Theory only", "LLM revision\n(2 rounds, dev half)", "Researchers' v3\n(in-sample)"),
                     expand = expansion(add = c(0.9, 0.2))) +
  scale_colour_manual(values = pal, guide = "none") +
  labs(x = NULL, y = "Cohen's kappa with human codes, test half") + theme_si
ggsave(file.path(FIG, "si_nk_heldout.pdf"), g5, width = 6.5, height = 3.6)
cat("wrote", length(list.files(FIG, pattern = "^si_")), "SI figures\n")

# Pairwise scaling: held-out accuracy against how common the target is (written by rv_11)
pb <- read.csv(file.path(OUT, "tables", "pairwise_by_study.csv"))
pb$outcome <- factor(ifelse(pb$rec_b & pb$rec_y, "Both recover the effect", ifelse(pb$rec_y, "Yes/no only",
                     ifelse(pb$rec_b, "Pairwise only", "Neither"))),
                     levels = c("Both recover the effect", "Yes/no only", "Pairwise only", "Neither"))
gp <- ggplot(pb, aes(100 * share, heldout_acc, colour = outcome, shape = outcome)) +
  geom_vline(xintercept = c(10, 25), linetype = 3, colour = "grey50") +
  geom_point(size = 2, alpha = 0.85) +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73", "#999999"), name = NULL) +
  scale_shape_manual(values = c(16, 17, 15, 1), name = NULL) +
  labs(x = "Answers that show the target construct (percent, yes/no label)",
       y = "Held-out pairwise accuracy") + theme_si
ggsave(file.path(FIG, "si_pairwise.pdf"), gp, width = 6.5, height = 3.8)

