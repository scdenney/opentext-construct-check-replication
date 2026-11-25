# rv_25_human_figure.R -- main-text figure: effects on the original authors' codes against the model label and
# the matched topic share, same answers (paper/revision/figures/note_human.pdf). Signs follow the theorized direction.
suppressMessages({ library(ggplot2) })
h <- read.csv("analysis/output/revision/tables/human_codes_effects.csv"); s <- h$expected_sign
a <- data.frame(x = s * h$est_human, y = s * h$est_model, p = "Zero-shot yes/no label")
t <- h[h$has_stm == TRUE & !is.na(h$est_stm), ]
b <- data.frame(x = t$expected_sign * t$est_human_t, y = t$expected_sign * t$est_stm, p = "Matched topic share")
cat("n", nrow(a), nrow(b), " r", cor(a$x, a$y), cor(b$x, b$y), " same sign", sum(sign(a$x) == sign(a$y)), sum(sign(b$x) == sign(b$y)), "\n")
pl <- c("Zero-shot yes/no label (percentage points)", "Matched topic share (points of topic share)")
a$p <- pl[1]; b$p <- pl[2]
d <- rbind(a, b); d$p <- factor(d$p, levels = pl); d[, 1:2] <- 100 * d[, 1:2]
ann <- data.frame(p = factor(pl, levels = pl), lab = sprintf("r = %.2f, n = %d", c(cor(a$x, a$y), cor(b$x, b$y)), c(nrow(a), nrow(b))))
g <- ggplot(d, aes(x, y)) + geom_hline(yintercept = 0, colour = "grey50") + geom_vline(xintercept = 0, colour = "grey50") +
  geom_abline(data = data.frame(p = factor(pl[1], levels = pl)), aes(intercept = 0, slope = 1), linetype = 2) +
  geom_point(size = 1.8, colour = "#0072B2", alpha = .8) + facet_wrap(~p, scales = "free_y") +
  geom_text(data = ann, aes(x = -Inf, y = Inf, label = lab), hjust = -0.1, vjust = 1.6, size = 3.5) +
  labs(x = "Effect on the human code (percentage points)", y = "Effect on the measure") +
  theme_bw(base_size = 12) + theme(panel.background = element_rect(fill = "grey95"), panel.grid = element_line(colour = "white"),
                                   strip.text = element_text(size = 9.5))
ggsave("paper/revision/figures/note_human.pdf", g, width = 6.5, height = 3.5)
