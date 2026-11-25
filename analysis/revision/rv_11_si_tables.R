# LaTeX tables for the SI (paper/revision/si_tables/*.tex).
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_11_si_tables.R

# The study list (catalog) is written by rv_18_study_catalog.R.

suppressMessages(library(dplyr))
source("analysis/revision/rv_config.R")  # instrument_of(), fmt_num(), plain names
TEX <- "paper/revision/si_tables"
dir.create(TEX, showWarnings = FALSE)
REF <- "qwen36_35b"
esc <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  gsub("~", "\\\\textasciitilde{}", x)
}
# identifiers in narrow table columns: allow a line break after each underscore
brk <- function(x) gsub("\\_", "\\_\\allowbreak{}", esc(x), fixed = TRUE)
write_rows <- function(rows, file) writeLines(enc2utf8(rows), file.path(TEX, file), useBytes = TRUE)

# Primary target contrasts per study: reference model, three card-based arms
# 24 Nov 2026: estimation-sample arm sizes n1 and n0 (yes/no rows of effects_a1.csv) added as two columns
a1 <- read.csv(file.path(OUT, "effects_a1.csv"))
cell <- function(est, p) ifelse(is.na(est), "--", sprintf("%s%s", fmt_num(est), ifelse(p < .05, "*", "")))
prim <- a1 %>% filter(role == "target", primary, arm %in% c("A1_presence", "C_rating", "B_pairwise"))
wide <- function(m) {
  prim %>% filter(model == m) %>%
    mutate(val = ifelse(arm == "A1_presence", estimate, est_sd)) %>%
    select(dataset, construct, contrast, expected_sign, arm, val, p_ri, n1, n0) %>%
    group_by(dataset, construct, contrast, expected_sign) %>%
    summarise(a1 = cell(val[arm == "A1_presence"][1], p_ri[arm == "A1_presence"][1]),
              c = cell(val[arm == "C_rating"][1], p_ri[arm == "C_rating"][1]),
              b = cell(val[arm == "B_pairwise"][1], p_ri[arm == "B_pairwise"][1]),
              n1 = n1[arm == "A1_presence"][1], n0 = n0[arm == "A1_presence"][1], .groups = "drop") %>%
    arrange(dataset, construct, contrast)
}
for (m in "qwen36_35b") {
  w <- wide(m)
  write_rows(sprintf("\\texttt{%s} & %s & %s & %s & %s & %s & %s & %s & %s \\\\", brk(w$dataset), plain_id(w$construct),
                     plain_id(w$contrast), ifelse(is.na(w$expected_sign), "0", ifelse(w$expected_sign > 0, "+", "$-$")),
                     formatC(w$n1, big.mark = ",", format = "d"), formatC(w$n0, big.mark = ",", format = "d"), w$a1, w$c, w$b), sprintf("primary_%s_rows.tex", m))
}

# Summary by model and arm
MODEL_ORDER <- c("qwen36_35b", "gpt6_luna", "gemma4_31b", "gemma4_e4b", "majority", "stm")
ARM_ORDER <- c("A1_presence", "A1_presence_soft", "C_rating", "B_pairwise", "D_topic")
sm <- a1 %>% filter(role == "target", primary, arm %in% ARM_ORDER, model %in% MODEL_ORDER) %>%
  # GPT-6 Luna returns labels only, so its "probability" is the label again
  filter(!(model == "gpt6_luna" & arm == "A1_presence_soft")) %>%
  group_by(model, arm) %>%
  summarise(n = n(), d = n_distinct(dataset), sig = mean(p_ri < .05),
            rec = mean(p_ri < .05 & !is.na(expected_sign) & sign(estimate) == expected_sign),
            ok = mean(sign(estimate) == expected_sign, na.rm = TRUE), .groups = "drop") %>%
  arrange(match(model, MODEL_ORDER), match(arm, ARM_ORDER))
write_rows(c(sprintf("%s & %s & %d (%d) & %s & %s & %s \\\\", MODEL_LABEL[sm$model], ARM_LABEL[sm$arm], sm$n, sm$d,
                     fmt_num(sm$rec), fmt_num(sm$sig), fmt_num(sm$ok))), "summary_rows.tex")

# Placebos: yes/no question and rating side by side (the majority vote has no rating)
pl <- a1 %>% filter(role == "placebo", arm %in% c("A1_presence", "C_rating"), model %in% MODEL_ORDER) %>%
  group_by(model, arm) %>% summarise(n = n(), up = mean(p_ri < .05 & estimate > 0),
                                     down = mean(p_ri < .05 & estimate < 0), .groups = "drop")
pl <- full_join(pl %>% filter(arm == "A1_presence") %>% select(model, n, up, down),
                pl %>% filter(arm == "C_rating") %>% select(model, n_c = n, up_c = up, down_c = down), by = "model") %>%
  arrange(match(model, MODEL_ORDER))
if (any(!is.na(pl$n_c) & pl$n_c != pl$n)) stop("placebo estimates differ in number between the yes/no and rating arms")
write_rows(sprintf("%s & %d & %s & %s & %s & %s \\\\", MODEL_LABEL[pl$model], pl$n, fmt_num(pl$up), fmt_num(pl$down),
                   fmt_num(pl$up_c), fmt_num(pl$down_c)), "placebo_rows.tex")

# Other models against the reference model on the contrasts both ran; the same
# quantities as rv_15_text_numbers.R, section "Other models against the reference model"
key <- c("dataset", "construct", "contrast")
pa <- a1[a1$primary & a1$role == "target", ]
pa$sig <- pa$p_ri < 0.05
pa$sign_ok <- ifelse(is.na(pa$expected_sign) | pa$expected_sign == 0, NA, sign(pa$estimate) == pa$expected_sign)
pa$rec <- pa$sig & !is.na(pa$sign_ok) & pa$sign_ok
am <- read.csv(file.path(OUT, "agreement_models.csv"))
pair <- function(mk, a) merge(pa[pa$model == mk & pa$arm == a, c(key, "est_sd", "rec")],
                              pa[pa$model == REF & pa$arm == a, c(key, "est_sd", "rec")], by = key, suffixes = c("", "_ref"))
ma <- do.call(rbind, lapply(c("gemma4_e4b", "gpt6_luna", "gemma4_31b"), function(mk) {
  y <- pair(mk, "A1_presence"); r <- pair(mk, "C_rating")
  if (!identical(sort(do.call(paste, y[key])), sort(do.call(paste, r[key])))) stop(mk, ": yes/no and rating cover different contrasts")
  data.frame(model = mk, studies = n_distinct(y$dataset), contrasts = nrow(y),
             rec_y = mean(y$rec), rec_y_ref = mean(y$rec_ref), rec_c = mean(r$rec), rec_c_ref = mean(r$rec_ref),
             r_y = cor(y$est_sd, y$est_sd_ref, use = "complete.obs"), r_c = cor(r$est_sd, r$est_sd_ref, use = "complete.obs"),
             same_y = mean(y$rec == y$rec_ref), same_c = mean(r$rec == r$rec_ref),
             kappa = median(am$kappa[(am$model_a == mk & am$model_b == REF) | (am$model_a == REF & am$model_b == mk)], na.rm = TRUE))
}))
write_rows(sprintf("%s & %d & %d & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\", MODEL_LABEL[ma$model], ma$studies, ma$contrasts,
                   fmt_num(ma$rec_y), fmt_num(ma$rec_y_ref), fmt_num(ma$rec_c), fmt_num(ma$rec_c_ref), fmt_num(ma$r_y), fmt_num(ma$r_c),
                   fmt_num(ma$same_y), fmt_num(ma$same_c), fmt_num(ma$kappa)), "model_agreement_rows.tex")
print(ma, digits = 3)

# NK held-out
NK_VERSION <- c(v0 = "Theory only", rev1 = "Revised by LLM, round 1", rev2 = "Revised by LLM, round 2",
                rev_final = "Revised by LLM, final", v3_published = "Revised by researchers on all items (in-sample)")
nk <- read.csv(file.path(OUT, "tables", "nk_heldout_summary.csv"))
nk <- nk %>% filter(half == "test") %>% arrange(model, match(version, names(NK_VERSION)))
write_rows(sprintf("%s & %s & %s & %s \\\\", MODEL_LABEL[nk$model], NK_VERSION[nk$version], fmt_num(nk$kappa, 3), fmt_num(nk$acc, 3)),
           "nk_rows.tex")

# Double-dipping, summarized over studies. False-positive rates are multiples
# of 1/200, so they print to three decimals.
dd <- read.csv(file.path(OUT, "tables", "doubledip_fpr.csv"))
dd_row <- function(x, lab) sprintf("%s & %s & %s & %s & %s & %d \\\\", lab, fmt_num(mean(x), 3), fmt_num(min(x), 3), fmt_num(max(x), 3),
                                   fmt_num(mean(tapply(x, instrument_of(dd$dataset), mean)), 3), sum(x > 0.35))
write_rows(c(dd_row(dd$fpr_old, "Full sample, topic selected"), dd_row(dd$fpr_split, "Split, topic selected"),
             dd_row(dd$fpr_old_fixed, "Full sample, topic fixed in advance"), dd_row(dd$fpr_split_fixed, "Split, topic fixed in advance")),
           "doubledip_summary_rows.tex")
cat(sprintf("double-dipping summary: %d studies, %d samples\n", nrow(dd), n_distinct(instrument_of(dd$dataset))))
bt <- read.csv(file.path(OUT, "bt_diagnostics.csv"))

# Topic-model robustness (rv_13e_stm_robustness.R): share of all primary target
# contrasts recovered, by number of topics and by how the topic is chosen
sr <- read.csv(file.path(OUT, "tables", "stm_robustness.csv"))
n_all <- nrow(sr[sr$K == sr$K_used, ])
lab_rec <- a1 %>% filter(role == "target", primary, model == "stm", arm == "D_topic") %>%
  summarise(r = sum(p_ri < .05 & sign(estimate) == expected_sign)) %>% pull(r)
srow <- function(d, lab, labeler = NA) sprintf("%s & %s & %s & %s & %s \\\\", lab, fmt_num(labeler), fmt_num(mean(d$rec_best)),
                                              fmt_num(mean(d$any_topic)), fmt_num(mean(d$any_topic_bonf)))
bestK <- do.call(rbind, lapply(split(sr, paste(sr$dataset, sr$construct, sr$contrast)), function(d) d[which.max(d$r_best), ]))
write_rows(c(srow(sr[sr$K == sr$K_used, ], "Number of topics as chosen on the discovery sample", lab_rec / n_all),
             vapply(sort(unique(sr$K)), function(k) srow(sr[sr$K == k, ], sprintf("%d topics", k)), ""),
             srow(bestK, "Best number of topics in hindsight")), "stm_robustness_rows.tex")
cat(sprintf("topic-model robustness: %d contrasts; labeler's topic recovered %d (%.3f)\n", n_all, lab_rec, lab_rec / n_all))

# Pairwise scaling by prevalence of the target (reference model). Prevalence: the
# share of estimation-fold answers the yes/no label marks as showing the target.
key <- c("dataset", "construct", "contrast")
pq <- a1 %>% filter(role == "target", primary, model == "qwen36_35b") %>%
  mutate(rec = p_ri < .05 & sign(estimate) == expected_sign)
pw <- inner_join(pq %>% filter(arm == "B_pairwise") %>% select(all_of(key), rec_b = rec),
                 pq %>% filter(arm == "A1_presence") %>% select(all_of(key), rec_y = rec, mean_control, estimate, n1, n0), by = key) %>%
  mutate(share = (mean_control * n0 + (mean_control + estimate) * n1) / (n0 + n1))
band <- function(x) cut(x, c(-Inf, .10, .25, Inf), labels = c("Under 10 percent", "10 to 25 percent", "Over 25 percent"), right = FALSE)
sh <- pw %>% group_by(dataset) %>% summarise(share = mean(share), .groups = "drop")
btq <- bt %>% filter(model == "qwen36_35b") %>% inner_join(sh, by = "dataset")
write.csv(inner_join(btq, pw %>% group_by(dataset) %>% summarise(rec_b = any(rec_b), rec_y = any(rec_y), .groups = "drop"), by = "dataset"),
          file.path(OUT, "tables", "pairwise_by_study.csv"), row.names = FALSE)
pr <- pw %>% mutate(b = band(share)) %>% group_by(b) %>% summarise(n = n(), y = mean(rec_y), p = mean(rec_b), .groups = "drop") %>%
  inner_join(btq %>% mutate(b = band(share)) %>% group_by(b) %>%
               summarise(fits = n(), acc = median(heldout_acc, na.rm = TRUE), gap = median(abs(order_gap), na.rm = TRUE), .groups = "drop"), by = "b")
tot <- data.frame(b = "All", n = nrow(pw), y = mean(pw$rec_y), p = mean(pw$rec_b), fits = nrow(btq),
                  acc = median(btq$heldout_acc, na.rm = TRUE), gap = median(abs(btq$order_gap), na.rm = TRUE))
pr <- rbind(as.data.frame(pr), tot)
write_rows(sprintf("%s & %d & %d & %s & %s & %s & %s \\\\", pr$b, pr$fits, pr$n, fmt_num(pr$y), fmt_num(pr$p), fmt_num(pr$acc), fmt_num(pr$gap)), "pairwise_prevalence_rows.tex")

