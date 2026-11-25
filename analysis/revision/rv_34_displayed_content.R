# rv_34_displayed_content.R -- detection by whether the target restates content the treatment displayed.
# Post hoc (24 Nov 2026, after the pre-submission review). Rule: revision/coding/displayed_content_rule.md,
# written before coding. Two coders read the registered cards only and saw no estimate: coder A (Claude
# Opus 5.5, four agents) and coder B (Claude Sonnet 5.5, two agents). Coder A is the reported coding.
# Intervals: cluster bootstrap over survey samples (instrument_of), 5,000 draws, seed 42, as in rv_24.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_34_displayed_content.R
source("analysis/revision/rv_config.R")
rd <- function(p) do.call(rbind, lapply(Sys.glob(file.path("revision/coding", p)), read.csv, stringsAsFactors = FALSE))
A <- rd("coderA_chunk*.csv"); B <- rd("coderB_chunk*.csv"); k <- function(d) paste(d$dataset, d$contrast, d$construct, sep = "|")
cards <- merge(unique(A[, c("dataset", "routed", "examples_read", "results_known")]),
               unique(B[, c("dataset", "routed", "examples_read", "results_known")]), by = "dataset", suffixes = c("_A", "_B"))
stopifnot(nrow(cards) == 139)
A <- A[!is.na(A$displayed), ]; B <- B[!is.na(B$displayed), ]; stopifnot(nrow(A) == 205, setequal(k(A), k(B)))
B <- B[match(k(A), k(B)), ]
kappa <- function(a, b) { po <- mean(a == b); pe <- mean(a) * mean(b) + mean(1 - a) * mean(1 - b); c(agree = po, kappa = (po - pe) / (1 - pe)) }
cod <- data.frame(A[, c("dataset", "contrast", "construct")], displayed_A = A$displayed, conf_A = A$confidence, displayed_B = B$displayed,
                  conf_B = B$confidence, note_A = A$card_note, note_B = B$card_note, reason_A = A$reason, reason_B = B$reason)
write.csv(cod, "revision/coding/coded_contrasts.csv", row.names = FALSE); write.csv(cards, "revision/coding/coded_cards.csv", row.names = FALSE)
cat("displayed: A", sum(cod$displayed_A), "B", sum(cod$displayed_B), "of 205; "); print(round(kappa(cod$displayed_A, cod$displayed_B), 3))
hi <- cod$conf_A == "high" & cod$conf_B == "high"; cat("both high confidence:", sum(hi), "; "); print(round(kappa(cod$displayed_A[hi], cod$displayed_B[hi]), 3))
cat("card note: A", sum(cod$note_A), "B", sum(cod$note_B), "; "); print(round(kappa(cod$note_A, cod$note_B), 3))
print(table(A = cod$displayed_A, B = cod$displayed_B))

eff <- rbind(read.csv(file.path(OUT, "effects_a1.csv")), read.csv(file.path(OUT, "effects_stmcov.csv")))
eff <- eff[eff$role == "target" & eff$primary, ]
det <- function(model, arm) { e <- eff[eff$model == model & eff$arm == arm, ]; d <- e$p_ri < .05 & e$estimate * e$expected_sign > 0
  x <- d[match(k(cod), k(e))]; x[is.na(x)] <- FALSE; x }   # unmatched target = undetected
M <- data.frame(llm35 = det("qwen36_35b", "A1_presence"), rating = det("qwen36_35b", "C_rating"), e4b = det("gemma4_e4b", "A1_presence"),
                g31 = det("gemma4_31b", "A1_presence"), stm = det("stm", "D_topic"), stmcov = det("stmcov", "D_topic"))
stopifnot(colSums(M)[c("llm35", "stm", "stmcov", "e4b")] == c(138, 74, 79, 127))
cod$own <- grepl("^(sktw|aks21|ifes2023|nwo26|prc|roc|rok)", cod$dataset); inst <- instrument_of(cod$dataset); ui <- unique(inst)
strata <- list(displayed_A = cod$displayed_A == 1, displayed_B = cod$displayed_B == 1, both = cod$displayed_A == 1 & cod$displayed_B == 1,
               neither = cod$displayed_A == 0 & cod$displayed_B == 0, card_note_A = cod$note_A == 1)
set.seed(42); draws <- replicate(5000, unlist(lapply(sample(ui, replace = TRUE), function(i) which(inst == i))), simplify = FALSE)
ci <- function(f) { v <- vapply(draws, f, 0); sprintf("[%.3f, %.3f]", quantile(v, .025, na.rm = TRUE), quantile(v, .975, na.rm = TRUE)) }
out <- do.call(rbind, lapply(names(strata), function(s) do.call(rbind, lapply(names(M), function(m) { g <- strata[[s]]; y <- M[[m]]
  comp <- if (s %in% c("both", "neither")) NULL else !g
  r <- data.frame(coding = s, measure = m, group = "yes", n = sum(g), detected = sum(y[g]), rate = mean(y[g]), ci = ci(function(i) mean(y[i][g[i]])))
  if (!is.null(comp)) r <- rbind(r, data.frame(coding = s, measure = m, group = "no", n = sum(comp), detected = sum(y[comp]), rate = mean(y[comp]),
                                                ci = ci(function(i) mean(y[i][comp[i]]))),
                                 data.frame(coding = s, measure = m, group = "yes-no", n = NA, detected = NA, rate = mean(y[g]) - mean(y[comp]),
                                            ci = ci(function(i) mean(y[i][g[i]]) - mean(y[i][comp[i]]))))
  r }))))
gap <- do.call(rbind, lapply(c("displayed_A", "displayed_B", "card_note_A"), function(s) do.call(rbind, lapply(c("stm", "stmcov", "e4b"), function(m) { g <- strata[[s]]; d <- M$llm35 - M[[m]]
  data.frame(coding = s, measure = paste0("llm35-", m), group = c("yes", "no", "yes-no"), n = c(sum(g), sum(!g), NA), detected = NA,
             rate = c(mean(d[g]), mean(d[!g]), mean(d[g]) - mean(d[!g])),
             ci = c(ci(function(i) mean(d[i][g[i]])), ci(function(i) mean(d[i][!g[i]])), ci(function(i) mean(d[i][g[i]]) - mean(d[i][!g[i]])))) }))))
out <- rbind(out, gap); write.csv(out, file.path(OUT, "tables", "displayed_content.csv"), row.names = FALSE)
print(out[out$coding %in% c("displayed_A", "displayed_B", "card_note_A") & out$measure %in% c("llm35", "stm", "stmcov", "e4b", "llm35-stm", "llm35-e4b"), ], digits = 3, row.names = FALSE)
cat("own-survey contrasts displayed (A):", sum(cod$displayed_A[cod$own]), "of", sum(cod$own), "; public:", sum(cod$displayed_A[!cod$own]), "of", sum(!cod$own), "\n")
cat("cards: routed A", sum(cards$routed_A), "B", sum(cards$routed_B), "| examples_read A"); print(table(cards$examples_read_A)); print(table(cards$examples_read_B)); cat("results_known A", sum(cards$results_known_A), "B", sum(cards$results_known_B), "\n")

# Card-level codes (coder A): example answers read, text results by arm known, item routed on a post-treatment answer.
cards$own <- grepl("^(sktw|aks21|ifes2023|nwo26|prc|roc|rok)", cards$dataset)
cards$read <- cards$own | cards$examples_read_A != "0"; cards$known <- cards$results_known_A == 1
cat("public cards:", sum(!cards$own), "| example answers read (A):", sum(cards$read & !cards$own), "of which item not stated:",
    sum(cards$examples_read_A == "ambiguous" & !cards$own), "| none read:", sum(!cards$read), "| results by arm known:", sum(cards$known & !cards$own),
    "| neither:", sum(!cards$read & !cards$known), "| all cards with answers seen:", sum(cards$read), "\n")
cat("coder agreement, public cards: examples read", mean((cards$examples_read_A != "0") == (cards$examples_read_B != "0"))[1],
    "results known", mean(cards$results_known_A == cards$results_known_B), "routed", mean(cards$routed_A == cards$routed_B), "\n")
ex <- read.csv(file.path(OUT, "tables", "exclusions_by_arm.csv")); pcol <- grep("^p", names(ex), value = TRUE)[1]
diffex <- ex$dataset[!is.na(ex[[pcol]]) & ex[[pcol]] < .01 & ex$dataset != "chopra2024_newsdemand"]
sets <- list(public = !cod$own, own = cod$own, public_none_read = cod$dataset %in% cards$dataset[!cards$read],
             public_none_read_no_results = cod$dataset %in% cards$dataset[!cards$read & !cards$known],
             not_routed = !cod$dataset %in% cards$dataset[cards$routed_A == 1], no_differential_exclusion = !cod$dataset %in% diffex)
sub <- do.call(rbind, lapply(names(sets), function(s) do.call(rbind, lapply(c("llm35", "stm", "stmcov", "e4b"), function(m) { g <- sets[[s]]
  data.frame(set = s, measure = m, studies = length(unique(cod$dataset[g])), n = sum(g), detected = sum(M[[m]][g]), rate = mean(M[[m]][g])) }))))
write.csv(sub, file.path(OUT, "tables", "card_subsets.csv"), row.names = FALSE); print(sub[sub$measure %in% c("llm35", "stm"), ], digits = 3, row.names = FALSE)
cat("studies with exclusions differing by arm at p < .01:", diffex, "\n")

# Subgroup gaps the note describes, with cluster-bootstrap intervals, and per-sample rates over all 205 contrasts.
ref <- eff[eff$model == "qwen36_35b" & eff$arm == "A1_presence", ]; ref <- ref[match(k(cod), k(ref)), ]
big <- pmin(ref$n1, ref$n0) >= 200; y <- M$llm35
gapci <- function(g) c(yes = mean(y[g]), no = mean(y[!g]), gap = mean(y[g]) - mean(y[!g]),
                       setNames(quantile(vapply(draws, function(i) mean(y[i][g[i]]) - mean(y[i][!g[i]]), 0), c(.025, .975), na.rm = TRUE), c("lo", "hi")))
gaps <- rbind(arm_200 = gapci(big), own_vs_public = gapci(cod$own)); print(round(gaps, 3)); cat("contrasts with smaller arm >= 200:", sum(big), "below:", sum(!big), "\n")
write.csv(data.frame(gap = rownames(gaps), gaps), file.path(OUT, "tables", "subgroup_gaps.csv"), row.names = FALSE)
psw <- function(v) weighted.mean(v, 1 / ave(v, inst, FUN = length))
cat("per sample over all 205 (unmatched = undetected):", paste(names(M), sprintf("%.3f", vapply(M, psw, 0)), collapse = "; "), "\n")

# 24 Nov 2026: LaTeX rows for the SI table of detection by displayed content (paper/revision/si_tables/displayed_rows.tex)
pct <- function(x) sub("^-", "$-$", sprintf("%.1f", 100 * x))
pci <- function(s) { v <- as.numeric(strsplit(gsub("\\[|\\]", "", s), ",\\s*")[[1]]); sprintf("[%s, %s]", pct(v[1]), pct(v[2])) }
labs <- c(llm35 = "Yes/no question", stm = "Topic share", stmcov = "Topic share, treatment covariate",
          e4b = "Compact model (Gemma 4 E4B)", "llm35-stm" = "Yes/no minus topic share")
heads <- c(displayed_A = "Coder A (reported coding)", displayed_B = "Coder B", card_note_A = "The card's own note")
tex <- unlist(lapply(names(heads), function(s) { o <- out[out$coding == s, ]
  c(sprintf("\\multicolumn{4}{l}{\\textit{%s: %d contrasts coded yes, %d coded no}} \\\\", heads[s],
            o$n[o$group == "yes"][1], o$n[o$group == "no"][1]),
    vapply(names(labs), function(m) { r <- o[o$measure == m, ]
      g <- function(x) { q <- r[r$group == x, ]; sprintf("%s %s", pct(q$rate), pci(q$ci)) }
      sprintf("\\quad %s & %s & %s & %s \\\\", labs[[m]], g("yes"), g("no"), g("yes-no")) }, ""), "\\addlinespace") }))
writeLines(tex[-length(tex)], "paper/revision/si_tables/displayed_rows.tex")
