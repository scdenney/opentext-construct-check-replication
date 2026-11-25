# rv_31_flow.R -- respondent flow: rows dropped by `exclude` flags and empty text before the split, by study
# and by arm (post hoc, 24 Nov 2026, after the pre-submission review). Never prints or writes the text column.
# Run: LANG=en_US.UTF-8 Rscript analysis/revision/rv_31_flow.R
source("analysis/revision/rv_config.R"); source("analysis/revision/rv_treatments.R")
TAB <- file.path(OUT, "tables"); TEX <- file.path(ROOT, "paper/revision/si_tables")
ids <- read.csv(file.path(OUT, "corpus_table.csv"), stringsAsFactors = FALSE)$dataset
raw <- function(id) read.csv(harmonized_path(id), encoding = "UTF-8", stringsAsFactors = FALSE)  # as read_harmonized() reads it
truthy <- function(x) toupper(trimws(as.character(x))) %in% "TRUE"
split1 <- function(id) { s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json"))); s[s$split == 1, ] }
arm_of <- function(id, d) if (!is.null(TREATMENTS[[id]])) TREATMENTS[[id]](d)[[1]]$treat else suppressWarnings(as.integer(d$treatment))

# 1. Flow by study
flow <- do.call(rbind, lapply(ids, function(id) {
  d <- raw(id); has <- "exclude" %in% names(d)
  flag <- if (has) truthy(d$exclude) else rep(FALSE, nrow(d)); drop <- if (has) d$exclude %in% c(TRUE, "TRUE") else flag
  a <- read_harmonized(id); s <- split1(id)
  data.frame(dataset = id, rows = nrow(d), flagged = sum(flag), dropped_exclude = sum(drop),
             dropped_empty = nrow(d) - sum(drop) - nrow(a), analyzed = nrow(a),
             discovery = sum(s$fold == "D"), estimation = sum(s$fold == "E"),
             split_ids_match = setequal(s$id, a$id))
}))
write.csv(flow, file.path(TAB, "flow_by_study.csv"), row.names = FALSE)
tot <- c(rows = sum(flow$rows), flagged = sum(flow$flagged), files_flagged = sum(flow$flagged > 0),
         dropped_exclude = sum(flow$dropped_exclude), files_dropped = sum(flow$dropped_exclude > 0),
         dropped_empty = sum(flow$dropped_empty), files_empty = sum(flow$dropped_empty > 0),
         analyzed = sum(flow$analyzed), discovery = sum(flow$discovery), estimation = sum(flow$estimation))
expect <- c(315756, 12810, 53, 12553, 52, 123, 36, 303080, 90922, 212158)
print(data.frame(total = tot, expected = expect, match = tot == expect))
cat("Studies whose split ids differ from the analyzed rows:", sum(!flow$split_ids_match), "\n")

# 2. Flagged rows by arm of the primary contrast
arms <- do.call(rbind, lapply(flow$dataset[flow$flagged > 0], function(id) {
  d <- raw(id); d$text <- NULL; flag <- truthy(d$exclude)
  arm <- tryCatch(arm_of(id, d), error = function(e) NULL)
  note <- if (id == "chopra2024_newsdemand") "flag stored as True/False strings; flagged rows not dropped" else ""
  if (is.null(arm) || length(arm) != nrow(d) || all(is.na(arm[flag])))
    return(data.frame(dataset = id, flagged = sum(flag), n_treated = NA, flag_treated = NA, share_treated = NA,
                      n_comparison = NA, flag_comparison = NA, share_comparison = NA, flag_arm_na = sum(flag),
                      test = NA, p = NA, note = trimws(paste(note, "arm not built for flagged rows"))))
  ok <- !is.na(arm) & arm %in% c(0, 1); tb <- table(factor(arm[ok], 0:1), factor(flag[ok], c(FALSE, TRUE)))
  ex <- suppressWarnings(chisq.test(tb)$expected); fis <- any(ex < 5, na.rm = TRUE) || any(is.na(ex))
  p <- if (fis) fisher.test(tb)$p.value else chisq.test(tb)$p.value
  data.frame(dataset = id, flagged = sum(flag), n_treated = sum(tb[2, ]), flag_treated = tb[2, 2], share_treated = tb[2, 2] / sum(tb[2, ]),
             n_comparison = sum(tb[1, ]), flag_comparison = tb[1, 2], share_comparison = tb[1, 2] / sum(tb[1, ]),
             flag_arm_na = sum(flag & !ok), test = if (fis) "fisher" else "chisq", p = p, note = note)
}))
arms <- arms[order(arms$dataset, method = "radix"), ]
write.csv(arms, file.path(TAB, "exclusions_by_arm.csv"), row.names = FALSE)
cat("Studies with flagged rows:", nrow(arms), "; arm not built:", sum(is.na(arms$p)),
    "; p < .01:", sum(arms$p < .01, na.rm = TRUE), "; p < .05:", sum(arms$p < .05, na.rm = TRUE), "\n")
print(arms[!is.na(arms$p) & arms$p < .05, c("dataset", "flagged", "share_treated", "share_comparison", "test", "p")], digits = 3, row.names = FALSE)

# 3. Task-level studies: respondents with rows in both folds of split 1 (respondent id as in rv_26)
both <- do.call(rbind, lapply(c("marsh2025_lifestyleconjoint", "krishnarajan2022_rationalizing", "graeber2024_storiesmemory", "roos2024_subjectivity"), function(id) {
  d <- read_harmonized(id); s <- split1(id)
  resp <- if ("respondent_id" %in% names(d)) as.character(d$respondent_id) else sub("_[^_]*$", "", d$id)
  f <- s$fold[match(d$id, s$id)]; n_both <- length(intersect(resp[f %in% "D"], resp[f %in% "E"]))
  data.frame(dataset = id, respondents = length(unique(resp)), both_folds = n_both, share = n_both / length(unique(resp)))
}))
write.csv(both, file.path(TAB, "both_folds.csv"), row.names = FALSE); print(both, digits = 3, row.names = FALSE)

# 4. sktw_naturalization_2024: counts by country, arm and fold
d <- read_harmonized("sktw_naturalization_2024"); s <- split1("sktw_naturalization_2024")
d$arm <- ifelse(arm_of("sktw_naturalization_2024", d) == 1, "shown", "withheld"); d$fold <- s$fold[match(d$id, s$id)]
sk <- as.data.frame(table(country = d$country, arm = d$arm, fold = d$fold, useNA = "ifany"), responseName = "n")
write.csv(sk, file.path(TAB, "sktw_arms.csv"), row.names = FALSE)
print(table(d$country, d$arm)); print(table(d$fold, d$arm))

# 5. SI rows for the studies in item 2
pct <- function(x) ifelse(is.na(x), "--", fmt_num(100 * x, 1))
pv <- function(p) ifelse(is.na(p), "--", ifelse(p < .001, "$<$.001", fmt_num(p, 3)))
brk <- function(x) gsub("_", "\\_\\allowbreak{}", x, fixed = TRUE)
rows <- sprintf("\\texttt{%s}%s & %s & %s & %s & %s \\\\", brk(arms$dataset), ifelse(arms$dataset == "chopra2024_newsdemand", "$^\\dagger$", ""),
                formatC(arms$flagged, format = "d", big.mark = ","), pct(arms$share_treated), pct(arms$share_comparison), pv(arms$p))
writeLines(rows, file.path(TEX, "exclusions_rows.tex"))
