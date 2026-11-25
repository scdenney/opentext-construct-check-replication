# ==============================================================================
# rv_00_pilot.R -- discovery-fold pilot of the theory-only prompts
# ==============================================================================
# 100 random discovery-fold responses per dataset x every A1/C prompt from the
# card (presence and rating for target and placebos). Checks prompt-following
# (valid-answer rate, probability mass on valid tokens) before anything runs on
# the estimation fold. Discovery data only, so no lock is needed.
# Usage: Rscript analysis/revision/rv_00_pilot.R <dataset> [n=100]
# ==============================================================================

source("analysis/revision/rv_config.R")

args <- commandArgs(trailingOnly = TRUE)
id <- args[1]
n <- if (length(args) > 1) as.integer(args[2]) else 100L
card_path <- registered_cards()[[id]]
card <- oc_study_read(card_path)
s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
d <- read_harmonized(id)
d <- d[nchar(trimws(d$text)) > 0, ]
dd <- oc_discovery_data(d, s, log = ACCESS_LOG, purpose = paste("pilot", n, "D items"))
pick <- opencheck:::with_seed(20261028, sample(sort(dd$id, method = "radix"), min(n, nrow(dd))))
dd <- dd[dd$id %in% pick, ]
pr <- a1_prompts(card, card_path)
pr <- pr[vapply(pr, function(p) p$kind != "pairwise", logical(1))]
out_dir <- file.path(JOBS, "pilot", id)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
lines <- character(0)
for (nm in names(pr)) {
  f <- file.path(out_dir, paste0(nm, ".jsonl"))
  oc_export_requests(dd, pr[[nm]], f)
  lines <- c(lines, opencheck:::read_lines_utf8(f))
}
opencheck:::write_utf8(lines, file.path(out_dir, "requests.jsonl"))
cat(id, ":", length(lines), "pilot requests from", nrow(dd), "D items\n")
