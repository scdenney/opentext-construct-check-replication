# ==============================================================================
# rv_05_published_on_E.R -- the published SK/TW pipeline (A3), restricted to E
# ==============================================================================
# The published codebook (v2 in the paper) was built with the full corpus in
# view, so it is contaminated. Re-estimating its civic-commitment contrast on
# the estimation fold alone shows how much of the published estimate survives
# when only responses outside discovery count. Also reports the full-sample
# published-style estimate for comparison. Majority = 4+ of 6 models.
# Writes analysis/output/revision/published_on_E.csv.
# ==============================================================================

source("analysis/revision/rv_config.R")
id <- "sktw_naturalization_2024"
m <- read.csv(file.path(ROOT, "analysis/output/classification/majority_classifications.csv"),
              stringsAsFactors = FALSE)
d <- read_harmonized(id)
s <- oc_split_read(file.path(SPLITS, paste0(id, ".holdout.json")))
lock <- oc_lock_read(file.path(LOCKS, paste0(id, ".a1.k1.json")))
e <- oc_estimation_data(d, s, lock, log = ACCESS_LOG, purpose = "published labels on E")
m <- m[m$ResponseId %in% d$id, ]
m$treatment <- d$treatment[match(m$ResponseId, d$id)]
m$civic <- as.integer(m$majority_code == "civic_commitment" & m$n_agree >= 4)
rows <- list()
for (country in c("South Korea", "Taiwan")) {
  for (sample in c("full", "E")) {
    x <- m[m$country == country & (sample == "full" | m$ResponseId %in% e$id), ]
    x <- x[!is.na(x$civic), ]
    ef <- oc_effect(x$civic, x$treatment, n_perm = 2000L, seed = 42L)
    rows[[length(rows) + 1]] <- data.frame(dataset = id, country = country, sample = sample,
      measure = "published majority (4+/6) civic_commitment", ef)
  }
}
out <- do.call(rbind, rows)
write.csv(out, file.path(OUT, "published_on_E.csv"), row.names = FALSE)
print(out[, c("country", "sample", "estimate", "se", "p_ri", "n1", "n0")], row.names = FALSE, digits = 3)
