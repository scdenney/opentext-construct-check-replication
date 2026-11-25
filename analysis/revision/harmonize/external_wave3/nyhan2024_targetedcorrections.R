# ==============================================================================
# nyhan2024_targetedcorrections.R -- harmonized files for cards
#   nyhan2024_targetedcorrections_seats2022  (explain expected 2022 fraud; Q42d_w3)
#   nyhan2024_targetedcorrections_seats2020  (explain believed 2020 fraud; Q42e_new_w3)
# ==============================================================================
# Data: Harvard Dataverse doi:10.7910/DVN/NCZKCH (CC0), deposited for Carey et
# al. (2025), Political Behavior 47(2). The cards use the Wave 1 experiment of
# the same panel (DART0053 wave 1 = DART0042 wave 3, Oct.-Nov. 2022), reported
# as Study 1 of Carey, Fogarty, Gehrke, Nyhan & Reifler (2025), Science Advances;
# preregistration OSF gpy3s.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/nyhan2024_targetedcorrections.R
#
# Input: data/external/nyhan2024_targetedcorrections/raw/DART0053_OUTPUT_updated.dta
#   (deposited Stata original, fetched with ?format=original; the .tab ingest in
#   raw/ is not used). Wave 2 (DART0053_W2_OUTPUT) is not used: no card (see
#   MANIFEST.md, criterion ii).
# Outputs: data/external/nyhan2024_targetedcorrections/harmonized_<card_id>.csv
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(haven))

raw <- "data/external/nyhan2024_targetedcorrections/raw/DART0053_OUTPUT_updated.dta"
out_dir <- "data/external/nyhan2024_targetedcorrections"

d <- read_dta(raw)
stopifnot(nrow(d) == 3772, !anyDuplicated(d$caseid))

# Treatment: condition_treat value labels 1 = Prebunking condition,
# 2 = Correction condition (the "credible sources" correction), 3 = Placebo
# condition (8 skipped, 9 not asked; none occur). Simple randomization p = 1/3
# (OSF gpy3s, Wave 1). The authors' code uses the same mapping
# (arizona-replication.do, l. 322 inoculation = condition_treat==1; l. 335
# correction = condition_treat==2).
ct <- as.integer(zap_labels(d$condition_treat))
stopifnot(all(ct %in% 1:3))
arm <- c("prebunking", "credible", "placebo")[ct]

# Exclusions: the preregistration states "Individual respondents will not be
# excluded from analysis" (OSF gpy3s, data exclusion section); the only
# registered exclusion (first 70 participants) concerns Wave 3, not this file.
# The authors' code drops no Wave 1 rows (arizona-replication.do, ll. 27-31).
# So exclude = FALSE for every row with text.

pid7 <- as.integer(zap_labels(d$pid7))

build <- function(card_id, text_col, closed_col, pre_col, pre_name) {
  closed <- as.integer(zap_labels(d[[closed_col]]))  # 1 None, 2 One or two, 3 Three to nine, 4 Ten or more
  pre    <- as.integer(zap_labels(d[[pre_col]]))
  txt    <- as.character(d[[text_col]])
  txt[is.na(txt)] <- ""
  Encoding(txt) <- "UTF-8"
  stopifnot(all(validUTF8(txt)))
  # The explanation is asked only if the closed item is not "None" (Online
  # Appendix A, Study 1 questionnaire). Rows with closed == 1 hold a fixed
  # placeholder string from the survey export, not an answer: they are not
  # asked and are dropped. Check that no asked row carries that placeholder.
  asked <- closed %in% 2:4
  ph <- names(sort(table(txt[!asked]), decreasing = TRUE))[1]
  stopifnot(mean(txt[!asked] == ph) == 1)
  stopifnot(sum(txt[asked] == ph) == 0)
  keep <- asked & nzchar(trimws(txt))
  cat(card_id, ": asked", sum(asked), "| asked with empty text dropped", sum(asked & !keep),
      "| not asked (closed item None)", sum(!asked), "\n")
  h <- data.frame(
    id        = as.character(d$caseid[keep]),     # YouGov case id (caseid)
    dataset   = card_id,
    country   = "US",
    lang      = "en",
    text      = trimws(txt[keep]),
    exclude   = FALSE,                            # see exclusions above
    treatment = arm[keep],                        # prebunking / credible / placebo
    closed_post = closed[keep],                   # closed item the text explains (post-treatment; 2-4 by construction)
    stringsAsFactors = FALSE
  )
  h[[pre_name]] <- pre[keep]                      # pre-treatment version of the closed item (1-4; 8/9 = skipped/not asked)
  h$pid7   <- pid7[keep]                          # YouGov 7-point party ID (pre-treatment profile; 8 = not sure)
  h$weight <- as.numeric(d$weight[keep])          # YouGov general-population weight
  out_csv <- file.path(out_dir, paste0("harmonized_", card_id, ".csv"))
  write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

  # ---- Checks ----
  cat("Rows written:", nrow(h), "\n")
  cat("Non-excluded rows per arm:\n"); print(table(h$treatment[!h$exclude]))
  r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
  cat("Re-read text identical:", identical(r$text, h$text), "\n")
  stopifnot(identical(r$text, h$text))
  lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
  n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
  cat("<U+ escapes in file:", n_esc, "\n\n")
  stopifnot(n_esc == 0)
  invisible(h)
}

# 2022: Q42c_w3 "Candidate will not be the rightful winner - post treatment"
# (authors: seats_won_fraud_2022_w3_post, l. 190); pre-treatment Q22b_w3
# (seats_won_fraud_2022_w3, l. 42). Text Q42d_w3 "Explain answer in Q42c".
h22 <- build("nyhan2024_targetedcorrections_seats2022", "Q42d_w3", "Q42c_w3",
             "Q22b_w3", "closed_pre")
# 2020: Q42a_post_w3 (authors: seats_won_fraud_2020_w3_post, l. 184);
# pre-treatment Q42a_w3 (seats_won_fraud_2020_w3, l. 36). Text Q42e_new_w3
# "Explain answer in Q42a_post_w3".
h20 <- build("nyhan2024_targetedcorrections_seats2020", "Q42e_new_w3", "Q42a_post_w3",
             "Q42a_w3", "closed_pre")
cat("Respondents in both files:", length(intersect(h22$id, h20$id)), "\n")
