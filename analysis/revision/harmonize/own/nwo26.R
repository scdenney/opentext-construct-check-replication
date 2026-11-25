# Harmonize nwo26 Exp2 (EXP2_OPENTEXT, after task 5 of 11) and Exp3
# (EXP3_COMPREHENSION, treatment arm only) from the snapshot of record
# 2026-08-31T1349Z. Blind: open-text values are moved, never printed.
# Run: LANG=en_US.UTF-8 Rscript nwo26.R
suppressPackageStartupMessages(library(data.table))
source("analysis/revision/harmonize/own/common.R")  # path changed on recovery (2026-11-17)

NWO <- "/home/buddleman/Documents/github/research/projects/nwo26-immigration-backlash"
STAMP <- "2026-08-31T1349Z"
SNAP <- file.path(NWO, "tools/softlaunch-audit/data/snapshots", STAMP, "data")
IDS <- file.path(NWO, "tools/paper-figures/runs", STAMP, "long")
LANG_OF <- c(US = "en", DE = "de", KR = "ko", SG = "en")

read_snap <- function(cc, cols) {
  f <- file.path(SNAP, paste0(cc, ".csv"))
  h <- names(fread(f, nrows = 0))
  cols <- intersect(cols, h)
  # Qualtrics label export: header + question-text row + ImportId row.
  # readr, not fread: fread left escaped quotes ("") doubled in ~110 texts.
  d <- suppressMessages(readr::read_csv(
    f, col_types = readr::cols(.default = "c"), col_select = dplyr::all_of(cols),
    na = "", trim_ws = FALSE, progress = FALSE,
    locale = readr::locale(encoding = "UTF-8")))
  as.data.table(d)[-(1:2)]
}
sample_ids <- function(cc) {
  x <- readLines(file.path(IDS, sprintf("sample_ids_%s.txt", cc)), encoding = "UTF-8")
  x[!grepl("^#", x) & nzchar(trimws(x))]
}
strip_html <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", x)))
nonempty <- function(x) !is.na(x) & nchar(trimws(x)) > 0

# ---------------------------------------------------------------- Exp 2
SUPPORT <- c("Support" = 1L, "Oppose" = 0L, "Befürworten" = 1L, "Ablehnen" = 0L,
             "찬성" = 1L, "반대" = 0L, "支持" = 1L, "反对" = 0L)
exp2 <- rbindlist(lapply(c("US", "DE", "KR", "SG"), function(cc) {
  fl <- grep("^FL_.*_DO_(\\[ZH\\])?Exp2Block$",
             names(fread(file.path(SNAP, paste0(cc, ".csv")), nrows = 0)), value = TRUE)
  d <- read_snap(cc, c("ResponseId", "EXP2_OPENTEXT", "EXP2_OPENTEXT_ZH",
                       "C2_SUPPORT_5", "C2_SUPPORT_5_ZH",
                       "C2-K-5-1-domain", "C2-K-5-1-procedure",
                       "C2-K-5-1-implementation", "C2-K-5-1-composition",
                       "C2-F-5-1-1", "C2-F-5-1-2", "C2-F-5-1-3", "C2-F-5-1-4", fl))
  zh <- if ("C2_SUPPORT_5_ZH" %in% names(d)) !is.na(d$C2_SUPPORT_5_ZH) else rep(FALSE, nrow(d))
  y_raw <- fcoalesce(d$C2_SUPPORT_5, if ("C2_SUPPORT_5_ZH" %in% names(d)) d$C2_SUPPORT_5_ZH else NA_character_)
  reached <- !is.na(y_raw)               # answered task 5; the open-end page follows it
  txt <- if ("EXP2_OPENTEXT_ZH" %in% names(d)) ifelse(zh, d$EXP2_OPENTEXT_ZH, d$EXP2_OPENTEXT) else d$EXP2_OPENTEXT
  pos <- Reduce(fcoalesce, lapply(fl, function(v) d[[v]]))
  dom <- d[["C2-K-5-1-domain"]]
  out <- data.table(
    id = d$ResponseId, country = cc,
    lang = ifelse(zh, "zh-Hans", LANG_OF[[cc]]),
    text = txt, exclude = !(d$ResponseId %in% sample_ids(cc)),
    task = 5L,
    profile_area = sub("_(expand|restrict)$", "", dom),
    profile_direction = sub("^.*_(expand|restrict)$", "\\1", dom),
    profile_procedure = d[["C2-K-5-1-procedure"]],
    profile_implementation = d[["C2-K-5-1-implementation"]],
    profile_composition = d[["C2-K-5-1-composition"]],
    profile_policy_shown = strip_html(d[["C2-F-5-1-1"]]),
    profile_groups_shown = strip_html(d[["C2-F-5-1-2"]]),
    profile_process_shown = strip_html(d[["C2-F-5-1-3"]]),
    profile_application_shown = strip_html(d[["C2-F-5-1-4"]]),
    task5_support = unname(SUPPORT[y_raw]),
    exp2_block_position = as.integer(pos)
  )[reached]
  if (anyNA(out$task5_support)) stop("unmapped support label in ", cc)
  if (anyNA(out$profile_procedure)) stop("missing task-5 attributes in ", cc)
  out
}))
write_harmonized(exp2, "nwo26_exp2", manifest = list(
  source = "nwo26-immigration-backlash/tools/softlaunch-audit/data/snapshots/2026-08-31T1349Z/data/{US,DE,KR,SG}.csv; exclusions from tools/paper-figures/runs/2026-08-31T1349Z/long/sample_ids_<CC>.txt",
  design = "Single-profile factorial conjoint (11 tasks incl. repeat), binary support; open-end after task 5 about the task-5 policy; 4 countries pooled with country column",
  lang = "en (US, SG), de, ko, zh-Hans (SG Chinese fork)",
  treatment_cols = "task, profile_area, profile_direction, profile_procedure, profile_implementation, profile_composition, profile_*_shown (displayed level text), task5_support, exp2_block_position",
  human_codes = "none (NMF topic labels only, not per-response codes)",
  routing = "API-eligible",
  card = "y",
  caveats = "Reached = answered C2_SUPPORT_5 (either SG fork); everyone who answered task 6 has a non-empty open-end. exclude = not in the Paper-2 analytic sample (sample_ids: Progress==100, >=10 Exp1 tasks, registered exclusions, vendor QC removal set for DE/KR/SG). exp_order embedded field is empty in the snapshot; block order taken from FL_*_DO_Exp2Block (1 = Exp2 first). SG 'asylum' level is fielded as low-wage Work Permit labor. Prior exploratory NMF + keyword analysis of this text (Paper 2 SI-F).",
  unverified = ""))

# ---------------------------------------------------------------- Exp 3
VARIANT_KEY <- list(
  # labels as in national-identity-experiment/analysis/50_si_tables.py VARIANT_LABEL
  US = c(v1 = "v1_roosevelt", v2 = "v2_reagan", v3 = "v3_obama", v4 = "v4_bush"),
  DE = c(v1 = "v1_rau", v2 = "v2_basic_law", v3 = "v3_gauck_steinmeier"),
  KR = c(v1 = "v1_constitutional", v2 = "v2_universal_dignity", v3 = "v3_free_democratic_order"))
exp3 <- rbindlist(lapply(c("US", "DE", "KR"), function(cc) {
  d <- read_snap(cc, c("ResponseId", "cn_arm", "cn_variant", "EXP3_CLARITY", "EXP3_COMPREHENSION"))
  reached <- d$cn_arm %in% "treatment" & (!is.na(d$EXP3_CLARITY) | nonempty(d$EXP3_COMPREHENSION))
  d <- d[reached]
  key <- unname(VARIANT_KEY[[cc]][d$cn_variant])
  if (anyNA(key)) stop("unmapped cn_variant in ", cc)
  data.table(id = d$ResponseId, country = cc, lang = LANG_OF[[cc]],
             text = d$EXP3_COMPREHENSION,
             exclude = !(d$ResponseId %in% sample_ids(cc)),
             treatment = key, cn_variant = d$cn_variant, cn_arm = d$cn_arm)
}))
write_harmonized(exp3, "nwo26_exp3_comprehension", manifest = list(
  source = "nwo26-immigration-backlash/tools/softlaunch-audit/data/snapshots/2026-08-31T1349Z/data/{US,DE,KR}.csv; exclusions from tools/paper-figures/runs/2026-08-31T1349Z/long/sample_ids_<CC>.txt",
  design = "Between-subjects persuasion experiment; open-end (gist recall) shown to the treatment arm only, after the passage; within-arm equal-probability draw of tradition-keyed variant (US 4, DE 3, KR 3)",
  lang = "en, de, ko",
  treatment_cols = "treatment (variant key), cn_variant, cn_arm (all 'treatment')",
  human_codes = "none found (registered 'blinded comprehension codebook' not located)",
  routing = "API-eligible",
  card = "y",
  caveats = "Treatment-arm only: no control-arm text, so the only randomized contrast is between passage variants (registered as exploratory). Reached = cn_arm treatment and EXP3_CLARITY answered or non-empty text. exclude = not in the shared analytic sample (sample_ids). Singapore did not field Exp3.",
  unverified = "The per-protocol 'blinded comprehension codebook fixed before fielding' (foundation_note/osf.tex, Analysis plan) was not found; the paper's reporting table says the comprehension-gated estimate was not reported because hand coding was not completed."))
