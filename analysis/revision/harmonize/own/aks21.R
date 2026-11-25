# Harmonize AKS21 (Korea 2021, Rakuten Insights via Delvinia, N = 2,009) open-ends.
# Text source of record: vendor OECE export (ConjointLoopC1-C4), keyed by respid,
# cross-checked against the LoopDW files (equal after normalization, 2007-2008 of
# 2008). integrate_text.xlsx (C1) and c3_open text.csv (C3) are NOT used: their
# respid -> text link disagrees with the vendor export (checked by equality counts).
# Task-level attributes: LoopDWC1-C4 (profile-level rows, English labels).
# BLIND: open-text columns are never printed; only counts are reported.
# Run: LANG=en_US.UTF-8 Rscript aks21.R

source("analysis/revision/harmonize/own/common.R")  # path changed on recovery (2026-11-17)
suppressPackageStartupMessages(library(readxl))

A <- path.expand("~/Documents/github/research/projects/_archive/Pre-2024")
D <- file.path(A, "AKS21/Data/C_UOT-21-2101 p995570460322 261121")
oece <- function(stub) file.path(D, paste0("C_UOT-21-2101 OECE 261121_", stub, ".xlsx"))
rd <- function(f) suppressMessages(read_excel(f, col_types = "text", na = character(), trim_ws = FALSE))

# Respondent-level frame: all 2,009 delivered completes (status == "complete").
main <- rd(file.path(D, "C_UOT-21-2101 OECE 261121.xlsx"))
stopifnot(all(main$status == "complete"), !anyDuplicated(main$respid))
frame <- data.frame(id = main$respid, list_arm_raw = main$dVII1, stringsAsFactors = FALSE)

# Open text from the ConjointLoop file at the task that carried it.
get_text <- function(k, var, task) {
  x <- rd(oece(paste0("ConjointLoopC", k)))
  x <- x[x[[paste0("ConjointLoopC", k)]] == paste("Choice", task), c("respid", var)]
  stopifnot(!anyDuplicated(x$respid))
  setNames(x, c("id", "text"))
}

# Chosen / other profile attributes for one task from a LoopDW file.
pair_attrs <- function(k, task, side_col, choice_col, amap) {
  x <- rd(oece(paste0("LoopDWC", k)))
  x <- x[grepl(paste0("^Choice ", task, " - "), x[[paste0("LoopDWC", k)]]),
         c("respid", side_col, choice_col, names(amap))]
  x$side <- sub("^.* ", "", x[[side_col]])                       # "A" / "B"
  sel <- x[x[[choice_col]] == "Selected", ]
  oth <- x[x[[choice_col]] == "Not selected", ]
  stopifnot(!anyDuplicated(sel$respid), !anyDuplicated(oth$respid))
  out <- data.frame(id = sel$respid, chosen_profile = sel$side, stringsAsFactors = FALSE)
  for (v in names(amap)) out[[paste0("chosen_", amap[[v]])]] <- sel[[v]]
  o <- oth[match(out$id, oth$respid), ]
  for (v in names(amap)) out[[paste0("other_", amap[[v]])]] <- o[[v]]
  out
}

assemble <- function(txt, task, attrs, extra = NULL) {
  df <- merge(frame[, "id", drop = FALSE], txt, by = "id", all.x = TRUE)
  df <- merge(df, attrs, by = "id", all.x = TRUE)
  if (!is.null(extra)) df <- merge(df, extra, by = "id", all.x = TRUE)
  df$country <- "KR"; df$lang <- "ko"; df$exclude <- FALSE; df$task <- as.integer(task)
  front <- c("id", "country", "lang", "text", "exclude", "task")
  df[, c(front, setdiff(names(df), front))]
}

src_main <- "_archive/Pre-2024/AKS21/Data/C_UOT-21-2101 p995570460322 261121/ (OECE ConjointLoopC*, LoopDWC*, main OECE)"
excl_note <- "exclude all FALSE: vendor delivered completes only (status==complete, N=2009); project code defines no screen-outs. SLloop straight-lining flags (1404 resp., rating grids) and the SI text-derived 'attention' code were not used as exclusions. Text is byte-identical to the OECE ConjointLoop export; the LoopDW export differs from it only by outer whitespace (76-92 rows per item)."

# ---------------------------------------------------------------- C1 --------
c1_map <- c(dSexC1 = "sex", dAgeC1 = "age", dChildrenC1 = "has_children",
            dRelationshipC1 = "relationship", dOccupationC1 = "occupation",
            dPartyMemberC1 = "former_kwp_member", dTimeInChinaC1 = "time_in_china",
            dReligionC1 = "religion", dTimeInSKC1 = "time_in_south_korea",
            dSocialNetworksC1 = "social_networks")
c1_choices <- function(task) {
  x <- rd(oece("LoopDWC1"))
  x <- x[grepl(paste0("^Choice ", task, " - "), x$LoopDWC1), ]
  x$side <- sub("^.* ", "", x$dProfileC1)
  f <- function(v) { s <- x[x[[v]] == "Selected", ]; s$side[match(frame$id, s$respid)] }
  data.frame(id = frame$id, choice_vote = f("dChoiceVoteC1"),
             choice_hire = f("dChoiceHireC1"), choice_neighbor = f("dChoiceNeighborC1"),
             stringsAsFactors = FALSE)
}
c1_items <- list(
  list(id = "aks21_c1_vote", var = "DV7votingC1", task = 7, choice = "dChoiceVoteC1",
       contrast = "chosen_former_kwp_member / other_former_kwp_member"),
  list(id = "aks21_c1_hire", var = "DV7hiringC1", task = 3, choice = "dChoiceHireC1",
       contrast = "chosen_occupation / other_occupation"),
  list(id = "aks21_c1_neighbor", var = "DV7neighborC1", task = 5, choice = "dChoiceNeighborC1",
       contrast = "chosen_social_networks / other_social_networks"))
for (it in c1_items) {
  df <- assemble(get_text(1, it$var, it$task), it$task,
                 pair_attrs(1, it$task, "dProfileC1", it$choice, c1_map),
                 c1_choices(it$task))
  write_harmonized(df, it$id, manifest = list(
    source = paste0(src_main, "; text var ", it$var),
    design = sprintf("Paired CBC on NK defector-migrant profiles (10 attrs, 8 tasks; vote/hire/neighbor choices per task); open-end after task %d about the %s choice", it$task, sub("aks21_c1_", "", it$id)),
    lang = "ko",
    treatment_cols = paste("task; chosen_profile; chosen_*/other_* (10 attrs); choice_vote/choice_hire/choice_neighbor (A/B in that task); primary:", it$contrast),
    human_codes = "none kept (SI 'attention' code in Data/df_vote_check.xlsx is keyed to a misaligned respid-text file; vote item only)",
    routing = "API-eligible", card = "y",
    caveats = paste("Task per WP footnote 1 and OECE (text non-empty only in that iteration). Vendor datamap lists DV7hiringC1 right after the vote choice DV1C1, DV7neighborC1 after the hire choice, DV7votingC1 after the neighbor choice, so on-screen placement may not match the variable name; choice_* columns allow re-deriving chosen/other for the alternative outcome.", excl_note, "English translations exist in integrate_text.xlsx (not kept)."),
    unverified = "Which choice question the open-end sat under on screen (name vs datamap order); Korean prompt from pre-fielding draft docx"))
}

# ---------------------------------------------------------------- C2 --------
c2_map <- c(dEndorserC2 = "endorser", dGoalC2 = "policy_goal",
            dInstrumentC2 = "policy_instrument", dCostC2 = "public_cost",
            dViewsBusComC2 = "business_community_views",
            dViewsCivilSocietyC2 = "civil_society_views",
            dEligibleRecpC2 = "eligibility", dReasonC2 = "policy_rationale")
P2 <- file.path(A, "NK-business-support_conjoint/Data")
rob <- read.csv(file.path(P2, "open_text_C2-robust.csv"), fileEncoding = "UTF-8",
                colClasses = "character", na.strings = character(0))
why <- read.csv(file.path(P2, "whycheck.csv"), fileEncoding = "UTF-8",
                colClasses = "character", na.strings = character(0))
codes <- data.frame(id = rob$respid,
                    codePW = suppressWarnings(as.integer(rob$codePW)),
                    codeCG = suppressWarnings(as.integer(rob$codeCG)),
                    stringsAsFactors = FALSE)
codes$valuenew <- suppressWarnings(as.integer(why$valuenew[match(codes$id, why$respid)]))
df <- assemble(get_text(2, "DV3C2", 4), 4,
               pair_attrs(2, 4, "dPolicyC2", "dChoiceC2", c2_map), codes)
write_harmonized(df, "aks21_c2_nkbusiness", manifest = list(
  source = paste0(src_main, "; text var DV3C2; human codes from _archive/Pre-2024/NK-business-support_conjoint/Data/open_text_C2-robust.csv (codePW, codeCG) and whycheck.csv (valuenew)"),
  design = "Paired CBC on NK-entrepreneurship support policies (8 attrs incl. randomized policy rationale, 5 tasks); open-end after task 4 about the chosen policy",
  lang = "ko",
  treatment_cols = "task; chosen_profile; chosen_*/other_* (8 attrs); primary: chosen_policy_rationale / other_policy_rationale",
  human_codes = "codePW, codeCG (two coders; 1 economic, 2 normative, 3 other, 4 uninformative), valuenew (resolved: CG where coders disagree); codeCG has one value 11 and 2 NA; valuenew NA for 4 respids absent from whycheck.csv",
  routing = "API-eligible", card = "y",
  caveats = paste("Kor text in open_text_C2-robust.csv equals the vendor export at the same respid (1990/1990 non-empty), so codes attach by respid.", excl_note, "Codes, LDA and LLM classification already run on this item (IMR paper; paper repo analysis/R/10_nk_validation.R, 11_calibration.R)."),
  unverified = "Korean prompt from pre-fielding draft docx"))

# ---------------------------------------------------------------- C3 --------
c3_map <- c(dPolSystemC3 = "political_system", dTimingC3 = "timing",
            dUnificationTaxC3 = "unification_tax",
            dNSTravelRestrictionsC3 = "ns_travel_restrictions",
            dMilitaryPresenceC3 = "military_presence_in_nk",
            dKoreanWorkersPartyPC3 = "kwp_participation",
            dParticipationSKC3 = "sk_firms_in_nk", dNKMiltaryStatusC3 = "nk_military_status",
            dNKnuclearWeaponsC3 = "nk_nuclear_weapons", dNKleadershipC3 = "nk_leadership")
df <- assemble(get_text(3, "DV3C3", 7), 7,
               pair_attrs(3, 7, "dScenarioC3", "dChoiceC3", c3_map))
write_harmonized(df, "aks21_c3_unification", manifest = list(
  source = paste0(src_main, "; text var DV3C3"),
  design = "Paired CBC on post-unification scenarios (10 attrs, 8 tasks); open-end after task 7 about the chosen scenario",
  lang = "ko",
  treatment_cols = "task; chosen_profile; chosen_*/other_* (10 attrs); primary: chosen_political_system / other_political_system",
  human_codes = "none",
  routing = "API-eligible", card = "y",
  caveats = paste("_archive/Pre-2024/Korean-unification_conjoint/c3_open text.csv not used: only 1126 of its respids exist in the survey and none of its texts match the vendor export at the same respid; AKS21/Data/c3_open text.xlsx has no respid.", excl_note, "No paper or PAP found; target construct inferred from the design and main_c3.R."),
  unverified = "Construct source (no manuscript located); Korean prompt from pre-fielding draft docx"))

# ---------------------------------------------------------------- C4 --------
c4_map <- c(dEndorserC4 = "endorser", dPolicyPositionC4 = "policy_position",
            dRationaleC4 = "rationale")
arm <- data.frame(id = frame$id, list_arm_raw = frame$list_arm_raw, stringsAsFactors = FALSE)
arm$treatment <- c("A - 3 statements" = "list_control",
                   "B - 4 Statements + Sensitive Item 1" = "list_unification_item",
                   "C - 4 Statements + Sensitive Item 2" = "list_nsa_abolition_item")[arm$list_arm_raw]
names(arm)[names(arm) == "list_arm_raw"] <- "dVII1"
stopifnot(!anyNA(arm$treatment))
df <- assemble(get_text(4, "DV3C4", 7), 7,
               pair_attrs(4, 7, "dPositionC4", "dChoiceC4", c4_map), arm)
df <- df[, c(setdiff(names(df), c("treatment", "dVII1")), "treatment", "dVII1")]
write_harmonized(df, "aks21_c4_nsa", manifest = list(
  source = paste0(src_main, "; text var DV3C4; list arm dVII1 from main OECE"),
  design = "Paired CBC on National Security Act positions (3 attrs: endorser, position, rationale; 8 tasks); open-end after task 7 about the chosen position. Preceded by a 3-arm list experiment (dVII1).",
  lang = "ko",
  treatment_cols = "task; chosen_profile; chosen_*/other_* (3 attrs); primary: chosen_rationale / other_rationale; secondary respondent-level: treatment (from dVII1: list control / unification item / NSA-abolition item, ~670 each)",
  human_codes = "none",
  routing = "API-eligible", card = "y",
  caveats = paste("C4opentext.xlsx matches the vendor export at the same respid (1990/1991 non-empty); vendor export used.", excl_note, "List experiment immediately precedes C4 in the datamap and questionnaire, so arm C shows 'Abolishing the National Security Act' just before the NSA conjoint."),
  unverified = "Published paper (Green & Denney 2024, Democratization) not available locally, so construct and direction come from the design; Korean prompt from pre-fielding draft docx"))
