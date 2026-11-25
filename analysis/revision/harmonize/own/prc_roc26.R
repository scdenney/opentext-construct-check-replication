# Harmonize PRC 2025-26 (Q22 national, Q60 NGO) and Taiwan 2026 (Q12_6).
# BLINDNESS: open-text columns are copied into `text` and counted only.
# Run: LANG=en_US.UTF-8 Rscript prc_roc26.R

source("analysis/revision/harmonize/own/common.R")  # path changed on recovery (2026-11-17)
suppressPackageStartupMessages({ library(readxl); library(readr) })

PRN <- "/home/buddleman/Documents/github/research/projects/political-regimes-nationalism"
PRC_XLSX <- file.path(PRN, "prc-cbcs/data/Final_data_text.xlsx")
PRC_C1 <- file.path(PRN, "prc-cbcs/data/clean/c1_patriotism_ready.rds")
PRC_C2 <- file.path(PRN, "prc-cbcs/data/clean/c2_ngo_trust_ready.rds")
ROC_CSV <- file.path(PRN, "roc-cbc26/data/roc_raw.csv")

# ---- attribute maps (source label -> harmonized English) -------------------
# Column names follow roc2024_q74_national (Taiwan 2024 CBC #2: ancestry, origin,
# language, legal_status, residence, politics, civicness) where the attribute is the
# same; patriotism and social_values have no 2024 counterpart (2024 sentiment =
# national self-identification, culture = views on traditions) and keep own names.
# National-identity levels use the PRC x ROC harmonized level set from
# replication/code/00_utils.R (harmonized_levels) and 02_analysis.R, so PRC Q22
# and ROC Q12_6 share column names and level labels.
c1_prc <- list(
  origin = list(src = "出生地点", map = c("中国大陆" = "Own country", "台湾省" = "Cross-strait other",
    "韩国" = "South Korea", "德国" = "Germany", "印度尼西亚" = "Indonesia", "刚果" = "Congo",
    "奈及利亞" = "Nigeria (pilot design)")),
  ancestry = list(src = "族群", map = c("汉族人" = "Han Chinese", "中国少数民族" = "Domestic minority",
    "非汉族东亚人" = "Non-Han East Asian", "东南亚人" = "Southeast Asian", "欧洲人" = "European",
    "非洲人" = "African")),
  legal_status = list(src = "身份状态", map = c("短期居留许可" = "Temporary permit",
    "永久居留证" = "Permanent residence", "中国绿卡" = "Permanent residence", "持中国护照" = "Passport")),
  residence = list(src = "在中国生活的时间", map = c("7年(含)及以下" = "7 years or less",
    "14年" = "14 years", "21年" = "21 years", "大部分时间或一生" = "Most/entire life")),
  language = list(src = c("语言能力", "语文能力"), map = c("不会中文" = "No Mandarin",
    "中文能力有限" = "Limited Mandarin", "中文流利" = "Fluent Mandarin")),
  patriotism = list(src = "爱国主义", map = c("爱国情感不强" = "Not patriotic",
    "有爱国情感，但不常表达" = "Patriotic, rarely expresses",
    "有爱国情感，并且时常表达" = "Patriotic, often expresses")),
  politics = list(src = "政治观点", map = c("对政治没什么看法" = "No political opinion",
    "认为政治领导人应该由人民一人一票直接选出来。" = "Pro-democracy",
    "认为媒体应该有不受政府干预发表新闻和观点的权利。" = "Pro-media freedom",
    "认为我们国家最好直接由一位强有力的领导人执政，而不需要经过选举产生领导人。" = "Pro-strongman")),
  social_values = list(src = "社会价值观", map = c("认为在人际交往中，人们应该宽容务实。" = "Tolerant/pragmatic",
    "认为当周围的人不同意时，人们不应该坚持自己的看法。" = "Conformist",
    "认为即使周围的人不同意，人们也应该坚持自己的看法。" = "Individualist")),
  civicness = list(src = "公共参与", map = c("平时不是在家里，就是和朋友喝茶。" = "Stays home",
    "参加政府组织的社区活动" = "Govt community events", "组织别人一起向政府表达意见。" = "Organizes petitions"))
)

c1_roc <- list(
  origin = list(src = "出生地點", map = c("台灣" = "Own country", "中國大陸" = "Cross-strait other",
    "韓國" = "South Korea", "德國" = "Germany", "印尼（印度尼西亞）" = "Indonesia")),
  ancestry = list(src = "族群", map = c("漢人" = "Han Chinese", "台灣原住民" = "Domestic minority",
    "非漢族之東亞人" = "Non-Han East Asian", "東南亞人" = "Southeast Asian", "歐洲人" = "European",
    "非洲人" = "African")),
  legal_status = list(src = "身分狀態", map = c("持有短期居留許可" = "Temporary permit",
    "持有永久居留證" = "Permanent residence", "持有中華民國護照" = "Passport")),
  residence = list(src = "在台居住時間", map = c("7年(含)及以下" = "7 years or less",
    "14年" = "14 years", "21年" = "21 years", "大部分時間或一生都住在台灣" = "Most/entire life")),
  language = list(src = "語言能力", map = c("完全不會說國語（華語）" = "No Mandarin",
    "國語（華語）能力有限" = "Limited Mandarin", "國語（華語）流利" = "Fluent Mandarin")),
  patriotism = list(src = "愛國情感", map = c("愛國情感不太強烈" = "Not patriotic",
    "有愛國情感，但不常表達" = "Patriotic, rarely expresses",
    "有愛國情感，且經常表達" = "Patriotic, often expresses")),
  politics = list(src = "政治觀點", map = c("對政治沒有特定的看法" = "No political opinion",
    "認為領導人應由全民「一人一票」直接選舉產生" = "Pro-democracy",
    "認為媒體應享有新聞自由，不受政府干預" = "Pro-media freedom",
    "認為國家最好由一位強而有力的領導人執政，不需要經過選舉" = "Pro-strongman")),
  social_values = list(src = "處世價值觀", map = c("認為待人處事應該寬容且務實" = "Tolerant/pragmatic",
    "認為當周圍的人不同意時，個人不應堅持己見" = "Conformist",
    "認為即使周圍的人不同意，個人仍應堅持己見" = "Individualist")),
  civicness = list(src = "公共參與", map = c("休閒時間多待在家或與朋友聚會聊天" = "Stays home",
    "參加政府或社區組織的活動" = "Govt community events",
    "會組織他人集體向政府表達訴求" = "Organizes petitions"))
)

c2_prc <- list(
  registration = list(src = "注册状态", map = c("未注册" = "Unregistered", "商业注册" = "Commercial",
    "民办非企业单位" = "Private non-enterprise", "社会团体" = "Social organization", "基金会" = "Foundation")),
  origin = list(src = "组织来源", map = c("本土组织" = "Domestic", "国际组织" = "International")),
  background = list(src = "组织背景", map = c("由民间自发成立的组织" = "Grassroots",
    "由政府部门发起成立的组织" = "Government-initiated")),
  staff_size = list(src = "全职员工规模", map = c("1-5" = "1-5", "6-10" = "6-10", "11-15" = "11-15",
    "16 人及以上" = "16+")),
  assessment_rating = list(src = "社会组织评估等级", map = c("未参加" = "Not evaluated",
    "1A (最低)" = "1A (lowest)", "3A (中等)" = "3A (medium)", "5A (最高)" = "5A (highest)")),
  party_building = list(src = "党建参与情况", map = c("未参与" = "Not participating", "正在筹备" = "Preparing",
    "已参与" = "Already participating")),
  social_media = list(src = "社交媒体活跃度", map = c("无公众号/网站" = "None",
    "有不常更新的公众号/网站" = "Inactive account", "有定期更新的公众号/网站" = "Regularly updated")),
  service_area = list(src = "服务领域", map = c("工人权益" = "Worker rights",
    "环境保护" = "Environmental protection", "扶贫" = "Poverty relief")),
  govt_cooperation = list(src = "政府合作程度", map = c("暂时没有无合作" = "No cooperation",
    "偶尔合作" = "Occasional", "经常合作（中标过政府购买服务或经常沟通建言）" = "Frequent"))
)

# Profile attributes for one task: returns list(p1 = df, p2 = df) of English
# labels, one column per harmonized attribute. Stops on unmapped levels.
task_attrs <- function(raw, prefix, task, amap, n_attr = 9) {
  out <- list()
  for (p in 1:2) {
    df <- data.frame(row.names = seq_len(nrow(raw)))
    for (a in names(amap)) {
      v <- rep(NA_character_, nrow(raw))
      for (i in seq_len(n_attr)) {
        nmc <- raw[[sprintf("%s-F-%d-%d", prefix, task, i)]]
        val <- raw[[sprintf("%s-F-%d-%d-%d", prefix, task, p, i)]]
        hit <- !is.na(nmc) & nmc %in% amap[[a]]$src
        v[hit] <- val[hit]
      }
      v[!is.na(v) & v == ""] <- NA
      un <- setdiff(unique(na.omit(v)), names(amap[[a]]$map))
      if (length(un)) stop("unmapped levels for ", a, ": ", paste(un, collapse = " / "))
      df[[a]] <- unname(amap[[a]]$map[v])
    }
    out[[p]] <- df
  }
  out
}

# chosen_/other_ columns given a choice vector of 1/2/NA
chosen_other <- function(pr, ch) {
  res <- data.frame(row.names = seq_along(ch))
  for (a in names(pr[[1]])) {
    a1 <- pr[[1]][[a]]; a2 <- pr[[2]][[a]]
    res[[paste0("chosen_", a)]] <- ifelse(ch == 1, a1, ifelse(ch == 2, a2, NA))
  }
  for (a in names(pr[[1]])) {
    a1 <- pr[[1]][[a]]; a2 <- pr[[2]][[a]]
    res[[paste0("other_", a)]] <- ifelse(ch == 1, a2, ifelse(ch == 2, a1, NA))
  }
  res
}

# ============================================================================
# PRC
# ============================================================================
raw <- read_excel(PRC_XLSX, col_types = "text", na = character(), trim_ws = FALSE)[-1, ]   # row 1 = Qualtrics labels
stopifnot(all(grepl("^R_", raw$ResponseId)), !anyDuplicated(raw$ResponseId))
stopifnot(all(raw$UserLanguage == "ZH-S"))

# Pilot-design respondents (project rule, 01_data_prep.R: any C1 profile with a
# Place of Birth outside the final 6 levels, i.e. Nigeria): excluded from C1.
pob_vals <- character(0)
pilot <- rep(FALSE, nrow(raw))
for (t in 1:6) for (i in 1:9) for (p in 1:2) {
  nmc <- raw[[sprintf("C1-F-%d-%d", t, i)]]; val <- raw[[sprintf("C1-F-%d-%d-%d", t, p, i)]]
  pilot <- pilot | (!is.na(nmc) & nmc == "出生地点" & !is.na(val) & val == "奈及利亞")
}
cat("PRC pilot-design respondents:", sum(pilot), "\n")

# Attention item Q47 ("select 22"): instrument-defined quality check, not
# enforced in the flow nor filtered by the project (sensitivity only, t36).
attn_q47_pass <- !is.na(raw$Q47) & raw$Q47 == "22"

list_lab <- c("0" = "Control list", "1" = "Placebo list (pride in family)",
              "2" = "Sensitive list (pride in being Chinese)")
surv_lab <- c("0" = "Control", "1" = "Private security", "2" = "National security",
              "3" = "Public morality")

# ---- Q22: C1 task 3 --------------------------------------------------------
ch1 <- ifelse(raw$Q21 == "人物A", 1L, ifelse(raw$Q21 == "人物B", 2L, NA_integer_))
pr1 <- task_attrs(raw, "C1", 3, c1_prc)
co1 <- chosen_other(pr1, ch1)
attr_missing1 <- is.na(pr1[[1]]$origin) | is.na(pr1[[2]]$origin)
reached1 <- !is.na(ch1)   # Q22 sits on the task-3 page with the Q21 choice

q22 <- data.frame(
  id = raw$ResponseId, country = "CN", lang = "zh-Hans", text = raw$Q22,
  exclude = pilot | attr_missing1 | !attn_q47_pass,
  exclude_reason = trimws(paste(ifelse(pilot, "pilot_design", ""),
                                ifelse(attr_missing1, "task3_attributes_missing", ""),
                                ifelse(!attn_q47_pass, "failed_attention_Q47", ""))),
  attn_q47_pass = attn_q47_pass,
  task = 3L, task_choice = c("A", "B")[ch1],
  co1,
  Patriotism_list = as.integer(raw$Patriotism_list),
  list_exp_arm = unname(list_lab[raw$Patriotism_list]),
  surveillance_exp = as.integer(raw$surveillance_exp),
  stringsAsFactors = FALSE
)[reached1, ]
q22$exclude_reason <- gsub(" +", ";", q22$exclude_reason)

# Verify against the clean RDS: task-3 chosen profile per respondent
c1c <- readRDS(PRC_C1)
c1c <- c1c[c1c$task == 3 & c1c$chosen == 1,
           c("ResponseId", "Place.of.Birth", "Ethnicity", "Legal.Status", "Time.in.China",
             "Language", "Patriotism", "Political.Views", "Social.Values", "Civic.Involvement")]
back <- c("Own country" = "China", "Cross-strait other" = "Taiwan",
          "Domestic minority" = "Chinese ethnic minority", "Passport" = "Chinese passport")
unh <- function(x) ifelse(x %in% names(back), back[x], x)
m <- merge(q22, c1c, by.x = "id", by.y = "ResponseId")
pairs1 <- list(c("chosen_origin", "Place.of.Birth"), c("chosen_ancestry", "Ethnicity"),
  c("chosen_legal_status", "Legal.Status"), c("chosen_residence", "Time.in.China"),
  c("chosen_language", "Language"), c("chosen_patriotism", "Patriotism"),
  c("chosen_politics", "Political.Views"), c("chosen_social_values", "Social.Values"),
  c("chosen_civicness", "Civic.Involvement"))
mm1 <- sapply(pairs1, function(p) sum(unh(m[[p[1]]]) != as.character(m[[p[2]]]), na.rm = TRUE))
cat("C1 clean-RDS check: matched respondents =", nrow(m), "; clean respondents =",
    length(unique(c1c$ResponseId)), "; mismatches per attribute =", paste(mm1, collapse = ","), "\n")
cat("C1 clean ids == non-excluded-by-pilot/missing ids:",
    setequal(c1c$ResponseId, q22$id[!grepl("pilot_design|task3_attributes_missing", q22$exclude_reason)]), "\n")

write_harmonized(q22, "prc2026_q22_national", manifest = list(
  source = "political-regimes-nationalism/prc-cbcs/data/Final_data_text.xlsx (Q22, C1-F-3-*, Q21); checked against prc-cbcs/data/clean/c1_patriotism_ready.rds",
  design = "Paired-profile national-identity conjoint (6 tasks, 9 attributes); open-end on the task-3 page",
  lang = "zh-Hans",
  treatment_cols = "task, task_choice, chosen_* / other_* (9 harmonized attributes, task 3); also Patriotism_list/list_exp_arm (pre-Q22 list experiment) and surveillance_exp (assigned after Q22: falsification only)",
  human_codes = "none",
  routing = "API-eligible",
  card = "y",
  caveats = "Rows = respondents with a task-3 choice (Q21), N=2582 of 2586 exported. exclude = pilot design (project drops from C1) | task-3 attributes missing (project drops) | failed Q47 attention item (instrument-defined, not filtered by project; attn_q47_pass column allows reverting). Level labels use the PRC x ROC harmonized set (Own country = mainland China, Cross-strait other = Taiwan). chosen_* is post-choice; the randomized contrast is over the displayed pair. Attribute columns share names with roc2024_q74_national where the attribute matches (origin, ancestry, language, legal_status, residence, politics, civicness); patriotism and social_values have no 2024 counterpart. Export: Final_data_text.xlsx (the only PRC export, dated 2026-03-27 on disk), the same file the clean RDS was built from; ids and task-3 linkage match it exactly.",
  unverified = "none"
))

# ---- Q60: C2 task 3 + surveillance vignette --------------------------------
ch2 <- ifelse(raw$Q59 == "组织A", 1L, ifelse(raw$Q59 == "组织B", 2L, NA_integer_))
pr2 <- task_attrs(raw, "C2", 3, c2_prc)
co2 <- chosen_other(pr2, ch2)
attr_missing2 <- is.na(pr2[[1]]$registration) | is.na(pr2[[2]]$registration)
reached2 <- !is.na(ch2)   # Q60 sits on the task-3 page with the Q59 choice

q60 <- data.frame(
  id = raw$ResponseId, country = "CN", lang = "zh-Hans", text = raw$Q60,
  exclude = attr_missing2 | !attn_q47_pass,
  exclude_reason = trimws(paste(ifelse(attr_missing2, "task3_attributes_missing", ""),
                                ifelse(!attn_q47_pass, "failed_attention_Q47", ""))),
  attn_q47_pass = attn_q47_pass,
  treatment = unname(surv_lab[raw$surveillance_exp]),
  surveillance_exp = as.integer(raw$surveillance_exp),
  task = 3L, task_choice = c("A", "B")[ch2],
  co2,
  stringsAsFactors = FALSE
)[reached2, ]
q60$exclude_reason <- gsub(" +", ";", q60$exclude_reason)
stopifnot(!anyNA(q60$treatment))

c2c <- readRDS(PRC_C2)
c2c <- c2c[c2c$task == 3 & c2c$chosen == 1,
           c("ResponseId", "Registration", "Origin", "Background", "Staff.Size", "Assessment.Rating",
             "Party.Building", "Social.Media", "Service.Area", "Govt.Cooperation", "surveillance_exp")]
m2 <- merge(q60, c2c, by.x = "id", by.y = "ResponseId", suffixes = c("", ".clean"))
pairs2 <- list(c("chosen_registration", "Registration"), c("chosen_origin", "Origin"),
  c("chosen_background", "Background"), c("chosen_staff_size", "Staff.Size"),
  c("chosen_assessment_rating", "Assessment.Rating"), c("chosen_party_building", "Party.Building"),
  c("chosen_social_media", "Social.Media"), c("chosen_service_area", "Service.Area"),
  c("chosen_govt_cooperation", "Govt.Cooperation"))
mm2 <- sapply(pairs2, function(p) sum(m2[[p[1]]] != as.character(m2[[p[2]]]), na.rm = TRUE))
cat("C2 clean-RDS check: matched respondents =", nrow(m2), "; clean respondents =",
    length(unique(c2c$ResponseId)), "; mismatches per attribute =", paste(mm2, collapse = ","),
    "; surveillance_exp mismatches =", sum(m2$surveillance_exp != m2$surveillance_exp.clean), "\n")
cat("Q60 arms (all reached):\n"); print(table(q60$treatment, q60$exclude))

write_harmonized(q60, "prc2026_q60_ngo", manifest = list(
  source = "political-regimes-nationalism/prc-cbcs/data/Final_data_text.xlsx (Q60, C2-F-3-*, Q59, surveillance_exp); checked against prc-cbcs/data/clean/c2_ngo_trust_ready.rds",
  design = "4-arm surveillance (Skynet) vignette, respondent-level, before a paired-profile NGO-trust conjoint (6 tasks, 9 attributes); open-end on the task-3 page",
  lang = "zh-Hans",
  treatment_cols = "treatment / surveillance_exp (Control, Private security, National security, Public morality); task, task_choice, chosen_* / other_* (9 NGO attributes, task 3)",
  human_codes = "none",
  routing = "API-eligible",
  card = "y",
  caveats = "Rows = respondents with a task-3 choice (Q59), N=2582. exclude = task-3 attributes missing (project drops) | failed Q47 attention item. The vignette is separated from the NGO conjoint by the attention item and the volunteering block. No PAP or paper states a hypothesis for vignette -> NGO-trust reasons; the card's target is proposed from the instrument. Questionnaire 2.5 describes the item as voluntary ('如果可以的话'), but the fielded QSF wording omits that and all who reached it answered.",
  unverified = "Target construct and direction for the surveillance arms: no PAP/paper found"
))

# ============================================================================
# ROC (Taiwan 2026)
# ============================================================================
rr <- read_csv(ROC_CSV, col_types = cols(.default = "c"), na = character(), trim_ws = FALSE, progress = FALSE)
# literal "NA" answers stay text; blank non-text cells become NA below
for (v in setdiff(names(rr), "Q12_6")) rr[[v]][!is.na(rr[[v]]) & rr[[v]] == ""] <- NA
rr <- rr[-c(1, 2), ]   # Qualtrics label row + ImportId row
stopifnot(!anyDuplicated(rr$ResponseId), all(rr$UserLanguage == "ZH-T"))
ch3 <- ifelse(rr$Q12_5 == "人物A", 1L, ifelse(rr$Q12_5 == "人物B", 2L, NA_integer_))
pr3 <- task_attrs(rr, "C2", 3, c1_roc)   # the ROC national-identity conjoint is block C2
co3 <- chosen_other(pr3, ch3)
attr_missing3 <- is.na(pr3[[1]]$origin) | is.na(pr3[[2]]$origin)
# Project rule (replication/code/01_roc_data_prep.R): Finished and Q5 attention pass
roc_ok <- rr$Finished == "True" & !is.na(rr$Q5) & rr$Q5 == "非常不同意"
reached3 <- !is.na(ch3)

q12 <- data.frame(
  id = rr$ResponseId, country = "TW", lang = "zh-Hant", text = rr$Q12_6,
  exclude = !roc_ok | attr_missing3,
  task = 3L, task_choice = c("A", "B")[ch3],
  co3,
  stringsAsFactors = FALSE
)[reached3, ]

# Same harmonized columns as PRC Q22
stopifnot(identical(grep("^(chosen|other)_", names(q12), value = TRUE),
                    grep("^(chosen|other)_", names(q22), value = TRUE)))

# Cross-check against the project's long file (replication/data/roc_long.rds)
rl <- readRDS(file.path(PRN, "replication/data/roc_long.rds"))
rl <- rl[rl$task == 3 & rl$chosen == 1, c("ResponseId", "Place of Birth", "Ethnicity", "Patriotism",
                                          "Political Views", "Civic Involvement")]
m3 <- merge(q12, rl, by.x = "id", by.y = "ResponseId")
mm3 <- c(sum(m3$chosen_origin != as.character(m3[["Place of Birth"]])),
         sum(m3$chosen_ancestry != as.character(m3$Ethnicity)),
         sum(m3$chosen_patriotism != as.character(m3$Patriotism)),
         sum(m3$chosen_politics != as.character(m3[["Political Views"]])),
         sum(m3$chosen_civicness != as.character(m3[["Civic Involvement"]])))
cat("ROC roc_long check: matched =", nrow(m3), "; mismatches (5 attrs) =", paste(mm3, collapse = ","), "\n")

write_harmonized(q12, "roc2026_q12_national", manifest = list(
  source = "political-regimes-nationalism/roc-cbc26/data/roc_raw.csv (Q12_6, C2-F-3-*, Q12_5); checked against replication/data/roc_long.rds",
  design = "Paired-profile national-identity conjoint (6 tasks, same 9 attributes as PRC C1, harmonized levels); open-end on the task-3 page",
  lang = "zh-Hant",
  treatment_cols = "task, task_choice, chosen_* / other_* (9 harmonized attributes, task 3; same columns as prc2026_q22_national)",
  human_codes = "none",
  routing = "API-eligible",
  card = "y",
  caveats = "Rows = respondents with a task-3 choice (Q12_5), N=2500 (the export already holds only finished, attention-passing respondents). exclude = not Finished | failed Q5 attention (project rule) | task-3 attributes missing: none flagged. Prompt differs from PRC Q22: asks the main reason for the choice, not why the person is a national. No respondent-level arm precedes the open-end. Place of birth (origin) has 5 levels (no Congo). Same attribute column names as prc2026_q22_national and, where the attribute matches, roc2024_q74_national. Export: roc_raw.csv (final Qualtrics pull 2026-06-17); replication/data/roc_long.rds matches it (2500/2500 ids, 0 task-3 mismatches). Read with NA conversion off: 2 literal NA answers are kept as text.",
  unverified = "none"
))
