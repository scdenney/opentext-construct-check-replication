# Harmonize IFES 2023 open-ends (DE, SK, NK-migrant samples) into data/harmonized/.
# BLINDNESS: no open-text value is printed, sampled or summarized; only counts.
# Run: LANG=en_US.UTF-8 Rscript ifes2023.R
source("analysis/revision/harmonize/own/common.R")  # path changed on recovery (2026-11-17)
base <- "/home/buddleman/Documents/github/research/projects/_archive/IFES2023"
src <- c(DE = "Germans/Data/IFES 2023 - German_February 12, 2024_18.11-choice_text.csv",
         SK = "South Koreans/Data/IFES 2023 - ROK_February 12, 2024_16.45.csv",
         NK = "North Koreans/Data/choice-text_IFES+2023+-+North+Koreans_October+23,+2023_09.46.csv")

# Qualtrics export: row 1 names, rows 2-3 label/ImportId (multi-line quoted).
# Names are duplicated in DE (Q71-Q75) and NK (Q1-Q4, Q42-Q44); make.unique keeps
# the first occurrence under the bare name, which is the one used here
# (DE Q72/Q74 politician tasks, NK Q2/Q3/Q4 background items precede the duplicates).
read_q <- function(p) {
  # na.strings = character(0): no string (not even a literal "NA" answer) becomes NA
  d <- read.csv(p, header = FALSE, colClasses = "character", encoding = "UTF-8", na.strings = character(0))
  d[] <- lapply(d, function(x) { x[x == ""] <- NA_character_; x })  # empty cells -> NA (text written back as "")
  names(d) <- make.unique(as.character(d[1, ]), sep = "...")
  d <- d[-(1:3), , drop = FALSE]; rownames(d) <- NULL; d
}
nonempty <- function(x) !is.na(x) & nchar(trimws(x)) > 0

# ---- English labels for attribute names and levels ----
attr_en <- c(
  # politician (C1)
  "Ansichten zur Demokratie" = "democracy", "Ansichten zur Wirtschaft" = "economy",
  "Ansichten zur Sozialhilfe" = "welfare", "Ansichten zur den Geschlechtern" = "gender",
  "Ansichten zur Diversität" = "diversity",
  "민주주의에 대한 견해" = "democracy", "경제에 대한 견해" = "economy",
  "복지에 대한 견해" = "welfare", "성별에 대한 견해" = "gender", "다양성에 대한 견해" = "diversity",
  # job training
  "Alter" = "age", "Familienstand" = "family_status", "Geschlecht" = "sex",
  "Aktuelle oder letzte Beschäftigung" = "occupation", "Vorstrafen" = "criminal_record",
  "Herkunft" = "birth_origin",   # NB: in the DE natid block "Herkunft" is birthplace; handled below
  "나이" = "age", "가족 관계" = "family_status", "성별" = "sex", "직업" = "occupation",
  "전과 기록" = "criminal_record", "출생지" = "birth_origin",
  # national identity
  "Aufenthalt" = "residence", "Einstellung zum politischen System und Gesetzen" = "political_institutions",
  "Familienhintergrund" = "ethnic_ancestry", "Nationales Empfinden" = "national_feeling",
  "Religion" = "religion", "Sprachvermögen" = "language", "Staatsbürgerschaft" = "citizenship",
  "Verständnis von Tradition und Geschichte" = "history_tradition",
  "거주지" = "residence", "국민 정서" = "national_feeling", "민족성" = "ethnic_ancestry",
  "시민권 여부" = "citizenship", "언어 능력" = "language", "역사 및 전통 이해도" = "history_tradition",
  "정치 제도 및 법 태도" = "political_institutions")

lvl_en <- c(
  # politician: English wording of Brehm, Zhou & Denney (2025) Table 2
  "In einer Demokratie funktioniert das Wirtschaftssystem schlecht" = "In a democracy, the economic system runs badly",
  "Demokratische Systeme sind unentschlossen und schlecht darin, Ordnung aufrechtzuerhalten" = "Democratic systems are indecisive and bad at maintaining order",
  "Die Demokratie mag Probleme haben, aber sie ist besser als jede andere Regierungsform" = "Democracy may have problems but it is better than any other form of government",
  "민주주의는 경제력을 악화시킨다" = "In a democracy, the economic system runs badly",
  "민주주의는 결정력이 부족하며 질서를 유지하지 못한다" = "Democratic systems are indecisive and bad at maintaining order",
  "민주주의는 문제점이 있지만, 다른 어떤 정부 형태보다는 나은 방식이다" = "Democracy may have problems but it is better than any other form of government",
  "Der Staat sollte Unternehmen mehr Freiheit geben" = "The state should give more freedom to firms",
  "Der Staat sollte Unternehmen effektiver kontrollieren" = "The state should control firms more effectively",
  "국가는 사기업을 확대해야 한다" = "The state should give more freedom to firms",
  "국가는 국영기업을 확대해야 한다" = "The state should control firms more effectively",
  "Die Menschen sollten mehr Verantwortung übernehmen, um für sich selbst zu sorgen" = "People should take more responsibility to provide for themselves",
  "Die Regierung sollte mehr Verantwortung übernehmen, um sicherzustellen, dass für alle gesorgt ist" = "The government should take more responsibility to ensure that everyone is provided for",
  "당사자가 각자의 생계에 책임을 져야 한다" = "People should take more responsibility to provide for themselves",
  "정부가 복지에 더 책임을 져야 한다" = "The government should take more responsibility to ensure that everyone is provided for",
  "Das Geschlecht sollte bei der Bestimmung sozialer Rollen nicht wichtig sein" = "Gender should not be important in determining social roles",
  "Um Gleichheit zu fördern, sollten Frauen bei Entscheidung bezüglich der beruflichen Anstellung gegenüber Männern bevorzugt werden, wenn sie gleich qualifiziert sind" = "To promote equity, women should be prioritized over men for hiring decisions if equally qualified",
  "Wenn Arbeitsplätze knapp sind, sollten Männer mehr Anrecht auf einen Arbeitsplatz haben als Frauen" = "When jobs are scarce, men should have more of a right to a job than women",
  "사회생활에서 역할을 분담할 때 성별이 중요하지 않다" = "Gender should not be important in determining social roles",
  "평등한 채용결정을 위해 지원자들의 능력이 동일한 경우에도 여성이 남성보다 우선적으로 심사되어야 한다" = "To promote equity, women should be prioritized over men for hiring decisions if equally qualified",
  "일자리가 귀할 때에는 여자보다 남자에게 일자리를 우선 부여해야 한다" = "When jobs are scarce, men should have more of a right to a job than women",
  "Ethnische und kulturelle Vielfalt untergräbt die Einheit eines Landes" = "Ethnic and cultural diversity erodes a country's unity",
  "Ethnische und kulturelle Diversität ist von ökonomischem Nutzen und sollte daher akzeptiert werden" = "Ethnic and cultural diversity should be accepted because it is an economic benefit to the country",
  "Ethnische und kulturelle Vielfalt bereichern das Leben der Menschen in unserem Land" = "Ethnic and cultural diversity enriches the lives of people in our country",
  "인종 및 문화의 다양성은 국가의 결속력을 약화시킨다" = "Ethnic and cultural diversity erodes a country's unity",
  "인종 및 문화의 다양성은 나라에 경제적 이득이 되기 때문에 받아들여야 한다" = "Ethnic and cultural diversity should be accepted because it is an economic benefit to the country",
  "인종 및 문화의 다양성은 나라에 경제적 이득이 되기 때문에 받아들여야 한다." = "Ethnic and cultural diversity should be accepted because it is an economic benefit to the country",
  "인종과 문화의 다양성은 시민들의 삶을 향상시킨다" = "Ethnic and cultural diversity enriches the lives of people in our country",
  # job training: labels of the replication data dictionaries (Origin, Record, ...)
  "25" = "25", "36" = "36", "45" = "45", "62" = "62",
  "25세" = "25", "36세" = "36", "45세" = "45", "62세" = "62",
  "Ledig, keine Kinder" = "Single, no children", "Ledig, 1 Kind" = "Single, 1 child",
  "Verheiratet, 1 Kind" = "Married, 1 child", "Verheiratet, 2 Kinder" = "Married, 2 children",
  "미혼, 무자녀" = "Single, no children", "미혼, 자녀 1명" = "Single, 1 child",
  "기혼, 자녀 1명" = "Married, 1 child", "기혼, 자녀 2명" = "Married, 2 children",
  "Männlich" = "Male", "Weiblich" = "Female", "남성" = "Male", "여성" = "Female",
  "Teilzeitanstellung in einem Supermarkt" = "Part-time supermarket employee",
  "Anstellung in einem Kaufhaus" = "Employee in a department store",
  "Sicherheitsdienst" = "Security guard", "Filialleiter*in" = "Store manager", "Filialleiter" = "Store manager",
  "마트 시간제 근무 직원" = "Part-time supermarket employee", "백화점 직원" = "Employee in a department store",
  "경비원" = "Security guard", "가게 매니저" = "Store manager",
  "Keine" = "None", "Diebstahl" = "Theft", "Steuerhinterziehung" = "Tax evasion",
  "전과 기록 없음" = "None", "소액절도" = "Theft", "탈세" = "Tax evasion",
  "Sachsen, DE" = "Saxony, DE", "Hamburg, DE" = "Hamburg, DE", "Bayern, DE" = "Bavaria, DE",
  "Bukarest, RO" = "Bucharest, RO",
  "북한, 함경북도" = "North Hamgyong, DPRK", "함경북도, 북한" = "North Hamgyong, DPRK",
  "대한민국, 부산" = "Busan, ROK", "부산, 대한민국" = "Busan, ROK",
  "대한민국, 경기" = "Gyeonggi, ROK", "경기, 대한민국" = "Gyeonggi, ROK",
  "베트남, 하노이" = "Hanoi, Vietnam", "하노이, 베트남" = "Hanoi, Vietnam",
  # national identity (DE)
  "In Deutschland geboren" = "Born in Germany", "Nicht in Deutschland geboren" = "Not born in Germany",
  "Hat den Großteil des Lebens in Deutschland verbracht" = "Has lived most of life in Germany",
  "Hat den Großteil des Lebens außerhalb Deutschlands verbracht" = "Has lived most of life outside Germany",
  "Hat deutsche Vorfahren" = "Has German ancestors", "Hat keine deutschen Vorfahren" = "Has no German ancestors",
  "Besitzt die deutsche Staatsbürgerschaft" = "German citizen",
  "Besitzt eine andere als die deutsche Staatsbürgerschaft" = "Holds a citizenship other than German",
  "Ist mit deutscher Geschichte und Tradition vertraut" = "Familiar with German history and tradition",
  "Ist kaum mit deutscher Geschichte und Tradition vertraut" = "Hardly familiar with German history and tradition",
  "Hat Respekt vor den politischen Institutionen und Gesetzen Deutschlands" = "Respects Germany's political institutions and laws",
  "Zeigt wenig Beachtung für oder Kenntnis der politischen Institutionen und Gesetze Deutschlands" = "Shows little regard for or knowledge of Germany's political institutions and laws",
  "Spricht deutsch" = "Speaks German", "Spricht kein deutsch" = "Does not speak German",
  "Empfindet sich als deutsche Person" = "Feels German", "Empfindet sich nicht als deutsche Person" = "Does not feel German",
  "Christlich" = "Christian", "Nicht christlich" = "Not Christian",
  # national identity (NK-migrant sample, Korean)
  "한국에서 태어남" = "Born in South Korea", "한국에서 태어나지 않음" = "Not born in South Korea",
  "인생 대부분을 한국에서 보냄" = "Has lived most of life in South Korea",
  "인생 대부분을 한국에서 보내지 않음" = "Has not lived most of life in South Korea",
  "한국계" = "Ethnic Korean", "비한국계" = "Not ethnic Korean",
  "한국 국적 보유" = "South Korean citizen", "시민권이 없는 외국인" = "Foreigner without citizenship",
  "한국의 역사와 전통을 이해함" = "Understands Korea's history and traditions",
  "한국의 역사와 전통에 대해 아는 것이 없음" = "Knows nothing about Korea's history and traditions",
  "한국의 정치 제도와 규범을 존중함" = "Respects Korea's political institutions and norms",
  "한국의 정치 제도와 규범을 전혀 존중하지 않음" = "Does not respect Korea's political institutions and norms at all",
  "한국어를 함" = "Speaks Korean", "한국어를 할 수 없음" = "Cannot speak Korean",
  "한국인임을 느낌" = "Feels Korean", "한국인임을 느끼지 않음" = "Does not feel Korean")

# Attribute levels of one task, per profile (1 = A, 2 = B), keyed by English attribute.
task_levels <- function(d, block, task, natid_de = FALSE) {
  an <- grep(paste0("^", block, "-F-", task, "-[0-9]+$"), names(d), value = TRUE)
  out <- list()
  for (a in an) {
    j <- sub(".*-", "", a)
    nm <- unname(attr_en[trimws(d[[a]])])
    if (natid_de) nm[trimws(d[[a]]) %in% "Herkunft"] <- "birthplace"
    for (p in 1:2) {
      lv <- trimws(d[[paste0(block, "-F-", task, "-", p, "-", j)]])
      lab <- unname(lvl_en[lv])
      bad <- !is.na(lv) & is.na(lab)
      if (any(bad)) stop("unmapped level(s): ", paste(unique(lv[bad]), collapse = " | "))
      for (u in unique(na.omit(nm))) {
        key <- paste0(p, "_", u)
        if (is.null(out[[key]])) out[[key]] <- rep(NA_character_, nrow(d))
        out[[key]][nm %in% u] <- lab[nm %in% u]
      }
    }
    if (any(!is.na(d[[a]]) & is.na(nm))) stop("unmapped attribute in ", a, ": ",
                                              paste(unique(d[[a]][is.na(nm) & !is.na(d[[a]])]), collapse = " | "))
  }
  out
}

# Build one harmonized frame.
build <- function(d, k, item, text_col, choice_col, block, task, natid_de = FALSE, nk_natid = FALSE) {
  reached <- nonempty(d[[choice_col]])
  tl <- task_levels(d, block, task, natid_de)
  if (nk_natid) {  # in the NK natid block, 출생지 is birthplace, not job-training origin
    names(tl) <- sub("birth_origin$", "birthplace", names(tl))
  }
  pick <- ifelse(grepl("A\\s*$", d[[choice_col]]), 1L, ifelse(grepl("B\\s*$", d[[choice_col]]), 2L, NA_integer_))
  ord <- c("democracy", "economy", "welfare", "gender", "diversity",
           "age", "family_status", "sex", "occupation", "criminal_record", "birth_origin",
           "birthplace", "residence", "ethnic_ancestry", "citizenship", "history_tradition",
           "political_institutions", "language", "national_feeling", "religion")
  attrs <- unique(sub("^[12]_", "", names(tl)))
  stopifnot(all(attrs %in% ord))
  attrs <- ord[ord %in% attrs]
  df <- data.frame(id = d$ResponseId,
                   country = if (k == "DE") "DE" else "KR",
                   lang = if (k == "DE") "de" else "ko",
                   text = d[[text_col]],
                   population = unname(c(DE = "german", SK = "south_korean", NK = "nk_migrant")[k]),
                   stringsAsFactors = FALSE)
  df$exclude <- d$.exclude
  df$attn_check_pass <- d$.attn_pass
  df$task <- as.integer(task)
  df$choice <- c("A", "B")[pick]
  for (a in attrs) {
    l1 <- tl[[paste0("1_", a)]]; l2 <- tl[[paste0("2_", a)]]
    df[[paste0("chosen_", a)]] <- ifelse(pick == 1L, l1, l2)
    df[[paste0("other_", a)]] <- ifelse(pick == 1L, l2, l1)
  }
  df <- df[reached, , drop = FALSE]
  if (any(is.na(df$choice))) stop("unparsed choice in ", item)
  df
}

val <- list(DE = read.csv(file.path(base, "replication/data/germany_job_training.csv"))$ResponseId,
            SK = read.csv(file.path(base, "replication/data/korea_job_training.csv"))$ResponseId)
att <- c(DE = "Q114", SK = "Q110", NK = "Q153")
pass <- c(DE = "Stimme überhaupt nicht zu", SK = "매우 동의하지 않음", NK = "매우 동의하지 않음")
dat <- list()
for (k in names(src)) {
  d <- read_q(file.path(base, src[k]))
  d <- d[d$Status %in% c("IP Address", "Spam"), , drop = FALSE]   # drop previews / test runs
  d$.attn_pass <- !is.na(d[[att[k]]]) & d[[att[k]]] == pass[k]
  if (k %in% c("DE", "SK")) {
    d$.exclude <- !(d$ResponseId %in% val[[k]]) | d$Status == "Spam"
  } else {
    yob <- suppressWarnings(as.numeric(d[["Q2"]])); dod <- suppressWarnings(as.numeric(d[["Q4"]]))
    yrs <- dod - yob
    d$.exclude <- d$Status == "Spam" | d[["Q3"]] %in% "북조선 외부" | is.na(yrs) | yrs < 15
  }
  dat[[k]] <- d
}

srcp <- function(k) file.path("_archive/IFES2023", src[k])
common_caveat <- paste(
  "Reached = answered the choice of the referenced task; the open-end was forced (non-empty text equals reached).",
  "Qualtrics previews/test runs dropped; Status=Spam kept with exclude=TRUE.",
  "UserLanguage is 'EN' in every export; lang is set from the fielded instrument.")
excl_dsk <- paste(
  "exclude = not in the Ward & Denney (2025) final validated sample (ResponseIds of replication/data/*_job_training.csv; SI Appendix A: manual + attention + manipulation checks, DE 1,882 / SK 1,763) or Status=Spam.",
  "The validation used the open-ends as manipulation checks, so exclusion may depend on text content; attn_check_pass (embedded item DE Q114 / SK Q110) is an outcome-independent alternative.")
excl_nk <- paste(
  "exclude = CPCS/SI eligibility: born outside North Korea (Q3 '북조선 외부') or under 15 years in North Korea (Q4 defection year - Q2 birth year < 15, or unknown) or Status=Spam (IFES23-Pol-order.qmd).",
  "The project did not filter on the attention item Q153 (commented out); attn_check_pass reports it.",
  "Q154 (last four phone digits) and all other free text dropped.")

specs <- list(
  ifes2023_de_politician = list(k = "DE", text = "Q86", choice = "Q80", block = "C1", task = 6,
    design = "Paired politician CBC, 8 tasks; open-end Q86 after task 6 choice (Q80)"),
  ifes2023_sk_politician = list(k = "SK", text = "Q20", choice = "Q90", block = "C1", task = 5,
    design = "Paired politician CBC, 7 tasks; open-end Q20 after task 5 choice (Q90)"),
  ifes2023_nk_politician = list(k = "NK", text = "Q48", choice = "Q36", block = "C1", task = 5,
    design = "Paired politician CBC, 10 tasks; open-end Q48 after task 5 choice (Q36)"),
  ifes2023_de_jobtraining = list(k = "DE", text = "Q110", choice = "Q106", block = "C3", task = 5,
    design = "Paired job-training CBC, 6 tasks; open-end Q110 after task 5 choice (Q106)"),
  ifes2023_sk_jobtraining = list(k = "SK", text = "Q106", choice = "Q98", block = "C2", task = 4,
    design = "Paired job-training CBC, 7 tasks; open-end Q106 after task 4 choice (Q98)"),
  ifes2023_de_natid = list(k = "DE", text = "Q98", choice = "Q94", block = "C2", task = 5, natid_de = TRUE,
    design = "Paired national-identity CBC, 6 tasks; open-end Q98 after task 5 choice (Q94); religion attribute shown to a random half (respondent level)"),
  ifes2023_nk_natid = list(k = "NK", text = "Q114", choice = "Q106", block = "C2", task = 4, nk_natid = TRUE,
    design = "Paired national-identity CBC, 7 tasks; open-end Q114 after task 4 choice (Q106)"))

`%||%` <- function(a, b) if (is.null(a)) b else a
# unverified notes are kept from the existing manifest (written after ifes2023_verify.R)
for (id in names(specs)) {
  s <- specs[[id]]; d <- dat[[s$k]]
  df <- build(d, s$k, id, s$text, s$choice, s$block, s$task,
              natid_de = isTRUE(s$natid_de), nk_natid = isTRUE(s$nk_natid))
  tcols <- c("task", "choice", grep("^(chosen|other)_", names(df), value = TRUE))
  extra <- ""
  if (id == "ifes2023_de_natid") {
    an <- grep("^C2-F-[0-9]+-[0-9]+$", names(d), value = TRUE)
    rs <- apply(d[an], 1, function(r) any(r %in% "Religion"))
    df$religion_attr_shown <- as.integer(rs[match(df$id, d$ResponseId)])
    df$treatment <- df$religion_attr_shown
    tcols <- c("treatment", "religion_attr_shown", tcols)
    extra <- " treatment = religion attribute (Christian / Not Christian) displayed in all 6 tasks (1) vs withheld (0); derived from the C2-F-k-j attribute-name columns (no separate randomizer variable in the export); final report fn. 1 to Table 4.6 says it was randomly assigned."
    cat("religion shown among reached:\n"); print(table(df$treatment, df$exclude, dnn = c("shown", "excl")))
  }
  nk <- s$k == "NK"
  write_harmonized(df, id, manifest = list(
    source = srcp(s$k), design = s$design, lang = if (s$k == "DE") "de" else "ko",
    treatment_cols = paste(tcols, collapse = ", "),
    human_codes = "none",
    routing = if (nk) "alice_only" else "API-eligible",
    card = "y",
    caveats = paste(common_caveat, if (nk) excl_nk else excl_dsk, extra),
    unverified = if (file.exists(mf <- file.path(OH_MANIFEST, paste0(id, ".json")))) jsonlite::read_json(mf)$unverified %||% "" else ""))
}
