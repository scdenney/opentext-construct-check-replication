# Build harmonized_<card>.csv for the first external batch.
# Blind: never prints text values. Run via run.sh (LANG=en_US.UTF-8, stderr captured).
source("analysis/revision/harmonize/external_wave1/inspect.R")  # path changed on recovery (2026-11-17)
suppressMessages({ library(haven); library(readxl) })
EXT <- "data/external"  # path changed on recovery (2026-11-17): inputs, relative to the repository root
OUT <- Sys.getenv("HARMONIZE_OUT", EXT)  # path changed on recovery (2026-11-17): output root (default: write next to raw/, as originally)
SUMMARY <- Sys.getenv("HARMONIZE_SUMMARY", file.path(tempdir(), "harmonize_ext_summary.rds"))  # path changed on recovery (2026-11-17)
summ <- list()

clean_text <- function(x) { x <- trimws(fix_enc(x)); x[is.na(x)] <- ""; x }
join_nonempty <- function(m) apply(m, 1, function(r) paste(r[nchar(r) > 0], collapse = " | "))
lab <- function(x) { x <- as.character(x); x[is.na(x)] <- "NA"; x }

write_h <- function(df, dataset, card, note = "") {
  df$text <- clean_text(df$text)
  n_na <- sum(df$text == "NA")   # literal "NA" answers: read.csv() reads them as missing, so drop as empty
  if (n_na) cat("  dropped", n_na, "literal \"NA\" answers\n")
  df <- df[nchar(df$text) > 0 & df$text != "NA", , drop = FALSE]
  df$dataset <- card
  df$exclude <- as.logical(df$exclude); df$exclude[is.na(df$exclude)] <- FALSE
  df$id <- as.character(df$id)
  stopifnot(!anyNA(df$id), !anyDuplicated(df$id), all(validUTF8(df$text)))
  front <- c("id", "dataset", "country", "lang", "text", "exclude", "treatment")
  df <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
  path <- file.path(OUT, dataset, paste0("harmonized_", card, ".csv"))  # path changed on recovery (2026-11-17)
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  re <- read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  same <- identical(re$text, df$text)
  ucount <- as.integer(system2("grep", c("-c", shQuote("<U+"), shQuote(path)), stdout = TRUE))
  keep <- !df$exclude
  tl <- table(lab(df$treatment[keep]))
  hc <- grep("^human_", names(df), value = TRUE)
  cat(sprintf("%-26s rows=%d excluded=%d identical=%s grepU=%d literalNA=%d cols=%s\n", card, nrow(df), sum(df$exclude),
              same, ucount, sum(df$text == "NA"), paste(names(df), collapse = ",")))
  print(tl)
  summ[[card]] <<- list(card = card, file = file.path("data/external", dataset, basename(path)), n = nrow(df),
                        n_excl = sum(df$exclude), treat = tl, lang = paste(sort(unique(df$lang)), collapse = "/"),
                        country = paste(sort(unique(df$country)), collapse = "/"), human = hc, grepU = ucount,
                        identical = same, note = note, extra = setdiff(names(df), c(front, hc)))
  invisible(df)
}

# ---- Egami (original CSVs; .tab ingests add backslash-escaped quotes) ----
for (s in list(c("egami2022_prison_exp2", "Experiment2_original.csv"), c("egami2022_prison_exp3", "experiment3_original.csv"))) {
  d <- read.csv(file.path(EXT, "egami2022_prison/raw", s[2]), stringsAsFactors = FALSE, encoding = "UTF-8")
  write_h(data.frame(id = paste0(s[1], "_", seq_len(nrow(d))), country = "US", lang = "en", text = d$text,
                     exclude = FALSE, treatment = as.integer(d$treat)), "egami2022_prison", s[1])
}

# ---- Ofosu (original CSV) ----
o <- read.csv(file.path(EXT, "ofosu2024_malawi/raw/mm_respondent_data_original.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
tr <- c("Control" = "control", "Low viability" = "low_viability", "High viability" = "high_viability")[o$tr_video]
write_h(data.frame(id = o$KEY, country = "MW", lang = "en", text = o$q23,
                   exclude = !(o$validdata %in% TRUE) | is.na(tr), treatment = unname(tr)), "ofosu2024_malawi", "ofosu2024_malawi")

# ---- Bisgaard (text from the original .RData; .tab ingests are lossy) ----
for (s in c(1, 3, 4)) {
  e <- new.env(); load(file.path(EXT, sprintf("bisgaard2019_econnews/raw/study%d.RData", s)), envir = e)
  r <- as.data.frame(get(ls(e)[1], e))
  t <- as.data.frame(fread(file.path(EXT, sprintf("bisgaard2019_econnews/raw/study%d.tab", s)), encoding = "UTF-8", showProgress = FALSE))
  stopifnot(identical(as.character(r$id), as.character(t$id)))
  trt <- as.character(r$treatment); trt[trt %in% ""] <- NA
  if (s != 1) trt <- c(neg = 0L, pos = 1L)[trt]
  write_h(data.frame(id = r$id, country = if (s == 1) "US" else "DK", lang = if (s == 1) "en" else "da",
                     text = r$respOpen, exclude = is.na(trt), treatment = unname(trt), gov = r$gov,
                     human_c1raw = r$c1raw, human_c2raw = r$c2raw),
          "bisgaard2019_econnews", sprintf("bisgaard2019_study%d", s))
}

# ---- Gidron ----
g <- as.data.frame(readRDS(file.path(EXT, "gidron2025_polarization/raw/workfile.rds")))
txt <- ifelse(g$treated_culture == 1, g$open_end_immigration_eng, ifelse(g$treated_econ == 1, g$open_end_economics_eng, NA))
write_h(data.frame(id = g$meta_UUID, country = as.character(g$dem_country_code), lang = "en", text = txt, exclude = FALSE,
                   treatment = ifelse(g$treated_culture == 1, 1L, ifelse(g$treated_econ == 1, 0L, NA_integer_))),
        "gidron2025_polarization", "gidron2025_polarization")

# ---- Glazier (original xlsx; headers 'Poverty Cause k') ----
x <- as.data.frame(suppressWarnings(read_excel(file.path(EXT, "glazier2021_selfcoding/raw/self_coding_survey_original.xlsx"), col_types = "text")))
xi <- grep("Poverty Cause", names(x)); stopifnot(length(xi) == 10)
cm <- sapply(xi, function(i) clean_text(x[[i]]))
gm <- sapply(paste0("GEDO", 1:10), function(v) { y <- x[[v]]; y[is.na(y) | y == ""] <- "NA"; y })
gtext <- join_nonempty(cm)
gcode <- vapply(seq_len(nrow(cm)), function(i) paste(gm[i, nchar(cm[i, ]) > 0], collapse = " | "), "")
nso <- rowSums(matrix(gm %in% c("1", "2", "3"), nrow = nrow(gm)))
th <- suppressWarnings(as.integer(as.numeric(x$thematic)))
write_h(data.frame(id = paste0("glazier2021_selfcoding_", seq_len(nrow(x))), country = "US", lang = "en", text = gtext,
                   exclude = nso == 0 | is.na(th), treatment = th, human_GEDO = gcode),
        "glazier2021_selfcoding", "glazier2021_selfcoding")

# ---- Gadarian ----
ga <- readRDS(file.path(EXT, "gadarian2014_anxiety/raw/gadarian.rds"))
write_h(data.frame(id = paste0("gadarian2014_anxiety_", seq_len(nrow(ga))), country = "US", lang = "en",
                   text = ga$open.ended.response, exclude = FALSE, treatment = as.integer(ga$treatment)),
        "gadarian2014_anxiety", "gadarian2014_anxiety")

# ---- Hartnett (original CSV) ----
h <- read.csv(file.path(EXT, "hartnett2022_whytrump/raw/unconditional_support_original.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
hw <- nonempty(h$whytrump); MED <- median(h$margin[hw])
cat("Hartnett median margin (text rows):", MED, " non-excluded:", median(h$margin[hw & !is.na(h$tres11)]), "\n")
write_h(data.frame(id = h$rid, country = "US", lang = "en", text = h$whytrump, exclude = is.na(h$tres11),
                   treatment = h$margin, treatment_binary = as.integer(h$margin > MED),
                   human_tres11 = h$tres11, human_tres22 = h$tres22, human_tres33 = h$tres33),
        "hartnett2022_whytrump", "hartnett2022_whytrump")

# ---- Busby ----
bcodes <- c("badactor_r", "badelite_r", "goodpeople_r", "badactor_n", "badelite_n", "goodpeople_n", "badactor", "badelite", "goodpeople", "populism")
for (s in 1:2) {
  b <- read_dta(file.path(EXT, sprintf("busby2019_populism/raw/study%d.dta", s)))
  tv <- as.numeric(b$TREATMENT)
  trt <- if (s == 1) as.integer(tv) else ifelse(tv == 1, 1L, ifelse(tv == 2, 0L, NA_integer_))
  nocode <- is.na(b$goodpeople) & is.na(b$badactor) & is.na(b$badelite)   # analysis.R comply == 0
  if (s == 2) cat("Busby S2 recomputed comply == deposited comply:", identical(as.numeric(!nocode), as.numeric(b$comply)), "\n")
  df <- data.frame(id = as.character(b$QID), country = "US", lang = "en", text = as.character(b$PROB_why),
                   exclude = nocode | is.na(trt), treatment = trt)
  for (v in bcodes) df[[paste0("human_", v)]] <- as.numeric(b[[v]])
  write_h(df, "busby2019_populism", sprintf("busby2019_study%d", s))
}

# ---- Janssen ----
R <- file.path(EXT, "janssen2026_transgressions/raw/Replication")
man <- as.data.frame(suppressWarnings(read_excel(file.path(R, "Open answer analysis/Data/20250717_manual_coding_sample.xlsx"), col_types = "text")))
oc <- c("OpenRecipProtest", "OpenRecipProtest2", "OpenRecipVote", "OpenRecipVote2", "OpenRecipPressRight", "OpenRecipPressLeft", "OpenRecipPress2")
for (s in list(c("janssen2026_england", "England/Posted/ProlificEngland.dta", "GB"), c("janssen2026_us", "US/Posted/ProlificUS.dta", "US"))) {
  d <- read_dta(file.path(R, s[2]))
  m <- sapply(oc, function(v) clean_text(d[[v]]))
  # issue from the closed reciprocity item answered (each respondent saw one issue)
  cl <- list(protest = intersect(c("RecipProtest_original", "recipProtest"), names(d))[1],
             vote = intersect(c("RecipVote_original", "recipVote"), names(d))[1],
             press_right = "RecipPressRight", press_left = "RecipPressLeft")
  im <- sapply(cl, function(v) !is.na(as.numeric(d[[v]])))
  issue <- apply(im, 1, function(r) if (sum(r) == 1) names(cl)[r] else NA_character_)
  # cross-check against the text column used
  tcol <- apply(m, 1, function(r) if (any(nchar(r) > 0)) oc[nchar(r) > 0][1] else NA_character_)
  tiss <- c(OpenRecipProtest = "protest", OpenRecipProtest2 = "protest", OpenRecipVote = "vote", OpenRecipVote2 = "vote",
            OpenRecipPressRight = "press_right", OpenRecipPressLeft = "press_left", OpenRecipPress2 = "press")[tcol]
  ok <- !is.na(tiss)
  cat(s[1], "issue NA among text rows:", sum(is.na(issue) & ok), " multi-closed:", sum(rowSums(im) > 1), " agree with text col:",
      sum(issue[ok] == tiss[ok] | (tiss[ok] == "press" & issue[ok] %in% c("press_right", "press_left")), na.rm = TRUE), "of", sum(ok),
      " rows with >1 text col:", sum(rowSums(m != "") > 1), "\n")
  mc <- man$Manual_code[match(d$ResponseId, man$ResponseId)]
  write_h(data.frame(id = d$ResponseId, country = s[3], lang = "en", text = join_nonempty(m), exclude = is.na(as.numeric(d$treatment)),
                     treatment = as.integer(as.numeric(d$treatment)), prime = as.integer(as.numeric(d$prime)), issue = issue,
                     human_manual_code = mc),
          "janssen2026_transgressions", s[1])
}

# ---- Dur (sheet VALUES; arm labels verified against sheet LABELS) ----
D <- file.path(EXT, "dur2026_tradeconflict/raw/Data")
for (s in list(c("dur2026_china", "ORD-952027-W1G7_Final_2024-08-21.xlsx", "RESPID", "CN", "zh-Hans"),
               c("dur2026_us", "ORD-1288355-G1M5_DD_DEF_2025-04-04.xlsx", "RECORD", "US", "en"))) {
  v <- as.data.frame(suppressWarnings(read_excel(file.path(D, s[2]), sheet = "VALUES", col_types = "text")))
  a <- as.integer(v$LF_EXP1)
  trt <- if (s[4] == "CN") c("rival", "rival", "rival", "rival", "home", "home", "control")[a]
         else c("control", "adversary", "adversary", "adversary", "adversary", "home", "home")[a]
  write_h(data.frame(id = v[[s[3]]], country = s[4], lang = s[5], text = v$QOPEN, exclude = FALSE, treatment = trt, LF_EXP1 = a),
          "dur2026_tradeconflict", s[1])
}

# ---- Ziegler ----
e <- new.env(); load(file.path(EXT, "ziegler2022_openended/raw/kaneData.rda"), envir = e); k <- as.data.frame(e$kaneData)
write_h(data.frame(id = paste0("ziegler2022_kane_", seq_len(nrow(k))), country = "US", lang = "en", text = as.character(k$openResponse),
                   exclude = FALSE, treatment = tolower(as.character(k$Conditions)),
                   human_codeCorrect = k$codeCorrect, human_correct = k$correct), "ziegler2022_openended", "ziegler2022_kane")
e <- new.env(); load(file.path(EXT, "ziegler2022_openended/raw/zieglerData.rda"), envir = e); z <- as.data.frame(e$zieglerData)
tv <- fix_enc(z$textViewed)
topic <- ifelse(grepl("80 a", tv), "control", ifelse(grepl("terroris", tv), "conflict", ifelse(grepl("humanos", tv), "human_rights",
         ifelse(grepl("econom", tv), "economy", ifelse(grepl("duca", tv), "socio_political", NA)))))
stopifnot(!anyNA(topic), length(unique(paste(topic, z$Country))) == 10)
rk <- cbind(conflict = z$rank_Conflict_position, socio_political = z$rank_SocioPolitic_position, human_rights = z$rank_HumanRight_position)
# position 4 = most important (Concordant == 1 iff viewed topic has position 4); economy is the unlisted 4th issue
mii <- apply(rk, 1, function(r) if (any(r == 4)) colnames(rk)[r == 4][1] else "economy")
cat("Ziegler church: Concordant==1 & topic==mii:", sum(z$Concordant == "1" & topic == mii), " Concordant==1 & topic!=mii:", sum(z$Concordant == "1" & topic != mii),
    " Concordant==0 & topic==mii:", sum(z$Concordant == "0" & topic == mii), "\n")
# strict Latin-1 (byte-preserving) for the church text: the bytes are mangled UTF-8 upstream, keep them reversible
vc <- as.character(z$validityCheck); vb <- !validUTF8(vc); vc[vb] <- iconv(vc[vb], "latin1", "UTF-8")
write_h(data.frame(id = paste0("ziegler2022_church_", seq_len(nrow(z))), country = c(Brazil = "BR", Mexico = "MX")[z$Country],
                   lang = c(Brazil = "pt", Mexico = "es")[z$Country], text = vc, exclude = FALSE,
                   treatment = topic, mii_pretreat = mii, Concordant = as.integer(as.character(z$Concordant)),
                   rank_Conflict_position = z$rank_Conflict_position, rank_SocioPolitic_position = z$rank_SocioPolitic_position,
                   rank_HumanRight_position = z$rank_HumanRight_position),
        "ziegler2022_openended", "ziegler2022_church")

saveRDS(summ, SUMMARY)
cat("DONE", length(summ), "cards\n")
