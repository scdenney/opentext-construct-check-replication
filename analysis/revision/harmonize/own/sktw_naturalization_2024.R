# Recovered 2026-11-17 from an inline `Rscript -e` in the October session's main thread
# (transcript ac14b90b-...jsonl, Bash tool call at 2026-10-28T21:20:11Z). The R code below is verbatim
# except the marked path lines. In that call the code ran after `mv` of the earlier, C-locale-corrupted file.
# Run from the repository root: LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/own/sktw_naturalization_2024.R
x <- read.csv("replication/data/citizenship_conjoint/corpus.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
h <- data.frame(id = x$ResponseId, dataset = "sktw_naturalization_2024", country = x$country,
                lang = ifelse(x$country == "South Korea", "ko", "zh-Hant"),
                treatment = x$cue_shown, random_text_flag = x$RandomTextFlag, text = x$open.text,
                stringsAsFactors = FALSE)
OUT <- Sys.getenv("HARMONIZE_OUT", "data/harmonized")  # path changed on recovery (2026-11-17)
write.csv(h, file.path(OUT, "sktw_naturalization_2024.csv"), row.names = FALSE, fileEncoding = "UTF-8")  # path changed on recovery (2026-11-17)
y <- read.csv(file.path(OUT, "sktw_naturalization_2024.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)  # path changed on recovery (2026-11-17)
cat("identical text:", identical(enc2utf8(y$text), enc2utf8(h$text)), " mean chars:", mean(nchar(y$text)), "\n")
cat("mean chars by lang:", tapply(nchar(y$text), y$lang, mean), "\n")
