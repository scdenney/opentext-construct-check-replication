# ==============================================================================
# peyton2022_imaginedcontact.R -- harmonized file for card peyton2022_imaginedcontact
# ==============================================================================
# Many Labs 1 imagined-contact study (Klein et al. 2014, Social Psychology
# 45(3): 142-152, doi:10.1027/1864-9335/a000178; replicates Husnu & Crisp 2010).
# The file sits in the replication package of Peyton, Huber & Coppock (2022,
# JEPS), "The generalizability of online experiments conducted during the
# COVID-19 pandemic": they use only the ML1 gain/loss-framing columns of the
# MTurk and Project Implicit samples as a pre-COVID benchmark for their Study 3
# (appendix_section_a.R, lines 333-339) and ship the whole ML1 data file.
# Data: Yale ISPS Data Archive D173, doi:10.60600/YU/JTINWB ("Under same
# license as original source": Harvard Dataverse doi:10.7910/DVN/38UTBF, CC0
# 1.0). The ML1 OSF project (osf.io/wx7ck) is also CC0 1.0.
#
# Run from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave3/peyton2022_imaginedcontact.R
#
# Input: data/external/peyton2022_imaginedcontact/raw/manylabs_asian_disease.csv
#   (ISPS curator's CSV conversion of the authors' manylabs_asian_disease.sav;
#   Yale file id 28689). 6,344 rows = 36 ML1 samples, one row per participant.
# Output: data/external/peyton2022_imaginedcontact/harmonized_peyton2022_imaginedcontact.csv
#
# Text item: imagineddescribe. ML1 materials (OSF xvpu3, Measures_US_Version,
# "Imagined Contact"; page imagineddescribe.html in the codebook zip, OSF
# pqf9r): "Describe as many aspects of the scenario you just imagined as
# possible: (Please do not use the "Enter" key, just type your thoughts in a
# paragraph)." Same wording in both arms, after one minute of imagining.
#
# Blindness: no text value is printed; only counts.
# ==============================================================================

suppressPackageStartupMessages(library(data.table))

raw_file <- "data/external/peyton2022_imaginedcontact/raw/manylabs_asian_disease.csv"
out_csv  <- "data/external/peyton2022_imaginedcontact/harmonized_peyton2022_imaginedcontact.csv"
card_id  <- "peyton2022_imaginedcontact"

d <- fread(raw_file, encoding = "UTF-8", colClasses = "character")
stopifnot(nrow(d) == 6344, !anyDuplicated(d$session_id), all(d$session_id != ""))

# ---- Treatment ----------------------------------------------------------------
# ContactGroup: ML1 SPSS syntax (OSF 7rzan, Syntax.Manylabs.sps, lines 1535-1557
# and the value labels at line 1560): ContactGroup = 1 if the task sequence
# (task_id.3 ... task_id.24) contains "imagineda", else 0; labels 0 'Control
# group', 1 'Contact group'. In the experiment script (manylabs2.expt.xml,
# lines 160-166) the branch "imaginedselect" picks one of imagineda (meet a
# Muslim stranger) and imaginedb (walk in the outdoors). Verified here from
# the task sequence in the file itself:
tk <- grep("^task_id\\.", names(d), value = TRUE)
has_task <- function(x) Reduce(`|`, lapply(tk, function(v) !is.na(d[[v]]) & d[[v]] == x))
ta <- has_task("imagineda"); tb <- has_task("imaginedb")
stopifnot(all(xor(ta, tb)), all(d$ContactGroup[ta] == "1"), all(d$ContactGroup[tb] == "0"))

# ---- Exclusions ----------------------------------------------------------------
# The ML1 syntax drops test sessions before any analysis (lines 9-10: select if
# nativelang2 / imagineddescribe NE 'test'); this file already excludes them.
# The imagined-contact block (lines 1523-1600) applies no filter: the authors'
# t-tests use every row with a ContactGroup. So exclude = FALSE throughout.
exclude <- rep(FALSE, nrow(d))

# ---- Text encoding ----------------------------------------------------------------
# 283 values are not valid UTF-8: charles (Prague) 75, swps (Sopot) 64,
# swpson (Polish online) 142, brasilia 1, unipd 1. In the Polish rows valid
# two-byte letters (e.g. U+0119, U+0105) sit next to lone continuation bytes
# (0x82, 0x9B, 0x84, ...) whose lead byte is missing; in the Czech rows lone
# 0xED, 0xD6, 0xBB, 0xEE bytes occur and no other non-ASCII character. The
# authors' .sav (Yale file 28679) carries the same damage in another form, so
# it predates the curator's conversion. The lost letters cannot be restored
# without reading the answers: each invalid byte becomes U+FFFD (replacement
# character). No other change to the text.
raw_txt <- d$imagineddescribe
bad <- !validUTF8(raw_txt)
cat("Values not valid UTF-8 (repaired with U+FFFD):", sum(bad), "\n")
print(table(site = d$referrer[bad]))
raw_txt[bad] <- iconv(raw_txt[bad], "UTF-8", "UTF-8", sub = "�")
stopifnot(all(validUTF8(raw_txt)))
# Lead decision (17 Nov 2026): answers whose letters were lost before deposit
# are excluded, as the damaged Ziegler church text was in wave 1. The damage
# follows the site (the Czech and Polish samples), not the arm.
exclude <- exclude | bad

# ---- Site, country, language --------------------------------------------------
# Site = referrer (36 samples). Country and language from the ML1 site table
# (OSF cmi6b, Table_S1_-_Detailed_Site_and_Sample_Characteristics.xlsx:
# Location and Study Link, where the international links are named by
# language: mlbrasilia1, mlczech2, mlturkishonline1, mlpoland1,
# mlpolandonline1, mldutch1, mlitalianonline1; mlmalaysia1, mlcanada1 and
# mlengland1 are English versions). Counted check (not displayed): rows with
# common function words of the site language dominate in each non-English site.
site_country <- c(brasilia = "BR", charles = "CZ", help = "MY", ku = "TR",
                  laurier = "CA", msvu = "CA", lse = "GB", swps = "PL",
                  swpson = "PL", tilburg = "NL", unipd = "IT")
site_lang <- c(brasilia = "pt-BR", charles = "cs", ku = "tr", swps = "pl",
               swpson = "pl", tilburg = "nl", unipd = "it")
ref <- d$referrer
country <- ifelse(ref %in% names(site_country), site_country[ref], "US")
lang    <- ifelse(ref %in% names(site_lang), site_lang[ref], "en")
# us_or_international: 1 = international (sum of the 11 non-US samples, 1,365).
stopifnot(all((country != "US") == (d$us_or_international == "1")))

num <- function(v) suppressWarnings(as.numeric(v))
h <- data.frame(
  id          = d$session_id,
  dataset     = card_id,
  country     = unname(country),
  lang        = unname(lang),
  text        = trimws(raw_txt),
  exclude     = exclude,
  # 1 = imagined meeting a Muslim stranger, 0 = imagined walking outdoors
  treatment   = as.integer(d$ContactGroup),
  site        = ref,
  # lab_or_online: 1 = online (sum of the 9 online samples, 3,348; site table)
  online      = as.integer(d$lab_or_online),
  # Independently randomized ML1 factor: currency priming (syntax lines 1410-1436:
  # 1 = money image shown). Task order was randomized, so the prime came before
  # the imagined-contact task for some participants and after it for others.
  money_prime = as.integer(d$MoneyGroup),
  # Closed DV: mean of imaginedexplicit1-4 (intentions to interact with
  # Muslims, 1-9; syntax line 1569). 8 missing.
  imagined_dv = num(d$Imagineddv),
  stringsAsFactors = FALSE
)

n_empty <- sum(is.na(h$text) | h$text == "")
cat("Rows with empty text dropped:", n_empty, "\n")
h <- h[!is.na(h$text) & h$text != "", ]

write.csv(h, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- Checks -------------------------------------------------------------------
cat("Rows written:", nrow(h), "| excluded:", sum(h$exclude), "\n")
cat("Non-excluded rows per arm (1 = Muslim stranger):\n")
print(table(h$treatment[!h$exclude]))
cat("Non-excluded rows per language x arm:\n")
print(table(h$lang[!h$exclude], h$treatment[!h$exclude]))
cat("Rows holding U+FFFD, by site:\n")
print(table(h$site[grepl("�", h$text, fixed = TRUE)]))
r <- read.csv(out_csv, encoding = "UTF-8", as.is = TRUE, na.strings = character(0))
cat("Re-read text identical:", identical(r$text, h$text), "\n")
stopifnot(identical(r$text, h$text))
lines <- readLines(out_csv, encoding = "UTF-8", warn = FALSE)
n_esc <- sum(lengths(regmatches(lines, gregexpr("<U+", lines, fixed = TRUE))))
cat("<U+ escapes in file:", n_esc, "\n")
stopifnot(n_esc == 0)
