# Recovered 2026-11-17 from an inline `Rscript -e` in agent a5416643704dd1ce6 (OSF datasets), Bash tool call at
# 2026-10-28T21:31:52Z. Code verbatim (one line). It wrote the ID list that osf/harmonize.py takes as its 2nd argument.
# Usage, from the repository root:
#   LANG=en_US.UTF-8 Rscript analysis/revision/harmonize/external_wave2/osf/grat_clean_ids.R \
#     data/external/coles2026_gratitude/raw/GlobalGratitude_Final_Cleaned.Rds <out>/grat_clean_ids.txt
a <- commandArgs(TRUE); d <- readRDS(a[1]); cat(class(d), dim(d), "\n"); cat(intersect(names(d), c("ResponseId","condition","lab","UserLanguage","exclusion","excluded")), "\n"); writeLines(as.character(d$ResponseId), a[2]); print(table(d$condition, useNA="ifany"))
