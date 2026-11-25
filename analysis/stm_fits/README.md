# Saved topic-model fits

Copy of the fitted structural topic models and topic choices in `data/jobs/stm/`
(that folder is ignored by git). `<id>.stm.rds` is the fit without a covariate,
`<id>.stmcov.rds` the fit with the randomized arms as a prevalence covariate.
Refitting does not reproduce across machines, so exact results need these files.
SHA-256 hashes are in `revision/locks/*.stm.k1.json` and `*.stmcov.k1.json`.
The fits hold vocabulary and topic proportions, no response text. The labeling
requests (`label_requests*.jsonl`) contain response excerpts and are not included.
