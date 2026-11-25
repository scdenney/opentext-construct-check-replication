# opencheck pipeline: the seven steps of the skill, from start to finish.
#
# With `data_path <- NULL` the script generates a small synthetic study and
# uses a mock backend (a keyword rule) in place of a language model, so it
# runs with no data and no model:
#
#   Rscript pipeline.R
#
# For a real study, copy this file into the project, change the settings
# below, and run it one section at a time with the researcher: each step ends
# with something the researcher has to confirm before the next one starts.
# The file is installed with the package at
# system.file("skills", "opencheck", "reference", "pipeline.R", package = "opencheck").

library(opencheck)

# ---- Settings: change these ------------------------------------------------

data_path   <- NULL      # .csv or .rds, one row per respondent (or task);
                         # NULL runs the synthetic example
id_col      <- "id"      # unique id
text_col    <- "text"    # the open-text answer
treat_col   <- "treat"   # the randomized treatment
strata_cols <- "region"  # blocking variables of the randomization (not the
                         # treatment); NULL if there were none
lang        <- "en"      # language code of the answers ("ko", "zh", "de", ...)
tokenizer   <- NULL      # Korean: oc_tokenizer_kiwi() (needs Python + kiwipiepy)

# Contrasts on the card: treated and control values of `treat_col`. The first
# one is the primary contrast. A multi-arm design lists one entry per contrast.
contrasts <- list(primary = c(treated = "1", control = "0"))

work_dir    <- "opencheck_run"  # study card, registry, split, lock, caches,
                                # access log and results are written here
card_path   <- file.path(work_dir, "study_card.yaml")
registry    <- file.path(work_dir, "registry")
split_path  <- file.path(work_dir, "split.json")
lock_path   <- file.path(work_dir, "lock.json")
access_log  <- file.path(work_dir, "access_log.jsonl")
cache_try   <- file.path(work_dir, "cache_discovery.jsonl")
cache_est   <- file.path(work_dir, "cache_estimation.jsonl")
results_path <- file.path(work_dir, "results.csv")

p_discovery <- 0.3       # share of each stratum in the discovery fold
split_seed  <- 20261119L # drawn once; never change it after the split exists
K           <- 4L        # topics; NULL chooses K over 4-12 (slower)
n_try       <- 60L       # discovery answers used to try the questions
n_perm      <- 2000L     # permutations for the randomization p-value

backend_choice <- "mock" # "mock": keyword rule, no model (synthetic example)
                         # "local": a model served by Ollama (see step 0)
local_url   <- "http://127.0.0.1:11434/v1"
local_model <- NULL      # NULL: the local tier's primary model (gemma4:e4b)

dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Synthetic example -----------------------------------------------------
# Several hundred respondents, randomized within two regions. The treatment
# (a cue about the applicant's community activities) raises mentions of
# community involvement from 20 to 45 percent; mentions of work and income
# (the placebo) stay at 30 percent in both arms.

make_synthetic <- function(n = 600L, seed = 7L) {
  set.seed(seed)
  region <- rep(c("north", "south"), length.out = n)
  treat <- integer(n)
  for (r in unique(region)) {
    i <- which(region == r)
    treat[i] <- sample(rep(0:1, length.out = length(i)))
  }
  civic <- c("volunteers at the food bank", "helps organize the neighborhood festival",
             "takes part in community events", "coaches a youth soccer team",
             "joined the local residents council", "helps elderly neighbors")
  work <- c("has a steady job", "pays taxes", "runs a small business",
            "earns a good salary", "has useful work skills")
  other <- c("seems honest", "has lived here for ten years",
             "speaks the language well", "has family in the country",
             "looks friendly", "has a clean record")
  says_civic <- stats::runif(n) < ifelse(treat == 1, 0.45, 0.20)
  says_work <- stats::runif(n) < 0.30
  text <- vapply(seq_len(n), function(i) {
    parts <- c(if (says_civic[i]) sample(civic, 1),
               if (says_work[i]) sample(work, 1),
               if (!says_civic[i] && !says_work[i] || stats::runif(1) < 0.3) sample(other, 1))
    paste0(sample(c("She", "He", "This person"), 1), " ",
           paste(parts, collapse = " and "), ".")
  }, "")
  data.frame(id = sprintf("R%04d", seq_len(n)), region = region, treat = treat,
             text = text, stringsAsFactors = FALSE)
}

# The mock backend's keyword rule, one pattern per construct on the card.
mock_rules <- c(community_involvement = "volunteer|festival|community|coach|council|neighbor",
                work_and_income = "job|tax|business|salary|work")

mock_backend <- function() {
  oc_backend_mock(function(messages) {
    system <- messages[[1]]$content
    hit <- names(mock_rules)[vapply(names(mock_rules), function(nm) {
      grepl(paste0(nm, ":"), system, fixed = TRUE)
    }, logical(1))]
    if (length(hit) == 1) {
      answer <- strsplit(messages[[2]]$content, "\n\n", fixed = TRUE)[[1]][1]
      yes <- grepl(mock_rules[[hit]], answer, ignore.case = TRUE)
    } else {
      yes <- TRUE  # any other request, e.g. the setup check in step 0
    }
    list(content = if (yes) "yes" else "no",
         logprobs = data.frame(token = if (yes) c("yes", "no") else c("no", "yes"),
                               logprob = log(c(0.95, 0.05))))
  }, id = "mock-keyword-rule")
}

# ---- Step 0. Check the setup ------------------------------------------------
# Rule 7: text stays on the researcher's machine. The default model is a
# local one; a commercial API only if consent and ethics approval allow it.

if (backend_choice == "mock") {
  backend <- mock_backend()
} else if (backend_choice == "local") {
  # ---- Switching to a local model -------------------------------------------
  # oc_profile("local") recommends Gemma 4 E4B served by Ollama; it returns
  # the model names and the backend call as text, so the backend is built
  # here with oc_backend_openai(). Start the server first (`ollama serve`,
  # `ollama pull gemma4:e4b`). Any OpenAI-compatible endpoint works the same
  # way (vLLM, llama.cpp server); for a commercial API add `api_key_env`.
  #
  # Gemma 4 thinks before it answers unless told not to; with max_tokens = 5
  # the answer is then empty. `reasoning_effort = "none"` turns thinking off
  # on Ollama's OpenAI-compatible endpoint (on vLLM, use
  # `chat_template_kwargs = list(enable_thinking = FALSE)` instead). The test
  # request below shows whether the answer is a plain yes or no.
  #
  # Without first-token log-probabilities: labels still come from the
  # answer text ("yes" or "no"), so the measure and every estimate below are
  # unchanged, but the p_yes and p_valid columns are NA, and "valid" only
  # says whether the answer was yes or no. An endpoint that rejects the
  # request for log-probabilities with an HTTP error cannot be used through
  # oc_backend_openai().
  prof <- oc_profile("local")
  if (is.null(local_model)) local_model <- prof$models[["primary"]]
  backend <- oc_backend_openai(local_url, local_model, temperature = 0,
                               seed = 42L, max_tokens = 5L,
                               extra = list(reasoning_effort = "none"))
} else {
  stop("backend_choice must be \"mock\" or \"local\".")
}

# One test request before going on.
probe <- backend$call(list(
  list(role = "system", content = "Answer yes or no. Write only yes or no."),
  list(role = "user", content = "Is 4 larger than 2? Answer yes or no:")))
has_logprobs <- !is.null(probe$logprobs) && nrow(probe$logprobs) > 0
cat("Model:", backend$id, "| test answer:", shQuote(probe$content),
    "| first-token log-probabilities:", if (has_logprobs) "yes" else "no", "\n")
if (!grepl("^[[:space:][:punct:]]*(yes|no)\\b", tolower(probe$content))) {
  stop("The test request did not return a plain yes or no (got ",
       shQuote(probe$content), "). If the model is thinking first, turn ",
       "thinking off (see the block above).")
}
if (!has_logprobs) {
  message("This endpoint returns no log-probabilities: the yes/no answers are ",
          "used as hard labels and p_yes stays empty.")
}

if (is.null(data_path)) {
  dat <- make_synthetic()
} else if (grepl("\\.rds$", data_path, ignore.case = TRUE)) {
  dat <- readRDS(data_path)
} else {
  dat <- utils::read.csv(data_path, stringsAsFactors = FALSE, encoding = "UTF-8")
}
needed <- c(id_col, text_col, treat_col, strata_cols)
if (!all(needed %in% names(dat))) {
  stop("Columns not found in the data: ",
       paste(setdiff(needed, names(dat)), collapse = ", "))
}

# ---- Step 1. Read the design and draft the study card ------------------------
# Rule 1: the card (contrast, target, expected direction, placebos) is written
# and registered before any answer is read. Rule 8: the target comes from the
# researcher's theory, not from the answers.

template <- system.file("templates", "study_card.yaml", package = "opencheck")
if (!file.exists(card_path)) {
  if (!is.null(data_path)) {
    file.copy(template, card_path)
    stop("A blank study card was copied to ", card_path, ". Fill it in from ",
         "the questionnaire and the pre-analysis plan or paper, then run again.")
  }
  card <- yaml::read_yaml(template)
  card$id <- "synthetic_example"
  card$title <- "Synthetic example: a community cue and reasons for support"
  card$source <- list(citation = "Synthetic data generated by pipeline.R",
                      doi = "none", data = "generated in the script")
  card$languages <- lang
  card$context <- paste("Respondents read a short profile of an immigrant",
                        "applying for citizenship and explained in one sentence",
                        "why they would support the application. Some profiles",
                        "mentioned the applicant's community activities.")
  card$id_column <- id_col
  card$text <- list(column = text_col,
                    prompt = "Why would you support this application?",
                    position = "Directly after the profile")
  card$treatment <- list(variable = treat_col,
                         contrast = "community cue shown (1) vs not shown (0)",
                         level = "respondent",
                         notes = "Randomized within region, half to each arm")
  card$constructs <- list(
    target = list(list(
      name = "community_involvement",
      definition = paste("The answer mentions the applicant taking part in",
                         "community life: volunteering, helping neighbors,",
                         "local events or associations."),
      source = "Design of the synthetic example",
      expected_direction = "increase")),
    placebo = list(list(
      name = "work_and_income",
      definition = "The answer mentions the applicant's job, income, business or taxes.",
      rationale = "The cue says nothing about work or income.")))
  card$outcomes <- list(closed_ended = "none in the synthetic example")
  card$provenance <- list(written_before_reading_text = TRUE,
                          contamination = "none", author = "pipeline.R",
                          date = format(Sys.Date()))
  yaml::write_yaml(card, card_path)
}
card <- oc_study_read(card_path)             # validates the card
oc_study_register(card_path, registry)        # hash and copy; logged
# A later change to the card (before step 5 only) is registered as an
# amendment with its reason:
#   oc_study_register(card_path, registry, amend = "clarified the target definition")

# ---- Step 2. Draw the split --------------------------------------------------
# Rule 2: one split, seeded and stratified, written to disk and never redrawn.
# If the manifest exists, it is read, not drawn again.

if (!file.exists(split_path)) {
  oc_split_write(oc_split(dat, id = id_col, strata = c(treat_col, strata_cols),
                          p_discovery = p_discovery, seed = split_seed),
                 split_path)
}
s <- oc_split_read(split_path)
print(s)

# Arm sizes in the estimation fold, from the assignments and the treatment
# column only (no answer is read).
e_ids <- s$id[s$split == 1 & s$fold == "E"]
e_arm <- as.character(dat[[treat_col]][match(e_ids, as.character(dat[[id_col]]))])
for (cn in names(contrasts)) {
  n_arm <- vapply(contrasts[[cn]], function(v) sum(e_arm == v), 0)
  cat("Estimation fold, contrast", cn, ":",
      paste(names(n_arm), n_arm, sep = " = ", collapse = ", "), "\n")
  if (min(n_arm) < 200) {
    message("The smaller arm of contrast ", cn, " has fewer than 200 ",
            "respondents in the estimation fold: the check will have little power.")
  }
}

# ---- Step 3. Explore the discovery fold ----------------------------------------
# Rule 3: before the lock exists, text is read through oc_discovery_data()
# only. Rule 4: the topic model is fit to the discovery fold and takes no
# treatment variable.

d <- oc_discovery_data(dat, s, log = access_log, purpose = "topic model and question trials")
fit <- oc_topics_fit(d, s, text = text_col, lang = lang, K = K, tokenizer = tokenizer)
print(fit)
# To keep the topic model as a comparison measure, save it now and add it to
# the lock in step 5:
#   oc_topics_save(fit, file.path(work_dir, "topics.rds"))

# ---- Step 4. Write the questions and try them on the discovery fold ------------
# Rule 3 again: the trial runs on discovery-fold answers. One yes/no question
# per target and placebo, built from the card's definitions; the card is the
# question's source file, so it must be frozen in step 5.

constructs <- c(
  lapply(card$constructs$target, function(x) {
    if (is.null(x$expected_direction)) stop("Target ", x$name, " has no expected_direction on the card.")
    c(x, role = "target")
  }),
  lapply(card$constructs$placebo, function(x) {
    c(x, role = "placebo", expected_direction = "no increase")
  }))
names(constructs) <- vapply(constructs, `[[`, "", "name")
context <- if (is.null(card$context)) "" else card$context
prompts <- lapply(constructs, oc_prompt_presence, context = context,
                  source = card_path)

set.seed(1)
d_try <- d[sample(nrow(d), min(n_try, nrow(d))), , drop = FALSE]
for (nm in names(prompts)) {
  r <- oc_classify(d_try, prompts[[nm]], backend, cache_try, id = id_col,
                   text = text_col, progress = FALSE)
  cat(sprintf("\n%s (%s): valid %.0f%%, yes %.0f%% of %d discovery answers\n",
              nm, constructs[[nm]]$role, 100 * mean(r$valid),
              100 * mean(r$label == "yes", na.rm = TRUE), nrow(r)))
  txt <- d_try[[text_col]][match(r$id, as.character(d_try[[id_col]]))]
  for (lab in c("yes", "no")) {
    cat("  ", lab, ":\n", paste0("    \"", utils::head(txt[r$label %in% lab], 3), "\"\n"),
        sep = "")
  }
}
# If the model misreads a construct: revise its definition on the card,
# register the amendment (step 1) and try again.

# ---- Step 5. Freeze ------------------------------------------------------------
# Rule 5: the card (source of the questions), the exact prompt texts, the
# model and its decoding settings go into the lockfile. A lock is written
# once; freezing different content to the same path is an error.

lock <- oc_freeze(lock_path, s,
                  components = c(study_card = card_path),
                  # add topics = file.path(work_dir, "topics.rds") if saved
                  models = list(id = backend$id, base_url = backend$base_url),
                  decoding = backend$settings,
                  prompts = prompts,
                  notes = paste("First-token log-probabilities returned:",
                                has_logprobs))

# ---- Step 6. Estimate on the estimation fold -----------------------------------
# Rule 5: the estimation fold opens only with the lock, and oc_classify()
# refuses any question that is not frozen in it. Rule 6: every target and
# every placebo under every contrast.

e <- oc_estimation_data(dat, s, lock_path, log = access_log,
                        purpose = "classify and estimate")
labels <- lapply(prompts, function(p) {
  oc_classify(e, p, backend, cache_est, id = id_col, text = text_col,
              lock = lock_path, progress = FALSE)
})

block <- if (length(strata_cols)) do.call(paste, c(e[strata_cols], sep = "|")) else NULL
rows <- list()
for (cn in names(contrasts)) {
  arm <- as.character(e[[treat_col]])
  in_c <- arm %in% contrasts[[cn]]
  tr <- as.integer(arm[in_c] == contrasts[[cn]][["treated"]])
  for (nm in names(labels)) {
    r <- labels[[nm]]
    # The measure: 1 if the model answered yes, 0 if no, NA if neither.
    y <- ifelse(r$valid, as.integer(r$label == "yes"), NA_integer_)[in_c]
    est <- oc_effect(y, tr, strata = block[in_c], n_perm = n_perm, seed = 42L)
    rows[[length(rows) + 1]] <- data.frame(
      contrast = cn, construct = nm, role = constructs[[nm]]$role,
      expected = constructs[[nm]]$expected_direction,
      estimate = est$estimate, se = est$se, ci_low = est$ci_low,
      ci_high = est$ci_high, p_ri = est$p_ri, n_treated = est$n1,
      n_control = est$n0, control_share = mean(y[tr == 0], na.rm = TRUE),
      stringsAsFactors = FALSE)
  }
}
results <- do.call(rbind, rows)
utils::write.csv(results, results_path, row.names = FALSE)

# ---- Step 7. Report ------------------------------------------------------------
# Rule 6: the table lists every construct that was specified. Anything done
# after the estimation fold was opened is exploratory.

cat("\nResults (estimation fold)\n")
shown <- results
num <- c("estimate", "se", "ci_low", "ci_high", "control_share")
shown[num] <- lapply(shown[num], round, 3)
shown$p_ri <- signif(shown$p_ri, 2)
old_width <- options(width = 200)
print(shown, row.names = FALSE)
options(old_width)

reproduce <- c(card_path, list.files(registry, full.names = TRUE), split_path,
               lock_path, access_log, cache_est, results_path)
cat("\nFiles that reproduce these results:\n")
cat(paste0("  ", reproduce, collapse = "\n"), "\n")
