#' Tokenize open-text answers for a topic model
#'
#' Korean (`"ko"`) is split into content words by `tokenizer` (for example
#' [oc_tokenizer_kiwi()]); without one it falls back to quanteda word
#' segmentation with a warning, keeping words of two or more characters.
#' Chinese and Japanese (`"zh"`, `"zh-Hans"`, `"zh-Hant"`, `"ja"`, ...) use
#' quanteda word segmentation and keep words of two or more characters. Other
#' languages are lower-cased, stripped of punctuation, numbers and symbols, and
#' of the Snowball stopwords of that language plus the English ones.
#'
#' @param text Character vector of answers.
#' @param lang One language code, or a vector as long as `text`.
#' @param stopwords Extra words to drop, in every language.
#' @param tokenizer Optional function for Korean text: takes a character
#'   vector and returns a list of character vectors of the same length.
#' @return A quanteda `tokens` object with one document per answer.
#' @export
oc_tokens <- function(text, lang = "en", stopwords = NULL, tokenizer = NULL) {
  need_pkg("quanteda")
  need_pkg("stopwords")
  text <- enc2utf8(as.character(text))
  text[is.na(text)] <- ""
  lang <- as.character(lang)
  if (length(lang) == 1L) lang <- rep(lang, length(text))
  if (length(lang) != length(text)) {
    stop("`lang` must be one code or a vector as long as `text`.",
         call. = FALSE)
  }
  if (anyNA(lang)) stop("`lang` has missing values.", call. = FALSE)
  if (!is.null(tokenizer) && !is.function(tokenizer)) {
    stop("`tokenizer` must be a function.", call. = FALSE)
  }
  extra <- enc2utf8(as.character(stopwords %||% character(0)))
  base <- sub("[-_].*$", "", lang)
  toks <- vector("list", length(text))
  ko <- base == "ko"
  cjk <- base %in% c("zh", "ja")
  if (any(ko)) {
    if (is.null(tokenizer)) {
      warning("No Korean tokenizer given; using quanteda word segmentation. ",
              "Pass `tokenizer = oc_tokenizer_kiwi()` for content words.",
              call. = FALSE)
      toks[ko] <- segment_words(text[ko])
    } else {
      tk <- tokenizer(text[ko])
      if (!is.list(tk) || length(tk) != sum(ko)) {
        stop("The tokenizer must return a list with one character vector ",
             "per answer.", call. = FALSE)
      }
      toks[ko] <- lapply(tk, function(w) enc2utf8(as.character(unlist(w))))
    }
  }
  if (any(cjk)) toks[cjk] <- segment_words(text[cjk])
  for (l in unique(lang[!ko & !cjk])) {
    i <- which(lang == l)
    tw <- quanteda::tokens(quanteda::char_tolower(text[i]), remove_punct = TRUE,
                           remove_numbers = TRUE, remove_symbols = TRUE)
    drop <- unique(c(snowball_stopwords(l), stopwords::stopwords("en"), extra))
    toks[i] <- as.list(quanteda::tokens_remove(tw, drop))
  }
  toks <- lapply(toks, function(x) {
    x <- as.character(x %||% character(0))
    x[!x %in% extra]
  })
  names(toks) <- paste0("text", seq_along(toks))
  quanteda::as.tokens(toks)
}

#' A Korean tokenizer that calls kiwipiepy
#'
#' Returns a function for the `tokenizer` argument of [oc_tokens()],
#' [oc_topics_fit()] and [oc_topics_project()]. It runs the bundled script
#' `system.file("python", "oc_kiwi_tokens.py", package = "opencheck")`, which
#' keeps nouns, verbs, adjectives, determiners, adverbs and roots (Kiwi tags
#' NNG, NNP, VV, VA, MM, MAG, XR) longer than one character.
#'
#' @param python Python executable with `kiwipiepy` installed.
#' @return A function taking a character vector and returning a list of
#'   character vectors.
#' @export
oc_tokenizer_kiwi <- function(python = "python3") {
  if (!nzchar(Sys.which(python))) {
    stop("Python executable not found: ", python, call. = FALSE)
  }
  ver <- suppressWarnings(system2(
    python, c("-c", shQuote("import kiwipiepy, sys; sys.stdout.write(kiwipiepy.__version__)")),
    stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(ver, "status")) || !length(ver)) {
    stop("kiwipiepy is not installed for ", python, ". Install it with `",
         python, " -m pip install kiwipiepy`, or pass another `python`.",
         call. = FALSE)
  }
  script <- system.file("python", "oc_kiwi_tokens.py", package = "opencheck")
  if (!nzchar(script)) stop("oc_kiwi_tokens.py is missing from the package.",
                            call. = FALSE)
  fun <- function(text) {
    text <- enc2utf8(as.character(text))
    text[is.na(text)] <- ""
    inp <- tempfile(fileext = ".json")
    out <- tempfile(fileext = ".json")
    err <- tempfile(fileext = ".txt")
    on.exit(unlink(c(inp, out, err)))
    write_utf8(jsonlite::toJSON(text), inp)
    status <- system2(python, shQuote(c(script, inp, out)), stdout = FALSE,
                      stderr = err)
    if (!identical(as.integer(status), 0L) || !file.exists(out)) {
      msg <- if (file.exists(err)) paste(utils::tail(read_lines_utf8(err), 3),
                                         collapse = "\n") else ""
      stop("The kiwipiepy tokenizer failed: ", msg, call. = FALSE)
    }
    res <- jsonlite::fromJSON(paste(read_lines_utf8(out), collapse = "\n"),
                              simplifyVector = FALSE)
    lapply(res, function(x) as.character(unlist(x)))
  }
  attr(fun, "oc_tokenizer") <- paste0("kiwipiepy ", ver[1],
                                      " (oc_kiwi_tokens.py)")
  fun
}

#' Fit a structural topic model on the discovery fold
#'
#' A topic model shows what respondents write about, whether the target
#' construct appears at all, and whether a placebo is missing. It is for
#' exploration; the frozen yes/no question is the measure. Two rules hold:
#' the model is fit on discovery-fold answers only (data with any other id
#' are refused), and treatment is never a covariate, since fitting
#' treatment-dependent topic prevalence on answers that are later tested
#' inflates false positives.
#'
#' Answers are tokenized with [oc_tokens()] and words in fewer than
#' `min_docfreq` answers are dropped. Without `K`, the number of topics is
#' chosen over `K_grid` with [stm::searchK()] (held-out seed `seed`) as the
#' largest sum of standardized semantic coherence and exclusivity. The model
#' uses spectral initialization, at most 75 EM iterations, and seed `seed`.
#'
#' @param discovery Discovery-fold data (from [oc_discovery_data()]).
#' @param split The `oc_split` (or manifest path) the data come from. The ids
#'   are checked against the discovery fold of split 1; for a two-fold split
#'   all ids must lie in one fold.
#' @param text Name of the text column.
#' @param lang Language code(s) as in [oc_tokens()]. `NULL` uses a column
#'   named `lang` if present, else English.
#' @param K Number of topics, or `NULL` to choose it over `K_grid`.
#' @param K_grid Candidate numbers of topics.
#' @param stopwords,tokenizer See [oc_tokens()].
#' @param min_docfreq Minimum number of answers a word must appear in.
#' @param seed Seed for the model and the held-out draw.
#' @param n_words Number of words shown per topic.
#' @param n_examples Number of example answers shown per topic.
#' @return An `oc_topics` object: a list with `model` (the `stm` fit),
#'   `vocab`, `K`, `grid` (the K-selection table, or `NULL` when `K` was
#'   given), `theta_mean` (mean topic proportions on D), `settings`,
#'   `topics` (one row per topic: `topic`, `letter`, `share`, `prob_words`,
#'   `frex_words`, `examples`), `n_docs` (answers in the model), `n_empty`
#'   (answers left out because no word survived trimming) and `split_hash`.
#' @export
oc_topics_fit <- function(discovery, split, text = "text", lang = NULL,
                          K = NULL, K_grid = c(4L, 6L, 8L, 10L, 12L),
                          stopwords = NULL, tokenizer = NULL,
                          min_docfreq = 3L, seed = 42L, n_words = 10L,
                          n_examples = 2L) {
  need_pkg("stm")
  need_pkg("quanteda")
  split <- as_split(split)
  id_col <- attr(split, "meta")$id_col
  if (!id_col %in% names(discovery)) {
    stop("Data lack the split's id column: ", id_col, call. = FALSE)
  }
  if (!text %in% names(discovery)) stop("Text column not found: ", text,
                                        call. = FALSE)
  dfold <- topics_fold(discovery[[id_col]], split)
  tok_name <- if (is.null(tokenizer)) NULL else {
    attr(tokenizer, "oc_tokenizer") %||%
      paste(deparse(substitute(tokenizer)), collapse = "")
  }
  texts <- enc2utf8(as.character(discovery[[text]]))
  texts[is.na(texts)] <- ""
  check_text_encoding(texts)
  lang <- topics_lang(discovery, lang)

  toks <- oc_tokens(texts, lang, stopwords = stopwords, tokenizer = tokenizer)
  m <- quanteda::dfm_trim(quanteda::dfm(toks), min_docfreq = min_docfreq)
  ok <- as.vector(Matrix::rowSums(m) > 0)
  if (sum(ok) < 2L) stop("Fewer than two answers have words left after ",
                         "trimming.", call. = FALSE)
  st <- quanteda::convert(m[ok, ], to = "stm")
  ck <- if (is.null(K)) {
    choose_topics_K(st, K_grid, seed)
  } else {
    list(K = as.integer(K), grid = NULL)
  }
  if (ck$K > 26L) stop("At most 26 topics are supported (letters A-Z).",
                       call. = FALSE)
  mod <- with_seed(seed, stm::stm(st$documents, st$vocab, K = ck$K,
                                  init.type = "Spectral", max.em.its = 75,
                                  verbose = FALSE, seed = seed))
  lab <- stm::labelTopics(mod, n = n_words)
  tx <- texts[ok]
  examples <- vapply(seq_len(ck$K), function(k) {
    ex <- stm::findThoughts(mod, texts = tx, topics = k, n = n_examples)$docs[[1]]
    ex <- vapply(ex, function(x) {
      if (nchar(x) > 160) paste0(substr(x, 1, 160), "...") else x
    }, "")
    paste(sprintf("\"%s\"", gsub("[\r\n]+", " ", ex)), collapse = " | ")
  }, "")
  theta_mean <- colMeans(mod$theta)
  topics <- data.frame(
    topic = seq_len(ck$K),
    letter = LETTERS[seq_len(ck$K)],
    share = theta_mean,
    prob_words = apply(lab$prob, 1, paste, collapse = ", "),
    frex_words = apply(lab$frex, 1, paste, collapse = ", "),
    examples = examples,
    stringsAsFactors = FALSE
  )
  settings <- list(
    text = text,
    languages = sort(unique(lang), method = "radix"),
    stopwords = as.character(stopwords %||% character(0)),
    custom_tokenizer = !is.null(tokenizer),
    tokenizer = tok_name,
    min_docfreq = as.integer(min_docfreq),
    K_grid = if (is.null(K)) as.integer(K_grid) else NULL,
    init_type = "Spectral",
    max_em_its = 75L,
    seed = as.integer(seed),
    n_words = as.integer(n_words),
    n_examples = as.integer(n_examples),
    split_k = 1L,
    discovery_fold = dfold
  )
  structure(list(model = mod, vocab = st$vocab, K = ck$K, grid = ck$grid,
                 theta_mean = theta_mean, settings = settings,
                 topics = topics, n_docs = sum(ok), n_empty = sum(!ok),
                 split_hash = oc_split_hash(split)),
            class = "oc_topics")
}

#' @rdname oc_topics_fit
#' @param x An `oc_topics` object.
#' @param ... Ignored.
#' @export
print.oc_topics <- function(x, ...) {
  cat("<oc_topics> K=", x$K, ", ", x$n_docs, " discovery answers",
      if (x$n_empty) paste0(" (", x$n_empty, " with no word left out)"),
      "\n", sep = "")
  if (!is.null(x$grid)) cat("K chosen from ", paste(x$grid$K, collapse = ", "),
                            "\n", sep = "")
  if (isTRUE(x$settings$custom_tokenizer)) {
    cat("Korean tokenizer: ", x$settings$tokenizer, "\n", sep = "")
  }
  tp <- x$topics
  for (k in seq_len(nrow(tp))) {
    cat("\nTopic ", tp$letter[k], " (share ", sprintf("%.3f", tp$share[k]),
        ")\n  Most probable words: ", tp$prob_words[k],
        "\n  Most distinctive words: ", tp$frex_words[k],
        "\n  Example answers: ", tp$examples[k], "\n", sep = "")
  }
  invisible(x)
}

#' Save and read a fitted topic model
#'
#' The saved file can be listed as a component in [oc_freeze()]. A custom
#' tokenizer is not saved: only its name is, and it must be passed again to
#' [oc_topics_project()] and [oc_topics_measure()]. A fit is saved once:
#' saving a different fit to an existing path is an error.
#' @param fit An `oc_topics` object from [oc_topics_fit()].
#' @param path File path (RDS).
#' @export
oc_topics_save <- function(fit, path) {
  if (!inherits(fit, "oc_topics")) stop("`fit` must be an oc_topics.",
                                        call. = FALSE)
  if (file.exists(path)) {
    old <- oc_topics_read(path)
    if (identical(strip_time(old), strip_time(fit))) {
      message("Identical topic model already at ", path, "; not rewritten.")
      return(invisible(path))
    }
    stop("A different topic model already exists at ", path, ". Save a new ",
         "fit under a new name.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(fit, path)
  invisible(path)
}

#' @rdname oc_topics_save
#' @export
oc_topics_read <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  fit <- readRDS(path)
  if (!inherits(fit, "oc_topics")) stop(path, " does not hold an oc_topics ",
                                        "fit.", call. = FALSE)
  fit
}

#' Ask a model which topic matches each construct
#'
#' Builds one request per target and placebo construct of the study card.
#' Each shows every topic's most probable and most distinctive words and its
#' example answers, and asks for the letter of the topic that best captures
#' the construct, or 0 if none does. Write the requests as JSON lines for the
#' batch runner, e.g. with `jsonlite::toJSON(r, auto_unbox = TRUE)` per
#' request, and read the results with [oc_topics_match_import()].
#' @param fit An `oc_topics` object.
#' @param study A study card (list) or its path.
#' @return A list of requests, each with `id` (`topics:<study id>::<construct>`),
#'   `prompt_hash`, `system`, `user` and `codes` (`"0"` and the topic letters).
#' @export
oc_topics_match_requests <- function(fit, study) {
  if (!inherits(fit, "oc_topics")) stop("`fit` must be an oc_topics.",
                                        call. = FALSE)
  if (is.character(study)) study <- oc_study_read(study)
  tp <- fit$topics
  K <- nrow(tp)
  desc <- sprintf(paste0("Topic %s\n  Most probable words: %s\n  Most ",
                         "distinctive words: %s\n  Example answers: %s"),
                  tp$letter, tp$prob_words, tp$frex_words, tp$examples)
  user <- paste0(paste(desc, collapse = "\n\n"), "\n\nTopic:")
  context <- study$context %||% paste0(
    "These are open-ended answers from a survey experiment (", study$title,
    "). Respondents answered: \"", study$text$prompt,
    "\". Read each answer in its original language.")
  cons <- c(study$constructs$target, study$constructs$placebo)
  lapply(cons, function(t) {
    system <- paste(c(
      trimws(context),
      paste("A topic model was fit to a sample of these answers. Each topic",
            "is shown with its most probable words, its most distinctive",
            "words, and two answers that load heavily on it."),
      paste0("Which topic best captures this consideration?\n", t$name, ": ",
             trimws(t$definition)),
      sprintf(paste("Answer with the topic letter (A-%s), or 0 if no topic",
                    "captures it. Write only the letter or 0."), LETTERS[K])
    ), collapse = "\n\n")
    list(id = paste0("topics:", study$id, "::", t$name),
         prompt_hash = hash_object(list(system = system, user = user)),
         system = system, user = user, codes = c("0", LETTERS[seq_len(K)]))
  })
}

#' Read the topic matches a model returned
#'
#' For each request from [oc_topics_match_requests()], the first-token
#' probabilities of `"0"` and the letters are compared; the most probable one
#' is the match and `p` is its share of that mass. Without logprobs the
#' answer text is used and `p` is `NA`. If a construct was asked more than
#' once, the last answer counts.
#' @param results Runner results (JSON-lines path) or a list of result
#'   records with `id`, `raw` and `top`.
#' @return A data frame: `construct`, `topic` (integer; 0 = no topic matches,
#'   `NA` = no usable answer) and `p`.
#' @export
oc_topics_match_import <- function(results) {
  recs <- if (is.character(results)) read_jsonl(results) else results
  recs <- Filter(function(r) {
    startsWith(as.character(r$id %||% ""), "topics:") && !isTRUE(r$skipped)
  }, recs)
  valid <- c("0", LETTERS)
  rows <- lapply(recs, function(r) {
    pick <- NA_character_
    p <- NA_real_
    top <- r$top
    if (length(top)) {
      tok <- trimws(vapply(top, function(x) as.character(x$token %||% ""), ""))
      lp <- vapply(top, function(x) as.numeric(x$logprob %||% NA), 0)
      keep <- tok %in% valid & !is.na(lp)
      if (any(keep)) {
        mass <- tapply(exp(lp[keep]), tok[keep], sum)
        pick <- names(mass)[which.max(mass)]
        p <- max(mass) / sum(mass)
      }
    }
    if (is.na(pick)) {
      a <- substr(trimws(as.character(r$raw %||% "")), 1, 1)
      if (a %in% valid) pick <- a
    }
    data.frame(construct = sub("^.*::", "", r$id),
               topic = if (is.na(pick)) NA_integer_ else
                 if (pick == "0") 0L else match(pick, LETTERS),
               p = p, stringsAsFactors = FALSE)
  })
  if (!length(rows)) {
    return(data.frame(construct = character(0), topic = integer(0),
                      p = numeric(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$construct, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Project answers onto a discovery-fold topic model
#'
#' Tokenizes `estimation` exactly as the discovery answers were, keeps the
#' words in the model's vocabulary, and estimates each answer's topic
#' proportions with [stm::fitNewDocuments()]. The model is not refit.
#' @param fit An `oc_topics` object.
#' @param estimation Data frame of answers (usually from
#'   [oc_estimation_data()]).
#' @param text Name of the text column.
#' @param lang As in [oc_topics_fit()].
#' @param tokenizer The Korean tokenizer, required again if the fit used one.
#' @param fill For answers with no word in the vocabulary: `TRUE` gives the
#'   mean topic proportions on D, `FALSE` gives `NA`.
#' @return A matrix with one row per row of `estimation` and one column per
#'   topic (named by letter). Attribute `"empty"` marks the answers with no
#'   word in the vocabulary.
#' @export
oc_topics_project <- function(fit, estimation, text = "text", lang = NULL,
                              tokenizer = NULL, fill = TRUE) {
  need_pkg("stm")
  need_pkg("quanteda")
  if (!inherits(fit, "oc_topics")) stop("`fit` must be an oc_topics.",
                                        call. = FALSE)
  used <- isTRUE(fit$settings$custom_tokenizer)
  if (used && is.null(tokenizer)) {
    stop("The fit used the Korean tokenizer ", fit$settings$tokenizer,
         "; pass it again as `tokenizer`.", call. = FALSE)
  }
  if (!used && !is.null(tokenizer)) {
    stop("The fit used no custom tokenizer; do not pass one here.",
         call. = FALSE)
  }
  if (!text %in% names(estimation)) stop("Text column not found: ", text,
                                         call. = FALSE)
  texts <- enc2utf8(as.character(estimation[[text]]))
  texts[is.na(texts)] <- ""
  check_text_encoding(texts)
  lang <- topics_lang(estimation, lang)
  toks <- oc_tokens(texts, lang, stopwords = fit$settings$stopwords,
                    tokenizer = tokenizer)
  m <- quanteda::dfm_match(quanteda::dfm(toks), fit$vocab)
  ok <- as.vector(Matrix::rowSums(m) > 0)
  theta <- matrix(NA_real_, length(texts), fit$K,
                  dimnames = list(NULL, LETTERS[seq_len(fit$K)]))
  if (any(ok)) {
    st <- suppressWarnings(quanteda::convert(m[ok, ], to = "stm"))
    # convert() sorts the words and drops those absent from these answers, so
    # its indices refer to st$vocab; map them to the model's vocabulary.
    docs <- lapply(st$documents, function(d) {
      d[1, ] <- match(st$vocab[d[1, ]], fit$vocab)
      d[, order(d[1, ]), drop = FALSE]
    })
    theta[ok, ] <- stm::fitNewDocuments(model = fit$model, documents = docs,
                                        newData = NULL, origData = NULL,
                                        prevalence = NULL,
                                        verbose = FALSE)$theta
  }
  if (fill && any(!ok)) {
    theta[!ok, ] <- matrix(fit$theta_mean, sum(!ok), fit$K, byrow = TRUE)
  }
  attr(theta, "empty") <- !ok
  theta
}

#' Topic-model measures for the matched constructs
#'
#' For each construct matched to a topic (topic > 0), the answer's proportion
#' of that topic, in a column `stm__<construct>`, ready for [oc_effect()].
#' Use it as a comparison measure on the estimation fold; the frozen yes/no
#' question remains the main measure.
#' @param fit An `oc_topics` object.
#' @param estimation Data frame of answers.
#' @param match Output of [oc_topics_match_import()].
#' @param id Name of the id column.
#' @param ... Passed to [oc_topics_project()] (`text`, `lang`, `tokenizer`,
#'   `fill`).
#' @return A data frame with the id column and one `stm__<construct>` column
#'   per matched construct.
#' @export
oc_topics_measure <- function(fit, estimation, match, id = "id", ...) {
  if (!id %in% names(estimation)) stop("Id column not found: ", id,
                                       call. = FALSE)
  if (!all(c("construct", "topic") %in% names(match))) {
    stop("`match` needs columns construct and topic.", call. = FALSE)
  }
  if (anyDuplicated(match$construct)) {
    stop("`match` lists a construct more than once.", call. = FALSE)
  }
  hit <- match[!is.na(match$topic) & match$topic > 0, , drop = FALSE]
  if (any(hit$topic > fit$K)) stop("`match` names a topic beyond K = ", fit$K,
                                   ".", call. = FALSE)
  out <- data.frame(estimation[[id]], stringsAsFactors = FALSE)
  names(out) <- id
  if (!nrow(hit)) return(out)
  theta <- oc_topics_project(fit, estimation, ...)
  for (i in seq_len(nrow(hit))) {
    out[[paste0("stm__", hit$construct[i])]] <- theta[, hit$topic[i]]
  }
  out
}

need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is needed for the topic-model step. Install ",
         "it with install.packages(\"", pkg, "\").", call. = FALSE)
  }
  invisible(TRUE)
}

segment_words <- function(text) {
  tz <- quanteda::tokens(text, remove_punct = TRUE, remove_numbers = TRUE,
                         remove_symbols = TRUE)
  lapply(as.list(tz), function(w) w[nchar(w) > 1])
}

snowball_stopwords <- function(l) {
  base <- sub("-.*$", "", l)
  if (base %in% stopwords::stopwords_getlanguages("snowball")) {
    stopwords::stopwords(base, source = "snowball")
  } else character(0)
}

topics_lang <- function(data, lang) {
  if (!is.null(lang)) return(lang)
  if ("lang" %in% names(data)) as.character(data$lang) else "en"
}

# The fold the answers come from: D in a holdout split; in a two-fold split,
# whichever fold of split 1 holds all of them.
topics_fold <- function(ids, split) {
  if (attr(split, "meta")$scheme == "holdout") {
    oc_assert_discovery(ids, split)
    return("D")
  }
  a <- split[split$split == 1L, ]
  fold <- if (all(as.character(ids) %in% a$id[a$fold == "A"])) "A" else "B"
  oc_assert_discovery(ids, split, fold = fold)
  fold
}

choose_topics_K <- function(st, K_grid, seed) {
  sk <- with_seed(seed, stm::searchK(st$documents, st$vocab, K = K_grid,
                                     init.type = "Spectral",
                                     heldout.seed = seed, max.em.its = 75,
                                     verbose = FALSE, cores = 1, seed = seed))
  r <- data.frame(K = unlist(sk$results$K), coh = unlist(sk$results$semcoh),
                  exc = unlist(sk$results$exclus),
                  heldout = unlist(sk$results$heldout))
  r$score <- as.numeric(scale(r$coh)) + as.numeric(scale(r$exc))
  list(K = as.integer(r$K[which.max(r$score)]), grid = r)
}

strip_time <- function(fit) {
  fit$model$time <- NULL
  fit
}
