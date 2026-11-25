skip_if_not_installed("stm")
skip_if_not_installed("quanteda")
skip_if_not_installed("stopwords")

# Two clearly separated vocabularies: civic answers and economic answers.
topic_data <- function() {
  civic <- c("volunteer", "community", "neighbors", "participate", "organize",
             "charity", "festival", "council")
  econ <- c("job", "taxes", "salary", "career", "employer", "income",
            "business", "skills")
  mk <- function(v, n) vapply(seq_len(n), function(i) {
    paste(sample(v, 6, replace = TRUE), collapse = " ")
  }, "")
  text <- with_seed(7, c(mk(civic, 80), mk(econ, 80)))
  dat <- data.frame(id = sprintf("R%03d", 1:160), treat = rep(0:1, 80),
                    civic = rep(c(TRUE, FALSE), each = 80), text = text,
                    stringsAsFactors = FALSE)
  s <- oc_split(dat, "id", strata = c("treat", "civic"), p_discovery = 0.5,
                seed = 1)
  list(dat = dat, split = s, d = oc_discovery_data(dat, s),
       e = dat[dat$id %in% s$id[s$fold == "E"], ])
}

topic_card <- function() {
  card <- yaml::read_yaml(system.file("templates", "study_card.yaml",
                                      package = "opencheck"))
  card$constructs$target[[1]]$name <- "civic_engagement"
  card$constructs$placebo[[1]]$name <- "economic_focus"
  card
}

td <- topic_data()
# K = 2 makes stm note that it equals a scaling model; that is expected here.
fit <- suppressWarnings(oc_topics_fit(td$d, td$split, K = 2))
civic_topic <- if (grepl("volunteer", fit$topics$prob_words[1])) 1L else 2L

test_that("the fit refuses estimation-fold answers", {
  expect_error(oc_topics_fit(td$dat, td$split, K = 2),
               "not in discovery fold D")
})

test_that("a given K is respected and K selection returns a grid", {
  expect_s3_class(fit, "oc_topics")
  expect_identical(fit$K, 2L)
  expect_null(fit$grid)
  expect_equal(nrow(fit$topics), 2)
  expect_named(fit$topics, c("topic", "letter", "share", "prob_words",
                             "frex_words", "examples"))
  expect_equal(sum(fit$topics$share), 1)
  expect_identical(fit$split_hash, oc_split_hash(td$split))
  expect_equal(fit$n_docs + fit$n_empty, nrow(td$d))
  expect_output(print(fit), "Topic B")
  g <- suppressWarnings(oc_topics_fit(td$d, td$split, K_grid = 2:4))
  expect_identical(g$grid$K, 2:4)
  expect_true(g$K %in% 2:4)
  expect_equal(nrow(g$topics), g$K)
})

test_that("projection gives proportions and marks out-of-vocabulary answers", {
  e <- rbind(td$e, data.frame(id = "X1", treat = 0, civic = FALSE,
                              text = "zzz qqq the"))
  th <- oc_topics_project(fit, e)
  expect_equal(dim(th), c(nrow(e), 2L))
  expect_equal(unname(rowSums(th)), rep(1, nrow(e)))
  empty <- attr(th, "empty")
  expect_identical(which(empty), nrow(e))
  expect_equal(unname(th[nrow(e), ]), unname(fit$theta_mean))
  expect_true(all(is.na(oc_topics_project(fit, e, fill = FALSE)[nrow(e), ])))
  # Words are matched to the model's vocabulary by name.
  one <- oc_topics_project(fit, data.frame(text = "volunteer festival"))
  expect_gt(one[1, civic_topic], 0.5)
})

test_that("match requests, import and measure fit together", {
  reqs <- oc_topics_match_requests(fit, topic_card())
  expect_length(reqs, 2)
  expect_identical(vapply(reqs, `[[`, "", "id"),
                   paste0("topics:short_snake_case_id::",
                          c("civic_engagement", "economic_focus")))
  expect_identical(reqs[[1]]$codes, c("0", "A", "B"))
  expect_named(reqs[[1]], c("id", "prompt_hash", "system", "user", "codes"))
  expect_match(reqs[[1]]$system, "(A-B), or 0", fixed = TRUE)
  expect_match(reqs[[2]]$system, "economic_focus:", fixed = TRUE)
  res <- tempfile(fileext = ".jsonl")
  pick <- LETTERS[civic_topic]
  writeLines(c(
    as.character(jsonlite::toJSON(list(id = reqs[[1]]$id, raw = pick,
      top = data.frame(token = c(pick, "0"), logprob = log(c(0.8, 0.2)))),
      auto_unbox = TRUE, digits = NA)),
    as.character(jsonlite::toJSON(list(id = reqs[[2]]$id, raw = "0",
      top = data.frame(token = "0", logprob = log(0.9))),
      auto_unbox = TRUE, digits = NA))), res)
  m <- oc_topics_match_import(res)
  expect_named(m, c("construct", "topic", "p"))
  expect_identical(m$topic, c(civic_topic, 0L))
  expect_equal(m$p, c(0.8, 1))
  meas <- oc_topics_measure(fit, td$e, m)
  expect_named(meas, c("id", "stm__civic_engagement"))
  expect_identical(meas$id, td$e$id)
  expect_gt(mean(meas$stm__civic_engagement[td$e$civic]),
            mean(meas$stm__civic_engagement[!td$e$civic]))
})

test_that("a custom tokenizer is required again at projection", {
  tok <- function(x) strsplit(x, " ", fixed = TRUE)
  ko <- td$d
  ko$lang <- "ko"
  kfit <- suppressWarnings(oc_topics_fit(ko, td$split, K = 2, tokenizer = tok))
  expect_true(kfit$settings$custom_tokenizer)
  expect_identical(kfit$settings$tokenizer, "tok")
  e <- transform(td$e, lang = "ko")
  expect_error(oc_topics_project(kfit, e), "pass it again")
  expect_equal(nrow(oc_topics_project(kfit, e, tokenizer = tok)), nrow(e))
  expect_warning(oc_tokens("지역 사회", "ko"), "No Korean tokenizer")
})

test_that("save and read round-trip and saving is write-once", {
  f <- tempfile(fileext = ".rds")
  oc_topics_save(fit, f)
  back <- oc_topics_read(f)
  expect_s3_class(back, "oc_topics")
  expect_identical(back$topics, fit$topics)
  expect_identical(back$vocab, fit$vocab)
  expect_message(oc_topics_save(fit, f), "not rewritten")
  other <- fit
  other$K <- 3L
  expect_error(oc_topics_save(other, f), "different topic model")
})
