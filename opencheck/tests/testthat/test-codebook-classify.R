write_codebook <- function(fewshot = TRUE) {
  p <- tempfile(fileext = ".yaml")
  lines <- c(
    "codebook:",
    "  name: test_cb",
    "  version: 1",
    "  context: Respondents explain why they chose a profile.",
    "  residual_code: none_of_above",
    "  instructions:",
    "    - Short answers still count.",
    "codes:",
    "  civic_commitment:",
    "    definition: Mentions volunteering or community participation.",
    "    negative_clarification: Language ability alone does not count.",
    "  economic_focus:",
    "    definition: Mentions jobs, taxes or economic contribution.",
    "  none_of_above:",
    "    definition: Uninformative.",
    if (fewshot) c(
      "fewshot:",
      "  - text: They volunteer every weekend.",
      "    code: civic_commitment",
      "    constructed: true",
      "  - text: has a good job",
      "    code: economic_focus",
      "    id: R002")
  )
  writeLines(lines, p)
  p
}

mock_from <- function(answer_fun, counter = NULL) {
  oc_backend_mock(function(messages) {
    if (!is.null(counter)) counter$n <- counter$n + 1
    answer_fun(messages[[2]]$content)
  }, id = "mock-model")
}

civic_or_econ <- function(user) {
  if (grepl("volunteer", user)) {
    list(content = "civic_commitment",
         logprobs = data.frame(token = c("civic", "economic", "none"),
                               logprob = log(c(0.8, 0.15, 0.05))))
  } else {
    list(content = "economic_focus",
         logprobs = data.frame(token = c("economic", "civic"),
                               logprob = log(c(0.9, 0.1))))
  }
}

test_that("codebook reads, validates and builds a prompt", {
  cb <- oc_codebook_read(write_codebook())
  expect_s3_class(cb, "oc_codebook")
  expect_identical(oc_codebook_example_ids(cb), "R002")
  pr <- oc_prompt(cb)
  expect_identical(pr$codes, c("civic_commitment", "economic_focus", "none_of_above"))
  expect_match(pr$system, "Language ability alone does not count", fixed = TRUE)
  expect_match(pr$system, "Code: civic_commitment", fixed = TRUE)
  expect_true(grepl("{text}", pr$user, fixed = TRUE))
})

test_that("few-shot examples must state their source", {
  p <- write_codebook()
  x <- readLines(p)
  writeLines(x[!grepl("constructed: true", x)], p)
  expect_error(oc_codebook_read(p), "constructed: true")
})

test_that("the prompt hash changes when the codebook changes", {
  p <- write_codebook()
  h1 <- oc_prompt(p)$hash
  x <- readLines(p)
  x <- sub("Uninformative.", "Uninformative or off-topic.", x, fixed = TRUE)
  writeLines(x, p)
  expect_false(identical(h1, oc_prompt(p)$hash))
})

test_that("classification returns labels and first-token probabilities", {
  pr <- oc_prompt(write_codebook())
  d <- data.frame(id = c("a", "b"), text = c("I volunteer", "pays taxes"))
  res <- oc_classify(d, pr, mock_from(civic_or_econ), tempfile(fileext = ".jsonl"),
                     progress = FALSE)
  expect_identical(res$label, c("civic_commitment", "economic_focus"))
  expect_equal(res$p_label, c(0.8, 0.9))
  expect_equal(res$p_valid, c(1, 1))
  expect_equal(res$p_civic_commitment[2], 0.1)
})

test_that("the cache resumes and a new prompt is not served old answers", {
  cb_path <- write_codebook()
  pr <- oc_prompt(cb_path)
  d <- data.frame(id = c("a", "b", "c"), text = c("volunteer", "job", "volunteer"))
  cache <- tempfile(fileext = ".jsonl")
  counter <- new.env()
  counter$n <- 0
  oc_classify(d[1:2, ], pr, mock_from(civic_or_econ, counter), cache, progress = FALSE)
  expect_equal(counter$n, 2)
  oc_classify(d, pr, mock_from(civic_or_econ, counter), cache, progress = FALSE)
  expect_equal(counter$n, 3)
  x <- readLines(cb_path)
  writeLines(sub("Uninformative.", "Nothing usable.", x, fixed = TRUE), cb_path)
  oc_classify(d, oc_prompt(cb_path), mock_from(civic_or_econ, counter), cache,
              progress = FALSE)
  expect_equal(counter$n, 6)
})

test_that("unparseable output is marked invalid, not guessed", {
  pr <- oc_prompt(write_codebook())
  odd <- function(user) list(content = "I cannot decide", logprobs = NULL)
  res <- oc_classify(data.frame(id = "a", text = "x"), pr, mock_from(odd),
                     tempfile(fileext = ".jsonl"), progress = FALSE)
  expect_false(res$valid)
  expect_true(is.na(res$label))
})

test_that("a token shared by two codes is kept apart", {
  p <- tempfile(fileext = ".yaml")
  writeLines(c("codes:",
               "  civic_commitment:", "    definition: Civic participation.",
               "  cultural_identity:", "    definition: Shared culture.",
               "  none_of_above:", "    definition: Uninformative."), p)
  amb <- function(user) list(content = "civic_commitment",
                             logprobs = data.frame(token = c("civic", "c", "none"),
                                                   logprob = log(c(0.6, 0.3, 0.1))))
  res <- oc_classify(data.frame(id = "a", text = "x"), oc_prompt(p), mock_from(amb),
                     tempfile(fileext = ".jsonl"), progress = FALSE)
  expect_equal(res$p_label, 0.6 / 0.7)
  expect_equal(res$p_valid, 1)
  first_shared <- function(user) list(content = "civic_commitment",
                                      logprobs = data.frame(token = c("c", "civic"),
                                                            logprob = log(c(0.8, 0.2))))
  res2 <- oc_classify(data.frame(id = "a", text = "x"), oc_prompt(p),
                      mock_from(first_shared), tempfile(fileext = ".jsonl"),
                      progress = FALSE)
  expect_true(is.na(res2$p_label))
  expect_identical(res2$label, "civic_commitment")
})

test_that("estimation runs require the codebook to be frozen in the lock", {
  dir <- tempfile("oc")
  dir.create(dir)
  cb_path <- file.path(dir, "cb.yaml")
  file.copy(write_codebook(), cb_path)
  d <- data.frame(id = sprintf("R%03d", 1:50), treat = rep(0:1, 25),
                  text = rep(c("volunteer", "job"), 25))
  s <- oc_split(d, "id", strata = "treat", seed = 1)
  other <- file.path(dir, "other.yaml")
  writeLines("x: 1", other)
  lock_wrong <- oc_freeze(file.path(dir, "lock_wrong.json"), s,
                          components = c(other = other))
  ed <- oc_estimation_data(d, s, lock_wrong)
  expect_error(oc_classify(ed, oc_prompt(cb_path), mock_from(civic_or_econ),
                           tempfile(), lock = lock_wrong, progress = FALSE),
               "not frozen")
  lock <- oc_freeze(file.path(dir, "lock.json"), s,
                    components = c(codebook = cb_path))
  res <- oc_classify(ed, oc_prompt(cb_path), mock_from(civic_or_econ),
                     tempfile(), lock = lock, progress = FALSE)
  expect_equal(nrow(res), nrow(ed))
})

test_that("presence prompts give yes/no probabilities", {
  pr <- oc_prompt_presence(list(name = "civic", definition = "Mentions volunteering."))
  yn <- function(user) list(content = "Yes",
                            logprobs = data.frame(token = c("Yes", " No"),
                                                  logprob = log(c(0.6, 0.4))))
  res <- oc_classify(data.frame(id = "a", text = "x"), pr, mock_from(yn),
                     tempfile(), progress = FALSE)
  expect_identical(res$label, "yes")
  expect_equal(res$p_yes, 0.6)
})

test_that("one-token answers resolve to the code they begin", {
  pr <- oc_prompt(write_codebook())
  one <- function(user) list(content = "economic",
                             logprobs = data.frame(token = c("economic", "civic"),
                                                   logprob = log(c(0.7, 0.3))))
  res <- oc_classify(data.frame(id = "a", text = "x"), pr, mock_from(one),
                     tempfile(fileext = ".jsonl"), progress = FALSE)
  expect_identical(res$label, "economic_focus")
  expect_equal(res$p_label, 0.7)
})

test_that("numbered prompts separate codes that share a first token", {
  p <- tempfile(fileext = ".yaml")
  writeLines(c("codes:",
               "  economic_need:", "    definition: Need.",
               "  economic_gain:", "    definition: Gain.",
               "  none_of_above:", "    definition: Nothing.",
               "codebook:", "  residual_code: none_of_above"), p)
  pr <- oc_prompt(p, answer = "number")
  expect_identical(pr$codes, c("1", "2", "3"))
  ans <- function(user) list(content = "2",
                             logprobs = data.frame(token = c("2", "1", "3"),
                                                   logprob = log(c(0.6, 0.3, 0.1))))
  res <- oc_classify(data.frame(id = "a", text = "x"), pr, mock_from(ans),
                     tempfile(fileext = ".jsonl"), progress = FALSE)
  expect_identical(res$label, "economic_gain")
  expect_equal(res$p_economic_gain, 0.6)
  expect_equal(res$p_economic_need, 0.3)
})
