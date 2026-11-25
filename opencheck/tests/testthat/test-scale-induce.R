civic <- list(name = "civic_commitment", definition = "Mentions volunteering or community participation.")

test_that("rating prompt gives an expected score from digit probabilities", {
  pr <- oc_prompt_rating(civic)
  expect_identical(pr$codes, as.character(0:9))
  mock <- oc_backend_mock(function(m) list(
    content = "7", logprobs = data.frame(token = c("7", "8", "0"),
                                         logprob = log(c(0.5, 0.3, 0.2)))))
  res <- oc_classify(data.frame(id = "a", text = "x"), pr, mock, tempfile(),
                     progress = FALSE)
  expect_equal(oc_rating_score(res), 0.5 * 7 + 0.3 * 8)
})

test_that("pairwise export asks held-out pairs both ways and import averages them", {
  ids <- sprintf("r%02d", 1:20)
  d <- data.frame(id = ids, text = paste("text", ids))
  pairs <- oc_pairs(ids, k = 3, holdout = 0.2, seed = 1)
  pr <- oc_prompt_pairwise(civic)
  req <- tempfile(fileext = ".jsonl")
  oc_export_pairs(pairs, d, pr, req)
  r <- lapply(read_lines_utf8(req), jsonlite::fromJSON)
  expect_equal(length(r), nrow(pairs) + sum(pairs$holdout))
  first <- pairs[1, ]
  expect_match(r[[1]]$user, paste("Response 1: text", first$id1), fixed = TRUE)
  expect_match(r[[1]]$user, paste("Response 2: text", first$id2), fixed = TRUE)
  # Fake results: fwd P("1") = 0.8 for every pair; rev P("1") = 0.4.
  res <- tempfile(fileext = ".jsonl")
  lines <- vapply(r, function(q) {
    p1 <- if (grepl(":fwd$", q$id)) 0.8 else 0.4
    as.character(jsonlite::toJSON(list(id = q$id, prompt_hash = q$prompt_hash,
      model = "fake", raw = "1",
      top = data.frame(token = c("1", "2"), logprob = log(c(p1, 1 - p1)))),
      auto_unbox = TRUE, digits = NA))
  }, "")
  writeLines(lines, res)
  y <- oc_import_pairs(res, pairs, pr)
  expect_equal(y$y[!y$holdout], rep(0.8, sum(!y$holdout)))
  expect_equal(y$y[y$holdout], rep((0.8 + 0.6) / 2, sum(y$holdout)))
  expect_equal(y$order_gap[y$holdout], rep(0.2, sum(y$holdout)))
})

test_that("a lock with frozen prompts refuses a different prompt", {
  dir <- tempfile("oc")
  dir.create(dir)
  card <- file.path(dir, "card.yaml")
  file.copy(system.file("templates", "study_card.yaml", package = "opencheck"), card)
  d <- data.frame(id = sprintf("R%03d", 1:40), treat = rep(0:1, 20), text = "x")
  s <- oc_split(d, "id", strata = "treat", seed = 2)
  p_ok <- oc_prompt_presence(civic, source = card)
  p_other <- oc_prompt_presence(list(name = "civic_commitment", definition = "Something else."),
                                source = card)
  lock <- oc_freeze(file.path(dir, "lock.json"), s, components = c(card = card),
                    prompts = list(p_ok))
  ed <- oc_estimation_data(d, s, lock)
  mock <- oc_backend_mock(function(m) list(content = "yes", logprobs = NULL))
  expect_silent(oc_classify(ed, p_ok, mock, tempfile(), lock = lock, progress = FALSE))
  expect_error(oc_classify(ed, p_other, mock, tempfile(), lock = lock, progress = FALSE),
               "differs from the prompts frozen")
  expect_error(oc_export_requests(ed, p_other, tempfile(), lock = lock),
               "differs from the prompts frozen")
})

test_that("induction request respects the budget and import validates the draft", {
  card_path <- system.file("templates", "study_card.yaml", package = "opencheck")
  d <- data.frame(id = sprintf("R%03d", 1:300),
                  text = strrep("word ", 50))
  req <- tempfile(fileext = ".jsonl")
  info <- oc_induce_request(card_path, d, req, seed = 3, max_chars = 2600)
  expect_lte(info$chars, 2600)
  expect_equal(info$n_shown, 10)
  q <- jsonlite::fromJSON(read_lines_utf8(req))
  expect_match(q$system, "target_construct", fixed = TRUE)
  draft <- paste0("Here is the codebook:\n```yaml\ncodebook:\n  name: x\n  residual_code: none_of_above\ncodes:\n",
                  "  target_construct:\n    definition: The thing.\n",
                  "  other_reason:\n    definition: Other.\n```\nDone.")
  res <- tempfile(fileext = ".jsonl")
  writeLines(as.character(jsonlite::toJSON(list(id = q$id, prompt_hash = q$prompt_hash,
    model = "m", raw = draft), auto_unbox = TRUE)), res)
  out <- tempfile(fileext = ".yaml")
  oc_induce_import(res, card_path, out, context = "ctx")
  cb <- oc_codebook_read(out)
  expect_setequal(names(cb$codes), c("target_construct", "other_reason", "none_of_above"))
  expect_identical(cb$residual_code, "none_of_above")
  bad <- sub("target_construct", "something_else", draft)
  writeLines(as.character(jsonlite::toJSON(list(id = q$id, prompt_hash = q$prompt_hash,
    model = "m", raw = bad), auto_unbox = TRUE)), res)
  expect_error(oc_induce_import(res, card_path, tempfile(fileext = ".yaml")), "target code")
})
