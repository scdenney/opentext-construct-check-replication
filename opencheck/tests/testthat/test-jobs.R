test_that("export -> runner (stub) -> import -> classify round-trips", {
  py <- Sys.which("python3")
  skip_if(!nzchar(py), "python3 not available")
  dir <- tempfile("ocjob")
  dir.create(dir)
  pr <- oc_prompt_presence(list(name = "civic", definition = "Mentions volunteering."))
  d <- data.frame(id = sprintf("R%02d", 1:12),
                  text = rep(c("봉사활동", "中文流利",
                               "job"), 4))
  req <- file.path(dir, "req.jsonl")
  oc_export_requests(d, pr, req)
  runner <- system.file("python", "oc_vllm_runner.py", package = "opencheck")
  res <- file.path(dir, "res.jsonl")
  out <- system2(py, c("-I", shQuote(runner), "--requests", shQuote(req), "--out",
                       shQuote(res), "--backend", "stub"), stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("12 to run", out)))
  be <- oc_backend_offline("stub")
  cache <- file.path(dir, "cache.jsonl")
  expect_equal(oc_import_results(res, pr, be, cache), 12L)
  expect_equal(oc_import_results(res, pr, be, cache), 0L)
  got <- oc_classify(d, pr, be, cache, progress = FALSE)
  expect_true(all(got$valid))
  expect_true(all(abs(got$p_yes + got$p_no - 1) < 1e-9))
  system2(py, c("-I", shQuote(runner), "--requests", shQuote(req), "--out",
                shQuote(res), "--backend", "stub"), stdout = TRUE, stderr = TRUE)
  expect_length(readLines(res), 12)
  d2 <- rbind(d, data.frame(id = "R99", text = "new"))
  expect_error(oc_classify(d2, pr, be, cache, progress = FALSE), "not in the cache")
})

test_that("requests keep CJK text intact", {
  pr <- oc_prompt_presence(list(name = "civic", definition = "x"))
  p <- tempfile(fileext = ".jsonl")
  oc_export_requests(data.frame(id = "a", text = "한국어 유창"), pr, p)
  r <- jsonlite::fromJSON(read_lines_utf8(p))
  expect_match(enc2utf8(r$user), "한국어 유창", fixed = TRUE)
})

test_that("SLURM script fills every field", {
  p <- tempfile(fileext = ".slurm")
  oc_slurm_job(p, root = "/zfsstore/user/denneysc/oc/sktw", requests = "req.jsonl",
               results = "res.jsonl", model = "Qwen/Qwen3.8-27B-FP8",
               job_name = "oc-sktw", pilot = 100)
  s <- readLines(p)
  expect_false(any(grepl("{{", s, fixed = TRUE)))
  expect_true(any(grepl("--gres=gpu:a40:1", s, fixed = TRUE)))
  expect_true(any(grepl("--pilot 100", s, fixed = TRUE)))
})

test_that("text with <U+XXXX> escapes is refused", {
  pr <- oc_prompt_presence(list(name = "civic", definition = "x"))
  bad <- data.frame(id = "a", text = "<U+D574><U+B2F9>")
  expect_error(oc_export_requests(bad, pr, tempfile()), "non-UTF-8 locale")
  expect_error(oc_classify(bad, pr, oc_backend_mock(function(m) list(content = "yes")),
                           tempfile(), progress = FALSE), "non-UTF-8 locale")
})
