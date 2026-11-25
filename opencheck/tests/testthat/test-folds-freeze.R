setup_project <- function() {
  dir <- tempfile("oc")
  dir.create(dir)
  d <- data.frame(rid = sprintf("R%03d", 1:200), treat = rep(0:1, 100),
                  text = paste("t", 1:200), stringsAsFactors = FALSE)
  s <- oc_split(d, "rid", strata = "treat", seed = 42)
  split_path <- file.path(dir, "split.json")
  oc_split_write(s, split_path)
  cb <- file.path(dir, "codebook.yaml")
  writeLines("codes: [a, b]", cb)
  pr <- file.path(dir, "prompt.txt")
  writeLines("Classify the response.", pr)
  list(dir = dir, d = d, s = s, split_path = split_path, cb = cb, pr = pr)
}

test_that("discovery data contain only the discovery fold", {
  p <- setup_project()
  dd <- oc_discovery_data(p$d, p$s)
  expect_setequal(dd$rid, p$s$id[p$s$fold == "D"])
  expect_equal(nrow(dd), 60)
})

test_that("estimation data need a valid lock", {
  p <- setup_project()
  expect_error(oc_estimation_data(p$d, p$s), "lockfile")
  lock_path <- file.path(p$dir, "lock.json")
  lock <- oc_freeze(lock_path, p$split_path,
                    components = c(codebook = p$cb, prompt = p$pr),
                    models = list(primary = "Qwen/Qwen3.8-27B-FP8"),
                    decoding = list(temperature = 0, seed = 42))
  ed <- oc_estimation_data(p$d, p$split_path, lock_path)
  expect_setequal(ed$rid, p$s$id[p$s$fold == "E"])
  expect_length(intersect(ed$rid, oc_discovery_data(p$d, p$s)$rid), 0)
})

test_that("changing a frozen file blocks estimation", {
  p <- setup_project()
  lock_path <- file.path(p$dir, "lock.json")
  oc_freeze(lock_path, p$s, components = c(codebook = p$cb))
  writeLines("codes: [a, b, c]", p$cb)
  expect_error(oc_estimation_data(p$d, p$s, lock_path), "codebook")
})

test_that("a lock frozen on another split cannot open this one", {
  p <- setup_project()
  lock_path <- file.path(p$dir, "lock.json")
  oc_freeze(lock_path, p$s, components = c(codebook = p$cb))
  other <- oc_split(p$d, "rid", strata = "treat", seed = 43)
  expect_error(oc_estimation_data(p$d, other, lock_path), "different split")
})

test_that("few-shot examples must come from the discovery fold", {
  p <- setup_project()
  d_ids <- p$s$id[p$s$fold == "D"]
  e_ids <- p$s$id[p$s$fold == "E"]
  expect_error(
    oc_freeze(file.path(p$dir, "lock1.json"), p$s,
              components = c(codebook = p$cb), example_ids = c(d_ids[1], e_ids[1])),
    "discovery fold")
  expect_silent(oc_assert_discovery(d_ids[1:3], p$s))
  lk <- oc_freeze(file.path(p$dir, "lock2.json"), p$s,
                  components = c(codebook = p$cb), example_ids = d_ids[1:3])
  expect_equal(lk$n_example_ids, 3)
})

test_that("a lock is written once and edits to it are detected", {
  p <- setup_project()
  lock_path <- file.path(p$dir, "lock.json")
  oc_freeze(lock_path, p$s, components = c(codebook = p$cb))
  expect_message(oc_freeze(lock_path, p$s, components = c(codebook = p$cb)),
                 "Identical")
  expect_error(oc_freeze(lock_path, p$s,
                         components = c(codebook = p$cb, prompt = p$pr)),
               "different lock")
  j <- jsonlite::fromJSON(lock_path)
  j$estimation_fold <- "D"
  writeLines(jsonlite::toJSON(j, auto_unbox = TRUE, digits = NA,
                              null = "null"), lock_path)
  expect_error(oc_lock_read(lock_path), "edited")
})

test_that("unknown ids (data changed after the split) are refused", {
  p <- setup_project()
  d2 <- rbind(p$d, data.frame(rid = "NEW", treat = 1, text = "x"))
  expect_error(oc_discovery_data(d2, p$s), "not in the split manifest")
})

test_that("two-fold splits swap discovery and estimation", {
  p <- setup_project()
  s2 <- oc_split(p$d, "rid", strata = "treat", seed = 5, scheme = "twofold")
  expect_error(oc_discovery_data(p$d, s2), "name the discovery fold")
  la <- oc_freeze(file.path(p$dir, "lockA.json"), s2,
                  components = c(codebook = p$cb), discovery_fold = "A")
  expect_identical(la$estimation_fold, "B")
  eb <- oc_estimation_data(p$d, s2, la)
  expect_setequal(eb$rid, s2$id[s2$fold == "B"])
})

test_that("access log records discovery and estimation reads", {
  p <- setup_project()
  log <- file.path(p$dir, "access.jsonl")
  oc_discovery_data(p$d, p$s, log = log, purpose = "stm")
  lock_path <- file.path(p$dir, "lock.json")
  oc_freeze(lock_path, p$s, components = c(codebook = p$cb))
  oc_estimation_data(p$d, p$s, lock_path, log = log)
  entries <- lapply(readLines(log), jsonlite::fromJSON)
  expect_equal(vapply(entries, `[[`, "", "access"), c("discovery", "estimation"))
})
