template <- function() {
  system.file("templates", "study_card.yaml", package = "opencheck")
}

test_that("the template card validates", {
  expect_silent(oc_study_read(template()))
})

test_that("cards with Korean and Chinese text parse in any locale", {
  prompt <- "해당 이민자를 선택하신 이유는? / 請說明原因"
  lines <- readLines(template())
  i <- grep("^  prompt:", lines)
  lines[i] <- paste0("  prompt: \"", prompt, "\"")
  p <- tempfile(fileext = ".yaml")
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), p)
  back <- oc_study_read(p)
  expect_identical(enc2utf8(back$text$prompt), enc2utf8(prompt))
  card <- read_yaml_utf8(p)
  expect_identical(oc_study_hash(back), oc_study_hash(card))
})

test_that("missing fields are reported", {
  card <- yaml::read_yaml(template())
  card$estimand <- NULL
  card$text$prompt <- NULL
  expect_error(oc_study_validate(card), "estimand.*text.prompt")
  card <- yaml::read_yaml(template())
  card$constructs$target <- list(list(name = "x"))
  expect_error(oc_study_validate(card), "constructs.target")
})

test_that("cards without placebos warn", {
  card <- yaml::read_yaml(template())
  card$constructs$placebo <- NULL
  expect_warning(oc_study_validate(card), "placebo")
})

test_that("hash ignores key order", {
  card <- yaml::read_yaml(template())
  expect_identical(oc_study_hash(card), oc_study_hash(rev(card)))
})

test_that("registration is append-only; changes need an amendment", {
  reg <- tempfile("registry")
  p <- tempfile(fileext = ".yaml")
  file.copy(template(), p)
  h1 <- oc_study_register(p, reg)
  expect_message(oc_study_register(p, reg), "already registered")
  card <- yaml::read_yaml(p)
  card$estimand <- "changed"
  yaml::write_yaml(card, p)
  expect_error(oc_study_register(p, reg), "amend")
  h2 <- oc_study_register(p, reg, amend = "clarified estimand")
  expect_false(identical(h1, h2))
  log <- lapply(readLines(file.path(reg, "registry.jsonl")), jsonlite::fromJSON)
  expect_equal(length(log), 2)
  expect_equal(log[[2]]$amendment, "clarified estimand")
  expect_length(list.files(reg, pattern = "\\.yaml$"), 2)
})
