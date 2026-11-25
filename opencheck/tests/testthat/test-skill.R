skill_dir <- function() {
  d <- system.file("skills", "opencheck", package = "opencheck")
  if (!nzchar(d)) d <- test_path("..", "..", "inst", "skills", "opencheck")
  d
}

skill_files <- function() {
  file.path(skill_dir(), c("SKILL.md", "reference/interpreting.md",
                           "reference/pipeline.R"))
}

test_that("the skill files are found and the frontmatter names the skill", {
  expect_true(all(file.exists(skill_files())))
  lines <- read_lines_utf8(skill_files()[1])
  expect_identical(lines[1], "---")
  end <- which(lines == "---")[2]
  fm <- yaml::yaml.load(paste(lines[2:(end - 1)], collapse = "\n"))
  expect_identical(fm$name, "opencheck")
  expect_type(fm$description, "character")
  expect_gt(nchar(fm$description), 50)
  expect_lte(nchar(fm$description), 1024)
})

test_that("every oc_ function the skill names is exported", {
  txt <- unlist(lapply(skill_files(), read_lines_utf8))
  named <- unique(unlist(regmatches(txt, gregexpr("oc_[a-z0-9_]+", txt))))
  expect_gt(length(named), 10)
  expect_identical(setdiff(named, getNamespaceExports("opencheck")),
                   character(0))
})

test_that("oc_skill_install copies, refuses to overwrite, and overwrites when asked", {
  dest <- file.path(tempfile("skills"), "opencheck")
  expect_message(out <- oc_skill_install(path = dest), "Installed")
  expect_identical(out, dest)
  expect_true(file.exists(file.path(dest, "SKILL.md")))
  expect_true(file.exists(file.path(dest, "reference", "pipeline.R")))
  writeLines("changed", file.path(dest, "SKILL.md"))
  writeLines("left over", file.path(dest, "stale.txt"))
  expect_error(oc_skill_install(path = dest), "overwrite = TRUE")
  expect_identical(readLines(file.path(dest, "SKILL.md")), "changed")
  suppressMessages(oc_skill_install(path = dest, overwrite = TRUE))
  expect_identical(read_lines_utf8(file.path(dest, "SKILL.md")),
                   read_lines_utf8(skill_files()[1]))
  expect_false(file.exists(file.path(dest, "stale.txt")))
})

test_that("the pipeline runs end to end on the synthetic example", {
  skip_if_not_installed("stm")
  skip_if_not_installed("quanteda")
  skip_if_not_installed("stopwords")
  dir <- tempfile("pipeline")
  dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  env <- new.env()
  utils::capture.output(suppressMessages(
    source(skill_files()[3], local = env)))
  res <- env$results
  expect_named(res, c("contrast", "construct", "role", "expected", "estimate",
                      "se", "ci_low", "ci_high", "p_ri", "n_treated",
                      "n_control", "control_share"))
  target <- res[res$role == "target", ]
  placebo <- res[res$role == "placebo", ]
  expect_equal(nrow(target), 1)
  expect_gt(target$estimate, 0)
  expect_lt(target$p_ri, 0.05)
  expect_false(placebo$estimate > 0 && placebo$p_ri < 0.05)
  expect_true(all(file.exists(file.path(dir, "opencheck_run",
    c("study_card.yaml", "split.json", "lock.json", "access_log.jsonl",
      "cache_estimation.jsonl", "results.csv")))))
})
