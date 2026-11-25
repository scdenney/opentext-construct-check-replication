make_data <- function(n = 400) {
  data.frame(
    rid = sprintf("R%04d", seq_len(n)),
    treat = rep(c(0, 1), length.out = n),
    lang = rep(c("ko", "zh"), each = n / 2),
    text = paste("response", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

test_that("holdout split is disjoint, complete, and stratified", {
  d <- make_data()
  s <- oc_split(d, "rid", strata = c("treat", "lang"), p_discovery = 0.3,
                seed = 42)
  expect_s3_class(s, "oc_split")
  expect_setequal(s$id, d$rid)
  expect_true(all(s$fold %in% c("D", "E")))
  by_stratum <- table(paste(d$treat, d$lang)[match(s$id, d$rid)], s$fold)
  expect_true(all(by_stratum[, "D"] == round(0.3 * 100)))
})

test_that("split does not depend on row order and is reproducible", {
  d <- make_data()
  s1 <- oc_split(d, "rid", strata = "treat", seed = 7)
  s2 <- oc_split(d[sample(nrow(d)), ], "rid", strata = "treat", seed = 7)
  expect_identical(oc_split_hash(s1), oc_split_hash(s2))
  s3 <- oc_split(d, "rid", strata = "treat", seed = 8)
  expect_false(identical(oc_split_hash(s1), oc_split_hash(s3)))
})

test_that("split leaves the caller's RNG state untouched", {
  set.seed(1)
  a <- runif(1)
  set.seed(1)
  invisible(oc_split(make_data(), "rid", seed = 99))
  expect_identical(runif(1), a)
})

test_that("K splits use different assignments", {
  s <- oc_split(make_data(), "rid", strata = "treat", seed = 1, n_splits = 3)
  expect_equal(sort(unique(s$split)), 1:3)
  f1 <- s$fold[s$split == 1]
  f2 <- s$fold[s$split == 2]
  expect_false(identical(f1, f2))
})

test_that("two-fold split gives balanced A/B halves", {
  s <- oc_split(make_data(), "rid", strata = "treat", seed = 3,
                scheme = "twofold")
  expect_equal(as.vector(table(s$fold)), c(200, 200))
})

test_that("manifest is written once and edits are detected", {
  d <- make_data()
  s <- oc_split(d, "rid", strata = "treat", seed = 42)
  p <- tempfile(fileext = ".json")
  oc_split_write(s, p)
  expect_message(oc_split_write(oc_split(d, "rid", strata = "treat", seed = 42),
                                p), "Identical")
  expect_error(oc_split_write(oc_split(d, "rid", strata = "treat", seed = 43),
                              p), "drawn once")
  r <- oc_split_read(p)
  expect_identical(oc_split_hash(r), oc_split_hash(s))
  j <- jsonlite::fromJSON(p)
  j$assignments$fold[1] <- ifelse(j$assignments$fold[1] == "D", "E", "D")
  writeLines(jsonlite::toJSON(j, auto_unbox = TRUE, digits = NA), p)
  expect_error(oc_split_read(p), "edited")
})

test_that("bad inputs are rejected", {
  d <- make_data()
  expect_error(oc_split(d, "rid"), "seed")
  d2 <- d
  d2$rid[2] <- d2$rid[1]
  expect_error(oc_split(d2, "rid", seed = 1), "unique")
  expect_error(oc_split(d, "rid", strata = "nope", seed = 1), "not found")
})

test_that("split and hash do not depend on the collation locale", {
  ids <- c("R_4cS4dAaQ", "R_4cs4daaq", "r_Zb1", "R_zb1", "R_ab9", "R_AB9",
           sprintf("R_%s", c(letters, LETTERS)))
  d <- data.frame(rid = ids, treat = rep(0:1, length.out = length(ids)))
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
  Sys.setlocale("LC_COLLATE", "C")
  h_c <- oc_split_hash(oc_split(d, "rid", strata = "treat", seed = 42))
  if (nzchar(Sys.setlocale("LC_COLLATE", "en_US.UTF-8"))) {
    expect_identical(oc_split_hash(oc_split(d, "rid", strata = "treat", seed = 42)), h_c)
  } else {
    skip("en_US.UTF-8 collation not available")
  }
})
