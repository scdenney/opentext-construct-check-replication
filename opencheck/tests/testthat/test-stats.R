test_that("Cohen's kappa matches a hand calculation", {
  a <- c(rep("x", 20), rep("y", 5), rep("x", 10), rep("y", 15))
  b <- c(rep("x", 20), rep("x", 5), rep("y", 10), rep("y", 15))
  # po = 35/50; pe = (30*25 + 20*25)/50^2 = 0.5
  expect_equal(unname(oc_kappa(a, b)["kappa"]), (0.7 - 0.5) / 0.5)
  expect_equal(unname(oc_kappa(a, a)["kappa"]), 1)
})

test_that("Fleiss' kappa is 1 under perfect agreement and handles 3 raters", {
  m <- data.frame(r1 = c("a", "b", "a", "c"), r2 = c("a", "b", "a", "c"),
                  r3 = c("a", "b", "a", "c"))
  expect_equal(unname(oc_fleiss(m)["kappa"]), 1)
  m$r3[1] <- "b"
  expect_lt(unname(oc_fleiss(m)["kappa"]), 1)
})

test_that("majority vote applies the threshold and refuses ties", {
  m <- data.frame(r1 = c("a", "a", "a"), r2 = c("a", "b", "b"),
                  r3 = c("a", "b", "c"), r4 = c("b", "a", "d"))
  expect_identical(oc_majority(m, min_agree = 3), c("a", NA, NA))
  expect_identical(oc_majority(m, min_agree = 2), c("a", NA, NA))
  m$r4[2] <- "b"
  expect_identical(oc_majority(m, min_agree = 3)[2], "b")
})

test_that("effect estimate, SE and RI p-value behave", {
  set.seed(10)
  n <- 2000
  treat <- rep(0:1, n / 2)
  y <- rbinom(n, 1, 0.2 + 0.1 * treat)
  e <- oc_effect(y, treat, n_perm = 500, seed = 1)
  expect_equal(e$estimate, mean(y[treat == 1]) - mean(y[treat == 0]))
  expect_equal(e$se, sqrt(var(y[treat == 1]) / 1000 + var(y[treat == 0]) / 1000))
  expect_lt(e$p_ri, 0.01)
  y0 <- rbinom(n, 1, 0.2)
  expect_gt(oc_effect(y0, treat, n_perm = 500, seed = 1)$p_ri, 0.01)
})

test_that("split aggregation uses the median and widens for disagreement", {
  est <- data.frame(estimate = c(0.10, 0.12, 0.14), se = c(0.02, 0.02, 0.02))
  a <- oc_aggregate_splits(est)
  expect_equal(a$estimate, 0.12)
  expect_equal(a$se, sqrt(median(0.02^2 + c(0.02, 0, 0.02)^2)))
  expect_equal(c(a$min, a$max), c(0.10, 0.14))
})

test_that("pair sampler gives unique, non-self, partly held-out pairs", {
  p <- oc_pairs(sprintf("r%03d", 1:200), k = 8, holdout = 0.1, seed = 3)
  expect_false(any(p$id1 == p$id2))
  key <- paste(pmin(p$id1, p$id2), pmax(p$id1, p$id2))
  expect_false(anyDuplicated(key) > 0)
  expect_equal(mean(p$holdout), 0.1, tolerance = 0.01)
  expect_identical(p, oc_pairs(sprintf("r%03d", 1:200), k = 8, seed = 3))
})

test_that("Bradley-Terry recovers simulated scores and predicts held-out pairs", {
  set.seed(4)
  ids <- sprintf("r%03d", 1:300)
  theta <- setNames(rnorm(300), ids)
  p <- oc_pairs(ids, k = 10, holdout = 0.1, seed = 5)
  prob <- 1 / (1 + exp(-(theta[p$id1] - theta[p$id2])))
  p$y <- rbinom(nrow(p), 1, prob)
  fit <- oc_bt(p[!p$holdout, ])
  expect_gt(cor(fit$estimate, theta[fit$id]), 0.85)
  v <- oc_bt_validate(fit, p[p$holdout, ])
  expect_gt(v$accuracy, 0.65)
  expect_lt(v$ici, 0.1)
  soft <- p
  soft$y <- prob
  expect_gt(cor(oc_bt(soft[!soft$holdout, ])$estimate, theta[sort(ids)]), 0.95)
})

test_that("the conjugate-gradient path matches the exact fit at a fixed penalty", {
  set.seed(9)
  ids <- sprintf("r%03d", 1:300)
  theta <- setNames(rnorm(300), ids)
  p <- oc_pairs(ids, k = 8, seed = 2)
  p$y <- plogis(theta[p$id1] - theta[p$id2])
  ex <- oc_bt(p, lambda = 1)
  cg <- oc_bt(p, lambda = 1, large_n = 10)
  expect_identical(cg$method[1], "cg")
  expect_lt(max(abs(ex$estimate - cg$estimate)), 1e-5)
})

test_that("blocked estimate removes confounding by stratum", {
  set.seed(3)
  s <- rep(c("a", "b"), each = 1000)
  treat <- c(rbinom(1000, 1, 0.2), rbinom(1000, 1, 0.8))   # share treated differs
  y <- ifelse(s == "b", 5, 0) + rnorm(2000)                # no treatment effect
  e <- oc_effect(y, treat, strata = s, n_perm = 200, seed = 1)
  expect_lt(abs(e$estimate), 0.2)
  expect_gt(e$p_ri, 0.01)
  pooled <- mean(y[treat == 1]) - mean(y[treat == 0])
  expect_gt(abs(pooled), 2)
})

test_that("numeric-looking ids are treated as labels, not positions", {
  set.seed(5)
  ids <- as.character(100001:100200)
  theta <- setNames(rnorm(200), ids)
  p <- oc_pairs(ids, k = 8, holdout = 0.1, seed = 4)
  p$y <- plogis(theta[p$id1] - theta[p$id2])
  f <- tempfile(fileext = ".csv")
  write.csv(p, f, row.names = FALSE)
  q <- read.csv(f)                      # ids come back as integers
  fit <- oc_bt(q[!q$holdout, ])
  v <- oc_bt_validate(fit, q[q$holdout, ])
  expect_false(is.na(v$accuracy))
  expect_gt(v$accuracy, 0.7)
})
