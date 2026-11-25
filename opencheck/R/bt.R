#' Sample comparison pairs for Bradley-Terry scaling
#'
#' Each response draws `k` random opponents; duplicates are removed and each
#' pair gets a random order. A share of pairs is held out to test the fitted
#' scale (DiGiuseppe & Flynn 2026; Ornstein's textscale).
#'
#' @param ids Response ids (estimation fold only).
#' @param k Opponents drawn per response.
#' @param holdout Share of pairs held out for validation.
#' @param seed Seed.
oc_pairs <- function(ids, k = 8L, holdout = 0.1, seed) {
  if (missing(seed)) stop("`seed` is required.", call. = FALSE)
  ids <- sort(unique(as.character(ids)), method = "radix")
  n <- length(ids)
  if (n < 3) stop("Need at least three responses.", call. = FALSE)
  with_seed(seed, {
    a <- rep(seq_len(n), each = k)
    b <- vapply(a, function(i) {
      j <- sample.int(n - 1, 1)
      if (j >= i) j + 1L else j
    }, 1L)
    lo <- pmin(a, b)
    hi <- pmax(a, b)
    keep <- !duplicated(paste(lo, hi))
    lo <- lo[keep]
    hi <- hi[keep]
    flip <- stats::runif(length(lo)) < 0.5
    id1 <- ifelse(flip, ids[hi], ids[lo])
    id2 <- ifelse(flip, ids[lo], ids[hi])
    test <- seq_along(id1) %in% sample.int(length(id1),
                                           round(holdout * length(id1)))
  })
  data.frame(pair = sprintf("p%06d", seq_along(id1)), id1 = id1, id2 = id2,
             holdout = test, stringsAsFactors = FALSE)
}

#' Fit a ridge-penalised Bradley-Terry model
#'
#' A port of `fit_bt_fast` from the jev replication archive (DiGiuseppe &
#' Denney): sparse Newton steps, a ridge penalty chosen by Laplace marginal
#' likelihood over a grid, and Laplace standard errors. Outcomes may be soft:
#' `y` is the probability that `id1` has more of the construct than `id2`.
#'
#' @param pairs Data frame with `id1`, `id2`, `y` and optionally `w` weights.
#' @param lambda Penalty; `NULL` to choose it from `grid`.
#' @param grid Candidate penalties.
#' @param tol,maxit Newton convergence settings.
#' @param se `"exact"` (diagonal of the inverse Hessian, as in `fit_bt_fast`),
#'   `"diag"` (1/sqrt of the Hessian diagonal; fast, slightly too small), or
#'   `"auto"`: exact up to 3,000 items, diag above.
#' @param large_n Above this many items the fit uses Newton steps solved by
#'   preconditioned conjugate gradients with a fixed penalty (`lambda`, default
#'   1), because exact sparse factorization of a random comparison graph is
#'   effectively dense.
oc_bt <- function(pairs, lambda = NULL, grid = 2^seq(-8, 8), tol = 1e-8,
                  maxit = 100L, se = c("auto", "exact", "diag"), large_n = 3000L) {
  se <- match.arg(se)
  stopifnot(all(c("id1", "id2", "y") %in% names(pairs)))
  # Ids are labels: numeric-looking ids read from CSV must not become positions.
  pairs$id1 <- as.character(pairs$id1)
  pairs$id2 <- as.character(pairs$id2)
  pairs <- pairs[!is.na(pairs$y) & !is.na(pairs$id1) & !is.na(pairs$id2) &
                   pairs$id1 != pairs$id2, , drop = FALSE]
  ids <- sort(unique(c(pairs$id1, pairs$id2)), method = "radix")
  n <- length(ids)
  i1 <- match(pairs$id1, ids)
  i2 <- match(pairs$id2, ids)
  m <- nrow(pairs)
  w_obs <- if ("w" %in% names(pairs)) pairs$w else rep(1, m)
  X <- Matrix::sparseMatrix(i = c(seq_len(m), seq_len(m)), j = c(i1, i2),
                            x = c(rep(1, m), rep(-1, m)), dims = c(m, n))
  y <- pairs$y

  newton <- function(lam) {
    theta <- rep(0, n)
    for (it in seq_len(maxit)) {
      eta <- as.vector(X %*% theta)
      p <- 1 / (1 + exp(-eta))
      wv <- w_obs * p * (1 - p)
      wv[wv < 1e-10] <- 1e-10
      g <- as.vector(Matrix::crossprod(X, w_obs * (y - p))) - lam * theta
      H <- Matrix::crossprod(X, Matrix::Diagonal(x = wv) %*% X) +
        lam * Matrix::Diagonal(n)
      step <- tryCatch(as.vector(Matrix::solve(H, g)),
                       error = function(e) rep(NA_real_, n))
      if (anyNA(step)) break
      theta <- theta + step
      if (max(abs(step)) < tol) break
    }
    eta <- as.vector(X %*% theta)
    p <- 1 / (1 + exp(-eta))
    p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
    ll <- sum(w_obs * (y * log(p) + (1 - y) * log(1 - p)))
    wv <- w_obs * p * (1 - p)
    wv[wv < 1e-10] <- 1e-10
    H <- Matrix::crossprod(X, Matrix::Diagonal(x = wv) %*% X) +
      lam * Matrix::Diagonal(n)
    list(theta = theta, ll = ll, H = H, lam = lam)
  }

  if (n > large_n) {
    # Large graphs: sparse Cholesky of a random comparison graph fills in to a
    # dense factor (cost ~ n^3 per solve), so use Newton with preconditioned
    # conjugate-gradient solves and a fixed penalty instead of the grid search.
    lam <- lambda %||% 1
    fit <- newton_cg(X, y, w_obs, lam, tol, maxit)
    return(data.frame(id = ids, estimate = fit$theta, se = 1 / sqrt(fit$hdiag),
                      se_type = "diag", lambda = lam, method = "cg",
                      stringsAsFactors = FALSE))
  }
  if (is.null(lambda)) {
    score <- vapply(grid, function(lam) {
      f <- newton(lam)
      ch <- tryCatch(Matrix::Cholesky(f$H, LDL = FALSE),
                     error = function(e) NULL)
      if (is.null(ch)) return(-Inf)
      logdetH <- 2 * sum(log(Matrix::diag(methods::as(ch, "Matrix"))))
      f$ll - 0.5 * lam * sum(f$theta^2) + 0.5 * n * log(lam) - 0.5 * logdetH
    }, numeric(1))
    lambda <- grid[which.max(score)]
    if (lambda %in% range(grid)) {
      warning("The chosen penalty is at the edge of the grid (", lambda,
              "); widen `grid`.", call. = FALSE)
    }
  }
  fit <- newton(lambda)
  exact <- se == "exact" || (se == "auto" && n <= 3000)
  sev <- if (exact) sqrt(Matrix::diag(Matrix::solve(fit$H))) else 1 / sqrt(Matrix::diag(fit$H))
  data.frame(id = ids, estimate = fit$theta, se = as.vector(sev),
             se_type = if (exact) "exact" else "diag",
             lambda = lambda, method = "exact", stringsAsFactors = FALSE)
}

newton_cg <- function(X, y, w_obs, lam, tol, maxit) {
  n <- ncol(X)
  theta <- rep(0, n)
  for (it in seq_len(maxit)) {
    eta <- as.vector(X %*% theta)
    p <- 1 / (1 + exp(-eta))
    wv <- pmax(w_obs * p * (1 - p), 1e-10)
    g <- as.vector(Matrix::crossprod(X, w_obs * (y - p))) - lam * theta
    hdiag <- as.vector(Matrix::crossprod(X^2, wv)) + lam
    hv <- function(v) as.vector(Matrix::crossprod(X, wv * as.vector(X %*% v))) + lam * v
    step <- pcg(hv, g, hdiag, tol = 1e-10, maxit = 500L)
    theta <- theta + step
    if (max(abs(step)) < max(tol, 1e-7)) break
  }
  eta <- as.vector(X %*% theta)
  p <- 1 / (1 + exp(-eta))
  wv <- pmax(w_obs * p * (1 - p), 1e-10)
  list(theta = theta, hdiag = as.vector(Matrix::crossprod(X^2, wv)) + lam)
}

# Preconditioned conjugate gradients for H x = b with a diagonal preconditioner.
pcg <- function(hv, b, mdiag, tol, maxit) {
  x <- rep(0, length(b))
  r <- b
  z <- r / mdiag
  pdir <- z
  rz <- sum(r * z)
  bnorm <- sqrt(sum(b^2))
  for (i in seq_len(maxit)) {
    hp <- hv(pdir)
    alpha <- rz / sum(pdir * hp)
    x <- x + alpha * pdir
    r <- r - alpha * hp
    if (sqrt(sum(r^2)) <= tol * max(bnorm, 1)) break
    z <- r / mdiag
    rz_new <- sum(r * z)
    pdir <- z + (rz_new / rz) * pdir
    rz <- rz_new
  }
  x
}

#' Held-out accuracy and calibration of a Bradley-Terry scale
#'
#' Predicts held-out pairs from the fitted scores. Reports accuracy on pairs
#' with a clear winner and the integrated calibration index (ICI): the mean
#' absolute gap between predicted and loess-smoothed observed probabilities.
#' @param fit Output of [oc_bt()].
#' @param test Held-out pairs with `id1`, `id2`, `y`.
oc_bt_validate <- function(fit, test) {
  th <- stats::setNames(fit$estimate, as.character(fit$id))
  test$id1 <- as.character(test$id1)
  test$id2 <- as.character(test$id2)
  ok <- test$id1 %in% fit$id & test$id2 %in% fit$id & !is.na(test$y)
  test <- test[ok, , drop = FALSE]
  p <- 1 / (1 + exp(-(th[test$id1] - th[test$id2])))
  clear <- abs(test$y - 0.5) > 1e-9
  acc <- mean((p[clear] > 0.5) == (test$y[clear] > 0.5))
  sm <- tryCatch(stats::loess(y ~ p, data = data.frame(y = test$y, p = p)),
                 error = function(e) NULL)
  ici <- if (is.null(sm)) NA_real_ else
    mean(abs(stats::predict(sm, newdata = data.frame(p = p)) - p), na.rm = TRUE)
  data.frame(n_test = nrow(test), accuracy = acc, ici = ici)
}
