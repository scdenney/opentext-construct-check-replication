#' Treatment contrast on a text measure
#'
#' Difference in means with the Neyman (HC2-equivalent) standard error and a
#' randomization-inference p-value that permutes treatment within strata. With
#' strata, the estimate is the blocked difference in means: within-stratum
#' differences weighted by stratum size, which stays unbiased when the share
#' treated differs across strata. Strata lacking either arm are dropped. Run it
#' on estimation-fold data only.
#'
#' @param y Outcome: a 0/1 indicator, a probability, or a scale score.
#' @param treat Treatment indicator (0/1).
#' @param strata Optional vector of strata (permutation is within strata).
#' @param n_perm Number of permutations for the RI p-value (0 to skip).
#' @param seed Seed for the permutations.
#' @param level Confidence level.
oc_effect <- function(y, treat, strata = NULL, n_perm = 2000L, seed = 42L,
                      level = 0.95) {
  ok <- !is.na(y) & !is.na(treat)
  y <- as.numeric(y[ok])
  treat <- as.integer(treat[ok])
  if (!all(treat %in% 0:1)) stop("`treat` must be 0/1.", call. = FALSE)
  strata <- if (is.null(strata)) rep(1L, length(y)) else as.character(strata[ok])
  both <- tapply(treat, strata, function(t) any(t == 1) && any(t == 0))
  keep <- strata %in% names(both)[both]
  y <- y[keep]; treat <- treat[keep]; strata <- strata[keep]
  est <- bdm(y, treat, strata)
  n1 <- sum(treat == 1)
  n0 <- sum(treat == 0)
  se <- sqrt(sum(vapply(split(seq_along(y), strata), function(i) {
    w <- length(i) / length(y)
    y1 <- y[i][treat[i] == 1]; y0 <- y[i][treat[i] == 0]
    v1 <- if (length(y1) > 1) stats::var(y1) / length(y1) else 0
    v0 <- if (length(y0) > 1) stats::var(y0) / length(y0) else 0
    w^2 * (v1 + v0)
  }, 0)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  p_ri <- NA_real_
  if (n_perm > 0) {
    null <- with_seed(seed, vapply(seq_len(n_perm), function(b) {
      bdm(y, permute_within(treat, strata), strata)
    }, 0))
    p_ri <- (1 + sum(abs(null) >= abs(est) - 1e-12)) / (n_perm + 1)
  }
  data.frame(estimate = est, se = se, ci_low = est - z * se,
             ci_high = est + z * se, p_ri = p_ri, n1 = n1, n0 = n0)
}

#' Aggregate estimates across random splits
#'
#' Reports the median estimate and the median-adjusted standard error of
#' Chernozhukov et al. (2018), sqrt(median(se_k^2 + (est_k - median)^2)),
#' plus the range of the split estimates, so the result shows how much it
#' depends on which responses built the coding scheme.
#' @param estimates Data frame with columns `estimate` and `se`, one row per
#'   split.
#' @param level Confidence level.
oc_aggregate_splits <- function(estimates, level = 0.95) {
  m <- stats::median(estimates$estimate)
  se <- sqrt(stats::median(estimates$se^2 + (estimates$estimate - m)^2))
  z <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(estimate = m, se = se, ci_low = m - z * se, ci_high = m + z * se,
             min = min(estimates$estimate), max = max(estimates$estimate),
             n_splits = nrow(estimates))
}

dm <- function(y, treat) mean(y[treat == 1]) - mean(y[treat == 0])

# Blocked difference in means: stratum differences weighted by stratum size.
bdm <- function(y, treat, strata) {
  idx <- split(seq_along(y), strata)
  if (length(idx) == 1) return(dm(y, treat))
  sum(vapply(idx, function(i) length(i) * dm(y[i], treat[i]), 0)) / length(y)
}

permute_within <- function(treat, strata) {
  out <- treat
  for (s in unique(strata)) {
    i <- which(strata == s)
    out[i] <- treat[i][sample.int(length(i))]
  }
  out
}
