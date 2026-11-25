#' Cohen's kappa for two raters
#' @param a,b Label vectors of equal length. Pairs with a missing label are
#'   dropped.
oc_kappa <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a <- as.character(a[ok])
  b <- as.character(b[ok])
  lv <- union(a, b)
  tab <- table(factor(a, lv), factor(b, lv))
  n <- sum(tab)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  c(kappa = if (pe == 1) NA_real_ else (po - pe) / (1 - pe), agreement = po,
    n = n)
}

#' Fleiss' kappa for several raters
#' @param labels A data frame or matrix: one row per item, one column per
#'   rater. Items with any missing label are dropped.
oc_fleiss <- function(labels) {
  labels <- as.data.frame(labels, stringsAsFactors = FALSE)
  labels <- labels[stats::complete.cases(labels), , drop = FALSE]
  lv <- sort(unique(unlist(labels)), method = "radix")
  counts <- t(apply(labels, 1, function(r) table(factor(r, lv))))
  m <- ncol(labels)
  n <- nrow(counts)
  p_j <- colSums(counts) / (n * m)
  p_i <- (rowSums(counts^2) - m) / (m * (m - 1))
  pbar <- mean(p_i)
  pe <- sum(p_j^2)
  c(kappa = (pbar - pe) / (1 - pe), n = n, raters = m)
}

#' Majority label across raters
#' @inheritParams oc_fleiss
#' @param min_agree Minimum number of raters that must agree; otherwise `NA`.
oc_majority <- function(labels, min_agree) {
  labels <- as.data.frame(labels, stringsAsFactors = FALSE)
  out <- apply(labels, 1, function(r) {
    r <- r[!is.na(r)]
    if (!length(r)) return(NA_character_)
    tab <- sort(table(r), decreasing = TRUE)
    if (tab[[1]] < min_agree || (length(tab) > 1 && tab[[1]] == tab[[2]])) {
      return(NA_character_)
    }
    names(tab)[1]
  })
  unname(out)
}
