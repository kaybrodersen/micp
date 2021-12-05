# Helper functions for the {micp} package.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Safe sigmoid function. Return values below 1e-8 will be set to 1e-8.
#'
#' @param a Numeric scalar or vector.
#'
#' @return Numeric scalar or vector.
#' @noRd
safesigm <- function(a) {
  b = 1 / (1 + exp(-a))
  b[b < 1e-8] <- 1e-8
  return(b)
}

#' Trapezoid rule numerical integration.
#'
#' @param x Support values.
#' @param y Function values.
#'
#' @return
#' @import assertthat
#' @noRd
trapz <- function(x, y) {
  assert_that(is.numeric(x), is.vector(x))
  assert_that(is.numeric(y), is.vector(y))
  assert_that(length(x) >= 2, length(y) == length(x))
  idx <- seq(2, length(x))
  s <- as.double((x[idx] - x[idx - 1]) %*% (y[idx] + y[idx - 1])) / 2
  return(s)
}

#' Replicate a matrix.
#'
#' @param X Vector or matrix.
#' @param m Number of vertical replications.
#' @param n Number of horizontal replications.
#'
#' @return Replicated matrix.
#' @import assertthat
#' @export
repmat <- function(X, m = 1L, n = 1L) {
  if (is.vector(X)) {
    X <- t(as.matrix(X))
  }
  assert_that(m >= 1, n >= 1)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = TRUE)
}
