# Helper functions for the {micp} package.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Safe sigmoid function. Return values below 1e-8 will be set to 1e-8.
#'
#' @param a Numeric scalar or vector.
#'
#' @return Numeric scalar or vector.
#' @export
#'
#' @NoRd
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
#' @export
#'
#' @NoRd
trapz <- function(x, y) {
  assert_that(is.numeric(x), is.vector(x))
  assert_that(is.numeric(y), is.vector(y))
  assert_that(length(x) >= 2, length(y) == length(x))
  idx <- seq(2, length(x))
  s <- as.double((x[idx] - x[idx - 1]) %*% (y[idx] + y[idx - 1])) / 2
  return(s)
}

erf <- function(x) {
  # Error function (see ?pnorm).

  return(2 * stats::pnorm(x * sqrt(2)) - 1)
}

repmat <- function(X, m, n) {
  if (is.vector(X)) X <- t(as.matrix(X))
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X, mx, nx*n)), mx*m, nx*n, byrow=T)
}
