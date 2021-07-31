# Helper functions for the {micp} package.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Logit transform
#'
#' @param b Numeric vector.
#'
#' @return Logit transform of the input vector.
#'
#' @NoRd
logit <- function(b) {
  # Logit transform.

  b[b < 0 | b > 1] <- NA_real_
  a <- log(b / (1 - b));
  return(a)
}

safesigm <- function(a) {
  # Safe sigmoid function. Return values below 1e-8 will be set to 1e-8.

  b = 1 / (1 + exp(-a))
  b[b < 1e-8] <- 1e-8
  return(b)
}

trapz <- function(x, y) {
  # Trapezoid rule numerical integration

  idx <- 2:length(x)
  s <- as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2
  return(s)
}

conv <- function(u, v) {
  # Equivalent of the MATLAB conv() function for discrete convolution

  return(stats::convolve(u, rev(v), type = "open"))
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
