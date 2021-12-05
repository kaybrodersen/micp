# An R implementation of the average of two logit-normal densities.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Cumulative distribution function of the average of two logit-normal variables
#'
#' @param x Vector of values.
#' @param mu1 Location parameter of the first distribution.
#' @param sigma1 Scale parameter of the first distribution.
#' @param mu2 Location parameter of the second distribution.
#' @param sigma2 Scale parameter of the second distribution.
#'
#' @return Value of the cumulative distribution function.
#' @import assertthat
#' @export
logitnavgcdf <- function(x, mu1, sigma1, mu2, sigma2) {
  assert_that(is.vector(x))
  assert_that(all(vapply(x, function(x) is.numeric(x) | is.na(x), TRUE)),
              msg = "x is not a numeric or integer vector")
  return(logitnsumcdf(2 * x, mu1, sigma1, mu2, sigma2))
}

.logitnavginv <- function(y, mu1, sigma1, mu2, sigma2) {
  assert_that(is.scalar(y), is.numeric(y) || is.na(y))
  if (isTRUE(is.na(y))) {
    x <- NA_real_
  } else {
    f <- function(z) logitnavgcdf(z, mu1, sigma1, mu2, sigma2) - y
    x <- stats::uniroot(f, c(0, 1))$root
  }
  return(x)
}

#' Inverse cumulative distribution function of the average of two logit-normal
#' variables
#'
#' @param y Vector of quantiles.
#' @param mu1 Location parameter of the first distribution.
#' @param sigma1 Scale parameter of the first distribution.
#' @param mu2 Location parameter of the second distribution.
#' @param sigma2 Scale parameter of the second distribution.
#'
#' @return Value of the inverse cumulative distribution function of the average
#' of two logit-normal variables.
#' @import assertthat
#' @export
logitnavginv <- Vectorize(.logitnavginv)

.logitnavgmean <- function(mu1, sigma1, mu2, sigma2) {
  res <- 0.001
  x <- seq(0, 2, res)
  conv <- logitnconv(res, mu1, sigma1, mu2, sigma2)
  mu <- sum(x * conv / 2) * res
  return(mu)
}

#' Expectation of the average of two logit-normal variables
#'
#' @param mu1 Location parameter of the first distribution.
#' @param sigma1 Scale parameter of the first distribution.
#' @param mu2 Location parameter of the second distribution.
#' @param sigma2 Scale parameter of the second distribution.
#'
#' @return Expectation of the average of two logit-normal variables.
#' @export
logitnavgmean <- Vectorize(.logitnavgmean)
