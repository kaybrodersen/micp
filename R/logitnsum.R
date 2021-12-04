# An R implementation of the convolution (sum) of two logit-normal densities.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Cumulative distribution function of the convolution (sum) of two logit-normal
#' variables
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
#'
#' @examples
logitnsumcdf <- function(x, mu1, sigma1, mu2, sigma2) {
  assert_that(is.vector(x))
  assert_that(all(vapply(x, function(x) is.numeric(x) | is.na(x), TRUE)),
              msg = "x is not a numeric or integer vector")

  # Compute the PDF once, then reuse it for each value in `x`. This is faster
  # than recomputing the PDF each time.
  res <- 0.001
  co <- logitnconv(res, mu1, sigma1, mu2, sigma2)

  # Sum the PDF up to point `x`.
  y <- rep(NA_real_, length(x))
  for (i in seq(length(y))) {
    idx <- round(x[i] / res)
    if (is.na(idx)) {
      y[i] <- NA_real_
    } else if (idx < 1) {
      y[i] <- 0
    } else if (idx > length(co)) {
      y[i] <- 1
    } else {
      y[i] <- trapz(1:idx, co[1:idx]) * res
    }
  }
  return(y)
}
