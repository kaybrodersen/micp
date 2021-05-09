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
#' @export
#'
#' @examples
logitnavgcdf <- function(x, mu1, sigma1, mu2, sigma2) {
  assert_that(is.vector(x), is.numeric(x))
  return(logitnsumcdf(2 * x, mu1, sigma1, mu2, sigma2))
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
#' @export
#'
#' @examples
logitnavginv <- function(y, mu1, sigma1, mu2, sigma2) {
  if (is.na(y)) return(NA)
  x <- rep(NA, length(mu1))
  for (i in (1:length(mu1))) {
    f <- function(z) logitnavgcdf(z, mu1[i], sigma1[i], mu2[i], sigma2[i]) - y
    x[i] <- uniroot(f, c(0, 1))$root
  }
  return(x)
}

# TODO: Clean up and vectorize logitnavginv().

#' Expectation of the average of two logit-normal variables
#'
#' @param mu1 Location parameter of the first distribution.
#' @param sigma1 Scale parameter of the first distribution.
#' @param mu2 Location parameter of the second distribution.
#' @param sigma2 Scale parameter of the second distribution.
#'
#' @return
#' @export
#'
#' @examples
logitnavgmean <- function(mu1, sigma1, mu2, sigma2) {
  if (is.na(mu1) || is.na(sigma1)) return(NA)
  if (is.na(mu2) || is.na(sigma2)) return(NA)
  if ((sigma1 < 0) || (sigma2 < 0)) return(NA)

  assert(all(sigma1 > 0) && all(sigma2 > 0))
  assert(((length(mu1) == 1) && (length(sigma1) == 1))
    || (length(mu1) > 0 && length(sigma1) > 0 && (dim(mu1) == dim(sigma1))))
  assert(((length(mu2) == 1) && (length(sigma2) == 1))
    || (length(mu2) > 0 && length(sigma2) > 0 && (dim(mu2) == dim(sigma2))))

  res <- 0.001;
  x <- seq(0, 2, by=res);

  mu.phi <- rep(NA, length(mu1));
  for (i in (1:length(mu1))) {
    c <- logitnconv(res, mu1[i], sigma1[i], mu2[i], sigma2[i]);
    mu.phi[i] <- sum(x*c/2) * res;
  }
  return(mu.phi)
}
