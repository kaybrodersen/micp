# An R implementation of the logit-normal density.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Logit-normal probability density function
#'
#' @param x Vector of values.
#' @param mu Location parameter.
#' @param sigma Scale parameter.
#'
#' @return Density of the logit-normal distribution.
#' @export
#'
#' @import assertthat
#'
#' @examples
#' logitnpdf(0.5, 1, 2)
#' logitnpdf(c(0, 0.5), c(0, 1), 2)
logitnpdf <- function(x, mu, sigma) {
  assert_that(is.numeric(x), is.numeric(mu), is.numeric(sigma))
  assert_that(all(sigma > 0), msg = "sigma must be positive")
  y <- 1 / (sigma * sqrt(2 * pi)) *
    exp(-((logit(x) - mu) ^ 2 / (2 * sigma ^ 2))) / (x * (1 - x))
  y[is.na(y)] <- 0
  return(y)
}

#' Logit-normal cumulative distribution function
#'
#' @param x Vector of values.
#' @param mu Location parameter.
#' @param sigma Scale parameter.
#'
#' @return Value of the logit-normal cumulative distribution function.
#' @export
#'
#' @import assertthat
#'
#' @examples
#' logitncdf(0.5, 1, 2)
#' logitncdf(c(0, 0.5), c(0, 1), 2)
logitncdf <- function(x, mu, sigma) {
  assert_that(is.numeric(x), is.numeric(mu), is.numeric(sigma))
  assert_that(all(sigma > 0), msg = "sigma must be positive")
  p <- 1/2 * (1 + erf((logit(x) - mu) / (sqrt(2) * sigma)))
  p[x <= 0] <- 0
  p[x >= 1] <- 1
  return(p)
}

.logitninv <- function(p, mu, sigma) {
  assert_that(is.scalar(p), is.scalar(mu), is.scalar(sigma),
              msg = "not yet implemented for vector input")
  assert_that(is.numeric(p), is.numeric(mu), is.numeric(sigma))
  assert_that(sigma > 0, msg = "sigma must be positive")
  if ((p < 0) || (p > 1) || is.na(mu) || is.na(sigma)) {
    x <- NA_real_
  } else if (p == 0) {
    x <- 0
  } else if (p == 1) {
    x <- 1
  } else {
    # Find the root of `logitncdf(x, mu, sigma) - p`.
    f <- function(z) logitncdf(z, mu, sigma) - p
    x = uniroot(f, c(0, 1))$root
  }
  return(x)
}

#' Logit-normal inverse cumulative distribution function
#'
#' @param p Vector of quantiles.
#' @param mu Location parameter.
#' @param sigma Scale parameter.
#'
#' @return Value of the logit-normal inverse cumulative distribution function.
#' @export
#'
#' @import assertthat
#'
#' @examples
#' logitninv(0.5, 1, 2)
#' logitninv(c(0, 0.5), c(0, 1), 2)
logitninv <- Vectorize(.logitninv)

logitnmean <- function(mu, sigma) {
  # Expectation.

  assert((length(mu) == 1 && length(sigma) == 1)
         || (length(mu) > 1 && length(sigma) > 1))

  e <- rep(NA, length(mu))
  grid <- seq(0, 1, by=0.0001)
  for (i in (1:length(mu))) {
      if (sigma[i] <= 0 || is.na(sigma[i]) || is.na(mu[i])) {
        e[i] <- NA
      } else {
        values <- grid * logitnpdf(grid, mu[i], sigma[i])
        e[i] <- trapz(grid, values)
      }
  }
  return(e)
}

logitnconv <- function(res, mu1, sigma1, mu2, sigma2) {
  # Convolution of two densities.

  # Set support
  x <- seq(0, 2, by=res)

  # Individual logit-normal pdfs
  f1 <- logitnpdf(x, mu1, sigma1)
  f2 <- logitnpdf(x, mu2, sigma2)

  # Compute convolution
  y <- conv(f1, f2)

  # Reduce to [0..2] support
  y <- y[1:length(x)]

  # Normalize (so that all values sum to 1/res)
  y <- y / (sum(y) * res)
  return(y)
}
