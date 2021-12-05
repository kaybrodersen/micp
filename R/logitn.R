# An R implementation of the logit-normal density.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Logit transform
#'
#' @param b Numeric vector.
#' @return Logit transform of the input vector.
#' @noRd
logit <- function(x) {
  y <- x
  y[y < 0 | y > 1] <- NA_real_
  y <- log(y / (1 - y))
  return(y)
}

#' Equivalent of the MATLAB conv() function for discrete convolution
#'
#' @param u
#' @param v
#' @return Value of the convolution.
#' @noRd
conv <- function(u, v) {
  return(stats::convolve(u, rev(v), type = "open"))
}

#' Error function.
#'
#' @param x Real-valued scalar or vector.
#' @return Value of the error function (see also ?pnorm).
#' @noRd
erf <- function(x) {
  return(2 * stats::pnorm(x * sqrt(2)) - 1)
}

#' Logit-normal probability density function
#'
#' @param x Vector of values.
#' @param mu Location parameter.
#' @param sigma Scale parameter.
#'
#' @return Density of the logit-normal distribution.
#'
#' @import assertthat
#' @export
#'
#' @examples
#' logitnpdf(0.5, 1, 2)
#' logitnpdf(c(0, 0.5), c(0, 1), 2)
logitnpdf <- function(x, mu, sigma) {
  y <- 1 / (sigma * sqrt(2 * pi)) *
    exp(-((logit(x) - mu) ^ 2 / (2 * sigma ^ 2))) / (x * (1 - x))
  y[is.na(y)] <- 0
  y[is.na(x + mu + sigma)] <- NA_real_
  y[sigma < 0] <- NaN
  return(y)
}

#' Logit-normal cumulative distribution function
#'
#' @param x Vector of values.
#' @param mu Location parameter.
#' @param sigma Scale parameter.
#'
#' @return Value of the logit-normal cumulative distribution function.
#'
#' @import assertthat
#' @export
#'
#' @examples
#' logitncdf(0.5, 1, 2)
#' logitncdf(c(0, 0.5), c(0, 1), 2)
logitncdf <- function(x, mu, sigma) {
  p <- 1/2 * (1 + erf((logit(x) - mu) / (sqrt(2) * sigma)))
  p[x <= 0] <- 0
  p[x >= 1] <- 1
  p[sigma < 0] <- NaN
  return(p)
}

.logitninv <- function(p, mu, sigma) {
  assert_that(is.scalar(p), is.numeric(p) || is.na(p))
  assert_that(is.scalar(mu), is.numeric(mu) || is.na(mu))
  assert_that(is.scalar(sigma), is.numeric(sigma) || is.na(sigma))
  assert_that(is.na(sigma) || sigma > 0, msg = "sigma must be positive")
  if ((p < 0) || (p > 1) || is.na(mu) || is.na(sigma)) {
    x <- NA_real_
  } else if (p == 0) {
    x <- 0
  } else if (p == 1) {
    x <- 1
  } else {
    # Find the root of `logitncdf(x, mu, sigma) - p`.
    f <- function(z) logitncdf(z, mu, sigma) - p
    x = stats::uniroot(f, c(0, 1))$root
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
#'
#' @import assertthat
#' @export
#'
#' @examples
#' logitninv(0.5, 1, 2)
#' logitninv(c(0, 0.5), c(0, 1), 2)
logitninv <- Vectorize(.logitninv)

.logitnmean <- function(mu, sigma) {
  assert_that(is.scalar(mu), is.numeric(mu) || is.na(mu))
  assert_that(is.scalar(sigma), is.numeric(sigma) || is.na(sigma))
  if (is.na(mu) || is.na(sigma)) {
    e <- NA_real_
  } else {
    grid <- seq(0, 1, by = 0.0001)
    values <- grid * logitnpdf(grid, mu, sigma)
    e <- trapz(grid, values)
  }
  return(e)
}

#' Expectation of the logit-normal distribution
#'
#' @param mu Location parameter.
#' @param sigma Scale parameter.
#'
#' @return Expectation.
#'
#' @import assertthat
#' @export
#'
#' @examples
#' logitnmean(0, 0.6)  # 0.5
logitnmean <- Vectorize(.logitnmean)

#' Convolution of two logit-normal distributions
#'
#' @param res Desired resolution, expressed as distribution support per data
#' point. Since the support is [0, 2], a resolution of 0.1 returns 21 points.
#' @param mu1 Location parameter of the first distribution.
#' @param sigma1 Scale parameter of the first distribution.
#' @param mu2 Location parameter of the second distribution.
#' @param sigma2 Scale parameter of the second distribution.
#'
#' @return A vector of values containing the density of the convolution of two
#' logit-normal distributions, evaluated on a grid of values between 0 and 2 at
#' the specified resolution.
#'
#' @import assertthat
#' @noRd
logitnconv <- function(res, mu1, sigma1, mu2, sigma2) {
  assert_that(is.scalar(res), is.numeric(res), !is.na(res), res > 0, res < 1)
  assert_that(is.scalar(mu1), is.scalar(sigma1))
  assert_that(is.scalar(mu2), is.scalar(sigma2))

  # Set support.
  x <- seq(0, 2, res)

  # Individual logit-normal PDFs.
  f1 <- logitnpdf(x, mu1, sigma1)
  f2 <- logitnpdf(x, mu2, sigma2)

  # Compute convolution.
  y <- conv(f1, f2)

  # Reduce to [0, 2] support.
  y <- y[1:length(x)]

  # Normalize (so that values sum to 1/res).
  y <- y / (sum(y) * res)
  return(y)
}
