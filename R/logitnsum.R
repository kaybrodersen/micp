# An R implementation of the convolution (sum) of two logit-normal densities.
#
# Author: Kay H. Brodersen, ETH Zurich

logitnsumcdf <- function(x, mu1, sigma1, mu2, sigma2) {
  # Cumulative distribution function.

  # Compute the PDF first (since we want the entire pdf rather than just one
  # value from it, using betaconv is computationally more efficient than using
  # betasumpdf)
  res <- 0.001
  co <- logitnconv(res, mu1, sigma1, mu2, sigma2)

  # Sum the PDF up to point x
  y <- rep(NA, length(x))
  for (i in (1:length(x))) {
    idx <- round(x[i] / res)
    if (idx < 1) {
      y[i] <- 0
    } else if (idx > length(co)) {
      y[i] <- 1
    } else {
      y[i] <- trapz(1:idx, co[1:idx]) * res
    }
  }
  return(y)
}
