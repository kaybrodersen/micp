# Variational Bayesian mixed-effects on classification performance.
#
# Author: Kay H. Brodersen, ETH Zurich

#' Variational Bayesian approximate mixed-effects inference on classification
#' accuracy using the normal-binomial model
#'
#' @param ks Vector of successes in each population member.
#' @param ns Vector of attempts in each population member.
#' @param verbose Level of output verbosity: 0 or 1.
#'
#' @return A list of posterior moments:
#'   mu_mu:    mean of the posterior population mean effect
#'   eta_mu:   precision of the posterior population mean effect
#'   a_lamba:  shape parameter of the posterior precision population effect
#'   b_lambda: scale parameter of the posterior precision population effect
#'   mu_rho:   vector of means of the posterior subject-specific effects
#'   eta_rho:  vector of precisions of posterior subject-specific effects
#'
#' @details The return 'effects' represent accuracies in logit space, which
#' has infinite support. In order to obtain, e.g., a posterior-mean estimate
#' of the population accuracy in the conventional [0..1] space, use:
#' `logitnmean(q$mu.mu, 1/sqrt(q$eta.mu))`
#'
#' @export
#'
#' @examples
#' q <- vbicp.unb(c(6, 8, 5), c(10, 10, 10), verbose = 0)
#'
#' @references
#'   K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley, J.M. Buhmann, &
#'   K.E. Stephan (2013). Variational Bayesian mixed-effects inference for
#'   classification studies. NeuroImage (2013).
#'   doi:10.1016/j.neuroimage.2013.03.008.
#'   https://kaybrodersen.github.io/publications/Brodersen_2013_NeuroImage.pdf
vbicp.unb <- function(ks, ns, verbose = 0) {
  # Check inputs.
  assert_that(is.vector(ks), is.numeric(ks))
  assert_that(is.vector(ns), is.numeric(ns))
  assert_that(length(ks) == length(ns))
  assert_that(is.scalar(verbose), verbose %in% c(0, 1))

  # Set algorithm constants.
  kMaxIter <- 50
  kConvergence <- 1e-3

  # Set data.
  data <- list(
    ks = ks,
    ns = ns,
    m = length(ks)
  )

  # Specify prior.
  # Prior as of 2021-02-21.
  prior <- list(
    mu.0 = 0,
    eta.0 = 1,
    a.0 = 1,
    b.0 = 1
  )

  # Initialize posterior.
  q <- list(
    mu.mu = prior$mu.0,
    eta.mu = prior$eta.0,
    a.lambda = prior$a.0,
    b.lambda = prior$b.0,
    # Hard-coded prior on lower level.
    mu.rho = rep(0, data$m),
    # Hard-coded prior on lower level.
    eta.rho = rep(0.1, data$m),
    F = -Inf
  )

  # Begin EM iterations.
  for (i in seq(kMaxIter)) {
    q.old <- q

    # 1st mean-field partition.
    q <- update.rho(data, prior, q)

    # 2nd mean-field partition.
    q <- update.mu(data, prior, q)

    # 3rd mean-field partition.
    q <- update.lambda(data, prior, q)

    # Free energy (q$F).
    q <- update.free.energy(data, prior, q)

    # Convergence?
    if (abs(q$F - q.old$F) < kConvergence) {
      break
    } else if (i == kMaxIter) {
      warning("vbicp.unb: reached maximum EM iterations (", kMaxIter, ")")
    }
  }
  return(q)
}

#' Update the 1st mean-field partition (rho)
#'
#' @param data List of `ks`, `ns`, and `m`.
#' @param prior List of `mu.0`, `eta.0`, `a.0`, `b.0`.
#' @param q List of `mu.mu`, `eta.mu`, `a.lambda`, `b.lambda`, `mu.rho`,
#'   `eta.rho`, `F`.
#'
#' @return A revised `q` with updated `mu.rho` and `eta.rho` elements.
update.rho <- function(data, prior, q) {
  # Gauss-Newton scheme to find the mode.
  # Define Jacobian and Hessian.
  dI <- function(rho) (data$ks - data$ns * safesigm(rho)) +
    q$a.lambda * q$b.lambda * (q$mu.mu * rep(1, data$m) - rho)
  d2I <- function(rho) -diag(data$ns * safesigm(rho) * (1 - safesigm(rho))) -
    q$a.lambda * q$b.lambda * diag(data$m)

  # Iterate until convergence to find maximum,
  # then update approximate posterior.
  max_iter <- 10
  for (i in seq(max_iter)) {
    old.mu.rho <- q$mu.rho

    # Update mean.
    q$mu.rho <- q$mu.rho - as.vector(solve(d2I(q$mu.rho), dI(q$mu.rho)))

    # Convergence?
    if (sum((q$mu.rho - old.mu.rho)^2) < 1e-3) {
      break
    } else if (i == max_iter) {
      warning("vbicp.unb: reached maximum GN iterations (", max_iter, ")")
    }
  }

  # Update precision.
  q$eta.rho <- t(diag(-d2I(q$mu.rho)))
  return(q)
}

#' Update the 2nd mean-field partition (mu)
#'
#' @param data List of `ks`, `ns`, and `m`.
#' @param prior List of `mu.0`, `eta.0`, `a.0`, `b.0`.
#' @param q List of `mu.mu`, `eta.mu`, `a.lambda`, `b.lambda`, `mu.rho`,
#'   `eta.rho`, `F`.
#'
#' @return A revised `q` with updated `mu.mu` and `eta.mu` elements.
update.mu <- function(data, prior, q) {
  q$mu.mu <- (prior$mu.0 * prior$eta.0 +
             q$a.lambda * q$b.lambda *sum(q$mu.rho)) /
             (data$m * q$a.lambda * q$b.lambda + prior$eta.0)
  q$eta.mu <- data$m * q$a.lambda * q$b.lambda + prior$eta.0
  return(q)
}

#' Update the 3rd mean-field partition (lambda)
#'
#' @param data List of `ks`, `ns`, and `m`.
#' @param prior List of `mu.0`, `eta.0`, `a.0`, `b.0`.
#' @param q List of `mu.mu`, `eta.mu`, `a.lambda`, `b.lambda`, `mu.rho`,
#'   `eta.rho`, `F`.
#'
#' @return A revised `q` with updated `a.lambda` and `b.lambda` elements.
update.lambda <- function(data, prior, q) {
  q$a.lambda <- prior$a.0 + data$m/2
  q$b.lambda <- 1/(1/prior$b.0 + 1/2 * sum((q$mu.rho - q$mu.mu)^2 +
                1/q$eta.rho + 1/q$eta.mu))
  return(q)
}

#' Compute an approximation to the free-energy
#'
#' @param data List of `ks`, `ns`, and `m`.
#' @param prior List of `mu.0`, `eta.0`, `a.0`, `b.0`.
#' @param q List of `mu.mu`, `eta.mu`, `a.lambda`, `b.lambda`, `mu.rho`,
#'   `eta.rho`, `F`.
#'
#' @return A revised `q` with an updated `F` field.
update.free.energy <- function(data, prior, q) {
  q$F <- 1/2*(log(prior$eta.0) - log(q$eta.mu)) -
         prior$eta.0/2*((q$mu.mu-prior$mu.0)^2 + 1/q$eta.mu) + q$a.lambda -
         prior$a.0*log(prior$b.0) + lgamma(q$a.lambda)-lgamma(prior$a.0) -
         q$a.lambda*q$b.lambda*(1/prior$b.0 + data$m/(2*q$eta.mu)) +
         (prior$a.0 + data$m/2)*log(q$b.lambda) +
         (prior$a.0 - q$a.lambda + data$m/2)*digamma(q$a.lambda) + 1/2 +
         sum(log(dbinom(data$ks, data$ns, safesigm(q$mu.rho))) -
          1/2*q$a.lambda*q$b.lambda*(q$mu.rho-q$mu.mu)^2 -
          1/2*log(q$eta.rho));
  return(q)
}
