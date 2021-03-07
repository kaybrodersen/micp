# ------------------------------------------------------------------------------
# Variational Bayesian mixed-effects on classification performance
#
# Kay H. Brodersen, ETH Zurich, Switzerland
# $Id: vbicp.unb.R 19175 2013-03-26 11:18:48Z bkay $
# ------------------------------------------------------------------------------
vbicp.unb <- function(ks, ns, verbose = 0) {
  # Variational Bayes algorithm for approximate mixed-effects inference on the
  # classification accuracy using the normal-binomial model.
  #
  # Usage:
  #   q <- vbicp.unb(ks, ns, verbose = 0)
  #
  # Arguments:
  #   ks: vector of correct trials
  #   ns: vector of trial numbers
  #   verbose: level of verbosity (0 or 1)
  #
  # Returns a list of posterior moments:
  #   mu_mu:    mean of the posterior population mean effect
  #   eta_mu:   precision of the posterior population mean effect
  #   a_lamba:  shape parameter of the posterior precision population effect
  #   b_lambda: scale parameter of the posterior precision population effect
  #   mu_rho:   vector of means of the posterior subject-specific effects
  #   eta_rho:  vector of precisions of posterior subject-specific effects
  #
  # Note that all above 'effects' represent accuracies in logit space which
  # has infinite support. In order to obtain, e.g., a posterior-mean estimate
  # of the population accuracy in the conventional [0..1] space, use:
  # logitnmean(q$mu.mu, 1/sqrt(q$eta.mu))
  #
  # Literature:
  #   K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley, J.M. Buhmann, &
  #   K.E. Stephan (2013). Variational Bayesian mixed-effects inference for
  #   classification studies. NeuroImage (in press).
  #   doi:10.1016/j.neuroimage.2013.03.008.

  # Check input
  assert(is.vector(ks), "ks must be a vector")
  assert(is.vector(ns), "ns must be a vector")
  assert((verbose == 0) || (verbose == 1), "invalid verbosity level")

  # Set data
  data        <- list()
  data$ks     <- ks
  data$ns     <- ns
  data$m      <- length(data$ks)

  # Specify prior
  # Prior as of 21/02/2012
  prior       <- list()
  prior$mu.0  <- 0
  prior$eta.0 <- 1
  prior$a.0   <- 1
  prior$b.0   <- 1

  # Initialize posterior
  q <- list()
  q$mu.mu     <- prior$mu.0
  q$eta.mu    <- prior$eta.0
  q$a.lambda  <- prior$a.0
  q$b.lambda  <- prior$b.0
  q$mu.rho    <- rep(0,   data$m)  # hard-coded prior on lower level
  q$eta.rho   <- rep(0.1, data$m)  # hard-coded prior on lower level
  q$F         <- -Inf

  # Begin EM iterations
  max.iter <- 50
  for (i in 1:max.iter) {
    q.old <- q

    # 1st mean-field partition
    q <- update.rho(data, prior, q)

    # 2nd mean-field partition
    q <- update.mu(data, prior, q)

    # 3rd mean-field partition
    q <- update.lambda(data, prior, q)

    # Free energy (q$F)
    q <- free.energy(data, prior, q)

    # Convergence?
    if (abs(q$F - q.old$F) < 1e-3) {
      break
    } else if (i == max.iter) {
      warning(paste0("vbicp.unb: reached maximum EM iterations (",max.iter,")"))
    }
  }
  return(q)
}

# ------------------------------------------------------------------------------
update.rho <- function(data, prior, q) {
  # Updates the 1st mean-field partition

  # Gauss-Newton scheme to find the mode
  # Define Jacobian and Hessian
  dI <-  function(rho) (data$ks - data$ns * safesigm(rho)) +
         q$a.lambda * q$b.lambda * (q$mu.mu * rep(1, data$m) - rho)
  d2I <- function(rho) -diag(data$ns * safesigm(rho) * (1-safesigm(rho))) -
         q$a.lambda * q$b.lambda * diag(data$m)

  # Iterate until convergence to find maximum,
  # then update approximate posterior
  max.iter <- 10
  for (i in 1:max.iter) {
    old.mu.rho <- q$mu.rho

    q$mu.rho <- q$mu.rho - as.vector(solve(d2I(q$mu.rho), dI(q$mu.rho)))

    # Convergence?
    if (sum((q$mu.rho - old.mu.rho)^2) < 1e-3) {
      break
    } else if (i == max.iter) {
      warning(paste0("vbicp.unb: reached maximum GN iterations (",max.iter,")"))
    }
  }

  # Update precision
  q$eta.rho <- t(diag(-d2I(q$mu.rho)))
  return(q)
}

# ------------------------------------------------------------------------------
update.mu <- function(data, prior, q) {
  # Updates the 2nd mean-field partition

  # Update approximate posterior
  q$mu.mu <- (prior$mu.0 * prior$eta.0 +
             q$a.lambda * q$b.lambda *sum(q$mu.rho)) /
             (data$m * q$a.lambda * q$b.lambda + prior$eta.0)
  q$eta.mu <- data$m * q$a.lambda * q$b.lambda + prior$eta.0
  return(q)
}

# ------------------------------------------------------------------------------
update.lambda <- function(data, prior, q) {
  # Updates the 3rd mean-field partition

  # Update approximate posterior
  q$a.lambda <- prior$a.0 + data$m/2
  q$b.lambda <- 1/(1/prior$b.0 + 1/2 * sum((q$mu.rho - q$mu.mu)^2 +
                1/q$eta.rho + 1/q$eta.mu))
  return(q)
}

# ------------------------------------------------------------------------------
free.energy <- function(data, prior, q) {
  # Approximation to the free energy
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
