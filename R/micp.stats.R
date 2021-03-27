# Mixed-effects inference on classification performance.
#
# Usage:
#   results <- micp.stats(ks, ns)
#
# Args:
#   When inferring on accuracies, the input arguments are:
#   ks:    m-sized vector of number of correct predictions (in each subject)
#   ns:    m-sized vector of total number of trials (in each subject)
#
#   When inferring on balanced accuracies, the input arguments are:
#   ks:    2xm matrix of correctly predicted positive trials (first row)
#          and corectly predicted negative trials (second row)
#   ns:    2xm matrix of total number of positive (first row) and negative
#          (second row) trials
#
# Returns a list with the following fields:
#   mu:    Posterior mean of the population mean accuracy or balanced accuracy.
#          This is the expected performance of the classifier at the group
#          level.
#   p:     Posterior infraliminal probability of the population mean. This is
#          the posterior belief that the classifier did not operate above chance
#          (50%). A small infraliminal probability represents evidence for
#          above-chance performance.
#   ci:    Posterior 95% credible interval of the population mean. This interval
#          can be used to show error bars around mu.
#   stats: Additional return values, depending on the selected model. See
#          individual inference functions for details.
#
# This function provides an interface to an underlying inference algorithm based
# on Variational Bayes. For details, see the respective help text. For MCMC
# implementations, see the corresponding MATLAB toolbox.
#
# Example:
#   ks <- rbind(c(19, 41, 15, 39, 39), c(41, 46, 43, 48, 37))
#   ns <- rbind(c(45, 51, 20, 46, 58), c(55, 49, 80, 54, 42))
#   results <- micp.stats(ks, ns)
#
# Literature:
#   K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley, J.M. Buhmann, &
#   K.E. Stephan (2013). Variational Bayesian mixed-effects inference for
#   classification studies. NeuroImage (in press).
#   doi:10.1016/j.neuroimage.2013.03.008.
#
#   K.H. Brodersen, C. Mathys, J.R. Chumbley, J. Daunizeau, C.S. Ong, J.M.
#   Buhmann, & K.E. Stephan (2012). Bayesian mixed-effects inference on
#   classification performance in hierarchical datsets. Journal of Machine
#   Learning Research, 13, 3133-3176.
#
#   K.H. Brodersen, C.S. Ong, J.M. Buhmann, & K.E. Stephan (2010). The balanced
#   accuracy and its posterior distribution. ICPR, 3121-3124.
#
# Kay H. Brodersen, ETH Zurich, khbrodersen@gmail.com
# $Id: micp.stats.R 19175 2013-03-26 11:18:48Z bkay $
# ------------------------------------------------------------------------------
micp.stats <- function(ks, ns) {

  # Check input
  r <- check.ks.ns(ks, ns)
  ks <- r$ks
  ns <- r$ns

  # Select appropriate model
  if (is.vector(ks)) model <- "unb.vb"
  if (is.matrix(ks)) model <- "tnb.vb"

  # Inference
  model <- tolower(model)
  switch(model,

    # Univariate normal-binomial model, VB
    unb.vb = {
      assert(is.vector(ks), paste("for inference on accuracies, ks and ns must",
                                  "be vectors"))
      q <- vbicp.unb(ks, ns)
      mu <- logitnmean(q$mu.mu, 1/sqrt(q$eta.mu))
      p <- logitncdf(0.5, q$mu.mu, 1/sqrt(q$eta.mu))
      ci <- c(logitninv(0.025, q$mu.mu, 1/sqrt(q$eta.mu)),
              logitninv(0.975, q$mu.mu, 1/sqrt(q$eta.mu)))
      stats <- list(mu=mu, p=p, ci=ci, q=q)
    },

    # Twofold normal-binomial model, VB
    tnb.vb = {
      assert(is.matrix(ks) && (nrow(ks)==2), paste("for inference on balanced",
        "accuracies, ks and ns must each contain two rows"))
      qp <- vbicp.unb(ks[1, ], ns[1, ])
      qn <- vbicp.unb(ks[2, ], ns[2, ])
      mu <- logitnavgmean(qp$mu.mu, 1/sqrt(qp$eta.mu), qn$mu.mu, 1/sqrt(qn$eta.mu))
      p <- logitnavgcdf(0.5, qp$mu.mu, 1/sqrt(qp$eta.mu), qn$mu.mu, 1/sqrt(qn$eta.mu))
      ci <- c(logitnavginv(0.025, qp$mu.mu, 1/sqrt(qp$eta.mu),
                                  qn$mu.mu, 1/sqrt(qn$eta.mu)),
              logitnavginv(0.975, qp$mu.mu, 1/sqrt(qp$eta.mu),
                                  qn$mu.mu, 1/sqrt(qn$eta.mu)))
      mu.phij <- rep(NA, ncol(ks))
      for (j in 1:ncol(ks))
        mu.phij[j] <- logitnavgmean(qp$mu.rho[j], 1/sqrt(qp$eta.rho[j]),
                                    qn$mu.rho[j], 1/sqrt(qn$eta.rho[j]))
      stats <- list(mu=mu, p=p, ci=ci, qp=qp, qn=qn, mu.phij=mu.phij)
    },
    stop("invalid model - type '?micp.stats' for help")
  )

  # Finalize and return
  stats$model <- tolower(model)
  class(stats) <- "micp"
  return(stats)
}

check.ks.ns <- function(ks, ns) {
  assert(is.vector(ks) || is.matrix(ks), "ks must be a vector or matrix")
  assert(all(ks <= ns), "ks cannot be bigger than ns")
  assert(all(ks >= 0), "ks must be non-negative")
  assert(all(ns >= 0), "ns must be non-negative")
  assert(!all(ns == 0), "ns must not be all zero")
  return(list(ks=ks, ns=ns))
}

summary.micp <- function(stats, ...) {
  print(stats, ...)
}

print.micp <- function(stats, ...) {
  switch(stats$model,

    # Univariate normal-binomial model, VB
    unb.vb = {
      cat("Variational Bayesian mixed-effects inference on classification\n")
      cat("accuracy\n")
      cat("\n")
      cat("Population inference\n")
      cat(paste0("  posterior mean accuracy:    ", round(stats$mu, 2),
          " (p = ", round(stats$p, 5), ")\n"))
      cat(paste0("  posterior 95% interval:     [",
                   round(stats$ci[1], 2), ", ", round(stats$ci[2], 2), "]\n"))
      cat("\n")
      cat("Subject-specific inference\n")
      q <- stats$q
      cat(paste0("  posterior logit means:      ",
                 paste(round(q$mu.rho,  2), collapse=", "), "\n"))
      cat(paste0("  posterior logit precisions: ",
                 paste(round(q$eta.rho, 2), collapse=", "), "\n"))
      cat("\n")
      cat("Bayesian model comparison\n")
      cat(paste0("  free energy F: ", round(stats$q$F,2)))
    },

    tnb.vb = {
      cat("Variational Bayesian mixed-effects inference on the balanced\n")
      cat("classification accuracy\n")
      cat("\n")
      cat("Population inference\n")
      cat(paste0("  posterior mean balanced accuracy:    ",
                   round(stats$mu, 2),
			          " (p = ", round(stats$p, 5), ")\n"))
      cat(paste0("  posterior 95% interval:              [",
                   round(stats$ci[1], 2), ", ", round(stats$ci[2], 2), "]\n"))
      cat("\n")
      cat("Subject-specific inference\n")
      cat(paste0("  posterior balanced accuracy means:   ",
                 paste(round(stats$mu.phij,  2), collapse=", "), "\n"))
      cat("\n")
      cat("Bayesian model comparison\n")
      cat(paste0("  free energy F: ", round(stats$qp$F + stats$qn$F,2)))
    },

    stop("invalid model - did you create this object using micp.stats()?")
  )
}
