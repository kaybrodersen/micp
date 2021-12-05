#' Mixed-effects inference on classification performance
#'
#' @param ks When inferring on accuracies: m-sized vector of number of correct
#' predictions (in each subject). When inferring on balanced accuracies: 2xm
#' matrix of correctly predicted positive trials (first row) and corectly
#' predicted negative trials (second row).
#'
#' @param ns When inferring on accuracies: m-sized vector of total number of
#' trials (in each subject). When inferring on balanced accuracies: 2xm matrix
#' of total number of positive (first row) and negative (second row) trials.
#'
#' @return A list with the following fields: mu, p, ci, and stats.
#'
#'   mu: Posterior mean of the population mean accuracy or balanced accuracy.
#'     This is the expected performance of the classifier at the group level.
#'
#'   p: Posterior infraliminal probability of the population mean. This is
#'     the posterior belief that the classifier did not operate above chance
#'     (50%). A small infraliminal probability represents evidence for
#'     above-chance performance.
#'
#'   ci: Posterior 95% credible interval of the population mean. This interval
#'     can be used to show error bars around mu.
#'
#'   stats: Additional return values, depending on the selected model. See
#'     individual inference functions for details.
#'
#' @author Kay H. Brodersen, ETH Zurich
#' @note Literature:
#'   K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley, J.M. Buhmann, &
#'   K.E. Stephan (2013). Variational Bayesian mixed-effects inference for
#'   classification studies. NeuroImage (in press).
#'   doi:10.1016/j.neuroimage.2013.03.008.
#'
#'   K.H. Brodersen, C. Mathys, J.R. Chumbley, J. Daunizeau, C.S. Ong, J.M.
#'   Buhmann, & K.E. Stephan (2012). Bayesian mixed-effects inference on
#'   classification performance in hierarchical datsets. Journal of Machine
#'   Learning Research, 13, 3133-3176.
#'
#'   K.H. Brodersen, C.S. Ong, J.M. Buhmann, & K.E. Stephan (2010). The balanced
#'   accuracy and its posterior distribution. ICPR, 3121-3124.
#'
#' @examples
#'   # Accuracy:
#'   ks <- c(19, 41, 15, 39, 39)
#'   ns <- c(45, 51, 20, 46, 58)
#'   results1 <- micp::micp.stats(ks, ns)
#'   print(results1)
#'
#'   # Balanced accuracy:
#'   ks <- rbind(c(19, 41, 15, 39, 39), c(41, 46, 43, 48, 37))
#'   ns <- rbind(c(45, 51, 20, 46, 58), c(55, 49, 80, 54, 42))
#'   results2 <- micp::micp.stats(ks, ns)
#'   print(results2)
#' @export
micp.stats <- function(ks, ns) {
  check_inputs(ks, ns)

  if (is.vector(ks)) {
    # Univariate normal-binomial model, VB.
    q <- vbicp.unb(ks, ns)
    mu <- logitnmean(q$mu.mu, 1 / sqrt(q$eta.mu))
    p <- logitncdf(0.5, q$mu.mu, 1 / sqrt(q$eta.mu))
    ci <- c(logitninv(0.025, q$mu.mu, 1 / sqrt(q$eta.mu)),
            logitninv(0.975, q$mu.mu, 1 / sqrt(q$eta.mu)))
    stats <- list(mu = mu, p = p, ci = ci, q = q, model = "unb.vb")

  } else if (is.matrix(ks)) {
    # Twofold normal-binomial model, VB.
    qp <- vbicp.unb(ks[1, ], ns[1, ])
    qn <- vbicp.unb(ks[2, ], ns[2, ])
    mu <- logitnavgmean(qp$mu.mu, 1 / sqrt(qp$eta.mu),
                        qn$mu.mu, 1 / sqrt(qn$eta.mu))
    p <- logitnavgcdf(0.5, qp$mu.mu, 1 / sqrt(qp$eta.mu),
                      qn$mu.mu, 1 / sqrt(qn$eta.mu))
    ci <- c(logitnavginv(0.025, qp$mu.mu, 1 / sqrt(qp$eta.mu),
                                qn$mu.mu, 1 / sqrt(qn$eta.mu)),
            logitnavginv(0.975, qp$mu.mu, 1 / sqrt(qp$eta.mu),
                                qn$mu.mu, 1 / sqrt(qn$eta.mu)))
    mu.phij <- rep(NA, ncol(ks))
    for (j in seq_len(ncol(ks))) {
      mu.phij[j] <- logitnavgmean(qp$mu.rho[j], 1 / sqrt(qp$eta.rho[j]),
                                  qn$mu.rho[j], 1 / sqrt(qn$eta.rho[j]))
    }
    stats <- list(mu = mu, p = p, ci = ci, qp = qp, qn = qn, mu.phij = mu.phij,
                  model = "tnb.vb")
  } else {
    stop("unexpected input")
  }
  class(stats) <- "micp"
  return(stats)
}

check_inputs <- function(ks, ns) {
  assert_that(
    (is.vector(ks) && is.vector(ns) && identical(length(ks), length(ns))) ||
      (is.matrix(ks) && is.matrix(ns) && identical(dim(ks), dim(ns))),
    msg = "ks and ns must have same dimensions")
  assert_that(all(ks <= ns), msg = "ks cannot be bigger than ns")
  assert_that(all(ks >= 0), msg = "ks must be non-negative")
  assert_that(all(ns >= 0), msg = "ns must be non-negative")
  assert_that(any(ns > 0), msg = "ns must not be all zero")
}

summary.micp <- function(stats, ...) {
  print(stats, ...)
}

#' Prints a summary of the inference
#'
#' @param x An object returned by `micp.stats()`.
#' @param ... Optional additional arguments (currently unused).
#'
#' @return The printed string.
#' @import assertthat
#' @export
#'
#' @examples
#'   ks <- c(19, 41, 15, 39, 39)
#'   ns <- c(45, 51, 20, 46, 58)
#'   results <- micp::micp.stats(ks, ns)
#'   print(results)
print.micp <- function(x, ...) {
  stats <- x
  assert_that(is.string(stats$model))
  summary <- switch(stats$model,
    unb.vb = paste0(
      "Variational Bayesian mixed-effects inference on classification\n",
      "accuracy\n\n",
      "Population inference\n",
      "  posterior mean accuracy:    ", round(stats$mu, 2),
      " (p = ", round(stats$p, 5), ")\n",
      "  posterior 95% interval:     [",
      round(stats$ci[1], 2), ", ", round(stats$ci[2], 2), "]\n\n",
      "Subject-specific inference\n",
      "  posterior logit means:      ",
      paste(round(stats$q$mu.rho,  2), collapse=", "), "\n",
      "  posterior logit precisions: ",
      paste(round(stats$q$eta.rho, 2), collapse=", "), "\n\n",
      "Bayesian model comparison\n",
      "  free energy F: ", round(stats$q$F,2)),
    tnb.vb = paste0(
      "Variational Bayesian mixed-effects inference on the balanced\n",
      "classification accuracy\n\n",
      "Population inference\n",
      "  posterior mean balanced accuracy:  ",
      round(stats$mu, 2), " (p = ", round(stats$p, 5), ")\n",
      "  posterior 95% interval:            [",
      round(stats$ci[1], 2), ", ", round(stats$ci[2], 2), "]\n\n",
      "Subject-specific inference\n",
      "  posterior balanced accuracy means: ",
      paste(round(stats$mu.phij,  2), collapse=", "), "\n\n",
      "Bayesian model comparison\n",
      "  free energy F: ", round(stats$qp$F + stats$qn$F,2)),
    stop("invalid model - object must be a result of micp.stats()")
  )
  cat(summary)
  invisible(summary)
}
