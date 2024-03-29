% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vbicp.unb.R
\name{vbicp.unb}
\alias{vbicp.unb}
\title{Variational Bayesian approximate mixed-effects inference on classification
accuracy using the normal-binomial model}
\usage{
vbicp.unb(ks, ns, verbose = NULL)
}
\arguments{
\item{ks}{Vector of successes in each population member.}

\item{ns}{Vector of attempts in each population member.}

\item{verbose}{Level of output verbosity. Deprecated parameter.}
}
\value{
A list of posterior moments:

  mu_mu:    mean of the posterior population mean effect

  eta_mu:   precision of the posterior population mean effect

  a_lamba:  shape parameter of the posterior precision population effect

  b_lambda: scale parameter of the posterior precision population effect

  mu_rho:   vector of means of the posterior subject-specific effects

  eta_rho:  vector of precisions of posterior subject-specific effects
}
\description{
Variational Bayesian approximate mixed-effects inference on classification
accuracy using the normal-binomial model
}
\details{
The return 'effects' represent accuracies in logit space, which
has infinite support. In order to obtain, e.g., a posterior-mean estimate
of the population accuracy in the conventional [0..1] space, use:
`logitnmean(q$mu.mu, 1/sqrt(q$eta.mu))`
}
\examples{
q <- vbicp.unb(c(6, 8, 5), c(10, 10, 10))

}
\references{
K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley, J.M. Buhmann, &
  K.E. Stephan (2013). Variational Bayesian mixed-effects inference for
  classification studies. NeuroImage (2013).
  doi:10.1016/j.neuroimage.2013.03.008.
  https://kaybrodersen.github.io/publications/Brodersen_2013_NeuroImage.pdf
}
\author{
Kay H. Brodersen, ETH Zurich
}
