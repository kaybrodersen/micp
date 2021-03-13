\name{micp.stats}
\alias{micp}
\title{Mixed-effects inference on classification performance}
\description{This function provides an interface to an underlying variational Bayesian inference algorithm. For details, see the respective help text. For MCMC implementations, see the corresponding MATLAB toolbox.}

\usage{
micp.stats(ks, ns)
}

\arguments{
\item{ks}{ When inferring on accuracies: m-sized vector of number of correct predictions (in each subject). When inferring on balanced accuracies: 2xm matrix of correctly predicted positive trials (first row) and corectly predicted negative trials (second row). }
\item{ns}{ When inferring on accuracies: m-sized vector of total number of trials (in each subject). When inferring on balanced accuracies: 2xm matrix of total number of positive (first row) and negative (second row) trials. }
}

\value{
A list with the following fields:
\item{mu}{ Posterior mean of the population mean accuracy or balanced accuracy. This is the expected performance of the classifier at the group level. }
\item{p}{ Posterior infraliminal probability of the population mean. This is the posterior belief that the classifier did not operate above chance (50\%). A small infraliminal probability represents evidence for above-chance performance. }
\item{ci}{ Posterior 95\% credible interval of the population mean. This interval can be used to show error bars around \code{mu}. }
\item{stats}{ Additional return values, depending on the selected model. See individual inference functions for details. }
}

\author{Kay H. Brodersen}

\examples{
ks <- rbind(c(19, 41, 15, 39, 39), c(41, 46, 43, 48, 37))
ns <- rbind(c(45, 51, 20, 46, 58), c(55, 49, 80, 54, 42))
results <- micp.stats(ks, ns)
print(results)
}

\keyword{variational Bayes}
