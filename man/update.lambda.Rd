% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vbicp.unb.R
\name{update.lambda}
\alias{update.lambda}
\title{Update the 3rd mean-field partition (lambda)}
\usage{
\method{update}{lambda}(data, prior, q)
}
\arguments{
\item{data}{List of `ks`, `ns`, and `m`.}

\item{prior}{List of `mu.0`, `eta.0`, `a.0`, `b.0`.}

\item{q}{List of approximate posterior moments.}
}
\value{
A revised `q` with updated `a.lambda` and `b.lambda` elements.
}
\description{
Update the 3rd mean-field partition (lambda)
}
