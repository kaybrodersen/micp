% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vbicp.unb.R
\name{update.free.energy}
\alias{update.free.energy}
\title{Compute an approximation to the free-energy}
\usage{
\method{update}{free.energy}(data, prior, q)
}
\arguments{
\item{data}{List of `ks`, `ns`, and `m`.}

\item{prior}{List of `mu.0`, `eta.0`, `a.0`, `b.0`.}

\item{q}{List of approximate posterior moments.}
}
\value{
A revised `q` with an updated `F` field.
}
\description{
Compute an approximation to the free-energy
}
