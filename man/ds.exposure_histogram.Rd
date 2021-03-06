% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.exposure_histogram.R
\name{ds.exposure_histogram}
\alias{ds.exposure_histogram}
\title{Draw histogram for Exposome Set exposure}
\usage{
ds.exposure_histogram(
  exp,
  exposure,
  show.trans = FALSE,
  ...,
  datasources = NULL
)
}
\arguments{
\item{exp}{\code{character} Name of the Exposome Set on the server side}

\item{exposure}{\code{character} Name of the exposure of the exposome set to visualize}

\item{show.trans}{\code{bool} (default \code{FALSE}) To show (\code{TRUE}) or not (\code{FALSE}) how
the exposure histogram would look like if \code{log/exp/sqrt} transformed}

\item{...}{Parameters passed to the \code{\link{ds.histogram}} function}

\item{datasources}{a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login}
}
\value{
List with plot parameters
}
\description{
Get a non-disclosive histogram plot for a selected exposure of a server side Exposome Set
}
\examples{
\dontrun{Refer to the package Vignette for examples.}
}
