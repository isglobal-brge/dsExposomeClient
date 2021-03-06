% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.exwas.R
\name{ds.exwas}
\alias{ds.exwas}
\title{Performs a non-disclosive EXposome-Wide Association Study}
\usage{
ds.exwas(model, Set, family, tef = TRUE, datasources = NULL)
}
\arguments{
\item{model}{\code{character} Formula, not including exposures, to be tested.}

\item{Set}{\code{character} Name of the Exposome Set object on the server side}

\item{family}{\code{character} Nature of the health outcome (\code{gaussian}, \code{binomial} or \code{poisson})}

\item{tef}{\code{bool} If \code{TRUE} computes the threshold for effective tests.}

\item{datasources}{a list of \code{\link{DSConnection-class}} objects obtained after login}
}
\value{
\code{list} that contains: \cr
- exwas_results: \code{data.frame} with exposure name, coefficient and p-value of the association \cr
- alpha_corrected: \code{numeric} effective tests
}
\description{
Takes as input an Exposome Set object on the study server and performs a 
ExWAS association study with the provided model.
}
\examples{
\dontrun{Refer to the package Vignette for examples.}
}
