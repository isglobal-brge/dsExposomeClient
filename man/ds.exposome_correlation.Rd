% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.exposome_correlation.R
\name{ds.exposome_correlation}
\alias{ds.exposome_correlation}
\title{Extract correlation matrix from Exposome Set exposures}
\usage{
ds.exposome_correlation(set, fam = NULL, datasources = NULL)
}
\arguments{
\item{set}{\code{character} Name of the Exposome Set object on the server side}

\item{fam}{\code{character vector} (default \code{NULL}) Families to subset the exposome dataset, 
the correlation matrix will be calculated using a subset of the exposures according to this argument}

\item{datasources}{a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login}
}
\value{
\code{list} with: \cr
- Output of the \code{\link{ds.cor}} function \cr
- Named \code{character vector} with the exposures on the correlation matrix and correspondent families.
This argument is used by the \code{\link{corPlot}} function to put the labels on the plot
}
\description{
Extract correlation matrix from Exposome Set exposures, the exposome set can be subsetted by families, as
complete exposome sets (exposures) tend to be matrices that could be disclosive when performing a correlation study
}
\examples{
\dontrun{Refer to the package Vignette for examples.}
}
