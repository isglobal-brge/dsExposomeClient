% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.exposome_pca.R
\name{ds.exposome_pca}
\alias{ds.exposome_pca}
\title{Principal components analysis of an Exposome Set}
\usage{
ds.exposome_pca(Set, standar = TRUE, datasources = NULL)
}
\arguments{
\item{Set}{\code{character} Name of the exposome set on the study server}

\item{standar}{\code{bool} Whether the values will be normalized prior the analysis (\code{TRUE}) or not (\code{FALSE})
Default \code{TRUE}}

\item{datasources}{a list of \code{\link{DSConnection-class}} objects obtained after login}
}
\value{
A \code{data.frame} with principal components and variables
}
\description{
Performs a non-disclosive PCA given an Exposome Set
}
\examples{
\dontrun{Refer to the package Vignette for examples.}

}