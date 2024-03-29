% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.exposures_pData.R
\name{ds.exposures_pData}
\alias{ds.exposures_pData}
\title{Extract exposure or phenotype data and save them to a data frame}
\usage{
ds.exposures_pData(
  set,
  type = "",
  name = NULL,
  exposures_type = NULL,
  rownames2col = FALSE,
  datasources = NULL
)
}
\arguments{
\item{set}{\code{character} Name of the Exposome Set object on the server side}

\item{type}{\code{character} To specify target of output table, \code{all} to include exposures and phenotypes,
\code{exposures} to include only the exposures and \code{phenotypes} to include only the phenotypes. Default \code{all}}

\item{name}{\code{character} (default \code{NULL}) Name of the new Exposome Set, if null the name 
will be \code{"set_table"} where \code{"set"} is the inputted argument}

\item{exposures_type}{\code{character} (default \code{NULL}) Type of the exposures to be kept, usually 
\code{"numeric"} or \code{"character"/"factor"}. If \code{NULL} all the exposures will be kept.}

\item{datasources}{a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login}
}
\value{
This function does not have an output. It creates an data frame object on the study server.
}
\description{
Extracts exposures, phenotypes or combined data and saves it to a data frame on the server side,
if no \code{name} argument is provided, the new Exposome Set object will be named 
\code{"set_table"} where \code{"set"} is the inputted argument
}
