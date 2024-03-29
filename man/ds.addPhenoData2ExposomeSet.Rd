% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.addPhenoData2ExposomeSet.R
\name{ds.addPhenoData2ExposomeSet}
\alias{ds.addPhenoData2ExposomeSet}
\title{Add Phenotype data to ExpressionSet}
\usage{
ds.addPhenoData2ExposomeSet(
  x,
  pheno,
  identifier_ExposomeSet = "ID",
  identifier_new_phenotypes = NULL,
  newobj.name = NULL,
  complete_cases = TRUE,
  datasources = NULL
)
}
\arguments{
\item{x}{\code{character} Name of the ExpressionSet on the study server}

\item{pheno}{\code{character} Name of the data.frame with the new phenotypes on the 
study server}

\item{identifier_ExposomeSet}{\code{character} (default \code{"ID"}) Name of the ID column on the ExposomeSet}

\item{identifier_new_phenotypes}{\code{character} (default \code{"ID"}) Name of the ID column on the phenotypes data.frame}

\item{newobj.name}{\code{character} (default \code{NULL}) If \code{NULL}, the original ExpressionSet will be overwritten,
otherwise the new ExpressionSet will be assigned to a variable named after this argument}

\item{complete_cases}{\code{bool} (default \code{TRUE}) If \code{TRUE} only the matching individuals 
between the ExpressionSet and the phenotypes table will be included on the resulting ExpressionSet. If 
\code{FALSE} all the individuals on the input ExpressionSet will be on the output ExpressionSet}

\item{datasources}{a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login}
}
\value{
This function does not have an output. It creates (or overwrites) a data frame on the study server.
}
\description{
Add new phenotype data contained on a data.frame to an ExpressionSet. The ExpressionSet
may or may not already have phenotype data. If the data.frame contains a phenotype already present 
on the ExpressionSet, the server function will throw an exception. The pehnotypes data.frame has to 
contain an ID column which does not need to contain exactly the same individuals as the ExpressionSet, 
only the matching individuals will be updated, no new individuals will be introduced or removed from the 
ExpressionSet
}
