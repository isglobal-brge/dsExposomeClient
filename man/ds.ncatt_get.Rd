% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.ncatt_get.R
\name{ds.ncatt_get}
\alias{ds.ncatt_get}
\title{Get attribute from netCDF file}
\usage{
ds.ncatt_get(nc, varid, attname, new.obj = "ncatt", datasources = NULL)
}
\arguments{
\item{nc}{(Object server name of: ) `character`
An object of class ncdf4 (loaded from a NetCDF resource), 
indicating what file to read from.}

\item{varid}{`character` The variable whose attribute is to be read.}

\item{attname}{`character` Name of the attribute to read}
}
\value{
Creates object on the server
}
\description{
Reads an attribute from a netCDF file.
}
