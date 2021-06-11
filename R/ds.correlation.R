#' @title Extract correlation matrix from Exposome Set exposures
#' 
#' @description Extract correlation matrix from Exposome Set exposures, the exposome set can be subsetted by families, as
#' complete exposome sets (exposures) tend to be matrices that could be disclosive when performing a correlation study
#'
#' @param set \code{character} Name of the Exposome Set object on the server side
#' @param fam \code{character vector} (default \code{NULL}) Families to subset the exposome dataset, 
#' the correlation matrix will be calculated using a subset of the exposures according to this argument
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{list} with: \cr
#' - Output of the \code{\link{ds.cor}} function \cr
#' - Named \code{character vector} with the exposures on the correlation matrix and correspondent families.
#' This argument is used by the \code{\link{corPlot}} function to put the labels on the plot
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.correlation <- function(set, fam = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(!is.null(fam)){
    ds.exposomeSubset(set, fam, "exposomeSetSubsetted", datasources)
    set <- "exposomeSetSubsetted"
  }
  
  checkForExposomeSet(set, datasources)
  
  ds.exposures_pData(set, "exposures", "ds.correlationExposures", datasources)
  
  data <- ds.cor(x = "ds.correlationExposures", type = "split" , datasources)
  
  names <- ds.familyNames(set, TRUE, conns)
  
  datashield.rm(datasources, "exposomeSetSubsetted")
  datashield.rm(datasources, "ds.correlationExposures")
  
  return(list(data, names))
  
}
