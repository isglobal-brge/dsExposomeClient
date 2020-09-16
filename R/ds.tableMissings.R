#' @title Get table of missings per variable
#' 
#' @description Obtain the number (or percentage) of missings of for the exposures or phenotypes of an ExposomeSet object
#'
#' @param exp \code{character} Name of the Exposome Set on the server side
#' @param set \code{character} (default \code{"exposures"}) Set to get the missings: \code{"exposures"} or \code{"phenotypes"}
#' @param output \code{character} (default \code{"n"}) Get missing number (\code{"n"}) or percentage (\code{"p"})
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Vector of named numerics, where the name corresponds to the variable and the associated numeric corresponds
#' to the missing (number or percentage, dependeing on the \code{output} parameter)
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.tableMissings <- function(exp, set = "exposures", output = "n", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(ds.exists(exp, datasources) == FALSE){
    stop(paste0("ExposomeSet, ", exp, ", not defined on the study servers"))
  }
  
  if(ds.class(exp, datasources) != "ExposomeSet"){
    stop(paste0(exp, ", is not of class ExposomeSet"))
  }
  
  cally <- paste0("tableMissingsDS(", exp, ", '", set, "', '", output, "')")
  missings <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  return(missings)
  
}