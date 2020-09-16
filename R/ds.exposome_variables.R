#' @title Get names from the exposures or phenotypes of an Exposome Set of the server side
#' 
#' @description Extract all the different variables of the phenotypes or exposures of an exposome dataset on the
#' server side
#'
#' @param Set \code{character} Name of the Exposome Set object on the server side
#' @param target \code{character} To specify target of output table, \code{exposures} for the exposure names,
#' \code{phenotypes} for the phenotypes. Default \code{exposures}
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return A \code{list} with the variable names selected
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exposome_variables <- function(Set, target = "exposures", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(Set, datasources)
  
  if(target == "exposures"){
    cally <- paste0("exposureNamesDS(", Set, ")")
  }
  else if(target == "phenotypes"){
    cally <- paste0("phenotypeNamesDS(", Set, ")")
  }
  else{
    stop("Invalid target input argument ('", target, "')")
  }

  varr <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(varr)
  
}