#' @title Get family names from Exposome Set
#' 
#' @description Get family names from Exposome Set on the server side
#'
#' @param object \code{character} Name of the Exposome Set object on the server side
#' @param by.exposure \code{bool} (default \code{FALSE}) If \code{TRUE} a vector labeled with each exposure 
#' name will be returned with the family of each exposures. If \code{FALSE} a vector with the 
#' (unique) name of the families of exposures will be returned.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return A \code{character vector} with the family names of the Exposome Set
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' 

ds.familyNames <- function(object, by.exposure = FALSE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("familyNamesDS(", object, ", ", by.exposure, ")")
  f_names <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  return(f_names)
  
}