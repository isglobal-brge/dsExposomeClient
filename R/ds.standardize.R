#' @title Standardize ExposomeSet
#' 
#' @description Standardize ExposomeSet on the study server
#'
#' @param set \code{character} Name of the exposome set on the study server
#' @param method \code{character} (default \code{"normal"}) Method of standarization. 
#' Options are \code{"normal"} which scales the exposures using the mean as the center 
#' and the standard variation as dispersion, \code{"robust"} which uses the median and median absolute deviation respectively
#' and \code{"interquartile range"} which uses the median as the center and the coeficient between the 
#' interquartile range of the exposure and the normal range between the percentile 75 and 25 as variance.
#' @param name \code{chraracter} (default \code{NULL}) If \code{NULL}, the standardized ExposomeSet will over-writte
#' the input \code{set}, otherwise the standardized ExposomeSet will be saved on the study-server with the name
#' specified on this argument.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return This function does not have an output. It creates a object on the study server with the standardized
#' ExposomeSet, the name of this object depends on the \code{name} input argument.
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.standardize <- function(set, method = "normal", name = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(set, datasources)
  
  if(is.null(name)){
    name <- set
  }
  
  cally <- paste0("standardizeDS(", set, ", '", method, "')")
  DSI::datashield.assign.expr(datasources, name, as.symbol(cally))
  
}