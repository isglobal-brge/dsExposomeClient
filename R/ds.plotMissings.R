#' @title Draw plot with percentage of missings
#' 
#' @description Create a plot with the percentage of missings for the exposures and phenotypes of an 
#' ExposomeSet object
#'
#' @param exp \code{character} Name of the Exposome Set on the server side
#' @param set \code{character} (default \code{"exposures"}) Set to get the missings plot: \code{"exposures"} or \code{"phenotypes"}
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Object of class \code{ggplot}, calling it will render the actual plot
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}

ds.plotMissings <- function(exp, set = "exposures", datasources = NULL){
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(ds.exists(exp, datasources) == FALSE){
    stop(paste0("ExposomeSet, ", exp, ", not defined on the study servers"))
  }
  
  if(ds.class(exp, datasources) != "ExposomeSet"){
    stop(paste0(exp, ", is not of class ExposomeSet"))
  }
  
  cally <- paste0("plotMissingsDS(", exp, ", '", set, "')")
  missings_plot <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  return(missings_plot)
  
}