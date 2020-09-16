#' @title Check if exposure exists inside an ExposomeSet
#' 
#' @description Internal function
#'
#' @param set \code{character} Name of the ExposomeSet on the study server
#' @param exposure \code{character} Name of the exposure to look for on the ExposomeSet
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return No return object, only stops the execution if the exposure is not on the ExposomeSet

checkForExposure <- function(set, exposure, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(!any(ds.exposome_variables(set , "exposures", conns)[[1]] == exposure)){
    stop(paste0("Exposure: ", exposure, "; is not inside the ExposomeSet: ", set), call. = FALSE)
  }
  
}