#' @title Check if object exists on the study server and if it's an ExposomeSet
#'
#' @description Internal function
#'
#' @param set \code{character} Name of the object to look for on the study server
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return No return object, only stops the execution if the object is not on the study server or if it's not an
#' ExposomeSet

checkForExposomeSet <- function(set, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(dsBaseClient::ds.exists(set, datasources)[[1]] == FALSE){
    stop(paste0("Object: ", set, "; does not exist on the study server"), call. = FALSE)
  }
  
  if(!any(dsBaseClient::ds.class(set, datasources)[[1]] == "ExposomeSet")){
    stop(paste0("Object: ", set, "; is not an ExposomeSet"), call. = FALSE)
  }
  
}
