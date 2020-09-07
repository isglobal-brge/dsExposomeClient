#' @title Perform a imputation of an Exposome Set
#' 
#' @description Perform a imputation of an Exposome Set on the server side using rexposome base function
#'
#' @param object \code{character} Name of the Exposome Set on the server side
#' @param name \code{character} (default \code{NULL}) Name to be assigned to the exposed set on the server side. If missing,
#' the input Exposome Set will be over written.
#' @param select \code{character} (default \code{NULL}) Exposures to be imputed. If missing, all exposes will be imputed.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates an Exposome Set object on the study server.
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' 

ds.imputation <- function(object, name = NULL, select = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(is.null(name)){
    name <- object
  }
  
  if(is.null(select)){
    cally <- paste0("imputationDS(", object, ")")
  }
  else{
    cally <- paste0("imputationDS(", object, ", '", select, "')")
  }
  
  DSI::datashield.assign.expr(datasources, name, as.symbol(cally))
  
}