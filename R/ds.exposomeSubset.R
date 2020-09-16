#' @title Subset Exposome Set by familie(s)
#' 
#' @description Subset an Exposome Set on the server side by familie(s), if no \code{name} argument is provided, 
#' the new Exposome Set object will be named \code{"set_subsettted"} where \code{"set"} is the inputted argument
#'
#' @param set \code{character} Name of the Exposome Set object on the server side
#' @param fam \code{character vector} Families to subset the exposome set
#' @param name \code{character} (default \code{NULL}) Name of the new Exposome Set, if null the name 
#' will be \code{"set_subsetted"} where \code{"set"} is the inputted argument
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates an Exposome Set object on the study server.
#' @export

ds.exposomeSubset <- function(set, fam, name = NULL, datasources = NULL){
  
  if(is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(is.null(name)){
    name <- paste0(set, "_subsetted")
  }
  
  cally <- paste0("exposomeSubsetDS(", set, ", c('", 
                  paste0(stringr::str_replace_all(fam, " ", ""), collapse = "','"), "'))")
  
  DSI::datashield.assign.expr(datasources, name, as.symbol(cally))
  
}