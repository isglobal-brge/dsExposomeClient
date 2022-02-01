#' @title Extract exposure or phenotype data and save them to a data frame
#' 
#' @description Extracts exposures, phenotypes or combined data and saves it to a data frame on the server side,
#' if no \code{name} argument is provided, the new Exposome Set object will be named 
#' \code{"set_table"} where \code{"set"} is the inputted argument
#'
#' @param set \code{character} Name of the Exposome Set object on the server side
#' @param type \code{character} To specify target of output table, \code{all} to include exposures and phenotypes,
#' \code{exposures} to include only the exposures and \code{phenotypes} to include only the phenotypes. Default \code{all}
#' @param name \code{character} (default \code{NULL}) Name of the new Exposome Set, if null the name 
#' will be \code{"set_table"} where \code{"set"} is the inputted argument
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates an data frame object on the study server.
#' @export

ds.exposures_pData <- function(set, type = "", name = NULL, exposures_type = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(is.null(name)){
    name <- paste0(set, "_table")
  }
  
  checkForExposomeSet(set, datasources)
  
  cally <- paste0("exposures_pData(", set, ", '", type, 
                  if(is.null(exposures_type)){"')"}else{paste0("','", exposures_type, "')")})
  DSI::datashield.assign.expr(datasources, name, as.symbol(cally))
  
}