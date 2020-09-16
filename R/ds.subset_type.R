#' @title Subset data frame by class
#' 
#' @description Create a subset of a data frame with the columns that have a shared class
#'
#' @param data \code{character} Name of the data frame on the study server
#' @param type \code{character} Data type to which subset the data frame
#' @param newobj \code{character} Name of the output object assigned to the server side, if \code{NULL} the input table 
#' will be over-written
#' @param datasources  a list of \code{\link{DSConnection-class}} objects obtained after login 
#'
#' @return This function does not have an output. It creates an Exposome Set object on the study server.
#' @export

ds.subset_type <- function(data, type = "numeric", newobj = NULL, datasources = NULL){
  
  if(is.null(data) | class(data) != "character"){
    stop("Input variable 'data' must have a value which is a character string")
  }
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (is.null(newobj)) {
    newobj <- data
  }
  
  cally <- paste0("subset_typeDS(", data, ", '", type, "')")
  DSI::datashield.assign.expr(datasources, newobj, as.symbol(cally))
  
}