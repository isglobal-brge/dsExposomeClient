#' @title Exposure Phenotype names
#' 
#' @description Extract the phenotypes names from a Expression Set object
#'
#' @param x \code{character} ExposomeSet object
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return \code{character vector} of the phenotypes names
#' @export

ds.phenotypeNames <- function(x, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(set = x, datasources = datasources)
  
  cally <- paste0("phenotypeNamesDS(", x, ")")
  return(DSI::datashield.aggregate(datasources, as.symbol(cally)))
  
}
