#' @title Extract ExposomeSet from imExposomeSet
#' 
#' @description Extract one of the imputed sets of an imputed ExposomeSet (imExposomeSet)
#'
#' @param x \code{character} Name of the \code{imExposomeSet}
#' @param rid \code{numeric} (default \code{1}) Number of imputed set to extract.
#' @param newobj.name \code{character} (default \code{NULL}) Name that will take the ExposomeSet. 
#' If \code{NULL} the input imExposomeSet will be overwritten. 
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return This function does not have an output. It creates a object on the study server named.
#' @export

ds.extractExposomeSetFromImputedSet <- function(x, rid = 1L, newobj.name = NULL, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  if(is.null(newobj.name)){
    newobj.name <- x
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  if(!any((cls %in% c("imExposomeSet")))){
    stop("'x' is not an 'imExposomeSet'")
  }
  
  cally <- paste0("extractExposomeSetFromImputedSetDS(", x, ", ", rid, ")")
  DSI::datashield.assign.expr(datasources, newobj.name, as.symbol(cally))
  
}