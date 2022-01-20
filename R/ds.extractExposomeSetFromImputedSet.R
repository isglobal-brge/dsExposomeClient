#' Title
#'
#' @param x 
#' @param rid 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
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