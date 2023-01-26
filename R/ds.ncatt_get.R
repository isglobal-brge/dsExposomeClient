#' Title
#'
#' @param nc 
#' @param varid 
#' @param new.obj 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.ncatt_get <- function(nc, varid, attname, new.obj = "ncatt", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("ncatt_getDS", as.symbol(nc), varid, attname)
  DSI::datashield.assign.expr(datasources, new.obj, calltext)
  
}