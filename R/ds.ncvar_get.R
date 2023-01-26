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
ds.ncvar_get <- function(nc, varid, new.obj = "ncvar", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("ncvar_getDS", as.symbol(nc), varid)
  DSI::datashield.assign.expr(datasources, new.obj, calltext)
  
}