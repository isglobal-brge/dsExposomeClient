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
ds.NetCDF_fillvalue_matrix <- function(mat, fillvalue, new.obj = "mat", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("NetCDF_fillvalue_matrixDS", as.symbol(mat), as.symbol(fillvalue))
  DSI::datashield.assign.expr(datasources, new.obj, calltext)
  
}