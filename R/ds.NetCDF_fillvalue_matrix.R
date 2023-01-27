#' @title Set NAs to matrix fillvalues
#' 
#' @description Use a fillvalue extracted using `ncatt_get` and place NAs
#'
#' @param mat (Object server name of: ) `matrix` Extracted from a `NetCDF` resource using `nc_dataDS`
#' @param fillvalue (Object server name of: ) `list` Extracted from the `_FillValue` slot of a `NetCDF`
#' resource using `ncatt_getDS` 
#'
#' @return Creates object on the server
#' @export
#'
ds.NetCDF_fillvalue_matrix <- function(mat, fillvalue, new.obj = "mat", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("NetCDF_fillvalue_matrixDS", as.symbol(mat), as.symbol(fillvalue))
  DSI::datashield.assign.expr(datasources, new.obj, calltext)
  
}