#' @title Get attribute from netCDF file
#' 
#' @description Reads an attribute from a netCDF file.
#'
#' @param nc (Object server name of: ) `character`
#' An object of class ncdf4 (loaded from a NetCDF resource), 
#' indicating what file to read from.
#' @param varid `character` The variable whose attribute is to be read.
#' @param attname `character` Name of the attribute to read
#'
#' @return Creates object on the server
#' @export
#'
ds.ncatt_get <- function(nc, varid, attname, new.obj = "ncatt", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("ncatt_getDS", as.symbol(nc), varid, attname)
  DSI::datashield.assign.expr(datasources, new.obj, calltext)
  
}