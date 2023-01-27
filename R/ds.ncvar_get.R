#' @title Read data from netCDF file
#' 
#' @description Reads data from an existing netCDF file.
#'
#' @param nc (Object server name of: )`character`
#' An object of class ncdf4 (loaded from a NetCDF resource), 
#' indicating what file to read from.
#' @param varid `character` What variable to read the data from.
#'
#' @return Creates object on the server
#' @export
#'
ds.ncvar_get <- function(nc, varid, new.obj = "ncvar", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("ncvar_getDS", as.symbol(nc), varid)
  DSI::datashield.assign.expr(datasources, new.obj, calltext)
  
}