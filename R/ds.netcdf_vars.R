#' @title Get variable names of NetCDF object
#'
#' @param nc (Object server name of: )`character`
#' An object of class ncdf4 (loaded from a NetCDF resource), 
#' indicating what file to get the variables from.
#'
#' @return `character vector` With the variable names
#' @export
#'
ds.netcdf_vars <- function(nc, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("netcdf_varsDS", as.symbol(nc))
  res <- DSI::datashield.aggregate(datasources, calltext)
  return(res)
  
}