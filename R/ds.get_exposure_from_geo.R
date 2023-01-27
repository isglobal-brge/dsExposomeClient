#' @title Associate `NetCDF` data and individual location
#' 
#' @description Merge the exposure data contained in a `NetCDF` object
#' with the location data of the individuals present on a `clinical` `data.frame`
#'
#' @param lat `character` (Object server name of: ) 
#' Latitude extracted from a `NetCDF` resource using `ncvar_getDS`
#' @param lon `character` (Object server name of: ) 
#'  Longitutde extracted from a `NetCDF` resource using `ncvar_getDS`
#' @param exposure `character` (Object server name of: ) 
#'  Exposure extracted from a `NetCDF` resource using `ncvar_getDS`
#' @param exposure_name `character` Name to assign to the `exposure`
#' @param clinical `character` (Object server name of: ) 
#'  Clinical data data of the individuals, which contain among
#' other variables, the longitude and latitude of the individuals to be associated with the 
#' `NetCDF` data.
#' @param clinical_lat_var `character` Name of the latitude variable on the `clinical` data
#' @param clinical_lon_var `character` Name of the longitude variable on the `clinical` data
#' @param clinical_id_var `character` Name of the IDs variable on the `clinical` data
#' @param exposures (default `NULL`) (Object server name of: ) 
#'  If provided, existing `data.frame` with exposure data.
#' The new exposure calculated from the `NetCDF` data will be added to this table by ID
#' @param exposures_id_var (default `NULL`) Required if `exposures` is provided. Name of the
#' IDs variable on the `exposures` data.
#'
#' @return Creates object on the server
#' @export
#'
ds.get_exposure_from_geo <- function(lat, lon, exposure, exposure_name,
                                     clinical, clinical_lat_var,
                                     clinical_lon_var, clinical_id_var,
                                     exposures = NULL, exposures_id_var = NULL, 
                                     new.obj = "exposures", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(!is.null(exposures)){
    calltext <- call("get_exposure_from_geoDS", as.symbol(lat), as.symbol(lon), 
                     as.symbol(exposure), exposure_name, as.symbol(clinical), 
                     clinical_lat_var, clinical_lon_var, clinical_id_var,
                     as.symbol(exposures), exposures_id_var)
  } else {
    calltext <- call("get_exposure_from_geoDS", as.symbol(lat), as.symbol(lon), 
                     as.symbol(exposure), exposure_name, as.symbol(clinical), 
                     clinical_lat_var, clinical_lon_var, clinical_id_var,
                     NULL, NULL)
  }
  
  DSI::datashield.assign.expr(datasources, new.obj, calltext)
  
}