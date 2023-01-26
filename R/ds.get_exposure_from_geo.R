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