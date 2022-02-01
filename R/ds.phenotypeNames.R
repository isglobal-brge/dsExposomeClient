#' Title
#'
#' @param x 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.phenotypeNames <- function(x, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  if(!any((cls %in% c("ExposomeSet")))){
    stop("'x' is not an 'ExposomeSet'")
  }
  
  cally <- paste0("phenotypeNamesDS(", x, ")")
  return(DSI::datashield.aggregate(datasources, as.symbol(cally)))
  
}
