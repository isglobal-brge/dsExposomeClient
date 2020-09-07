ds.exposures_pData <- function(set, type, name, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("exposures_pData(", set, ", '", type, "')")
  DSI::datashield.assign.expr(datasources, name, as.symbol(cally))
  
}