ds.loadExposome <- function(object_name, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if(is.null(object_name)){
    stop(" Please provide a name for the Exposome Set", call.=FALSE)
  }

  datasources <- DSI::datashield.connections_find()
  DSI::datashield.assign.expr(datasources, object_name, quote(loadExposomeDS(exposures, description, phenotypes)))

}
