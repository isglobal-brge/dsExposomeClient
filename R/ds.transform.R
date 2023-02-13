#' @title Apply a transformation to the exposure of an ExposomeSet
#'
#' @param Set `character` Exposomeset on the server which exposures will be transformed
#' @param exposure `character` Name of the exposure to be transformed
#' @param method `character` Function to be applied to the exposure
#' @param new.obj `character` (default `NULL`) If provided, name of the new object that will
#' be created. If `NULL`, the input `Set` will be overwritten with the results
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return This function does not have an output. It creates an Exposome Set object on the study server.
#' @export

ds.transform <- function(Set, exposure, method, new.obj = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (is.null(new.obj)){
    new.obj <- Set
  }
  
  cally <- paste0("transformDS(",
                  Set,
                  ", '",
                  exposure,
                  "', ",
                  method, ")")
  
  DSI::datashield.assign.expr(
    conns = datasources, 
    symbol = new.obj, 
    expr = cally
  )
}