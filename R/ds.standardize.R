#' Title
#'
#' @param set 
#' @param method 
#' @param name 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.standardize <- function(set, method = "normal", name = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(is.null(name)){
    name <- set
  }
  
  cally <- paste0("standardizeDS(", set, ", '", method, "')")
  DSI::datashield.assign.expr(datasources, name, as.symbol(cally))
  
}