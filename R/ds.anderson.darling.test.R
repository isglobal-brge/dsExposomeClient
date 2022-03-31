#' @title Anderson-Darling Normality Test
#' 
#' @description Perform the Anderson-Darling Normality Test on a numeric vector (or column of a data frame)
#'
#' @param x \code{character} Name of the numeric vector on the server side
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Results of the Anderson-Darling Normality Test
#' @export

ds.anderson.darling.test <- function(x, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("anderson.darling.testDS(", x, ")")
  test <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  return(test)
  
}