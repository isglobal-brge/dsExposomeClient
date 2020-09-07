#' @title Shapiro-Wilk Normality Test
#' 
#' @description Perform the Shapiro-Wilk Normality Test on a numeric vector (or column of a data frame)
#'
#' @param x \code{character} Name of the numeric vector on the server side
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Results of the Shapiro-Wilk Normality Test
#'

ds.shapiro.test <- function(x, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- dsBaseClient:::extract(x)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- dsBaseClient:::isDefined(datasources, varnames[i])
    }else{
      defined <- dsBaseClient:::isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  typ <- dsBaseClient:::checkClass(datasources, x)
  
  if(typ != 'integer' & typ != 'numeric'){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }
  
  cally <- paste0("shapiro.testDS(", x, ")")
  test <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  return(test)
  
}