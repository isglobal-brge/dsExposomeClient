#' @title Generate the summary of a variable of an exposome dataset
#' 
#' @description Generates the summary of a variable (phenotype or exposure) of a server side 'ExposomeSet' object
#'
#' @param Set \code{character} Name of the Exposome Set object on the server side
#' @param variable \code{character} Name of the variable to obtain it's summary (numeric or factor variable)
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return The function returns a \code{list}, the contents of it depend on the variable nature, for numeric variables: \cr
#' - \code{$class} Class of the variable \cr
#' - \code{$length} Number of observations of the variable \cr
#' - \code{$quantiles & mean} Quantiles and mean of the variable \cr
#' 
#' Categorical variable: \cr
#' - \code{$class} Class of the variable \cr
#' - \code{$length} Number of observations of the variable \cr
#' - \code{$categories} Levels of the categorical variable \cr
#' - \code{$counts of each level} Counts for each of the levels
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exposome_summary <- function(Set, variable, datasources = NULL){
  
  if(is.null(Set) | class(Set) != "character"){
    stop("Input variable 'Set' must have a value which is a character string")
  }
  
  if(is.null(variable) | class(variable) != "character"){
    stop("Input variable 'variable' must have a value which is a character string")
  }
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(as.logical(ds.exists("dta_all", datasources))){
    warning("The variable 'dta_all' already exists on the study server. ds.exposome_summary uses
         this variable during it's execution so it will be overwritten", call. = FALSE)
  }
  
  if(!unlist(ds.exists(Set))){
    stop("The exposome set ('" , Set, "') does not exist on the study server")
  }
  
  if(!(ds.class(Set) == "ExposomeSet")){
    stop("The study server variable ('" , Set, "') is not of class 'ExposomeSet'")
  }
  
  # Extract table with exposures and phenotypes and save it on the server (assign)
  cally <- paste0("exposures_pData(", Set, ")")
  DSI::datashield.assign.expr(datasources, "dta_all", as.symbol(cally))
  
  # Check if the input variable is on the exposome dataset
  if(!(variable %in% unlist(ds.colnames("dta_all")))){
    stop(paste0("Variable ('", variable, "') not found on the exposome set ('", Set, "')"))
  }
  
  # Get summary
  summ <- ds.summary(paste0("dta_all$", variable), datasources)
  
  # Remove created variables on the study server
  datashield.rm(datasources, "dta_all")
  
  return(summ)
  
}