#' @title Principal components analysis of an Exposome Set
#' 
#' @description Performs a non-disclosive PCA given an Exposome Set, the Exposome Set can be subsetted by families to 
#' perform the PCA
#'
#' @param Set \code{character} Name of the exposome set on the study server
#' @param fam \code{character vector} (default \code{NULL}) Families to subset the exposome set
#' @param standar \code{bool} Whether the values will be normalized prior the analysis (\code{TRUE}) or not (\code{FALSE})
#' Default \code{TRUE}
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return A \code{data.frame} with principal components and variables
#' 
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exposome_pca <- function(Set, fam = NULL, standar = TRUE, datasources = NULL){
  
  if(is.null(Set) | class(Set) != "character"){
    stop("Input variable 'Set' must have a value which is a character string")
  }
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(!is.null(fam)){
    ds.exposomeSubset(Set, fam, NULL, datasources)
    Set <- paste0(Set, "_subsetted")
  }
  
  cally <- paste0("exposures_pData(", Set, ", 'exposures')")
  DSI::datashield.assign.expr(conns, "dta", as.symbol(cally))
  
  ds.subset_type("dta", "numeric", "dta", datasources)
  
  pca <- ds.pca("dta", standar, datasources)
  
  # Remove created variables on the study server
  datashield.rm(datasources, "dta")
  
  if(!is.null(fam)){
    datashield.rm(datasources, Set)
  }
  
  return(pca)
  
}