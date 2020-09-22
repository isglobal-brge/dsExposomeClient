#' @title Principal components analysis of an Exposome Set
#' 
#' @description Performs a non-disclosive PCA given an Exposome Set, the Exposome Set can be subsetted by families to 
#' perform the PCA
#'
#' @param Set \code{character} Name of the exposome set on the study server
#' @param fam \code{character vector} (default \code{NULL}) Families to subset the exposome set
#' @param standar \code{bool} Whether the values will be normalized prior the analysis (\code{TRUE}) or not (\code{FALSE})
#' Default \code{TRUE}
#' @param method
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return A \code{data.frame} with principal components and variables
#' 
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exposome_pca <- function(Set, fam = NULL, standar = TRUE, method = "normal", datasources = NULL){
  
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
  
  if(standar){
    ds.standardize(Set, name = "pca_std_exposomeSet", method = method, datasources = datasources)
    Set <- "pca_std_exposomeSet"
  }
  
  checkForExposomeSet(Set, datasources)
  
  ds.exposures_pData(Set, "all", "ds.exposome_pca.dataset", datasources)
  
  cally <- paste0("exposome_pcaDS(", Set, ", ", "ds.exposome_pca.dataset", ")")
  DSI::datashield.assign.expr(datasources, "ds.exposome_pca.Results", as.symbol(cally))
  
  if(standar){
    datashield.rm(datasources, Set)
  }
}