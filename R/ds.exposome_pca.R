#' @title Principal components analysis of an Exposome Set
#' 
#' @description Performs a non-disclosive PCA given an Exposome Set on the study server, 
#' the Exposome Set can be subsetted by families to perform the PCA
#'
#' @param Set \code{character} Name of the exposome set on the study server
#' @param fam \code{character vector} (default \code{NULL}) Families to subset the exposome set
#' @param standar \code{bool} Whether the values will be normalized prior the analysis (\code{TRUE}) or not (\code{FALSE})
#' Default \code{TRUE}
#' @param method \code{character} (default \code("normal")) Method of standarization, only applies when \code{standar}
#' is set to \code{TRUE}. Options are \code("normal") which scales the exposures using the mean as the center 
#' and the standard variation as dispersion, \code{"robust"} which uses the median and median absolute deviation respectively
#' and \code{"interquartile range"} which uses the median as the center and the coeficient between the 
#' interquartile range of the exposure and the normal range between the percentile 75 and 25 as variance.
#' @param pca \code{bool} (default \code{TRUE}) If TRUE perform PCA (only numerical variables), 
#' if FALSE FAMD (numerical and categorical)
#' @param \code{numeric} (default \code{10}) Number of PC to be kept
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return This function does not have an output. It creates a object on the study server named \code{ds.exposome_pca.Results},
#' this object can be passed to the \code{\link{ds.exposome_pca_plot}} to visualize the results of the PCA.
#' 
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exposome_pca <- function(Set, fam = NULL, standar = TRUE, method = "normal", pca = TRUE, npc = 10, datasources = NULL){
  
  if(is.null(Set) | class(Set) != "character"){
    stop("Input variable 'Set' must have a value which is a character string")
  }
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(!is.null(fam)){
    ds.exposomeSubset(Set, fam, NULL, datasources)
    Set <- paste0(Set, "_subsetted")
    warning('The family subset of [', paste0(fam, collapse = ", "), '] yielded (', 
            unlist(ds.dim(Set)[1])[1], ') valid exposures.')
  }
  
  if(standar){
    ds.standardize(Set, name = "pca_std_exposomeSet", method = method, datasources = datasources)
    Set <- "pca_std_exposomeSet"
  }
  
  checkForExposomeSet(Set, datasources)
  
  cally <- paste0("exposome_pcaDS(", Set, ", npc = ", npc, ", pca = ", pca, ")")
  DSI::datashield.assign.expr(datasources, "ds.exposome_pca.Results", as.symbol(cally))
  
  if(standar){
    datashield.rm(datasources, Set)
  }
}