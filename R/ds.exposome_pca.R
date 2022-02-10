#' @title Principal components analysis of an Exposome Set
#' 
#' @description Performs a non-disclosive PCA given an Exposome Set on the study server, 
#' the Exposome Set can be subsetted by families to perform the PCA
#' 
#' @details The pooled PCA uses the block method for SVD decomposition. More information can be 
#' found on https://arxiv.org/pdf/1907.07410.pdf
#'
#' @param Set \code{character} Name of the ExposomeSet on the study server
#' @param fam \code{character vector} (default \code{NULL}) Families to subset the ExposomeSet
#' @param scale \code{bool} (default \code{TRUE}) If \code{TRUE} the ExposomeSet wil be scaled, if \code{FALSE} 
#' it will not be scaled. It is always advisable to scale the data when performing a PCA.
#' @param type \code{character}  (default \code{"meta"}) Type of analysis, if meta-analysis (\code{"meta"}), a PCA to 
#' each study server will be performed. If pooled-analysis (\code{"pooled"}) a pooled methodology will be performed.
#' @param pca \code{bool} (default \code{TRUE}) If TRUE perform PCA (only numerical variables), 
#' if FALSE FAMD (numerical and categorical). This argument only affects \code{type = "meta"}, the pooled methodology 
#' does not have the option of performing a FAMD analysis.
#' @param \code{numeric} (default \code{10}) Number of principal compoenents to be kept
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return This function does not have an output. It creates a object on the study server named \code{ds.exposome_pca.Results},
#' this object can be passed to the \code{\link{ds.exposome_pca_plot}} to visualize the results of the PCA.
#' 
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exposome_pca <- function(Set, fam = NULL, scale = TRUE, type = "meta", 
                            pca = TRUE, npc = 10, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(Set, datasources)
  
  if(!is.null(fam)){
    ds.exposomeSubset(Set, fam, NULL, datasources)
    Set <- paste0(Set, "_subsetted")
    warning('The family subset of [', paste0(fam, collapse = ", "), '] yielded (', 
            unlist(ds.dim(Set)[1])[1], ') valid exposures.')
  }
  
  if(scale){
    ds.exposome_scale_exposures(Set, new.obj = "pca_scaled_exposomeSet", datasources = datasources)
    Set <- "pca_scaled_exposomeSet"
  }
  
  if(type == "meta"){
    cally <- paste0("exposome_pcaDS(", Set, ", npc = ", npc, ", pca = ", pca, ")")
    DSI::datashield.assign.expr(datasources, "ds.exposome_pca.Results", as.symbol(cally))
  } else if (type == "pooled"){
    cally <- paste0("exposome_pca_pooledDS(", Set, ")")
    partial_svd <- t(Reduce("cbind", DSI::datashield.aggregate(datasources, as.symbol(cally))))
    total_svd <- FactoMineR::PCA(partial_svd, graph = FALSE, ncp = npc)
    
    DSI::datashield.assign.expr(datasources, "ds.exposome_pca.Results", 
                                paste0("exposome_pca_pooled_addPCDS(", Set, ", '",
                                       sf::rawToHex(serialize(total_svd, NULL)), "')"))
  } else {
    stop("Invalid 'type' argument. Valid options are ['pooled', 'meta']")
  }

  if(scale){
    datashield.rm(datasources, Set)
  }
}
