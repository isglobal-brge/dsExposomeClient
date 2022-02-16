#' @title Hierarchical clustering on principal components
#' 
#' @description  Perform a HCPC on the PCA/FAMD of an ExposomeSet
#' 
#' @details The suggested partition is the one with the higher relative loss of inertia (i(clusters n+1)/i(cluster n)). 
#' Make use of the function FactoMineR::plot.HCPC() to visualize the results.
#'
#' @param object \code{character} Object on the server created by the function \link{ds.exposome_pca}
#' @param nb.clust \code{numeric} (default \code{-1}) Number of clusters to find. If -1, the tree is automatically 
#' cut at the suggested level (see details). If a (positive) integer, the tree is cut with nb.cluters clusters.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Object created by FactoMineR::HCPC with the original dataset removed.
#' @export
#'

ds.exposome_HCPC <- function(object, nb.clust = -1, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(nb.clust == 0){nb.clust <- -1}
  
  cally <- paste0("exposome_HCPCDS(", object, ", ", nb.clust, ")")
  ans <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  return(ans)

}