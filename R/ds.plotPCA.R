#' Title
#'
#' @param pca_res 
#' @param dataset 
#'
#' @return

#'
#' @examples

ds.plotPCA <- function(pca_res, dataset, datasources = NULL){
  
  if(is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  pca_res <- pca_res$pca[, 1:2]
  cally <- paste0("plotPCADS(", paste0(as.character(unlist(pca_res)), collapse = ", "),
                  ", dataset = ", dataset, ")")
  DSI::datashield.assign.expr(datasources, "plotPCADSresult", as.symbol(cally))
  
  DSI::datashield.assign.expr(datasources, "plotPCADSresult", quote(scaleDS(plotPCADSresult)))
  
  ds.dataFrame("plotPCADSresult", newobj = "plotPCADSresult", datasources = datasources)
  
  ds.scatterPlot("plotPCADSresult$pc1", "plotPCADSresult$pc2")
  
  # return(dt)
  
}
