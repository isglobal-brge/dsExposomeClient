#' @title Principal components analysis
#' 
#' @description Performs a non-disclosive PCA given a data frame
#'
#' @param tab \code{character} Name of the data frame on the study server (can be of the form \code{table[, 1:10]})
#' @param standar \code{bool} Whether the values will be normalized prior the analysis (\code{TRUE}) or not (\code{FALSE})
#' Default \code{TRUE}
#' @param datasources  a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return A \code{data.frame} with principal components and variables
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

# ds.pca <- function(tab, standar = TRUE, datasources = NULL){
ds.pca <- function(x=NULL, datasources=NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("pcaDS(", x, ")")
  DSI::datashield.assign.expr(datasources, "ds.pcaResults", as.symbol(cally))
  
}

# # Benchmark base pca vs fast.pca
# library(microbenchmark)
# library(corpcor)
# 
# columns = 100
# obs = 130
# data = as.matrix(data.frame(replicate(columns,sample(0:100,obs,rep=TRUE))))
# 
# microbenchmark(
#   svd(data),
#   fast.svd(data),
#   times = 100
# )

