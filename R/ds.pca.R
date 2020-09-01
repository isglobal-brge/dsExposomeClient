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

ds.pca <- function(tab, standar = TRUE, datasources = NULL){
  
  if(is.null(tab) | class(tab) != "character"){
    stop("Input variable 'tab' must have a value which is a character string")
  }
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  DSI::datashield.assign.expr(datasources, symbol = "pca", as.symbol(tab))
  
  if(standar){
    DSI::datashield.assign.expr(datasources, "pca", quote(scaleDS(pca)))
  }
  
  var_cov <- ds.cov(x = "pca", type = "combine", datasources = datasources)$`Variance-Covariance Matrix`
  # Base svd function is good enough for exposome tables, for larger tables fast.svd from the corpcor 
  # library may be a good upgrade
  if(all(is.na(var_cov))){
    # Remove created variables on the study server
    datashield.rm(datasources, "pca")
    # Stop execution of function and return error message
    stop("The required operations to perform the PCA have disclosure risks, can't perform PCA", call.=FALSE)
  }
  
  var_cov_svd <- svd(var_cov)
  
  pca <- data.frame(var_cov_svd$u)
  
  rownames_pca <- ds.colnames("pca")
  rownames(pca) <- rownames_pca[[1]]
  colnames(pca) <- paste0(rep("PC", length(rownames_pca[[1]])),
                          seq(from = 1, by = 1, length.out = length(rownames_pca[[1]])))

  # Remove created variables on the study server
  datashield.rm(datasources, "pca")
  
  return(pca)

  
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

