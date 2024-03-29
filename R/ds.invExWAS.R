#' @title Performs a non-disclosive Inverse EXposome-Wide Association Study
#' 
#' @description Takes as input an Exposome Set object on the study server and performs an 
#' Inverse ExWAS association study with the provided model.
#' 
#' @param object \code{character} Name of the Exposome Set object on the server side
#' @param phenotype \code{character} Phenotype objective
#' @param covariables \code{character vector} Adjusting phenotype covariables
#' @param tef \code{bool} If \code{TRUE} computes the threshold for effective tests.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return \code{list} that contains: \cr
#' - exwas_results: \code{data.frame} with exposure name, coefficient and p-value of the association \cr
#' - alpha_corrected: \code{numeric} effective tests
#'
#' @examples 
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.invExWAS <- function(object, phenotype, covariables = NULL, tef = FALSE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("invExWASDS(", object, ", '", phenotype, "', ", tef, if(is.null(covariables)){")"}else{
    paste0(", '", paste(covariables, collapse = "','"), "')")
  })
  res <- DSI::datashield.aggregate(datasources, as.symbol(cally))[[1]]
  
  inv_exwas_results <- res$comparison
  inv_exwas_results <- tibble::rownames_to_column(as.data.frame(inv_exwas_results), "exposure")
  
  assoc <- ds.familyNames(object, TRUE)[[1]]
  assoc <- data.frame(family = assoc, exposure = names(assoc))
  
  inv_exwas_results <- merge(assoc, inv_exwas_results)
  colnames(inv_exwas_results) <- c("exposure", "family", "coefficient", "minE", "maxE", "p.value")
  
  alpha_corrected <- res$effective
  
  results <- list(exwas_results = inv_exwas_results, alpha_corrected = alpha_corrected)
  class(results) <- c(class(results), "dsExWAS_pooled")
  
  return(results)
  
}
