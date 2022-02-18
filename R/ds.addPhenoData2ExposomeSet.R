#' @title Add Phenotype data to ExpressionSet
#' 
#' @description Add new phenotype data contained on a data.frame to an ExpressionSet. The ExpressionSet
#' may or may not already have phenotype data. If the data.frame contains a phenotype already present 
#' on the ExpressionSet, the server function will throw an exception. The pehnotypes data.frame has to 
#' contain an ID column which does not need to contain exactly the same individuals as the ExpressionSet, 
#' only the matching individuals will be updated, no new individuals will be introduced or removed from the 
#' ExpressionSet
#'
#' @param x \code{character} Name of the ExpressionSet on the study server
#' @param pheno \code{character} Name of the data.frame with the new phenotypes on the 
#' study server
#' @param identifier_ExposomeSet \code{character} (default \code{"ID"}) Name of the ID column on the ExposomeSet
#' @param identifier_new_phenotypes \code{character} (default \code{"ID"}) Name of the ID column on the phenotypes data.frame
#' @param newobj.name \code{character} (default \code{NULL}) If \code{NULL}, the original ExpressionSet will be overwritten,
#' otherwise the new ExpressionSet will be assigned to a variable named after this argument
#' @param complete_cases \code{bool} (default \code{TRUE}) If \code{TRUE} only the matching individuals 
#' between the ExpressionSet and the phenotypes table will be included on the resulting ExpressionSet. If 
#' \code{FALSE} all the individuals on the input ExpressionSet will be on the output ExpressionSet
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates (or overwrites) a data frame on the study server.
#' @export

ds.addPhenoData2ExposomeSet <- function(x, pheno, identifier_ExposomeSet = "ID", identifier_new_phenotypes = NULL,
                            newobj.name = NULL, complete_cases = TRUE, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  if(is.null(newobj.name)){
    newobj.name <- x
  }
  
  checkForExposomeSet(set = x, datasources = datasources)
  cls2 <- dsBaseClient::ds.class(x = pheno, datasources = datasources)
  if(!any((cls2 %in% c("data.frame")))){
    stop("'", pheno, "' is not a 'data.frame'")
  }
  
  cally <- paste0("addPhenoData2ExposomeSetDS(", x, ", ", pheno, ", '", identifier_ExposomeSet, "', ", 
                  if(is.null(identifier_new_phenotypes)){"NULL, "}else{paste0("'",identifier_new_phenotypes,"', ")},
                  complete_cases, ")")
  DSI::datashield.assign.expr(datasources, newobj.name, as.symbol(cally))
  
}
