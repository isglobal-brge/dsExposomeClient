#' @title Create an ExposomeSet from \code{data.frames}
#' 
#' @description Takes the three tables of an exposome dataset and coerces them into a Exposome Set object
#' on the study server
#'
#' @param exposures \code{character} Name of the exposures variable on the study server
#' @param description \code{character} Name of the description variable on the study server
#' @param phenotypes \code{character} Name of the phenotypes variable on the study server
#' @param description.famCol (default \code{"family"}) Index where the family's
#' name (per exposures) if found in file "description". It can be both numeric
#' or character.
#' @param exposures.asFactor \code{numeric} (default \code{5}) The exposures with more
#' than this number of unique items will be considered as "continuous" while
#' the exposures with less or equal number of items will be considered as
#' "factor".
#' @param warnings \code{bool} (default \code{FALSE}) If \code{TRUE} shows useful
#' information/warnings from the process of loading the exposome.
#' @param object_name \code{character} (default \code{"exposome_set"}) Name assigned to the Exposome Set on the study server
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates an Exposome Set object on the study server.
#'
#' @examples 
#' \dontrun{Refer to the package Vignette for examples.}
#' 

ds.loadExposome <- function(exposures, description, phenotypes, description.famCol = "family", 
                            exposures.asFactor = 5, warnings = FALSE, object_name = "exposome_set", 
                            datasources = NULL) {
  
  if(is.null(exposures) | class(exposures) != "character"){
    stop("Input variable 'exposures' must have a value which is a character string")
  }
  
  if(is.null(description) | class(description) != "character"){
    stop("Input variable 'description' must have a value which is a character string")
  }
  
  if(is.null(phenotypes) | class(phenotypes) != "character"){
    stop("Input variable 'phenotypes' must have a value which is a character string")
  }

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(class(description.famCol) == "character") {
    cally <- paste0("loadExposomeDS(", exposures, ", ", description, ", ", phenotypes, ", '", 
                    description.famCol, "', ", exposures.asFactor, ", ", warnings, ")")
  }
  else{
    cally <- paste0("loadExposomeDS(", exposures, ", ", description, ", ", phenotypes, ", ", 
                    description.famCol, ", ", exposures.asFactor, ", ", warnings, ")")
  }
  
  DSI::datashield.assign.expr(datasources, object_name, as.symbol(cally))

}
