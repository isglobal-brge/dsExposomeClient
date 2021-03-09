#' @title Create an ExposomeSet from \code{data.frames}
#' 
#' @description Takes the three tables of an exposome dataset and coerces them into a Exposome Set object
#' on the study server
#'
#' @param exposures \code{character} Name of the exposures variable on the study server
#' @param description \code{character} Name of the description variable on the study server. If there is no description file 
#' input \code{NULL}, each exposure will be assigned the same family name as it's exposure name.
#' @param phenotypes \code{character} Name of the phenotypes variable on the study server
#' @param exposures.idcol \code{character} (default \code{"idcol"}) Name of the column in the Exposures file
#' that contains the individuals ID
#' @param phenotypes.idcol \code{character} (default \code{"idcol"}) Name of the column in the Phenotypes file
#' that contains the individuals ID
#' @param description.expCol \code{character} (default \code{"exposure"}) Name of the column in the Description file
#' that contains the Exposure names
#' @param description.famCol \code{character} (default \code{"family"}) Name of column where the family's
#' name (per exposures) if found in file "description". 
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
#' @export

ds.loadExposome <- function(exposures, description, phenotypes, 
                            exposures.idcol = "idcol", phenotypes.idcol = "idcol",
                            description.expCol = "exposure", description.famCol = "family", 
                            exposures.asFactor = 5, warnings = FALSE, object_name = "exposome_set", 
                            datasources = NULL) {
  
  if(is.null(exposures) | class(exposures) != "character"){
    stop("Input variable 'exposures' must have a value which is a character string")
  }
  
  if(!is.null(description) & class(description) != "character"){
    stop("Input variable 'description' must have a value which is a character string")
  }
  
  if(is.null(phenotypes) | class(phenotypes) != "character"){
    stop("Input variable 'phenotypes' must have a value which is a character string")
  }

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("loadExposomeDS(", exposures, ", ", if(is.null(description)){"NULL"}else{description}, ", ", phenotypes, ", '",
                  exposures.idcol, "', '", phenotypes.idcol, "', '", description.expCol, "', '",
                  description.famCol, "', ", exposures.asFactor, ", ", warnings, ")")

  DSI::datashield.assign.expr(datasources, object_name, as.symbol(cally))

}
