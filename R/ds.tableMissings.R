#' @title Get table of missings per variable
#' 
#' @description Obtain the number (or percentage) of missings of for the exposures or phenotypes of an ExposomeSet object
#'
#' @param exp \code{character} Name of the Exposome Set on the server side
#' @param set \code{character} (default \code{"exposures"}) Set to get the missings: \code{"exposures"} or \code{"phenotypes"}
#' @param output \code{character} (default \code{"n"}) Get missing number (\code{"n"}) or percentage (\code{"p"})
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Vector of named numerics, where the name corresponds to the variable and the associated numeric corresponds
#' to the missing (number or percentage, dependeing on the \code{output} parameter)
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.tableMissings <- function(exp, type = "pooled", set = "exposures", output = "n", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(exp, datasources)
  
  cally <- paste0("tableMissingsDS(", exp, ", '", set, "', '", output, "')")
  missings <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  if(type == "pooled"){
    if(output == "p"){
      ds.exposures_pData(set = exp, type = "all", name = "ds.tableMissings_aux", datasources = datasources)
      dimensions <- ds.dim(x = "ds.tableMissings_aux", type = "split", datasources = datasources)
      pooled_dimensions <- sum(unlist(lapply(dimensions, function(x){x[1]})))
      dimensionsPerMissings <- lapply(1:length(dimensions), function(x){
        dimensions[[x]][1] * missings[[x]]
      })
      sumdimensionsPerMissings <- rowSums(data.frame(dimensionsPerMissings))
      missings <- list(pooled = sumdimensionsPerMissings / pooled_dimensions)
    } else if (output == "n") {
      pooled_n <- rowSums(data.frame(missings))
      missings <- list(pooled = pooled_n)
    }
  }

  names(set) <- "set"
  names(output) <- "output"
  missings <- base::append(missings, set)
  missings <- base::append(missings, output)

  class(missings) <- c(class(missings), "ds.tableMissings")
  
  return(missings)
  
}
