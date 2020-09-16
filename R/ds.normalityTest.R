#' @title Normality test of exposures of ExposomeSet
#' 
#' @description Perform a normality test on all the exposures of an ExposomeSet object of the server side
#'
#' @param object \code{character} Name of the Exposome Set on the server side
#' @param th \code{numeric} (default \code{0.05}) Threshold to considere an exposure to follow a normal distribution.
#' @param min.val \code{numeric} (default \code{5}) Minimum number of values not missings to test the exposures.
#' @param na.rm \code{bool} (default \code{TRUE}) Removes the NA values to test the normality on the exposure.
#' @param warnings \code{bool} (default \code{TRUE}) Show warnings if required.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return list of \code{data.frame} (one for each study server) with \cr
#' - exposure: \code{character} Name of the exposures \cr
#' - normality: \code{bool} If exposure is normal (\code{TRUE}) or not (\code{FALSE}) \cr
#' - p.value: \code{numeric} P-value of the Shapiro-Wilk Normality Test
#' 
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.normalityTest <- function(object, th = 0.05, min.val = 5, na.rm = TRUE,
                             warnings = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("normalityTestDS(", object, ", ", th, ", ", min.val, ", ", na.rm, ", ", warnings, ")")
  norm <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  return(norm)
  
}