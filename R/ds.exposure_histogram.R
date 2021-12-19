#' @title Draw histogram for Exposome Set exposure
#' 
#' @description Get a non-disclosive histogram plot for a selected exposure of a server side Exposome Set
#'
#' @param exp \code{character} Name of the Exposome Set on the server side
#' @param exposure \code{character} Name of the exposure of the exposome set to visualize
#' @param show.trans \code{bool} (default \code{FALSE}) To show (\code{TRUE}) or not (\code{FALSE}) how
#' the exposure histogram would look like if \code{log/exp/sqrt} transformed
#' @param ... Parameters passed to the \code{\link{ds.histogram}} function 
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return List with plot parameters
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exposure_histogram <- function(exp, exposure, show.trans = FALSE, ..., datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(exp, datasources)
  
  checkForExposure(exp, exposure, datasources)
  
  ds.exposures_pData(exp, "exposures", "dta", datasources)
  
  if(ds.class(paste0("dta$", exposure), datasources) == "numeric"){
    if(show.trans){
      # Get number of column that corresponds to [exposures]
      cols <- ds.colnames("dta", datasources = datasources)[[1]]
      col_number <- which(exposure == cols)
      
      # Create rep of the length of the table to be used on the dataFrameSubset to keep all the rows
      ds.rep(x1 = 0, length.out = ds.length(paste0("dta$", exposure), datasources = datasources)[[1]],
             source.x1 = "clientside", source.times = "c", source.length.out = "c",
             source.each = "c", newobj = "ZEROES", datasources = datasources)

      # Remove 0's to apply log transform
      message("Attempting to subset exposure [", exposure, "] by removing 0s")
      ds.dataFrameSubset(df.name = "dta",
                         V1.name = "ZEROES",
                         V2.name = paste0("dta$", exposure),
                         Boolean.operator = "!=",
                         keep.cols = col_number,
                         rm.cols = NULL,
                         keep.NAs = FALSE,
                         newobj = "dta_no_zeroes",
                         datasources = datasources,
                         notify.of.progress = FALSE)
      warning("[", ds.length(paste0("dta$", exposure))[[1]] - ds.length("dta_no_zeroes")[[1]],
              "] expositions eliminated for log transformation (0 values)")
      
      # Remove negative values for sqrt transformation
      message("Attempting to subset exposure [", exposure, "] by removing NAs and values <= 0")
      ds.dataFrameSubset(df.name = "dta",
                         V1.name = "ZEROES",
                         V2.name = paste0("dta$", exposure),
                         Boolean.operator = "<=",
                         keep.cols = col_number,
                         rm.cols = NULL,
                         keep.NAs = FALSE,
                         newobj = "dta_no_negatives",
                         datasources = datasources,
                         notify.of.progress = FALSE)
      warning("[", ds.length(paste0("dta$", exposure))[[1]] - ds.length("dta_no_negatives")[[1]],
              "] expositions eliminated for sqrt transformation (negative values)")
      
      # Compute transformations
      ds.make(paste0("exp(dta$", exposure, ")"), "dta_exp", datasources)
      ds.make("log(dta_no_zeroes)", "dta_log", datasources)
      ds.make("(dta_no_negatives)^(0.5)", "dta_sqrt", datasources)
      
      pdf(NULL)
      hist1 <- ds.histogram(x = paste0("dta$", exposure), ...)
      dev.off()
      pdf(NULL)
      hist2 <- ds.histogram(x = "dta_exp", ...)
      dev.off()
      pdf(NULL)
      hist3 <- ds.histogram(x = "dta_log", ...)
      dev.off()
      pdf(NULL)
      hist4 <- ds.histogram(x = "dta_sqrt", ...)
      dev.off()
      
      hist1_pval <- ds.shapiro.test(paste0("dta$", exposure))[[1]]$p.value
      hist2_pval <- ds.shapiro.test("dta_exp")[[1]]$p.value
      hist3_pval <- ds.shapiro.test("dta_log")[[1]]$p.value
      hist4_pval <- ds.shapiro.test("dta_sqrt")[[1]]$p.value
      
      par(mfrow=c(2,2))
      plot(hist1, main = paste0(exposure, ", raw (pval: ", format(hist1_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      plot(hist2, main = paste0(exposure, ", exp (pval: ", format(hist2_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      plot(hist3, main = paste0(exposure, ", log (pval: ", format(hist3_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      plot(hist4, main = paste0(exposure, ", sqrt (pval: ", format(hist4_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      
      plot_h <- list(hist1, hist2, hist3, hist4)
      
      datashield.rm(datasources, "dta_exp")
      datashield.rm(datasources, "dta_log")
      datashield.rm(datasources, "dta_sqrt")
    }
    else{
      pdf(NULL)
      hist <- ds.histogram(x = paste0("dta$", exposure), ...)
      dev.off()
      hist_pval <- ds.shapiro.test(paste0("dta$", exposure))[[1]]$p.value
      par(mfrow=c(1,1))
      plot(hist, main = paste0(exposure, " (pval: ", format(hist_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      
      plot_h <- hist
    }
  }
  else{ # Factor
    warning(paste0("ds.exposure_histogram can't take a non-numeric exposure: ", exposure))
    plot_h <- NULL
  }
  
  
  datashield.rm(datasources, "dta")
  
  return(plot_h)
  
}
