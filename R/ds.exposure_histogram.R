#' @title Draw histogram for Exposome Set exposure
#' 
#' @description Get a non-disclosive histogram plot for a selected exposure of a server side Exposome Set. 
#' The normality results are calculated using a Shapiro-Wilks test for small samples (n < 5000) and with 
#' Anderson-Darling test for big samples (n >= 5000).
#' 
#' @details When using the \code{show.trans} option, the negative values will be removed for the 
#' sqrt-transformation and the 0 values will be removed for the log-transformation. Please be aware 
#' the removal of those values could yield a DataSHIELD disclosive problem when creating the subset. 
#' For example: If one exposure contains mostly negative values, when removing them to plot the sqrt-transformation 
#' the subset creation will fail.
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
  
  if(length(datasources) > 1){
    message("[warning] only the first Datasource will be used! Multiplot to be implemented")
    datasources <- datasources[1]
  }
  
  checkForExposomeSet(exp, datasources)
  
  checkForExposure(exp, exposure, datasources)
  
  ds.exposures_pData(set = exp, type = "exposures", name = "dta", datasources = datasources)
  
  if(dsBaseClient::ds.class(paste0("dta$", exposure), datasources = datasources) == "numeric"){
    if(show.trans){
      # Get number of column that corresponds to [exposures]
      cols <- dsBaseClient::ds.colnames("dta", datasources = datasources)[[1]]
      col_number <- which(exposure == cols)
      
      # Retrieve number of NAs
      nas <- dsBaseClient::ds.mean(paste0("dta$", exposure), datasources = datasources)$Mean.by.Study[1,]["Nmissing"]
      warning("[", nas, "] expositions eliminated due being NAs")
      
      # Create rep of the length of the table to be used on the dataFrameSubset to keep all the rows
      dsBaseClient::ds.rep(x1 = 0, length.out = dsBaseClient::ds.length(paste0("dta$", exposure), datasources = datasources)[[1]],
             source.x1 = "clientside", source.times = "c", source.length.out = "c",
             source.each = "c", newobj = "ZEROES", datasources = datasources)

      # Remove 0's to apply log transform
      message("Attempting to subset exposure [", exposure, "] by removing 0s")
      dsBaseClient::ds.dataFrameSubset(df.name = "dta",
                         V1.name = "ZEROES",
                         V2.name = paste0("dta$", exposure),
                         Boolean.operator = "!=",
                         keep.cols = col_number,
                         rm.cols = NULL,
                         keep.NAs = FALSE,
                         newobj = "dta_no_zeroes",
                         datasources = datasources,
                         notify.of.progress = FALSE)
      warning("[", dsBaseClient::ds.length(paste0("dta$", exposure), datasources = datasources)[[1]] - dsBaseClient::ds.length("dta_no_zeroes", datasources = datasources)[[1]] - nas,
              "] expositions eliminated for log transformation (0 values)")
      
      # Remove negative values for sqrt transformation
      message("Attempting to subset exposure [", exposure, "] by removing NAs and values <= 0")
      dsBaseClient::ds.dataFrameSubset(df.name = "dta",
                         V1.name = "ZEROES",
                         V2.name = paste0("dta$", exposure),
                         Boolean.operator = "<=",
                         keep.cols = col_number,
                         rm.cols = NULL,
                         keep.NAs = FALSE,
                         newobj = "dta_no_negatives",
                         datasources = datasources,
                         notify.of.progress = FALSE)
      warning("[", dsBaseClient::ds.length(paste0("dta$", exposure), datasources = datasources)[[1]] - dsBaseClient::ds.length("dta_no_negatives", datasources = datasources)[[1]] - nas,
              "] expositions eliminated for sqrt transformation (negative values)")
      
      # Compute transformations
      dsBaseClient::ds.make(paste0("exp(dta$", exposure, ")"), "dta_exp", datasources)
      dsBaseClient::ds.make("log(dta_no_zeroes)", "dta_log", datasources)
      dsBaseClient::ds.make("(dta_no_negatives)^(0.5)", "dta_sqrt", datasources)
      
      grDevices::pdf(NULL)
      hist1 <- dsBaseClient::ds.histogram(x = paste0("dta$", exposure), datasources = datasources, ...)
      grDevices::dev.off()
      grDevices::pdf(NULL)
      hist2 <- dsBaseClient::ds.histogram(x = "dta_exp", datasources = datasources, ...)
      grDevices::dev.off()
      grDevices::pdf(NULL)
      hist3 <- dsBaseClient::ds.histogram(x = "dta_log", datasources = datasources, ...)
      grDevices::dev.off()
      grDevices::pdf(NULL)
      hist4 <- dsBaseClient::ds.histogram(x = "dta_sqrt", datasources = datasources, ...)
      grDevices::dev.off()
      
      individuals <- ds.dim("dta", datasources = datasources)[[1]][1]
      if(individuals > 5000){
        hist1_pval <- ds.anderson.darling.test(paste0("dta$", exposure), datasources = datasources)[[1]]$p.value
        hist2_pval <- ds.anderson.darling.test("dta_exp", datasources = datasources)[[1]]$p.value
        hist3_pval <- ds.anderson.darling.test("dta_log", datasources = datasources)[[1]]$p.value
        hist4_pval <- ds.anderson.darling.test("dta_sqrt", datasources = datasources)[[1]]$p.value
      } else {
        hist1_pval <- ds.shapiro.test(paste0("dta$", exposure), datasources = datasources)[[1]]$p.value
        hist2_pval <- ds.shapiro.test("dta_exp", datasources = datasources)[[1]]$p.value
        hist3_pval <- ds.shapiro.test("dta_log", datasources = datasources)[[1]]$p.value
        hist4_pval <- ds.shapiro.test("dta_sqrt", datasources = datasources)[[1]]$p.value
      }
      
      
      graphics::par(mfrow=c(2,2))
      plot(hist1, main = paste0(exposure, ", raw (pval: ", format(hist1_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      plot(hist2, main = paste0(exposure, ", exp (pval: ", format(hist2_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      plot(hist3, main = paste0(exposure, ", log (pval: ", format(hist3_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      plot(hist4, main = paste0(exposure, ", sqrt (pval: ", format(hist4_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      
      plot_h <- list(hist1, hist2, hist3, hist4)
      
      DSI::datashield.rm(datasources, "dta_exp")
      DSI::datashield.rm(datasources, "dta_log")
      DSI::datashield.rm(datasources, "dta_sqrt")
    }
    else{
      grDevices::pdf(NULL)
      hist <- dsBaseClient::ds.histogram(x = paste0("dta$", exposure), datasources = datasources, ...)
      grDevices::dev.off()
      individuals <- ds.dim("dta", datasources = datasources)[[1]][1]
      if(individuals > 5000){
        hist_pval <- ds.anderson.darling.test(paste0("dta$", exposure), datasources = datasources)[[1]]$p.value
      } else {
        hist_pval <- ds.shapiro.test(paste0("dta$", exposure), datasources = datasources)[[1]]$p.value
      }
      graphics::par(mfrow=c(1,1))
      plot(hist, main = paste0(exposure, " (pval: ", format(hist_pval, scientific=TRUE, digits=5), ")"), xlab = "")
      
      plot_h <- hist
    }
  }
  else{ # Factor
    warning(paste0("ds.exposure_histogram can't take a non-numeric exposure: ", exposure))
    plot_h <- NULL
  }
  
  DSI::datashield.rm(datasources, "dta")
  
  return(plot_h)
  
}
