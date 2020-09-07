#' @title Draw histogram for Exposome Set exposure
#' 
#' @description Get an histogram plot for a selected exposure of a server side Exposome Set
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
#' 

ds.exposure_histogram <- function(exp, exposure, show.trans = FALSE, ..., datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  ds.exposures_pData(exp, "exposures", "dta", datasources)
  
  if(show.trans){
    ds.make(paste0("exp(dta$", exposure, ")"), "dta_exp", datasources)
    ds.make(paste0("log(dta$", exposure, ")"), "dta_log", datasources)
    ds.make(paste0("(dta$", exposure, ")^(0.5)"), "dta_sqrt", datasources)
    
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
    hist <- ds.histogram(x = paste0("dta$", exposure), ...)
    dev.off()
    hist_pval <- ds.shapiro.test(paste0("dta$", exposure))[[1]]$p.value
    par(mfrow=c(1,1))
    plot(hist, main = paste0(exposure, " (pval: ", format(hist_pval, scientific=TRUE, digits=5), ")"), xlab = "")
    
    plot_h <- hist
  }
  
  datashield.rm(datasources, "dta")
  
  return(plot_h)
  
}