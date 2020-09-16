#' @title Draws correlation plot
#' 
#' @description Takes the output of the \code{\link{ds.correlation}} function and draws a plot (circos or matrix)
#'
#' @param corr_list \code{list} Output of \code{\link{ds.correlation}}
#' @param type \code{character} Type of the plot, \code{"matrix"} for a matrix plot or \code{"circos"} for a circos
#' plot
#'
#' @return Renders the selected plot on the default graphical device
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export
 
corPlot <- function(corr_list, type = "matrix"){
  # ONLY USING ONE STUDY SERVER!
  corr <- corr_list[[1]][[1]]$`Correlation Matrix`
  colnames(corr) <- names(corr_list[[2]][[1]])
  corr <- corr[order(corr_list[[2]][[1]]), order(corr_list[[2]][[1]])]
  names <- names(corr_list[[2]][[1]])
  names <- names[order(corr_list[[2]][[1]])]
  fam <- corr_list[[2]][[1]]
  fam <- fam[order(corr_list[[2]][[1]])]
  desc <- data.frame(Family = fam)
  rownames(desc) <- names
  
  if(type == "matrix"){
    matrix_corPlotEXP(corr, desc)
  }
  else if(type == "circos"){
    circos_corPlotEXP(corr, desc)
  }
  
}
