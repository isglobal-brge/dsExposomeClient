#' @title Draw boxplot of a family of an Exposome Set of the server side
#' 
#' @description Draw a boxplot for a single family or a mosaic of boxplots for all the families (only numeric families)
#'
#' @param x \code{character} Name of the Exposome Set on the server side
#' @param family \code{character} Name of the familty that will be drawn.  \code{"all"} is to plot all the families
#' on a single mosaic (no grouping is used for the mosaic plot). 
#' @param group \code{character} If set it displays the family grouped by the given phenotype
#' @param group2 \code{character} If set it displays the family grouped by the given phenotype
#' @param scatter \code{bool} (default \code{TRUE}) If the family to be plotted is continuous, the samples will be shown
#' @param na.omit \code{bool} (default \code{TRUE}) Do not show NA values
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return
#' The function returns a ggplot object when a single family is selected and returns NULL for the mosaic (it renders
#' the plot on the current graphic device of the session)
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.plotFamily <- function(x, family, group = NULL, group2 = NULL, scatter = FALSE, 
                          na.omit=TRUE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(x, datasources)
  
  if(family == "all"){
    
    vplayout <- function(x, y) {
      grid::viewport(layout.pos.row = x, layout.pos.col = y)
    }
    
    nc <- round(sqrt(length(ds.familyNames(x, FALSE, datasources)[[1]])))
    if(nc * nc < length(ds.familyNames(x, FALSE, datasources)[[1]])) {
      nr <- nc + 1
    } else {
      nr <- nc
    }

    ff <- ds.familyNames(x, FALSE, datasources)[[1]]
    
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nr, nc)))
    
    idx <- 1
    for(ii in 1:nr) {
      for(jj in 1:nc) {
        if(idx < length(ff) + 1) {
          plt <- ds.plotFamily(x, stringr::str_replace_all(ff[idx], " ", ""), datasources = datasources)

          plt <- plt + ggplot2::ggtitle(ff[idx])
          if(jj != 1) {
            plt <- plt + ggplot2::ylab("")
          }
          if(ii != nr) {
            plt <- plt + ggplot2::xlab("")
          }
          print(plt, vp = vplayout(ii, jj))
          idx <- idx + 1
        }
      }
    }
    
  }
  else{
    cally <- paste0("plotFamilyDS(", x, ", '", stringr::str_replace_all(family, " ", ""), "', ", if(is.null(group)){NA}else{paste0("'",group,"'")}, ", ",
                    if(is.null(group2)){NA}else{paste0("'",group2,"'")}, ", FALSE, ", na.omit, ")")
    DSI::datashield.assign.expr(datasources, "exposomeFamilyPlotData", as.symbol(cally))
    
    plt <- tryCatch({
      ds.boxPlotGG("exposomeFamilyPlotData", group, group2, xlabel = "Exposure", ylabel = "Measure", type = "pooled", datasources)
    }, error = function(w){
      ggplot2::ggplot() + ggplot2::geom_blank()
    })
      
    return(plt)
  }
}