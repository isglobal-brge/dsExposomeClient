ds.plotFamily <- function(x, family, group = NULL, group2 = NULL, na.omit=TRUE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
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
    de
    ff <- ds.familyNames(x, FALSE, datasources)[[1]]
    
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nr, nc)))
    
    idx <- 1
    for(ii in 1:nr) {
      for(jj in 1:nc) {
        if(idx < length(ff) + 1) {
          cally <- paste0("plotFamilyDS(", x, ", '", ff[idx], "')")
          plt <- DSI::datashield.aggregate(datasources, as.symbol(cally))[[1]]
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
    cally <- paste0("plotFamilyDS(", x, ", '", family, "', ", if(is.null(group)){NA}else{paste0("'",group,"'")}, ", ",
                    if(is.null(group2)){NA}else{paste0("'",group2,"'")}, ", FALSE, ", na.omit, ")")
    pt <- DSI::datashield.aggregate(datasources, as.symbol(cally))

    return(pt)
  }
}