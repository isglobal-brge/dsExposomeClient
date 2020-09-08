ds.plotFamily <- function(x, family, group = NULL, group2 = NULL, na.omit=TRUE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("plotFamilyDS(", x, ", '", family, "', ", if(is.null(group)){NA}else{paste0("'",group,"'")}, ", ", 
                  if(is.null(group2)){NA}else{paste0("'",group2,"'")}, ", FALSE, ", na.omit, ")")
  pt <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  return(pt)
}