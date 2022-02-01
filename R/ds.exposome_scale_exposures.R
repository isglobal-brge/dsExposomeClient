#' Title
#'
#' @param Set 
#' @param new.obj 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.exposome_scale_exposures <- function(Set, method = c("combined", "split"), new.obj = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(Set, datasources)
  
  if(is.null(new.obj)){
    new.obj <- "exposomeSet_scaled"
  }
  
  if(method == "combined"){
    # Get ExposomeSet meta-means and n
    cally <- paste0("exposome_scale_exposures_meansDS(", Set, ")")
    means <- DSI::datashield.aggregate(datasources, cally)
    # Calculate pooled means
    means_servers <- Reduce("rbind", lapply(means, function(x){
      x$means
    }))
    ns_servers <- Reduce("rbind", lapply(means, function(x){
      x$n
    }))
    ns_totals <- colSums(ns_servers)
    
    means_x_ns <- as.matrix(means_servers) * as.matrix(ns_servers)
    means_x_ns_total <- colSums(means_x_ns)
    
    pooled_means <- means_x_ns_total / ns_totals

    # Get pooled SDs
    ds.exposures_pData(Set, "exposures", "aux_exposures", "numeric", datasources)
    cols <- ds.colnames("aux_exposures")[[1]]
    variance <- unlist(lapply(cols, function(x){
      ds.var(paste0("aux_exposures$", x), type = "combined", datasources = datasources)$Global.Variance[, 1]
    }))
    pooled_sds <- sqrt(variance)
    
    # Create new scaled exposomeSets
    cally <- paste0("exposome_scale_exposuresDS(", Set, ", means = c(",
                    paste(unlist(pooled_means), collapse = ","),
                    "), sds = c(",  paste(unlist(pooled_sds), collapse = ","),"))")
    DSI::datashield.assign.expr(datasources, new.obj, cally)
    
  } else if (method == "split") {
    cally <- paste0("standardizeDS(", Set, ")")
    DSI::datashield.assign.expr(datasources, new.obj, cally)
  } else {
    stop("Invalid 'method' argument. Valid options are ['combined', 'split']")
  }
  
  if(method == "combined"){
    datashield.rm(datasources, "aux_exposures")
  }
  
}
