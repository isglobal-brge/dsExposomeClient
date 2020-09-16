#' @title Draw plot with percentage of missings
#' 
#' @description Create a plot with the percentage of missings for the exposures and phenotypes of an 
#' ExposomeSet object
#'
#' @param exp \code{character} Name of the Exposome Set on the server side
#' @param set \code{character} (default \code{"exposures"}) Set to get the missings plot: \code{"exposures"} or \code{"phenotypes"}
#' @param output \code{character} (default \code{"n"}) Get missing number (\code{"n"}) or percentage (\code{"p"})
#' @param x.max \code{numeric} (default \code{100})  Fix the maxium value of the X-axis for the percentage plot.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return List of objects of class \code{ggplot}, calling it will render the actual plot
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.plotMissings <- function(exp, set = "exposures", output = "n", x.max = 100, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if(ds.exists(exp, datasources) == FALSE){
    stop(paste0("ExposomeSet, ", exp, ", not defined on the study servers"))
  }
  
  if(ds.class(exp, datasources) != "ExposomeSet"){
    stop(paste0(exp, ", is not of class ExposomeSet"))
  }
  
  missings <- ds.tableMissings(exp, set, output, datasources)

  plot <- list()  

  if(output == 'n'){
    for(i in 1:length(missings)){
      plot[[i]] <- ggplot2::ggplot(data.frame(missings[[i]]),
                              ggplot2::aes(seq_along(missings[[i]]), missings[[i]], fill = missings[[i]])) +
        ggplot2::geom_bar(stat = "identity", width = 1)
      plot[[i]] <- plot[[i]] + ggplot2::theme_bw() + ggplot2::xlim(names(missings[[i]]))
      plot[[i]] <- plot[[i]] + ggplot2::scale_fill_continuous(name = "%",
                                                    breaks = seq(0, 100, 20),
                                                    limits = c(0, 100), low="violet", high="violetred4")
      plot[[i]] <- plot[[i]] + ggplot2::ylab("Missing Data (ocurrences)")
      plot[[i]] <- plot[[i]] + ggplot2::xlab(set)
      plot[[i]] <- plot[[i]] + ggplot2::coord_flip()
    }
  }
  else{
    for(i in 1:length(missings)){
      plot[[i]] <- ggplot2::ggplot(data.frame(missings[[i]]) * 100,
                                   ggplot2::aes(seq_along(missings[[i]]), missings[[i]] * 100, fill = missings[[i]] * 100)) +
        ggplot2::geom_bar(stat = "identity", width = 1)
      plot[[i]] <- plot[[i]] + ggplot2::theme_bw() + ggplot2::xlim(names(missings[[i]]))
      plot[[i]] <- plot[[i]] + ggplot2::scale_fill_continuous(name = "%",
                                                              breaks = seq(0, 100, 20),
                                                              limits = c(0, 100), low="violet", high="violetred4")
      plot[[i]] <- plot[[i]] + ggplot2::ylab("Missing Data %")
      plot[[i]] <- plot[[i]] + ggplot2::xlab(set)
      plot[[i]] <- plot[[i]] + ggplot2::coord_flip()
      plot[[i]] <- plot[[i]] + ggplot2::scale_y_continuous(limits = c(0, x.max))
    }
  }
  
  names(plot) <- names(datasources)
  return(plot)
  
}