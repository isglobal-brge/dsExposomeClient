#' @title Renders boxplot
#' 
#' @description Internal function. Renders a ggplot boxplot by retrieving from the server side a list with the identity stats and other
#' parameters to render the plot without passing any data from the original dataset
#'
#' @param x \code{character} Name on the server side of the data frame to form a boxplot. Structure on the server 
#' of this object must be: \cr
#' 
#'  Column 'x': Names on the X axis of the boxplot, aka variables to plot \cr
#'  Column 'value': Values for that variable (raw data of columns rbinded) \cr
#'  Column 'group': (Optional) Values of the grouping variable \cr
#'  Column 'group2': (Optional) Values of the second grouping variable \cr
#'  
#' @param group \code{character} (default \code{NULL}) Name of the first grouping variable. 
#' @param group2 \code{character} (default \code{NULL}) Name of the second grouping variable. 
#' @param xlabel \code{caracter} (default \code{"x axis"}) Label to put on the x axis of the plot
#' @param ylabel \code{caracter} (default \code{"y axis"}) Label to put on the y axis of the plot
#' @param type \code{character} Return a pooled plot (\code{"pooled"}) or a split plot (one for each study server
#' \code{"split"})
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{ggplot} object



ds.boxPlotGG <- function(x, group = NULL, group2 = NULL, xlabel = "x axis", ylabel = "y axis", type = "pooled", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("boxPlotGGDS(", x, ", ", 
                  if(is.null(group)){paste0("NULL")}else{paste0("'",group,"'")}, ", ", 
                  if(is.null(group2)){paste0("NULL")}else{paste0("'",group2,"'")}, ")")
  
  pt <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  if(type == "pooled"){
    num_servers <- length(names(datasources))
    lower <- matrix(0, nrow = nrow(pt[[1]][[1]]), ncol = 1)
    upper <- matrix(0, nrow = nrow(pt[[1]][[1]]), ncol = 1)
    ymin <- matrix(0, nrow = nrow(pt[[1]][[1]]), ncol = 1)
    ymax <- matrix(0, nrow = nrow(pt[[1]][[1]]), ncol = 1)
    middle <- matrix(0, nrow = nrow(pt[[1]][[1]]), ncol = 1)
    n <- matrix(0, nrow = nrow(pt[[1]][[1]]), ncol = 1)
    for(i in 1:num_servers){
      lower <- lower + pt[[i]][[1]]$lower * pt[[i]]$counts$n
      upper <- upper + pt[[i]][[1]]$upper * pt[[i]]$counts$n
      ymin <- ymin + pt[[i]][[1]]$ymin * pt[[i]]$counts$n
      ymax <- ymax + pt[[i]][[1]]$ymax * pt[[i]]$counts$n
      middle <- middle + pt[[i]][[1]]$middle * pt[[i]]$counts$n
      n <- n + pt[[i]]$counts$n
    }
    
    pt[[1]][[1]]$lower <- lower / n
    pt[[1]][[1]]$upper <- upper / n
    pt[[1]][[1]]$ymin <- ymin / n
    pt[[1]][[1]]$ymax <- ymax / n
    pt[[1]][[1]]$middle <- middle / n
    
    if(pt[[1]][[length(pt[[1]])]] == "single_group"){
      plt <- ggplot2::ggplot(pt[[1]][[1]]) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle,
                                                              group = group, fill = fill)) +
        ggplot2::scale_x_discrete(limits = levels(pt[[1]][[2]])) +
        ggplot2::scale_fill_brewer(name = "Group", labels = rev((levels(pt[[1]][[3]])))) +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    else if(pt[[1]][[length(pt[[1]])]] == "double_group"){
      
      supp.labs <- as.character(levels(pt[[1]][[4]]))
      names(supp.labs) <- seq(from = 1, length.out = length(pt[[1]][[4]]))
      
      plt <- ggplot2::ggplot(pt[[1]][[1]]) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle,
                                                              group = group, fill = fill)) +
        ggplot2::facet_wrap(~ PANEL, labeller = ggplot2::labeller(PANEL = supp.labs)) +
        ggplot2::scale_x_discrete(limits = levels(pt[[1]][[2]])) +
        ggplot2::scale_fill_brewer(name = "Group", labels = rev((as.character(levels(pt[[1]][[3]]))))) +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    else{
      plt <- ggplot2::ggplot(pt[[1]][[1]]) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle,
                                                              group = group)) +
        ggplot2::scale_x_discrete(limits = levels(pt[[1]][[2]])) +
        ggplot2::scale_fill_brewer(name = "Group", labels = (pt[[1]][[3]])) +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    
  }
  else if(type == "split"){ 
    num_servers <- length(names(datasources))
    plt <- NULL
    for(i in 1:num_servers){
      if(pt[[i]][[length(pt[[i]])]] == "single_group"){
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle,
                                                                group = group, fill = fill)) +
          ggplot2::scale_x_discrete(limits = levels(pt[[i]][[2]])) +
          ggplot2::scale_fill_brewer(name = "Group", labels = rev((levels(pt[[i]][[3]])))) +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
      else if(pt[[i]][[length(pt[[i]])]] == "double_group"){
        
        supp.labs <- as.character(levels(pt[[i]][[4]]))
        names(supp.labs) <- seq(from = 1, length.out = length(pt[[i]][[4]]))
        
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle,
                                                                group = group, fill = fill)) +
          ggplot2::facet_wrap(~ PANEL, labeller = ggplot2::labeller(PANEL = supp.labs)) +
          ggplot2::scale_x_discrete(limits = levels(pt[[i]][[2]])) +
          ggplot2::scale_fill_brewer(name = "Group", labels = rev((as.character(levels(pt[[i]][[3]]))))) +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
      else{
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle,
                                                                group = group)) +
          ggplot2::scale_x_discrete(limits = levels(pt[[i]][[2]])) +
          ggplot2::scale_fill_brewer(name = "Group", labels = (pt[[i]][[3]])) +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
    }

    plt <- do.call(gridExtra::grid.arrange, plt)
    
  }
  else{stop("Invalid type")}
  
  
  return(plt)
  
}