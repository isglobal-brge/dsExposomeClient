#' Title
#'
#' @param pca_object 
#' @param set 
#' @param phenotype 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.exposome_pca_plot <- function(pca_object, set = "all", labels = FALSE, phenotype = NA, 
                                 method = 1, k = 5, noise = 10, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("exposome_pca_plotDS(", pca_object, ", set = '", set, "', phenotype = ", 
                  if(is.na(phenotype)){paste0("NA")}else{paste0("'",phenotype,"'")}, ", ", method,
                  ", ", k, ", ", noise, ")")
  output <- DSI::datashield.aggregate(datasources, as.symbol(cally))[[1]]

  if(set == "exposures"){
    plt <- ggplot2::ggplot(output$data) + 
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, group = group, colour = output$fams), stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotdash", color = "red", size = 1) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "red", size = 1) +
      ggplot2::xlab(output$xlabel) +
      ggplot2::ylab(output$ylabel)
    
    if(labels){
      plt <- plt + ggrepel::geom_text_repel(
        data = output$labels,
        ggplot2::aes_string("x", "y", label="label"),
        size = 3,
        box.padding = ggplot2::unit(0.35, "lines"),
        point.padding = ggplot2::unit(0.3, "lines"),
        color="#222222",
        segment.color="#BBBBBB"
      )
    }
  }
  else if(set == "samples"){
    plt <- ggplot2::ggplot(output$data) + 
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, group = group, colour = output$pheno), stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotdash", color = "red", size = 1) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "red", size = 1) +
      ggplot2::xlab(output$xlabel) +
      ggplot2::ylab(output$ylabel)
  }
  else if(set == "variance"){
    xticks <- paste0("PC", stringr::str_pad(1:length(output$data$x), pad = "0", width = 2))
    
    plt <- ggplot2::ggplot(output$data) + 
      ggplot2::geom_bar(ggplot2::aes(x = x, y = y), fill = output$data$fill, stat = "identity") + 
      ggplot2::geom_line(ggplot2::aes(x = x, y = y), colour = "LightSeaGreen", size = 1.3) + 
      ggplot2::geom_point(ggplot2::aes(x = x, y = y), size = 2, colour = "LightSeaGreen") +
      ggplot2::xlab("") +
      ggplot2::ylab(output$ylabel) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = output$data$x, labels = xticks) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  }
  else if(set == "variance_explained"){
    xticks <- paste0("PC", stringr::str_pad(1:length(output$data$x), pad = "0", width = 2))
    
    plt <- ggplot2::ggplot(output$data) + 
      ggplot2::geom_line(ggplot2::aes(x = x, y = y), colour = "LightSeaGreen", size = 1.3) + 
      ggplot2::geom_point(ggplot2::aes(x = x, y = y), size = 2, colour = "LightSeaGreen") +
      ggplot2::geom_vline(xintercept = output$yline$xintercept, colour = "FireBrick", size = 1, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = output$xline$yintercept, colour = "FireBrick", size = 1, linetype = "dashed") +
      ggplot2::xlab("") +
      ggplot2::ylab(output$ylabel) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = output$data$x, labels = xticks) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  }
  else if(set == "exposures_correlation"){
    plt <- ggplot2::ggplot(output$data, ggplot2::aes_string(x = "Dim", y = "Exposures")) + 
      ggplot2::geom_tile(ggplot2::aes_string(fill = "value"), color = "white") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90)
      ) +
      ggplot2::labs(fill="Correlation\n", colour="") +
      ggplot2::xlab("") + ggplot2::ylab("exposures") +
      ggplot2::scale_fill_gradient2(midpoint=0, low="red", mid="white",
                                    high="blue", space ="Lab")
  }
  else if(set == "phenotypes_correlation"){
    plt <- ggplot2::ggplot(output$data, ggplot2::aes_string(x = "Dim", y = "variable")) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = "PV"), color = "white") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90)
      ) +
      ggplot2::labs(fill="-log10(P-Value)\n", colour="") +
      ggplot2::xlab("") + 
      ggplot2::ylab("phenotype") +
      ggplot2::scale_fill_continuous(limits = c(0, max(-log10(output$data$value))), low="White", high="Navy")
  }
  else if(set == "all"){
    plt1 <- ds.exposome_pca_plot(pca_object, "exposures", FALSE, phenotype, method, k, noise, datasources) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Exposures Space")
    plt2 <- ds.exposome_pca_plot(pca_object, "samples", labels, NA, method, k, noise, datasources) +
      ggplot2::ggtitle("Samples Space")
    plt3 <- ds.exposome_pca_plot(pca_object, "variance", labels, phenotype, method, k, noise, datasources) +
      ggplot2::ggtitle("Explained Variance")
    plt4 <- ds.exposome_pca_plot(pca_object, "variance_explained", labels, phenotype, method, k, noise, datasources) +
      ggplot2::ggtitle("Accum. Explained Variance")
    plt <- gridExtra::grid.arrange(plt1, plt2, plt3, plt4, ncol = 2)
  }
  else{stop(paste0("Selected invalid set: ", set))}
  
  
  return(plt)
  
}