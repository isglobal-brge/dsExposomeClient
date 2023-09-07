#' @title Plot ExWAS results
#' 
#' @description Two different plots for the results of \code{\link{ds.exwas}}.
#'
#' @param exwas \code{list} Output of \code{\link{ds.exwas}}
#' @param type \code{character} Type of plot \code{"manhattan"} for a manhattan plot (p-values),
#' \code{"effect"} for a plot of the exposures effects.
#' @param thld_pvalue \code{numeric} (default \code{NULL}) Significance P.value threshold to visualize. 
#' If \code{NULL}, the exwas alpha corrected will be used
#'
#' @return \code{ggplot} object
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.plotExwas <- function(exwas, type = "manhattan", thld_pvalue = NULL){
  
  if(inherits(exwas, "dsExWAS_pooled")){
    plot_exwas <- exwasplotF(exwas, type, thld_pvalue)
  } else if (inherits(exwas, "dsExWAS_meta")) {
    plot_exwas <- lapply(exwas, function(x){
      exwasplotF(x, type, thld_pvalue)
    })
  } else {
    stop("Object passed is not of class ['dsExWAS_pooled' or 'dsExWAS_meta']. Generate those objects with `ds.exwas()`")
  }
  return(plot_exwas)
}

#' @title ExWAS plotter
#'
#' @param exwas \code{dsExWAS_meta} or \code{dsExWAS_pooled} Object produced by the \code{ds.exwas} function
#' @param type \code{character} Type of plot \code{"manhattan"} for a manhattan plot (p-values),
#' \code{"effect"} for a plot of the exposures effects.
#' @param thld_pvalue \code{numeric} Significance P.value threshold to visualize. If \code{NULL}, the exwas
#' alpha corrected will be used
#'
#' @return \code{ggplot} object
#' 

exwasplotF <- function(exwas, type, thld_pvalue = NULL){
  
  exwas$exwas_results$dir <- ifelse(exwas$exwas_results$coefficient >= 0,"+", "-")
  
  nm <- unique(as.character(exwas$exwas_results$family))
  colorPlte <- viridis::viridis(length(nm), option = "H")
  names(colorPlte) <- nm
  if(length(colorPlte) > length(nm)){
    colorPlte <- colorPlte[1:length(nm)]
  }
  
  exwas$exwas_results$p.value <- -log10(exwas$exwas_results$p.value)
  
  # Plot style from rexposome::plotExwas()
  if(type == "manhattan"){
    if(length(unique(exwas$exwas_results$family)) == 1){
      plt <- ggplot2::ggplot(exwas$exwas_results, ggplot2::aes_string(y = "p.value", x = "exposure", shape = "dir"))
    } else {
      plt <- ggplot2::ggplot(exwas$exwas_results, ggplot2::aes_string(y = "p.value", x = "exposure", color = "family", shape = "dir"))
    }
    plt <- plt +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.spacing = ggplot2::unit(0.5, 'lines'),
                     strip.text.y = ggplot2::element_text(angle = 0),
                     axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("") +
      ggplot2::ylab(expression(-log10(pvalue))) +
      ggplot2::labs(colour="Exposure's Families", shape="Exposure's Effect") +
      ggplot2::scale_color_manual(breaks = names(colorPlte),
                                  values = colorPlte) +
      ggplot2::theme(legend.position = "bottom")
      if(is.null(thld_pvalue)){
        repel_data <- exwas$exwas_results[exwas$exwas_results$p.value >= -log10(exwas$alpha_corrected),,drop=FALSE]
        intercept <- -log10(exwas$alpha_corrected)
      } else {
        repel_data <- exwas$exwas_results[exwas$exwas_results$p.value >= -log10(thld_pvalue),,drop=FALSE]
        intercept <- -log10(thld_pvalue)
      }
    plt <- plt + ggplot2::geom_hline(
      yintercept = intercept, colour="red") +
      ggrepel::geom_label_repel(
        data = repel_data,
        ggplot2::aes(label = exposure),
        color = "black",
        min.segment.length = 0,
        box.padding = 2
      )
  }
  else if(type == "effect"){
    plt <- ggplot2::ggplot(exwas$exwas_results, ggplot2::aes_string(x = "coefficient", y = "exposure")) +
      ggplot2::geom_point(shape=18, size=5, color="gray60") +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "minE", xmax = "maxE")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "WhiteSmoke", size = 0.3, linetype = "dashed"),
        panel.grid.minor = ggplot2::element_line(color = "gray40", size = 0.3, linetype = "dashed")
      ) + 
      ggplot2::ylab("") +
      ggplot2::xlab("effect")
  } else {stop("Invalid plot type: ", type)}
  return(plt)
  
}
