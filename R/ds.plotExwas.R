#' @title Plot ExWAS results
#' 
#' @description Two different plots for the results of \code{\link{ds.exwas}}.
#'
#' @param exwas \code{list} Output of \code{\link{ds.exwas}}
#' @param type \code{character} Type of plot \code{"manhattan"} for a manhattan plot (p-values),
#' \code{"effect"} for a plot of the exposures effects.
#'
#' @return \code{ggplot} object
#'
#' @examples
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.plotExwas <- function(exwas, type = "manhattan"){
  
  exwas$exwas_results$dir <- ifelse(exwas$exwas_results$coefficient >= 0,"+", "-")
  
  nm <- unique(as.character(exwas$exwas_results$family))
  colorPlte <- sample(grDevices::rainbow(length(nm)))
  names(colorPlte) <- nm
  
  exwas$exwas_results$p.value <- -log10(exwas$exwas_results$p.value)
  
  # Plot style from rexposome::plotExwas()
  if(type == "manhattan"){
    plt <- ggplot2::ggplot(exwas$exwas_results, ggplot2::aes_string(x = "p.value", y = "exposure", color = "family", shape = "dir")) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.spacing = ggplot2::unit(0.5, 'lines'),
                     strip.text.y = ggplot2::element_text(angle = 0)) +
      ggplot2::ylab("") +
      ggplot2::xlab(expression(-log10(pvalue))) +
      ggplot2::labs(colour="Exposure's Families", shape="Exposure's Effect") +
      ggplot2::scale_color_manual(breaks = names(colorPlte),
                                  values = colorPlte) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::geom_vline(
        xintercept = -log10(exwas$alpha_corrected), colour="Brown")
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
  }
  else{stop(paste0("Invalid plot type: ", type))}
  if(identical(exwas$exwas_results$exposure, exwas$exwas_results$family)){
    plt <- plt + aes(color = NULL)
  }
  return(plt)
  
}