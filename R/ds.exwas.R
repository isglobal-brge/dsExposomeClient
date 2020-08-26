#' @title Performs a non-disclosive EXposome-Wide Association Study
#' 
#' @description Takes as input an Exposome Set object on the study server and performs a 
#' ExWAS association study with the provided model.
#'
#' @param model \code{character} Formula, not including exposures, to be tested.
#' @param Set \code{character} Name of the Exposome Set object on the server side
#' @param family \code{character} Nature of the health outcome (\code{gaussian}, \code{binomial} or \code{poisson})
#' @param datasources  a list of \code{\link{DSConnection-class}} objects obtained after login
#' @param tef \code{bool} If \code{TRUE} computes the threshold for effective tests.
#'
#' @return \code{list} with: \code{data.frame} With exposure name, coefficient and p-value of the association,
#' and \code{numeric} effective tests
#'
#' @examples 
#' \dontrun{Refer to the package Vignette for examples.}
#' 

ds.exwas <- function(model, Set, family, tef = TRUE, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if(is.null(model)){
    stop(" Please provide a valid model formula", call.=FALSE)
  }

  # Extract table with exposures and phenotypes and save it on the server (assign)
  cally <- paste0("exposures_pData(", Set, ")")
  DSI::datashield.assign.expr(datasources, "dta", as.symbol(cally))

  # Source exposures list
  cally <- paste0("exposureNamesDS(", Set, ")")
  exposure_names <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  form <- as.character(formula(model))

  # Copy 'dta' into 'dta_all', 'dta' will be modified on each loop iteration
  DSI::datashield.assign.expr(datasources, symbol = "dta_all", quote(dta))

  items <- NULL
  for (exposure in exposure_names[[1]]) {# exposure_names) {
    # Build formula: pheno_objective ~ exposure + adjusting phenotypes
    frm <- as.formula(paste0(form[2], "~", exposure, "+", form[3]))
    # Get column indexes of pheno_objective, exposure and adjusting phenotypes
    cols_to_keep <- as.numeric(which(unlist(ds.colnames("dta_all")[1]) %in% all.vars(frm)))
    # Overwrite 'dta' with only the columns of pheno_objective, exposure and adjusting phenotypes
    # taken from 'dta_all'
    cally <- paste0("subsetDS(dt = 'dta_all', cs = c(",  paste0(cols_to_keep, collapse = ", "), "), complt = FALSE)")
    DSI::datashield.assign.expr(datasources, "dta", as.symbol(cally))

    # To do: Filter out rows that contain NAs from 'dta'!

    tryCatch({
      # Fit GLM using the non-disclosive function
      mod <- ds.glm(frm, family = family, data = 'dta', viewIter = FALSE)
      items <- rbind(items, cbind(exposure, mod$coefficients[2], mod$coefficients[5]))
    }, error = function(e){print("lesgo")})

  }

  colnames(items) <- c("exposure", "coefficient", "p-value")
  
  if(tef){
    # Extract table with exposures and save it on the server (assign)
    cally <- paste0("exposures_pData(", Set, ", 'exposures')")
    DSI::datashield.assign.expr(datasources, "dta_exposures", as.symbol(cally))
    # Extract correlation matrix
    corr <- ds.cor(x = "dta_exposures", naAction = "casewise.complete", type = "combine")
    # If it's potentially disclosive it will be full of NA
    if(all(is.na(corr$`Correlation Matrix`))){
      # If potentially disclosive display warning and return alpha_corrected = 0
      warning("Can't compute number of effective tests as it may be disclosive", call. = FALSE)
      alpha_corrected <- 0
    }
    else{
      cormat <- extract(corr)
      M <- ncol(cormat)
      lambdas <- base::eigen(cormat)$values
      Vobs <- sum(((lambdas - 1)^2)) / (M - 1)
      Meff <- M - sum((lambdas>1)*(lambdas-1))
      alpha_corrected <- 1 - (1 - 0.05)^(1 / Meff)
    }
  }
  else{
    alpha_corrected <- 0
  }
  
  
  return(list(items, alpha_corrected))

}
