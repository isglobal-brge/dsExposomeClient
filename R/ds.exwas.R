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
#' @return \code{list} that contains: \cr
#' - \code{data.frame} with exposure name, coefficient and p-value of the association \cr
#' - \code{numeric} effective tests
#'
#' @examples 
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exwas <- function(model, Set, family, tef = TRUE, datasources = NULL) {
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(Set, datasources)

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
      items <- rbind(items, cbind(exposure, mod$coefficients[2, 1], mod$coefficients[2, 5], 
                                  mod$coefficients[2, 6], mod$coefficients[2, 4]))
      # return(mod$coefficients)
    }, error = function(e){print("ds.GLM error")})

  }

  colnames(items) <- c("exposure", "coefficient", "minE", "maxE", "p.value")
  
  items <- as.data.frame(items)
  
  # Add column with family
    # Retrive association of family - exposures
  assoc <- ds.familyNames(Set, TRUE)[[1]]
  assoc <- data.frame(family = assoc, exposure = names(assoc))
  
  items <- merge(assoc, items)
  
  items$coefficient <- as.numeric(as.character(items$coefficient))
  items$p.value <- as.numeric(as.character(items$p.value))
  items$minE <- as.numeric(as.character(items$minE))
  items$maxE <- as.numeric(as.character(items$maxE))
  
  
  if(tef){
    # Get threshold for effective tests from the study server
    cally <- paste0("effective.testsDS(", Set, ")")
    alpha_corrected <- DSI::datashield.aggregate(datasources, as.symbol(cally))[[1]]
  }
  else{
    alpha_corrected <- 0
  }
  
  # Remove created variables on the study server
  datashield.rm(datasources, "dta")
  datashield.rm(datasources, "dta_all")
  datashield.rm(datasources, "dta_exposures")
  
  return(list(exwas_results = items, alpha_corrected = alpha_corrected))

}
