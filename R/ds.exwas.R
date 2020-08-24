#' @title Performs a non-disclosive EXposome-Wide Association Study
#' 
#' @description Takes as input an Exposome Set object on the study server and performs a 
#' ExWAS association study with the provided model.
#'
#' @param model \code{character} Formula, not including exposures, to be tested.
#' @param Set \code{character} Name of the Exposome Set object on the server side
#' @param family \code{character} Nature of the health outcome
#' @param datasources  a list of \code{\link{DSConnection-class}} objects obtained after login
#'
#' @return \code{list} with: \code{data.frame} With exposure name and p-value of the association,
#' and \code{numeric} effective tests
#' @export
#'
#' @examples 
#' \dontrun{Refer to the package Vignette for examples.}
#' 

ds.exwas <- function(model, Set, family, datasources = NULL) {

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
      items <- rbind(items, cbind(exposure, mod$coefficients[2]))
    }, error = function(e){print("lesgo")})

  }

return(items)

}