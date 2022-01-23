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
#' - exwas_results: \code{data.frame} with exposure name, coefficient and p-value of the association \cr
#' - alpha_corrected: \code{numeric} effective tests
#'
#' @examples 
#' \dontrun{Refer to the package Vignette for examples.}
#' @export

ds.exwas <- function(model, Set, family, type = c("pooled", "split"), exposures_family = NULL, tef = TRUE, datasources = NULL) {
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  checkForExposomeSet(Set, datasources)
  
  # Subset the exposome set if exposures_family
  if(!is.null(exposures_family)){
    # Check that family exists
    if(!(exposures_family %in% ds.familyNames(Set)[[1]])){
      stop("[", exposures_family, "] is not a valid family name of the ExposomeSet [", Set, "]. Please check the valid family names using `ds.familyNames(", Set, ")`")
    }
    # Subset the ExposomeSet
    ds.exposomeSubset(Set, fam = exposures_family, name = paste0(Set, "_subsetted4dsEXWAS"))
    Set <- paste0(Set, "_subsetted4dsEXWAS")
  }

  # Extract table with exposures and phenotypes and save it on the server (assign)
  cally <- paste0("exposures_pData(", Set, ")")
  DSI::datashield.assign.expr(datasources, "dta", as.symbol(cally))

  # Source exposures list
  cally <- paste0("exposureNamesDS(", Set, ")")
  exposure_names <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  
  # Check that all servers have the same exposures, otherwise the linear models will fail
  exposure_names <- lapply(exposure_names, function(x){
    stringr::str_sort(x)
  })
  if(!all(Vectorize(identical, 'x')(exposure_names, exposure_names[[1]]))){
    stop("The different ExposomeSets have different exposures, check it using `ds.exposome_variables('", Set, "')`")
  }
  
  # Make sure wehave the model as a formula element
  form <- formula(model)

  items <- NULL
  for (exposure in exposure_names[[1]]) {# exposure_names) {
    # Build formula: pheno_objective ~ exposure + adjusting phenotypes
    frm <- as.formula(paste0(form[[2]], "~", exposure, "+", form[[3]]))
    
    tryCatch({
      if(type == "pooled"){
        # Fit GLM using the non-disclosive function
        mod <- ds.glm(frm, family = family, data = 'dta', viewIter = FALSE, datasources = datasources)
        items <- rbind(items, cbind(exposure, mod$coefficients[2, 1], mod$coefficients[2, 5], 
                                    mod$coefficients[2, 6], mod$coefficients[2, 4]))
      } else if (type == "split") {
        items <- append(items, lapply(datasources, function(x){
          name_x <- x@name
          x <- list(x)
          names(x) <- name_x
          mod <- ds.glm(frm, family = family, data = 'dta', viewIter = FALSE, datasources = x)$coefficients
          return(cbind(exposure, mod[2, 1], mod[2, 5], 
                       mod[2, 6], mod[2, 4]))
        }))
      } else {
        stop("Invalid type argument")
      }
    }, error = function(e){print(datashield.errors())})
  }
  
  if(type == "split"){
    items <- tapply(unlist(items, use.names = FALSE), rep(names(items), lengths(items)), FUN = function(x){
      res <- data.frame(matrix(x, ncol = 5, byrow = T))
      colnames(res) <- c("exposure", "coefficient", "minE", "maxE", "p.value")
      return(res)
    })
  } else if (type == "pooled") {
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
  }
  if(tef){
    # Get threshold for effective tests from the study server
    cally <- paste0("effective.testsDS(", Set, ")")
    alpha_corrected <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  }
  else{
    alpha_corrected <- 0
  }
  
  # Remove created variables on the study server
  datashield.rm(datasources, "dta")
  # datashield.rm(datasources, "dta_all")
  datashield.rm(datasources, "dta_exposures")
  datashield.rm(datasources, Set)
  datashield.rm(datasources, "ZEROES")
  
  return(list(exwas_results = items, alpha_corrected = if(type == "pooled"){alpha_corrected[[1]]}else{alpha_corrected}))

}

