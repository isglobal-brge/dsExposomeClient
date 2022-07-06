################################################################################
#' @title Maturity offset estimation (pubertal development)
#' @description Calculate maturity offset according to the equations of Moore et al. 2015 (10.1249/MSS.0000000000000588)
#' @param data Dataset name (it must be a data frame)
#' @param sex Name of the sex variable in the input dataset (0 for males and 1 for females)
#' @param age Name of the Age variable in the input dataset (in days)
#' @param height Name of the Height variable in the input dataset (in centimeters)
#' @param new.obj Name of the output object containing maturity offset estimations (default: "var_maturityoffset")
#' @param datasources details for DS connections

#' @return Estimated maturity offset according to Moore's equations.
#' @details More details can be found at Moore et al. 2015 (10.1249/MSS.0000000000000588)

#' @export  
#' @author  Augusto Anguita-Ruiz & Xavier Escribá
################################################################################

ds.maturityoffset = function(data,sex,age,height,new.obj=NULL,datasources=NULL){

  if(is.null(new.obj)){
    new.obj <- "var_maturityoffset"
  }
  
  if (all(!is.null(data) & !is.null(sex) & !is.null(age) & !is.null(height))) {
  
  	if (!is.character(sex) | !is.character(age) | !is.character(height)) {
  	 
  		print("Error: The name of the input variables must be a character string.") 
  		break;
  		
  		}
  	
  	datanames=ds.names(deparse(substitute(data)))[[1]]
  	  
  	if (any(!c(sex,age,height) %in% datanames)) {
  	
  		print("Error: Please, correctly define the exact variable names for sex, age and height in the input dataset.") 
  		break;
  		
  		}
  	
  	} else {
  	
    		print("Error: Please, specify all required arguments to be passed to the function.") 
  		break;
  		
   		}


  cally <- call("maturityoffsetDS",deparse(substitute(data)), sex=sex,age=age,height=height)
  
  DSI::datashield.assign.expr(datasources, new.obj, cally)
    
  }
  
