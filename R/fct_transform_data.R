#' Function to transform data
#' @param ff Input flowframe to be transformed
#' @param method transformation method
#' @return transformed flowframe

transform_data <- function(ff = NULL, method = NULL) {
  
  vars <- flowCore::colnames(ff)[-grep('SSC|FSC|Time|Sample|Group', flowCore::colnames(ff))]
  
  if (method == 'Logicle') {
    
    ff <- flowCore::transform(ff, flowCore::estimateLogicle(ff, channels = vars))
    
    return(ff)
    
  } else if (method == 'Biexponential') {
   
    ff <- flowCore::transform(ff, flowCore::transformList(vars, biexponentialTransform()))
    
    return(ff)
    
  } else {
    
    return(ff)
    
  }
  
}