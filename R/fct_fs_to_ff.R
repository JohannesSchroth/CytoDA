#' Function to convert flowSet to flowFrame
#' @param flowSet imput flowSet object
#' @return flowFrame
#' 

fs_to_ff <- function(fs = NULL) {
  
  if (length(fs) > 1) {
    s <- fsApply(fs, function(x){
      
      x %>%
        flowCore::exprs() %>%
        nrow()
      
    })
    
    sample_id <- sapply(1:nrow(s), function(i) rep(i,s[i])) %>%
      unlist()
    
    ff <- fs %>%
      fsApply(FUN = flowCore::exprs) %>%
      cbind(SampleID = sample_id) %>%
      flowFrame()
    
    return(ff)
    
  } else {
    
    ff <- flowCore::flowFrame(flowCore::exprs(fs[[1]]))
    
    return(ff)
    
  }
}