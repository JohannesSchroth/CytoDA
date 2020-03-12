#' Utility Functions


#' Store reactive values in rv list
rv <- reactiveValues()

#' Poltly selected function
which_selected <- function(){
  
  d <- event_data("plotly_selected")
  
  if (!is.null(d)) d$pointNumber else NA
  
}


normalise <- function(data) {
  
  dat <- c()
  
  q <- quantile(data, c(0.01, 0.99))
  
  for (j in 1:length(data)) {
    dat[j] <- (data[j] - q[1])/(q[2]-q[1])
  }
  dat[dat < 0] <- 0
  dat[dat > 1] <- 1
  
  return(as.numeric(dat))
  
}








