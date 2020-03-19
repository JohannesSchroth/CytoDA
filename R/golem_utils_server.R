#' Utility Functions


#' Store reactive values in rv list
rv <- reactiveValues()

#' Poltly selected function
which_selected <- function(){
  
  d <- event_data("plotly_selected")
  
  if (!is.null(d)) d$pointNumber else NA
  
}


which_dim <- function(x) {
  
  if (x == '2D') {
    return(2)
  } else if (x == '3D') {
    return(3)
  }
  
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


pairwise_plot <- function(data, mapping) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_hex(bins = 30) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())
  return(p)
}








