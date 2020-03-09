#' Utility Functions


#' Store reactive values in rv list
rv <- reactiveValues()

#' Poltly selected function
which <- function(){
  
  d <- event_data("plotly_selected")
  
  if (!is.null(d)) d$pointNumber else NA
  
}








