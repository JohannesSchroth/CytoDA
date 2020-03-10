#' dimreduction_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dimreduction_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' dimreduction_plots Server Function
#'
#' @noRd 
mod_dimreduction_plots_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_dimreduction_plots_ui("dimreduction_plots_ui_1")
    
## To be copied in the server
# callModule(mod_dimreduction_plots_server, "dimreduction_plots_ui_1")
 
