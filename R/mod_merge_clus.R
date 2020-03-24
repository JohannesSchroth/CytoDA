#' merge_clus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_merge_clus_ui <- function(id, clus_col_mod){
  ns <- NS(id)
  
  tags$div(
    id=id,
    fluidRow(
      column(3,
             textInput(
               ns('name'), 
               label = 'Name',
               value = id)
      ),
      column(3,
             selectInput(
               ns('Clus'),
               label = 'Select clusters to merge', 
               choices = clus_col_mod, 
               multiple = T)
             
      )
    )
  )
}

#' merge_clus Server Function
#'
#' @noRd 
clusters_server <- function(input, output, session) {
  
  return_value <- reactive({input$Clus})
  
  list(return_value)
  
}
names_server <- function(input, output, session) {
  
  names <- reactive({input$name})
  list(names)
}

