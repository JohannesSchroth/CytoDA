#' dim_reduction_pca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dim_reduction_pca_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    
    sidebarLayout(
      
      sidebarPanel = sidebarPanel(
        
        selectInput(inputId = 'variables_pca', label = 'Variables', choices = c(), multiple = T),
        
        selectInput('dimension_num_pca', 'Number of Dimensions', c('2D', '3D')),
        
        selectInput('clustering_type', 'Select Clustering Algorithm', c('Rphenograph', 'ClusterX', 'DensVM'), multiple = T),
        
        actionButton('submit_variables_pca', 'Run'),
        
        width = 3),
      
      mainPanel = mainPanel(
        
        column(6,
               plotlyOutput('pca_plot', height = '500px', width = '500px') %>% 
                 shinycssloaders::withSpinner()),
        column(6,
               plotlyOutput('heatmap',height = '500px', width = '500px')),
        hr(),
        fluidRow(
          selectInput('colour_col1', label = 'Colour by:', c()))))
    
    
    
  )
}

#' dim_reduction_pca Server Function
#'
#' @noRd 
mod_dim_reduction_pca_server <- function(input, output, session, data){
  ns <- session$ns
  
  pca <- prcomp(as.matrix(data[ ,input$variables_pca]), center = TRUE, scale. = TRUE)
  
  rv$pca <- cbind(data, as.data.frame(pca$x[,1:3]))
  
  if (input$dimension_num_pca == '2D') { 
    
    output$pca_plot <- renderPlotly({
      
      plot_ly(data = rv$pca, x = rv$pca$PC1, y = rv$pca$PC2,
              marker = list(color = ~data[,input$colour_col1], colorscale = c('#FFE1A1', '#683531'))) %>%
        add_markers()
    })
    
  } else if (input$dimension_num_pca == '3D') {
    output$pca_plot <- renderPlotly({
      
      plot_ly(data = rv$pca, x = rv$pca$PC1, y = rv$pca$PC2, z = rv$pca$PC3,
              marker = list(color = ~data[,input$colour_col1], colorscale = c('#FFE1A1', '#683531'))) %>%
        add_markers()
    })
    
  }
  
  if(any(input$clustering_type == 'Rphenograph') == TRUE) {
    
    phenograph(variables = input$variables_pca, K = 30)
    
  } else {
    NULL
  }
}

## To be copied in the UI
# 

## To be copied in the server
# callModule(mod_dim_reduction_pca_server, "dim_reduction_pca_ui_1")

