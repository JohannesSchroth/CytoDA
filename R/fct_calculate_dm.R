#' Function to calculate Diffusion Map
#' 


calculate_dm <- function(input, output, session, data, vars, dims) {
  
  dm <- destiny::DiffusionMap(data[,vars])
  dm <- destiny::DPT(dm)
  rv$dm <- cbind(data, data.frame('DC1' = dm$DC1, 'DC2' = dm$DC2, 'DC3' = dm$DC3))
  d <- gsub('\\D','\\1', dims)
  output$dm_plot <- renderPlotly({
    
    plot_ly(data = rv$dm, x = rv$dm$DC1, y = rv$dm$DC2, z = if(d == 3){rv$dm$DC3}else{NULL},
            marker = list(color = ~rv$pca[,input$colour_col_dm], colorscale = c('#FFE1A1', '#683531'))) %>%
      add_markers()
  })
  
  
  
  
  
}