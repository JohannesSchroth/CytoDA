

calculate_pca <- function(input, output, session, data, dims, vars) {
  
  withProgress(message = 'Calculating PCA', value = 0, {
    
    incProgress(0.2)
    
    pca <- data[,vars] %>%
      as.matrix() %>%
      prcomp(center = TRUE, scale. = TRUE)
    
    incProgress(0.8)
    
    pca <- cbind(data, as.data.frame(pca$x[,1:3]))
    
    updateSelectInput(session = session, inputId = 'colour_col1', label = 'Colour by:', choices = colnames(pca), selected = 'Rphenograph_Clusters')
    
    output$pca_plot <- renderPlotly({
      
      plot_ly(data = pca, x = pca$PC1, y = pca$PC2, z = if(dims == '3D') pca$PC3 else NULL) %>%
        add_markers(color = ~pca[,input$colour_col1]) %>%
        layout(dragmode = 'lasso') %>%
        layout(font = list(size = 25)) %>%
        hide_colorbar() 
    })
    incProgress(1)
  })
  
  output$download_pca_data <- downloadHandler(
    
    filename = function() {
      paste('pca_data_', Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pca, file, row.names = FALSE)
    }
    
  )
  
  return(pca)
}

