


calculate_pca <- function(input, output, session, data, dims, vars, cluster) {
  
  pca <- prcomp(as.matrix(data[,vars]), center = TRUE, scale. = TRUE)
  
  pca <- cbind(data, as.data.frame(pca$x[,1:3]))
  
  if(is.null(cluster) != TRUE) {
    
    pca <- cbind(pca, cluster)
    
  }
  
  updateSelectInput(session = session,inputId = 'colour_col1', label = 'Colour by:', choices = colnames(pca))
 
  output$pca_plot <- renderPlotly({
    
    plot_ly(data = pca, x = pca$PC1, y = pca$PC2, z = if(dims == '3D'){pca$PC3}else{NULL},
            marker = list(color = ~pca[,input$colour_col1], colorscale = c('#FFE1A1', '#683531'))) %>%
      add_markers() %>%
      layout(dragmode = 'lasso')
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

