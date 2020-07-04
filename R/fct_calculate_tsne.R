#' Function to calculate tSNE
#' 

calculate_tsne <- function(input, output, session, data, vars, dims, cluster_df) {
  
  set.seed(123)
  
  print(data[,vars])
  
  tsne <- Rtsne::Rtsne(X = data[,vars], dims = which_dim(dims), verbose = T,
                       perplexity = input$perplexity_tsne, 
                       theta = input$theta_tsne, 
                       max_iter = input$max_iter_tsne)
  
  colnames(tsne$Y) <- c('tSNE1', 'tSNE2', 'tSNE3')[1:which_dim(dims)]
  
  tsne <- as.data.frame(cbind(data, tsne$Y))
  
  if(is.null(cluster_df) != TRUE) {
    tsne <- cbind(tsne, cluster_df)
    }
  
  updateSelectInput(session = session, inputId = 'colour_col2', label = 'Colour by:', choices = colnames(tsne))
  
  output$tsne_plot <- renderPlotly({
    
    plot_ly(data = tsne, x = tsne$tSNE1, y = tsne$tSNE2, z = if (dims == '3D') tsne$tSNE3 else NULL) %>%
      add_markers(color = ~tsne[,input$colour_col2]) %>%
      layout(dragmode = 'lasso') %>%
      layout(legend = list(title=list(text=''), orientation = 'h'))
    })
  
  return(tsne)
  
  output$download_tsne_data <- downloadHandler(
    
    filename = function() {
      paste('tsne_data_', Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$tsne, file, row.names = FALSE)
    }
    
  )
  
}