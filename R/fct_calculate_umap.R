#' Function to calculate UMAP
#' 

calculate_umap <- function(input, output, session, data, vars, dims, cluster_df) {
  
  withProgress(message = 'Calculating tSNE', value = 0, {
    
    set.seed(123)
    
    custom <- umap.defaults
    custom$verbose <- T
    custom$n_neighbors <- input$n_neighbours_umap
    custom$min_dist <- input$min_dist_umap
    custom$n_components <- which_dim(dims)
    
    incProgress(0.2)
    
    umap <- umap(data[,vars], config = custom)
    
    incProgress(0.8)
    
    colnames(umap$layout) <- c('UMAP1', 'UMAP2', 'UMAP3')[1:which_dim(dims)]
    umap <- as.data.frame(cbind(data, umap$layout))
    
    if(is.null(cluster_df) != TRUE) {
      umap <- cbind(umap, cluster_df)
    }
    
    updateSelectInput(session = session, inputId = 'colour_col3', label = 'Colour by:', choices = colnames(umap), selected = 'Rphenograph_Clusters')
    
    output$umap_plot <- renderPlotly({
      plot_ly(data = umap, x = umap$UMAP1, y = umap$UMAP2, z = if(dims == '3D') umap$UMAP3 else NULL) %>%
        add_markers(color = ~umap[,input$colour_col3]) %>%
        layout(dragmode = 'lasso') %>%
        layout(font = list(size = 25)) %>%
        hide_colorbar() 
    })
    
    incProgress(1)
    
  })
  output$download_umap_data <- downloadHandler(
    
    filename = function() {
      paste('umap_data_',Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(umap, file, row.names = FALSE)
    }
    
  )
  return(umap)
}