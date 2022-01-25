#' Function to calculate and render heatmap
#' 
#' 

calculate_heatmap <- function(input, output, session, data, vars, show_clus) {
  
  clus_heatmap <- renderPlotly({
    norm <- data[,vars]
    
    heat <- data.frame(norm, 'Clusters' = data[,'Rphenograph_Clusters'], check.names = FALSE)
    
    heat <- heat %>%
      group_by(Clusters) %>%
      summarise_all(median) %>% 
      as.data.frame() 

    # selected <- norm[which_selected(),] %>%
    #   summarise_all(median) %>%
    #   tibble::add_column(Clusters = 'Selected', .before = 1) %>%
    #   as.data.frame()
    # 
    # if(length(selected[,1] > 1)) {
    #   heat <- rbind(heat, selected)
    # }

    plot_ly(heat, x = colnames(heat)[-1], y = as.character(heat$Clusters),z = as.matrix(heat[,-1]), type = 'heatmap',
            colors = colorRamp(c("#4575B4",'#FFFFFF','#D73027'))) %>%
      layout(xaxis = list(type = "category"), 
             yaxis = list(type = "category"), font = list(size = 25))
  })
  
  return(clus_heatmap)
  
}
