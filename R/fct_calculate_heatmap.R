#' Function to calculate and render heatmap
#' 
#' 

calculate_heatmap <- function(input, output, session, data, vars, show_clus) {
  
  clus_heatmap <- renderPlotly({
    
    heat <- as.data.frame(sapply(data[,vars], normalise))
    
    heat <- data.frame(heat, 'Clusters' = data[,'Phenograph_Clusters'])
    
    heat <- heat %>%
      group_by(Clusters) %>%
      summarise_all(median) %>% 
      as.data.frame() 
    
    selected <- sapply(data[,vars],normalise)
    
    selected <- selected[which_selected(),] %>%
      as.data.frame() %>%
      summarise_all(median) %>%
      tibble::add_column(Clusters = 'Selected', .before = 1)
    
    if(length(selected[,1] > 1)) {
      heat <- rbind(heat, selected)
    }
    
    plot_ly(heat, x = colnames(heat)[-1], y = as.character(heat$Clusters),z = as.matrix(heat[,-1]), type = 'heatmap',
            colors = colorRamp(c("#4575B4",'#FFFFFF','#D73027'))) %>%
      layout(xaxis = list(type = "category"), 
             yaxis = list(type = "category"))
  })
  
  return(clus_heatmap)
  
}