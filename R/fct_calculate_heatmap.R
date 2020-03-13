#' Function to calculate and render heatmap
#' 
#' 

calculate_heatmap <- function(input, output, session, data, variables, show_clus) {
  
  heat <- as.data.frame(sapply(data[,variables], normalise))

  heat <- cbind(heat, 'Clusters' = data[,'Phenograph_Clusters'])

  print(heat)
    
  heat <- heat %>%
    group_by(Clusters) %>%
    summarise_all(median) %>% 
    as.data.frame() 
  
  print(heat)
  
  selected <- sapply(data[,variables],normalise)
  
  selected <- selected[which_selected(),] %>%
    as.data.frame() %>%
    summarise_all(median) %>%
    tibble::add_column(Clusters = 'Selected', .before = 1)
  
  heat <- rbind(heat, selected)
  
  clus_heatmap <- renderPlotly({
    
    plot_ly(heat, x = colnames(heat)[-1], y = as.character(heat$Clusters),z = as.matrix(heat[,-1]), type = 'heatmap',
            colors = colorRamp(c("#4575B4",'#FFFFFF','#D73027'))) %>%
      layout(xaxis = list(type = "category"), 
             yaxis = list(type = "category"))
  })
  
  return(clus_heatmap)
  
}